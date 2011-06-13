{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module App
    ( withLuach
    ) where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.Rpxnow
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Yesod.Form.Jquery
import Model
import Occurrence
import qualified Settings
import Settings (hamletFile, juliusFile, cassiusFile)
import Database.Persist.GenericSql
import StaticFiles
import Control.Applicative
import Control.Monad
import Web.Encodings
import Network.Mail.Mime
import System.Random
import Data.Time
import System.Locale
import Data.Time.Calendar.Hebrew (toHebrew)
import Data.Text (Text, pack, unpack)
import Data.Monoid (mempty)

data Luach = Luach
    { getStatic :: Static
    , connPool :: Settings.ConnectionPool
    , theApproot :: Text
    }
type Handler = GHandler Luach Luach

mkYesod "Luach" [parseRoutes|
/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ RootR GET
/auth AuthR Auth getAuth
/static StaticR Static getStatic
/event EventsR GET POST
/event/#EventId EventR GET POST
/event/#EventId/delete DeleteEventR POST
/settings/feedid FeedIdR GET
/feed/#Text FeedR GET
/day/#Text DayR GET
|]
instance Yesod Luach where
    approot = theApproot
    defaultLayout w = do
        y <- getYesod
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            w
            addScriptEither $ urlJqueryJs y
            addScriptEither $ urlJqueryUiJs y
            addStylesheetEither $ urlJqueryUiCss y
            addJulius $(juliusFile "analytics")
            addCassius $(cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")
    authRoute _ = Just RootR
instance YesodAuth Luach where
    type AuthId Luach = UserId

    loginDest _ = EventsR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                stdgen <- liftIO newStdGen
                let fid' = pack $ fst $ randomString 10 stdgen
                fmap Just $ insert $ User (credsIdent creds) fid'

    authPlugins = [authRpxnow "luach" "c8605aed1d6e38a58b180efd966bd7476b9a8e4c"]
instance YesodPersist Luach where
    type YesodDB Luach = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodJquery Luach where
    urlJqueryUiCss _ = Right "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/swanky-purse/jquery-ui.css"

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "static/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("" :: String)

getRootR :: Handler RepHtml
getRootR = do
    x <- maybeAuthId
    case x of
        Nothing -> do
            y <- getYesod
            pc <- widgetToPageContent $ do
                setTitle "Luach by Yesod Web Development"
                addStylesheet $ StaticR homepage_css
                addStylesheetEither $ urlJqueryUiCss y
                addJulius $(juliusFile "analytics")
            hamletToRepHtml $(hamletFile "homepage")
        Just _ -> redirect RedirectTemporary EventsR

getEventsR :: Handler RepHtmlJson
getEventsR = do
    (uid, user) <- requireAuth
    es <- runDB $ selectList [EventUserEq uid] [EventDayAsc, EventTitleAsc] 0 0
    os <- liftIO $ getOccurrencesIO $ map snd es
    render <- getUrlRender
    let json = jsonMap
            [ ("events", jsonList $ map (eventToJson (unpack . render)) es)
            , ("upcoming", occurrencesToJson os)
            ]
    (_, wform, enctype) <- runFormGet $ eventFormlets uid Nothing
    let html = do
            setTitle "Events"
            form <- extractBody wform
            addHamlet $(hamletFile "events")
            addCassius $(cassiusFile "events")
            addJulius $(juliusFile "events")
    defaultLayoutJson html json
  where
    notOne 1 = False
    notOne _ = True

postEventsR :: Handler RepHtml
postEventsR = do
    uid <- requireAuthId
    (res, wform, enctype) <- runFormPostNoNonce $ eventFormlets uid Nothing
    case res of
        FormSuccess e -> do
            _ <- runDB $ insert e
            setMessage "New event created"
            redirect RedirectTemporary EventsR
        _ -> defaultLayout $ do
            setTitle "Add Event"
            form <- extractBody wform
            addHamlet $(hamletFile "post-events")

eventFormlets :: UserId -> Formlet s Luach Event
eventFormlets uid e = fieldsToTable $ Event
    <$> pure uid
    <*> stringField "Title" (eventTitle <$> e)
    <*> dayField "Date" (eventDay <$> e)
    <*> boolField "English" (eventGregorian <$> e)
    <*> boolField "Hebrew" (eventHebrew <$> e)
    <*> boolField "After sunset" (eventAfterSunset <$> e)

getEventR :: EventId -> Handler RepHtml
getEventR eid = do
    uid <- requireAuthId
    e <- runDB $ get404 eid
    unless (uid == eventUser e) notFound
    (res, wform, enctype) <- runFormPostNoNonce $ eventFormlets uid $ Just e
    case res of
        FormSuccess e' -> do
            runDB $ replace eid e'
            setMessage "Event updated"
            redirect RedirectTemporary EventsR
        _ -> return ()
    y <- getYesod
    defaultLayout $ do
        setTitle "Edit Event"
        form <- extractBody wform
        addHamlet $(hamletFile "edit-event")

postEventR :: EventId -> Handler RepHtml
postEventR = getEventR

postDeleteEventR :: EventId -> Handler ()
postDeleteEventR eid = do
    uid <- requireAuthId
    e <- runDB $ get404 eid
    unless (uid == eventUser e) notFound
    runDB $ delete eid
    setMessage "The event has been deleted"
    redirect RedirectTemporary RootR

getFeedIdR :: Handler RepJson
getFeedIdR = do
    (uid, u) <- requireAuth
    forceReset <- runFormGet' $ boolInput "forceReset"
    fid <- if forceReset
                then do
                    stdgen <- liftIO newStdGen
                    let fid' = pack $ fst $ randomString 10 stdgen
                    runDB $ update uid [UserFeedId fid']
                    return fid'
                else return $ userFeedId u
    render <- getUrlRender
    jsonToRepJson $ jsonMap
        [ ("feedUrl", jsonScalar $ unpack $ render $ FeedR fid)
        ]

getFeedR :: Text -> Handler RepAtom
getFeedR fid = do
    mu <- runDB $ getBy $ UniqueFeedId fid
    (uid, _) <- maybe notFound return mu
    es <- runDB $ selectList [EventUserEq uid] [] 0 0
    now <- liftIO getCurrentTime
    os <- liftIO $ getOccurrencesIO $ map snd es
    atomFeed $ Feed
        { feedTitle = "Your Luach Reminders"
        , feedLinkSelf = FeedR fid
        , feedLinkHome = RootR
        , feedUpdated = now
        , feedEntries = map (helper now) os
        , feedDescription = "Your Luach Reminders"
        , feedLanguage = "en"
        }
  where
     helper now (d, os) = FeedEntry
        { feedEntryLink = DayR $ pack $ show d
        , feedEntryUpdated = now
        , feedEntryTitle = pack $ "Luach reminders for " ++ prettyDate d
        , feedEntryContent = [hamlet|\
<ul>
    $forall o <- os
        <li>#{otitle o} is #{show (years o)} years on the #{show (calendarType o)} calendar
|]
        }

getDayR :: Text -> Handler ()
getDayR _ = redirect RedirectTemporary EventsR

withLuach :: Text -> (Application -> IO a) -> IO a
withLuach ar f = Settings.withConnectionPool $ \p -> do
    flip Settings.runConnectionPool p $ runMigration $ do
        migrate (undefined :: User)
        migrate (undefined :: Event)
    let h = Luach s p ar
    toWaiApp h >>= f
  where
    s = static Settings.staticdir

eventToJson :: (LuachRoute -> String) -> (EventId, Event) -> Json
eventToJson render (eid, e) = jsonMap
    [ ("title", jsonScalar $ encodeHtml $ unpack $ eventTitle e)
    , ("rawtitle", jsonScalar $ unpack $ eventTitle e)
    , ("day", jsonScalar $ show $ eventDay e)
    , ("prettyday", jsonScalar $ prettyDate' $ eventDay e)
    , ("reminders", jsonList $
        (if eventGregorian e then [jsonScalar "Gregorian"] else []) ++
        (if eventHebrew e then [jsonScalar "Hebrew"] else []))
    , ("sunset", jsonScalar $ if eventAfterSunset e then "true" else "false")
    , ("updateUrl", jsonScalar $ render $ EventR eid)
    , ("deleteUrl", jsonScalar $ render $ DeleteEventR eid)
    ]

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %e, %Y"

showHDay :: Event -> String
showHDay e =
    show $ toHebrew $ f (eventDay e) $ eventAfterSunset e
  where
    f d False = d
    f d True = addDays 1 d

addthis :: Hamlet LuachRoute
addthis = $(hamletFile "add-this")
