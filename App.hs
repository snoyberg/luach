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
import Yesod.Mail
import System.Random
import Data.Time
import System.Locale
import Data.Time.Calendar.Hebrew (toHebrew)

data Luach = Luach
    { getStatic :: Static
    , connPool :: Settings.ConnectionPool
    }
type Handler = GHandler Luach Luach

mkYesod "Luach" [$parseRoutes|
/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ RootR GET
/auth AuthR Auth getAuth
/static StaticR Static getStatic
/event EventsR GET POST
/event/#EventId EventR GET POST
/event/#EventId/delete DeleteEventR POST
/settings/feedid FeedIdR GET
/feed/#String FeedR GET
/day/#String DayR GET
|]
instance Yesod Luach where
    approot _ = Settings.approot
    defaultLayout w = do
        y <- getYesod
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            w
            addScriptEither $ urlJqueryJs y
            addScriptEither $ urlJqueryUiJs y
            addStylesheetEither $ urlJqueryUiCss y
            addJavascript $(juliusFile "analytics")
            addStyle $(cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")
    authRoute _ = Just RootR
instance YesodAuth Luach where
    type AuthEntity Luach = User
    type AuthEmailEntity Luach = User -- ignored

    defaultDest _ = EventsR

    getAuthId creds _extra = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                stdgen <- liftIO newStdGen
                let fid' = fst $ randomString 10 stdgen
                fmap Just $ insert $ User (credsIdent creds) fid'

    rpxnowSettings _ = Just $ RpxnowSettings "luach" "c8605aed1d6e38a58b180efd966bd7476b9a8e4c"
instance YesodPersist Luach where
    type YesodDB Luach = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db
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
                addJavascript $(juliusFile "analytics")
            hamletToRepHtml $(hamletFile "homepage")
        Just _ -> redirect RedirectTemporary EventsR

getEventsR :: Handler RepHtmlJson
getEventsR = do
    (uid, user) <- requireAuth
    es <- runDB $ selectList [EventUserEq uid] [EventDayAsc, EventTitleAsc] 0 0
    os <- liftIO $ getOccurrencesIO $ map snd es
    render <- getUrlRender
    let json = jsonMap
            [ ("events", jsonList $ map (eventToJson render) es)
            , ("upcoming", occurrencesToJson os)
            ]
    (_, wform, enctype) <- runFormGet $ eventFormlets uid Nothing
    let html = do
            setTitle "Events"
            form <- extractBody wform
            addBody $(hamletFile "events")
            addStyle $(cassiusFile "events")
            addJavascript $(juliusFile "events")
    defaultLayoutJson html json
  where
    notOne 1 = False
    notOne _ = True

postEventsR :: Handler RepHtml
postEventsR = do
    uid <- requireAuthId
    (res, wform, enctype) <- runFormPost $ eventFormlets uid Nothing
    case res of
        FormSuccess e -> do
            _ <- runDB $ insert e
            setMessage "New event created"
            redirect RedirectTemporary EventsR
        _ -> defaultLayout $ do
            setTitle "Add Event"
            form <- extractBody wform
            addBody $(hamletFile "post-events")

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
    (res, wform, enctype) <- runFormPost $ eventFormlets uid $ Just e
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
        addBody $(hamletFile "edit-event")

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
                    let fid' = fst $ randomString 10 stdgen
                    runDB $ update uid [UserFeedId fid']
                    return fid'
                else return $ userFeedId u
    render <- getUrlRender
    jsonToRepJson $ jsonMap
        [ ("feedUrl", jsonScalar $ render $ FeedR fid)
        ]

getFeedR :: String -> Handler RepAtom
getFeedR fid = do
    mu <- runDB $ getBy $ UniqueFeedId fid
    (uid, _) <- maybe notFound return mu
    es <- runDB $ selectList [EventUserEq uid] [] 0 0
    now <- liftIO getCurrentTime
    os <- liftIO $ getOccurrencesIO $ map snd es
    atomFeed $ AtomFeed
        { atomTitle = "Your Luach Reminders"
        , atomLinkSelf = FeedR fid
        , atomLinkHome = RootR
        , atomUpdated = now
        , atomEntries = map (helper now) os
        }
  where
     helper now (d, os) = AtomFeedEntry
        { atomEntryLink = DayR $ show d
        , atomEntryUpdated = now
        , atomEntryTitle = "Luach reminders for " ++ prettyDate d
        , atomEntryContent = [$hamlet|
%ul
    $forall os o
        %li $otitle.o$ is $show.years.o$ years on the $show.calendarType.o$ calendar
|] id
        }

getDayR :: String -> Handler ()
getDayR _ = redirect RedirectTemporary EventsR

withLuach :: (Application -> IO a) -> IO a
withLuach f = Settings.withConnectionPool $ \p -> do
    flip Settings.runConnectionPool p $ runMigration $ do
        migrate (undefined :: User)
        migrate (undefined :: Event)
    let h = Luach s p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt

eventToJson :: (LuachRoute -> String) -> (EventId, Event) -> Json
eventToJson render (eid, e) = jsonMap
    [ ("title", jsonScalar $ encodeHtml $ eventTitle e)
    , ("rawtitle", jsonScalar $ eventTitle e)
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
