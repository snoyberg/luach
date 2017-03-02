{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
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
    , resourcesLuach
    ) where

import Yesod
import Yesod.Auth
import Yesod.Auth.Rpxnow
import Yesod.Static
import Yesod.AtomFeed
import Yesod.Form.Jquery
import Model
import Occurrence
import qualified Settings
import Settings (hamletFile, juliusFile, cassiusFile)
import Database.Persist.Sql
import StaticFiles
import Control.Monad
import Network.Mail.Mime
import System.Random
import Data.Time
import Data.Time.Calendar.Hebrew (toHebrew)
import Data.Text (Text, pack, unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import Control.Monad.Logger

data Luach = Luach
    { getStatic :: Static
    , connPool :: Settings.ConnectionPool
    , theApproot :: Text
    , httpManager :: Manager
    }
type LuachRoute = Route Luach
instance RenderMessage Luach FormMessage where
    renderMessage _ _ = defaultFormMessage

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
    approot = ApprootMaster theApproot
    defaultLayout w = do
        y <- getYesod
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            w
            addScriptEither $ urlJqueryJs y
            addScriptEither $ urlJqueryUiJs y
            addStylesheetEither $ urlJqueryUiCss y
            toWidget $(juliusFile "analytics")
            toWidget $(cassiusFile "default-layout")
        withUrlRenderer $(Settings.hamletFile "default-layout")
    authRoute _ = Just RootR

    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend 120 "config/client-session-key.aes"
instance YesodAuth Luach where
    type AuthId Luach = UserId

    loginDest _ = EventsR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                stdgen <- liftIO newStdGen
                let fid' = pack $ fst $ randomString 10 stdgen
                fmap Just $ insert $ User (credsIdent creds) fid'

    authPlugins _ = [authRpxnow "luach" "c8605aed1d6e38a58b180efd966bd7476b9a8e4c"]
    authHttpManager = httpManager
instance YesodPersist Luach where
    type YesodPersistBackend Luach = SqlBackend
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db
instance YesodAuthPersist Luach where
    type AuthEntity Luach = User

instance YesodJquery Luach where
    urlJqueryUiCss _ = Right "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/swanky-purse/jquery-ui.css"

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "static/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("" :: String)

getRootR :: Handler Html
getRootR = do
    x' <- maybeAuthId
    case x' of
        Nothing -> do
            y <- getYesod
            pc <- widgetToPageContent $ do
                setTitle "Luach by Yesod Web Development"
                addStylesheet $ StaticR homepage_css
                addStylesheetEither $ urlJqueryUiCss y
                toWidget $(juliusFile "analytics")
            withUrlRenderer $(hamletFile "homepage")
        Just _ -> redirect EventsR

getEventsR :: Handler TypedContent
getEventsR = do
    Entity uid user <- requireAuth
    es <- runDB $ selectList [EventUser ==. uid] [Asc EventDay, Asc EventTitle]
    os <- liftIO $ getOccurrencesIO $ map entityVal es
    render <- getUrlRender
    let json = object
            [ "events" .= array (map (eventToJson (unpack . render)) es)
            , "upcoming" .= occurrencesToJson os
            ]
    ((_, wform), enctype) <- runFormGet $ eventFormlets uid Nothing
    let html = do
            setTitle "Events"
            $(whamletFile "hamlet/events.hamlet")
            toWidget $(cassiusFile "events")
            toWidget $(juliusFile "events")
    defaultLayoutJson html (return json)
  where
    notOne 1 = False
    notOne _ = True

postEventsR :: Handler Html
postEventsR = do
    uid <- requireAuthId
    ((res, wform), enctype) <- runFormPostNoToken $ eventFormlets uid Nothing
    case res of
        FormSuccess e -> do
            _ <- runDB $ insert e
            setMessage "New event created"
            redirect EventsR
        _ -> defaultLayout $ do
            setTitle "Add Event"
            $(whamletFile "hamlet/post-events.hamlet")

eventFormlets :: UserId -> Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventFormlets uid e = renderTable $ Event
    <$> pure uid
    <*> areq textField "Title" (eventTitle <$> e)
    <*> areq dayField "Date" (eventDay <$> e)
    <*> areq checkBoxField "English" (eventGregorian <$> e)
    <*> areq checkBoxField "Hebrew" (eventHebrew <$> e)
    <*> areq checkBoxField "After sunset" (eventAfterSunset <$> e)

getEventR :: EventId -> Handler Html
getEventR eid = do
    uid <- requireAuthId
    e <- runDB $ get404 eid
    unless (uid == eventUser e) notFound
    ((res, wform), enctype) <- runFormPostNoToken $ eventFormlets uid $ Just e
    case res of
        FormSuccess e' -> do
            runDB $ replace eid e'
            setMessage "Event updated"
            redirect EventsR
        _ -> return ()
    defaultLayout $ do
        setTitle "Edit Event"
        $(whamletFile "hamlet/edit-event.hamlet")

postEventR :: EventId -> Handler Html
postEventR = getEventR

postDeleteEventR :: EventId -> Handler ()
postDeleteEventR eid = do
    uid <- requireAuthId
    e <- runDB $ get404 eid
    unless (uid == eventUser e) notFound
    runDB $ delete eid
    setMessage "The event has been deleted"
    redirect RootR

getFeedIdR :: Handler Value
getFeedIdR = do
    Entity uid u <- requireAuth
    forceReset <- runInputGet $ ireq boolField "forceReset"
    fid <- if forceReset
                then do
                    stdgen <- liftIO newStdGen
                    let fid' = pack $ fst $ randomString 10 stdgen
                    runDB $ update uid [UserFeedId =. fid']
                    return fid'
                else return $ userFeedId u
    render <- getUrlRender
    returnJson $ object
        [ "feedUrl" .= (render $ FeedR fid)
        ]

getFeedR :: Text -> Handler RepAtom
getFeedR fid = do
    mu <- runDB $ getBy $ UniqueFeedId fid
    Entity uid _ <- maybe notFound return mu
    es <- runDB $ selectList [EventUser ==. uid] []
    now <- liftIO getCurrentTime
    os <- liftIO $ getOccurrencesIO $ map entityVal es
    atomFeed $ Feed
        { feedTitle = "Your Luach Reminders"
        , feedAuthor = "Michael Snoyman"
        , feedLinkSelf = FeedR fid
        , feedLinkHome = RootR
        , feedUpdated = now
        , feedEntries = map (helper now) os
        , feedDescription = "Your Luach Reminders"
        , feedLanguage = "en"
        , feedLogo = Nothing
        }
  where
     helper now (d, os) = FeedEntry
        { feedEntryLink = DayR $ pack $ show d
        , feedEntryEnclosure = Nothing
        , feedEntryUpdated = now
        , feedEntryTitle = pack $ "Luach reminders for " ++ prettyDate d
        , feedEntryContent = [shamlet|\
<ul>
    $forall o <- os
        <li>#{otitle o} is #{show (years o)} years on the #{show (calendarType o)} calendar
|]
        }

getDayR :: Text -> Handler ()
getDayR _ = redirect EventsR

withLuach :: Text -> (Application -> IO a) -> IO a
withLuach ar f = runStdoutLoggingT $ Settings.withConnectionPool $ \p -> do
    flip Settings.runConnectionPool p $ runMigration migrateAll
    s <- liftIO $ static Settings.staticdir
    m <- liftIO $ newManager tlsManagerSettings
    let h = Luach s p ar m
    liftIO $ toWaiApp h >>= f

eventToJson :: (LuachRoute -> String) -> Entity Event -> Value
eventToJson render (Entity eid e) = object
    [ "title" .= renderHtml (toHtml $ eventTitle e)
    , "rawtitle" .= (eventTitle e)
    , "day" .= (show $ eventDay e)
    , "prettyday" .= (prettyDate' $ eventDay e)
    , ("reminders", array $
        (if eventGregorian e then ["Gregorian"] else []) ++
        (if eventHebrew e then ["Hebrew" :: Text] else []))
    , "sunset" .= (if eventAfterSunset e then "true" else "false" :: Text)
    , "updateUrl" .= (render $ EventR eid)
    , "deleteUrl" .= (render $ DeleteEventR eid)
    ]

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %e, %Y"

showHDay :: Event -> String
showHDay e =
    show $ toHebrew $ f (eventDay e) $ eventAfterSunset e
  where
    f d False = d
    f d True = addDays 1 d

addthis :: HtmlUrl LuachRoute
addthis = $(hamletFile "add-this")
