{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module App
    ( withLuach
    ) where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Model
import Occurrence
import qualified Settings
import Settings (hamletFile, juliusFile, cassiusFile)
import Database.Persist.GenericSql
import StaticFiles
import Control.Applicative
import Control.Monad
import Web.Encodings

data Luach = Luach
    { getStatic :: Static
    , connPool :: Settings.ConnectionPool
    }
type Handler = GHandler Luach Luach

mkYesod "Luach" [$parseRoutes|
/ HomepageR GET
/auth AuthR Auth getAuth
/static StaticR Static getStatic
/event EventsR GET POST
/event/#EventId EventR GET POST
/event/#EventId/delete DeleteEventR POST
/settings/feedid FeedIdR GET
/feed/#String FeedR GET
|]
instance Yesod Luach where
    approot _ = Settings.approot
instance YesodAuth Luach where
    type AuthEntity Luach = User
    type AuthEmailEntity Luach = User -- ignored

    defaultDest _ = HomepageR

    getAuthId creds _extra = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User $ credsIdent creds

    rpxnowSettings _ = Just $ RpxnowSettings "luach" "c8605aed1d6e38a58b180efd966bd7476b9a8e4c"
instance YesodPersist Luach where
    type YesodDB Luach = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db

getHomepageR :: Handler RepHtml
getHomepageR = do
    pc <- widgetToPageContent $ do
        addScriptRemote "http://cdn.jquerytools.org/1.1.1/full/jquery.tools.min.js"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.2/jquery-ui.min.js"
        addJavascript $(juliusFile "home")
        addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.0/themes/start/jquery-ui.css"
        addStyle $(cassiusFile "home")
    hamletToRepHtml $(hamletFile "home")

getEventsHelper :: UserId -> Handler RepJson
getEventsHelper uid = do
    es <- runDB $ selectList [EventUserEq uid] [EventDayAsc, EventTitleAsc] 0 0
    os <- liftIO $ getOccurrencesIO $ map snd es
    render <- getUrlRender
    jsonToRepJson $ jsonMap
        [ ("events", jsonList $ map (eventToJson render) es)
        , ("upcoming", occurrencesToJson os)
        ]

getEventsR :: Handler RepJson
getEventsR = requireAuthId >>= getEventsHelper

putEventHelper :: Maybe EventId -> Handler RepJson
putEventHelper meid = do
    uid <- requireAuthId
    e <- runFormPost' $ Event
            <$> pure uid
            <*> stringInput "title"
            <*> dayInput "day"
            <*> boolInput "remindGreg"
            <*> boolInput "remindHebrew"
            <*> boolInput "afterSunset"
    liftIO $ print e
    rr <- getRequest
    (pp, _) <- liftIO $ reqRequestBody rr
    liftIO $ print pp
    runDB $ case meid of
        Nothing -> insert e >> return ()
        Just eid -> replace eid e
    getEventsHelper uid

postEventsR :: Handler RepJson
postEventsR = putEventHelper Nothing

postEventR :: EventId -> Handler RepJson
postEventR = putEventHelper . Just

postDeleteEventR :: EventId -> Handler RepJson
postDeleteEventR eid = do
    uid <- requireAuthId
    e <- runDB $ get404 eid
    unless (uid == eventUser e) notFound
    runDB $ delete eid
    getEventsHelper uid

getEventR :: EventId -> Handler RepJson
getEventR eid = do
    uid <- requireAuthId
    e <- runDB $ get404 eid
    unless (uid == eventUser e) notFound
    render <- getUrlRender
    jsonToRepJson $ eventToJson render (eid, e)

getFeedIdR :: Handler RepJson
getFeedIdR = error "getFeedIdR" {- do
    i <- authIdentifier
    y <- getYesod
    forceReset <- runFormGet' $ boolInput "forceReset"
    feedId <- liftIO $ getFeedId (dbInfo y) i forceReset
    return $ jsonMap
        [ ("feedId", jsonScalar feedId)
        , ("ident", jsonScalar i)
        , ("feedUrl", FeedR feedId)
        ] -}

getFeedR :: String -> Handler RepAtom
getFeedR _feedId = error "getFeedR" {- do
    y <- getYesod
    ident' <- liftIO $ checkFeedId (dbInfo y) feedId
    ident <- case ident' of
                Nothing -> notFound
                Just x -> return x
    es <- liftIO $ getEvents (dbInfo y) ident
    now <- liftIO getCurrentTime
    os <- liftIO $ getOccurrencesIO es
    atomFeed $ AtomFeed "Your upcoming reminders"
                        (FeedR feedId)
                        HomepageR
                        now
                        (map (helper now) os)
      where
        helper :: UTCTime -> (Day, [Occurrence]) -> AtomFeedEntry
        helper now (d, os) = error "FIXME" {-AtomFeedEntry
                            (RelLoc $ "#" ++ cs d)
                            now
                            ("Reminders for " ++ prettyDate d)
                            $ Tag "ul" [] $ HtmlList $ map helper2 os -}
        helper2 :: Occurrence -> Html
        helper2 o = error "FIXME Tag \"li\" [] $ cs $ toHtmlObject o"
-}

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
