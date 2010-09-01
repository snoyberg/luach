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
import Yesod.Mail
import System.Random
import Data.Time

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
/day/#String DayR GET
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
                stdgen <- liftIO newStdGen
                let fid' = fst $ randomString 10 stdgen
                fmap Just $ insert $ User (credsIdent creds) fid'

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
        , atomLinkHome = HomepageR
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
getDayR _ = return ()

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
