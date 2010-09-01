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
/event/#String EventR GET PUT DELETE
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

getEventsHelper :: (UserId, User) -> Handler RepJson
getEventsHelper _i = do
    _y <- getYesod
    es <- error "liftIO $ getEvents (dbInfo y) i"
    os <- liftIO $ getOccurrencesIO es
    jsonToRepJson $ jsonMap
        [ ("events", jsonList $ map eventToJson es)
        , ("upcoming", occurrencesToJson os)
        ]

getEventsR :: Handler RepJson
getEventsR = requireAuth >>= getEventsHelper

putEventHelper :: Maybe Int -> Handler RepJson
putEventHelper = error "putEventHelper" {-
putEventHelper :: Maybe UUID.UUID -> Handler RepJson
putEventHelper uuid = do
    o <- authIdentifier
    (t, d, g, h, s) <- runFormPost $ (,,,,)
                        <$> notEmpty (required $ input "title")
                        <*> checkDay (required $ input "day")
                        <*> checkBool (input "remindGreg")
                        <*> checkBool (input "remindHebrew")
                        <*> checkBool (input "afterSunset")
    let r = (if g then [Gregorian] else []) ++
            (if h then [Hebrew] else [])
    let e = Event t d r s uuid o
    y <- getYesod
    liftIO $ putEvent (dbInfo y) e
    getEventsHelper o -}

postEventsR :: Handler RepJson
postEventsR = putEventHelper Nothing

putEventR :: String -> Handler RepJson -- FIXME take a UUID
putEventR = error "putEventR" {- do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid' -}

deleteEventR :: String -> Handler RepJson
deleteEventR = error "deleteEventR" {- do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent (dbInfo y) e
    getEventsHelper i -}

getEventR :: String -> Handler RepJson
getEventR = error "getEventR" {- FIXME do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    return e -}

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
