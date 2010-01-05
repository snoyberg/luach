{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module App
    ( app
    ) where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Model hiding (uuid)
import Occurrence
import Data.Object.Yaml
import Control.Applicative
import Data.Attempt
import Control.Monad
import Data.Object.String
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Network.AWS.SimpleDB
import qualified Data.UUID as UUID
import Data.Time
import Web.Encodings

data Luach = Luach
    { dbInfo :: DBInfo
    , rpxnowKey :: String
    , theApproot :: String
    }

data InvalidLuach = InvalidLuach StringObject
    deriving (Show, Typeable)
instance Exception InvalidLuach
instance ConvertAttempt YamlDoc Luach where
    convertAttempt = helper <=< ca where
        helper :: StringObject -> Attempt Luach
        helper o = wrapFailure (\_ -> InvalidLuach o) $ do
            m <- fromMapping o
            Luach <$> (DBInfo
                        <$> (amazonSimpleDBConnection
                                <$> lookupObject "access-key" m
                                <*> lookupObject "secret-key" m
                            )
                        <*> lookupObject "domain" m
                      )
                      <*> lookupObject "rpxnow-key" m
                      <*> lookupObject "approot" m


instance Yesod Luach where
    handlers = [$resources|
/:
    Get: homepage
/auth/*: authHandler
/event:
    Get: getEventsH
    Put: putEventH
/event/$uuid:
    Get: getEventH
    Put: updateEventH
    Delete: deleteEventH
/upcoming:
    Get: getUpcomingH
/static/*filepath: serveStatic'
/settings/feedid:
    Get: getFeedIdH
/feed/$ident/$feedId:
    Get: getFeedH
|]
    templateDir _ = "templates"
instance YesodApproot Luach where
    approot = Approot . theApproot
instance YesodAuth Luach where
    rpxnowApiKey = Just . rpxnowKey

homepage :: Handler Luach Template
homepage = do
    y <- getYesod
    template "index" "no-object" (cs "no-object") $ return
            [ ("approot", cs $ unApproot $ approot y)
            ]

getEventsHelper :: String -> Handler Luach HtmlObject
getEventsHelper i = do
    y <- getYesod
    liftIO $ helper <$> getEvents (dbInfo y) i
        where
            helper :: [Event] -> HtmlObject
            helper = Sequence . map cs

getEventsH :: Handler Luach HtmlObject
getEventsH = authIdentifier >>= getEventsHelper

putEventHelper :: Maybe UUID.UUID -> Handler Luach HtmlObject
putEventHelper uuid = do
    o <- authIdentifier
    t <- runRequest $ postParam "title"
    d <- runRequest $ postParam "day"
    g <- runRequest $ postParam "remindGreg"
    h <- runRequest $ postParam "remindHebrew"
    let r = (if g then [Gregorian] else []) ++
            (if h then [Hebrew] else [])
    s <- runRequest $ postParam "afterSunset"
    let e = Event t d r s uuid o
    y <- getYesod
    liftIO $ putEvent (dbInfo y) e
    getEventsHelper o

putEventH :: Handler Luach HtmlObject
putEventH = putEventHelper Nothing

updateEventH :: String -> Handler Luach HtmlObject
updateEventH uuid = do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid'

deleteEventH :: String -> Handler Luach HtmlObject
deleteEventH uuid = do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent (dbInfo y) e
    Approot ar <- getApproot
    getEventsHelper i

getEventH :: String -> Handler Luach Event
getEventH uuid = do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    return e

getUpcomingH :: Handler Luach Occurrences
getUpcomingH = do
    i <- authIdentifier
    y <- getYesod
    es <- liftIO $ getEvents (dbInfo y) i
    liftIO $ getOccurrencesIO es

getFeedIdH :: Handler Luach HtmlObject
getFeedIdH = do
    i <- authIdentifier
    y <- getYesod
    let (Approot ar) = approot y
    forceReset <- runRequest $ getParam "forceReset"
    feedId <- liftIO $ getFeedId (dbInfo y) i forceReset
    return $ toHtmlObject
                [ ("feedId", feedId)
                , ("ident", i)
                , ("feedUrl", ar ++ "feed/" ++ encodeUrl i ++ "/" ++
                              encodeUrl feedId ++ "/")
                ]

getFeedH :: String -> String -> Handler Luach AtomFeedResponse
getFeedH ident feedId = do
    y <- getYesod
    let (Approot ar) = approot y
    isValid <- liftIO $ checkFeedId (dbInfo y) ident feedId
    unless isValid notFound
    es <- liftIO $ getEvents (dbInfo y) ident
    now <- liftIO getCurrentTime
    os <- liftIO $ getOccurrencesIO es
    atomFeed $ AtomFeed "Your upcoming reminders"
                        (RelLoc $ "feed/" ++ encodeUrl ident ++ "/" ++
                         encodeUrl feedId ++ "/")
                        (RelLoc "")
                        now
                        (map (helper now) os)
      where
        helper :: UTCTime -> (Day, [Occurrence]) -> AtomFeedEntry
        helper now (d, os) = AtomFeedEntry
                            (RelLoc "")
                            now
                            ("Reminders for " ++ prettyDate d)
                            $ Tag "ul" [] $ HtmlList $ map helper2 os
        helper2 :: Occurrence -> Html
        helper2 o = Tag "li" [] $ Text $ cs o

serveStatic' :: Verb -> [String] -> Handler y [(ContentType, Content)]
serveStatic' = serveStatic $ fileLookupDir "static"

readLuach :: IO Luach
readLuach = readYamlDoc "settings.yaml" >>= convertAttemptWrap

app :: IO Application
app = toHackApp <$> readLuach
