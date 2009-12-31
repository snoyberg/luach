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

data Luach = Luach AWSConnection String

data InvalidLuach = InvalidLuach StringObject
    deriving (Show, Typeable)
instance Exception InvalidLuach
instance ConvertAttempt YamlDoc Luach where
    convertAttempt = helper <=< ca where
        helper :: StringObject -> Attempt Luach
        helper o = wrapFailure (\_ -> InvalidLuach o) $ do
            m <- fromMapping o
            Luach <$> (amazonSimpleDBConnection
                        <$> lookupObject "access-key" m
                        <*> lookupObject "secret-key" m
                      )
                  <*> lookupObject "domain" m


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
/feed:
    Get: getFeedH
/static/*filepath: serveStatic'
|]
instance YesodApproot Luach where
    approot _ = Approot "http://localhost:3000/" -- FIXME
instance YesodAuth Luach

homepage :: Handler Luach StaticFile
homepage = return $ StaticFile TypeHtml "templates/index.html"

getEventsHelper :: String -> Handler Luach HtmlObject
getEventsHelper i = do
    (Luach conn dn) <- getYesod
    liftIO $ helper <$> getEvents conn dn i
        where
            helper :: [Event] -> HtmlObject
            helper = Sequence . map cs

getEventsH :: Handler Luach HtmlObject
getEventsH = authIdentifier >>= getEventsHelper

putEventHelper :: Maybe UUID.UUID -> Handler Luach HtmlObject
putEventHelper uuid = do
    o <- authIdentifier
    t <- postParam "title"
    d <- postParam "day"
    g <- postParam "remindGreg"
    h <- postParam "remindHebrew"
    let r = (if g then [Gregorian] else []) ++
            (if h then [Hebrew] else [])
    s <- postParam "afterSunset"
    let e = Event t d r s uuid o
    Luach conn dn <- getYesod
    liftIO $ putEvent conn dn e
    getEventsHelper o

putEventH :: Handler Luach HtmlObject
putEventH = putEventHelper Nothing

updateEventH :: String -> Handler Luach HtmlObject
updateEventH uuid = do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid'

deleteEventH :: String -> Handler Luach HtmlObject
deleteEventH uuid = do
    Luach conn dn <- getYesod
    e' <- liftIO $ getEvent conn dn uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent conn dn e
    Approot ar <- getApproot
    getEventsHelper i

getEventH :: String -> Handler Luach Event
getEventH uuid = do
    Luach conn dn <- getYesod
    e' <- liftIO $ getEvent conn dn uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    return e

getFeedH :: Handler Luach Occurrences
getFeedH = do
    i <- authIdentifier
    Luach conn dn <- getYesod
    es <- liftIO $ getEvents conn dn i
    today <- liftIO $ utctDay <$> getCurrentTime
    let maxDay = addDays 7 today
    os <- liftIO $ getOccurrencesIO es
    let os' = filter (\(d, _) -> d <= maxDay) os
    return os'

serveStatic' :: Verb -> [String] -> Handler y [(ContentType, Content)]
serveStatic' = serveStatic $ fileLookupDir "static"

readLuach :: IO Luach
readLuach = readYamlDoc "settings.yaml" >>= convertAttemptWrap

app :: IO Application
app = toHackApp <$> readLuach
