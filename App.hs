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
|]
instance YesodAuth Luach

homepage :: Handler Luach StaticFile
homepage = return $ StaticFile TypeHtml "templates/index.html"

getEventsH :: Handler Luach HtmlObject
getEventsH = do
    (Luach conn dn) <- getYesod
    i <- identifier
    liftIO $ helper <$> getEvents conn dn i
        where
            helper :: [Event] -> HtmlObject
            helper = Sequence . map cs

putEventHelper :: Maybe UUID.UUID -> Handler Luach ()
putEventHelper uuid = do
    o <- identifier
    t <- postParam "title"
    d <- postParam "day"
    g <- postParam "remindGreg"
    h <- postParam "remindHebrew"
    let e = Event t d g h uuid o
    Luach conn dn <- getYesod
    liftIO $ putEvent conn dn e
    return ()

putEventH :: Handler Luach ()
putEventH = do
    putEventHelper Nothing

updateEventH :: String -> Handler Luach ()
updateEventH uuid = do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid'

deleteEventH :: String -> Handler Luach ()
deleteEventH uuid = do
    Luach conn dn <- getYesod
    e' <- liftIO $ getEvent conn dn uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- identifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent conn dn e

getEventH :: String -> Handler Luach Event
getEventH uuid = do
    Luach conn dn <- getYesod
    e' <- liftIO $ getEvent conn dn uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- identifier
    unless (i == owner e) permissionDenied
    return e

getFeedH :: Handler Luach Occurrences
getFeedH = do
    i <- identifier
    Luach conn dn <- getYesod
    es <- liftIO $ getEvents conn dn i
    today <- liftIO $ utctDay <$> getCurrentTime
    let maxDay = addDays 7 today
    os <- liftIO $ getOccurrencesIO es
    let os' = filter (\(d, _) -> d <= maxDay) os
    return os'

readLuach :: IO Luach
readLuach = readYamlDoc "settings.yaml" >>= convertAttemptWrap

app :: IO Application
app = toHackApp <$> readLuach
