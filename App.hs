{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module App
    ( app
    ) where

import Yesod
import Yesod.Helpers.Auth
import Model
import Data.Object.Yaml
import Control.Applicative
import Data.Attempt
import Control.Monad
import Data.Object.String
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Network.AWS.SimpleDB
import Data.Maybe (fromJust)
import qualified Data.UUID as UUID

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
|]
instance YesodAuth Luach

homepage = return $ StaticFile TypeHtml "templates/index.html"

instance ConvertSuccess Event HtmlObject where
    convertSuccess e = cs
        [ ("title", title e)
        , ("day", cs $ day e)
        , ("remindGreg", cs $ remindGreg e)
        , ("remindHebrew", cs $ remindHebrew e)
        , ("uuid", UUID.toString $ fromJust $ uuid e)
        ]
instance HasReps Event where
    reps = error "reps Event"
    chooseRep = chooseRep . toHtmlObject

getEventsH = do
    (Luach conn dn) <- getYesod
    i <- identifier
    liftIO $ helper <$> getEvents conn dn i
        where
            helper :: [Event] -> HtmlObject
            helper = Sequence . map cs

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

putEventH = do
    putEventHelper Nothing

updateEventH uuid = do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid'

deleteEventH uuid = do
    Luach conn dn <- getYesod
    e' <- liftIO $ getEvent conn dn uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- identifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent conn dn e

getEventH uuid = do
    Luach conn dn <- getYesod
    e' <- liftIO $ getEvent conn dn uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- identifier
    unless (i == owner e) permissionDenied
    return e

readLuach :: IO Luach
readLuach = readYamlDoc "settings.yaml" >>= convertAttemptWrap

app :: IO Application
app = toHackApp <$> readLuach
