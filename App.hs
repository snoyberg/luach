{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Object
import Control.Applicative
import Data.Attempt
import Control.Monad
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
    , getStatic :: Static
    }
type Handler = GHandler Luach Luach

data InvalidLuach = InvalidLuach StringObject
    deriving (Show, Typeable)
instance Exception InvalidLuach

mkLuach :: FromAttempt m
        => StringObject
        -> m Luach
mkLuach o = fa $ do
    m <- fromMapping o
    Luach <$> (DBInfo
                <$> (amazonSimpleDBConnection
                        <$> lookupScalar "access-key" m
                        <*> lookupScalar "secret-key" m
                    )
                <*> lookupScalar "domain" m
              )
              <*> lookupScalar "rpxnow-key" m
              <*> lookupScalar "approot" m

mkYesod "Luach" [$parseRoutes|
/ HomepageR GET
/auth AuthR Auth getAuth
/static StaticR Static getStatic
/event EventrR GET PUT
/event/#String EventR GET PUT DELETE
/settings/feedid FeedIdR GET
/feed/#String FeedR GET
|]
instance YesodApproot Luach where
    approot = theApproot
instance YesodAuth Luach where
    rpxnowApiKey = Just . rpxnowKey
instance YesodTemplate Luach where
    getTemplateGroup = luachTG

getHomepageR :: Handler ChooseRep
getHomepageR = do
    y <- getYesod
    template "index" (cs "FIXME") $ \_ -> return
        . setAttribute "approot" (toHtmlObject $ approot y)

getEventsHelper :: String -> Handler JsonResponse
getEventsHelper i = do
    y <- getYesod
    es <- liftIO $ getEvents (dbInfo y) i
    os <- liftIO $ getOccurrencesIO es
    return $ JsonResponse $ cs
        [ ("events", Sequence $ map toHtmlObject es)
        , ("upcoming", toHtmlObject os)
        ]

-- FIXME put into yesod
newtype JsonResponse = JsonResponse HtmlObject
instance HasReps JsonResponse where
    chooseRep (JsonResponse ho) _ = return (TypeJson, cs $ unJsonDoc $ cs ho)

getEventsR :: Handler JsonResponse
getEventsR = authIdentifier >>= getEventsHelper

putEventHelper :: Maybe UUID.UUID -> Handler JsonResponse
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
    getEventsHelper o

putEventsR :: Handler JsonResponse
putEventsR = putEventHelper Nothing

putEventR :: String -> Handler JsonResponse -- FIXME take a UUID
putEventR uuid = do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid'

deleteEventR :: String -> Handler JsonResponse
deleteEventR uuid = do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent (dbInfo y) e
    getEventsHelper i

getEventR :: String -> Handler Event
getEventR uuid = do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    return e

authIdentifier = do
    (

getFeedIdR :: Handler RepJson
getFeedIdR = do
    i <- authIdentifier
    y <- getYesod
    forceReset <- runFormGet' $ boolInput "forceReset"
    feedId <- liftIO $ getFeedId (dbInfo y) i forceReset
    return $ jsonMap
        [ ("feedId", jsonScalar feedId)
        , ("ident", jsonScalar i)
        , ("feedUrl", FeedR feedId)
        ]

getFeedR :: String -> Handler RepAtom
getFeedR feedId = do
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

readLuach :: IO Luach
readLuach = do
    so <- decodeFile "settings.yaml"
    mkLuach so

app :: IO Application
app = readLuach >>= toWaiApp
