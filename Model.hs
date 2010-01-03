{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model
    ( Event (..)
    , CalendarType (..)
    , getEvents
    , getEvent
    , putEvent
    , deleteEvent
    , getFeedId
    , checkFeedId
    , prettyDate
    ) where

import Data.Time.Calendar
--import Data.Time.Calendar.Hebrew
import Data.UUID
import Network.AWS.SimpleDB
import System.Random
import qualified Safe.Failure as SF
import Control.Failure
import Data.Convertible.Text
import Control.Applicative
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Yesod
import qualified Data.UUID as UUID
import Data.Function.Predicate
import Data.Maybe
import Control.Monad (when)
import Data.Time
import System.Locale

data Event = Event
    { title :: String
    , day :: Day
    , reminders :: [CalendarType]
    , afterSunset :: Bool
    , uuid :: Maybe UUID
    , owner :: String
    }
    deriving (Eq, Show)

prettyDate' :: Day -> String
prettyDate' = formatTime defaultTimeLocale "%b %e, %Y"

prettyDate :: Day -> String
prettyDate = formatTime defaultTimeLocale "%A %B %e, %Y"

instance ConvertSuccess Event HtmlObject where
    convertSuccess e = cs
        [ ("title", toHtmlObject $ title e)
        , ("day", cs $ (cs :: Day -> String) $ day e)
        , ("prettyday", cs $ prettyDate' $ day e)
        , ("reminders", cs $ reminders e)
        , ("sunset", cs $ (cs :: Bool -> String) $ afterSunset e)
        , ("uuid", cs $ maybe "" UUID.toString $ Model.uuid e)
        ]
instance HasReps Event where
    reps = error "reps Event"
    chooseRep = chooseRep . toHtmlObject

data CalendarType = Gregorian | Hebrew
    deriving (Eq, Show, Read)
instance ConvertAttempt String CalendarType where
    convertAttempt = SF.read
instance ConvertSuccess CalendarType String where
    convertSuccess = show
instance ConvertSuccess [CalendarType] HtmlObject where
    convertSuccess = cs . map show

instance ConvertSuccess Event (IO Item) where
    convertSuccess e = do
        name <- toString <$>
                    case uuid e of
                        Just u -> return u
                        Nothing -> randomIO
        return $ Item name $
            [ "day" := cs (day e)
            , "title" := title e
            , "afterSunset" := cs (afterSunset e)
            , "owner" := owner e
            ] ++ map (\x -> "reminders" := cs x) (reminders e)
            ++ if afterSunset e then ["afterSunset" := "true"]
                                else []

data InvalidUUID = InvalidUUID String
    deriving (Show, Typeable)
instance Exception InvalidUUID

instance ConvertAttempt Item Event where
    convertAttempt (Item name attrs) = do
        u <- case fromString name of
                Nothing -> failure $ InvalidUUID name
                Just u' -> return u'
        let m = map (\(k := v) -> (k, v)) attrs
        d <- SF.lookup "day" m >>= ca
        t <- SF.lookup "title" m
        r <- mapM (ca . snd) $ filter (fst `equals` "reminders") m
        let s = fromMaybe False $ do
                    "true" <- lookup "afterSunset" m
                    return True
        o <- SF.lookup "owner" m
        return $ Event t d r s (Just u) o

getEvents :: AWSConnection -> String -> String -> IO [Event]
getEvents conn domain owner' = do
    items <- select conn $ "select * from " ++ domain ++ " where owner='"
                           ++ owner' ++ "'"
    mapM convertAttemptWrap items

getEvent :: AWSConnection -> String -> String -> IO (Maybe Event)
getEvent conn domain uuid' = do
    i <- getAttributes conn domain uuid' []
    return $ convertAttemptWrap i

putEvent :: AWSConnection -> String -> Event -> IO ()
putEvent conn domain event = cs event >>= putAttributes conn domain

data DeleteMissingEvent = DeleteMissingEvent Event
    deriving (Show, Typeable)
instance Exception DeleteMissingEvent

deleteEvent :: AWSConnection -> String -> Event -> IO ()
deleteEvent conn domain event =
    case uuid event of
        Nothing -> failure $ DeleteMissingEvent event
        Just _ -> cs event >>= deleteAttributes conn domain

feedIdKey :: String
feedIdKey = "feedId"

getFeedId :: AWSConnection -> String -> String -> Bool -> IO String
getFeedId conn domain ident forceReset = do
    (feedId, toSet) <- case forceReset of
        True -> do
            a <- toString <$> randomIO
            return (a, True)
        False -> do
            i <- getAttributes conn domain ident [feedIdKey]
            case i of
                Item _ [] -> do
                    a <- toString <$> randomIO
                    return (a, True)
                Item _ [feedIdKey := feedId'] -> return (feedId', False)
                Item _ x -> error $ "Invalid getFeedId attribs: " ++ show x
    when toSet $
        putAttributes' conn domain (Item ident [feedIdKey := feedId])
                       [feedIdKey]
    return feedId

checkFeedId :: AWSConnection -> String -> String -> String -> IO Bool
checkFeedId conn domain ident feedId = do
    i <- getAttributes conn domain ident [feedIdKey]
    return $ case i of
                Item _ [feedIdKey := feedId] -> True
                _ -> False
