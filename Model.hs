{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model
    ( Domain
    , DBInfo (..)
    , Event (..)
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

type Domain = String
data DBInfo = DBInfo AWSConnection Domain

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
        , ("rawtitle", toHtmlObject $ Html $ cs $ title e)
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

getEvents :: DBInfo -> String -> IO [Event]
getEvents (DBInfo conn domain) owner' = do
    items <- select conn $ "select * from " ++ domain ++ " where owner='"
                           ++ owner' ++ "'"
    mapM convertAttemptWrap items

getEvent :: DBInfo -> String -> IO (Maybe Event)
getEvent (DBInfo conn domain) uuid' = do
    i <- getAttributes conn domain uuid' []
    return $ convertAttemptWrap i

putEvent :: DBInfo -> Event -> IO ()
putEvent (DBInfo conn domain) event = do
    i@(Item _ attrs) <- cs event
    let keys = map (\(k := _) -> k) attrs
    putAttributes' conn domain i keys

data DeleteMissingEvent = DeleteMissingEvent Event
    deriving (Show, Typeable)
instance Exception DeleteMissingEvent

deleteEvent :: DBInfo -> Event -> IO ()
deleteEvent (DBInfo conn domain) event =
    case uuid event of
        Nothing -> failure $ DeleteMissingEvent event
        Just _ -> cs event >>= deleteAttributes conn domain

feedIdKey :: String
feedIdKey = "feedId"

getFeedId :: DBInfo -> String -> Bool -> IO String
getFeedId (DBInfo conn domain) ident forceReset = do
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

checkFeedId :: DBInfo -> String -> IO (Maybe String)
checkFeedId (DBInfo conn domain) feedId = do
    i <- select conn $ "select itemName() from " ++ domain ++ " where " ++
                       feedIdKey ++ "='" ++ simpleEscape feedId ++ "'"
    return $ case i of
                [Item ident _] -> Just ident
                _ -> Nothing

simpleEscape = concatMap h where
    h '\'' = "\\'"
    h '\\' = "\\\\"
    h c = [c]
