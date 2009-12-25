{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Model
    ( Event (..)
    , getEvents
    , getEvent
    , putEvent
    , deleteEvent
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

data Event = Event
    { title :: String
    , day :: Day
    , remindGreg :: Bool
    , remindHebrew :: Bool
    , uuid :: Maybe UUID
    , owner :: String
    }
    deriving (Eq, Show)

instance ConvertSuccess Event (IO Item) where
    convertSuccess e = do
        name <- toString <$>
                    case uuid e of
                        Just u -> return u
                        Nothing -> randomIO
        return $ Item name
            [ "day" := cs (day e)
            , "title" := title e
            , "remindGreg" := cs (remindGreg e)
            , "remindHebrew" := cs (remindHebrew e)
            , "owner" := owner e
            ]

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
        g <- SF.lookup "remindGreg" m >>= ca
        h <- SF.lookup "remindHebrew" m >>= ca
        o <- SF.lookup "owner" m
        return $ Event t d g h (Just u) o

getEvents :: AWSConnection -> String -> String -> IO [Event]
getEvents conn domain owner' = do
    items <- select conn $ "select * from " ++ domain ++ " where owner='"
                           ++ owner' ++ "'"
    mapM convertAttemptWrap items

getEvent :: AWSConnection -> String -> String -> IO (Maybe Event)
getEvent conn domain uuid = do
    i <- getAttributes conn domain uuid []
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
