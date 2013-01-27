{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , staticroot
    , staticdir
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql
import Yesod (MonadBaseControl)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Data.Text.Encoding (encodeUtf8)
import Text.Shakespeare.Text (textFile)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)

hamletFile :: FilePath -> Q Exp
hamletFile x = H.hamletFile $ "hamlet/" ++ x ++ ".hamlet"

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = H.cassiusFile $ "cassius/" ++ x ++ ".cassius"
#else
cassiusFile x = H.cassiusFileReload $ "cassius/" ++ x ++ ".cassius"
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile x = H.juliusFile $ "julius/" ++ x ++ ".julius"
#else
juliusFile x = H.juliusFileReload $ "julius/" ++ x ++ ".julius"
#endif

connStr :: Text
connStr = toStrict $ toLazyText $ $(textFile "config/connstr.txt") undefined

connectionCount :: Int
connectionCount = 10

withConnectionPool :: (MonadIO m, MonadBaseControl IO m) => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool (encodeUtf8 connStr) connectionCount

runConnectionPool :: (MonadIO m, MonadBaseControl IO m) => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

approot :: String
#ifdef PRODUCTION
approot = "http://luach.snoyman.com"
#else
approot = "http://localhost:3000"
#endif

staticroot :: String
staticroot = approot ++ "/static"

staticdir :: FilePath
staticdir = "static"
