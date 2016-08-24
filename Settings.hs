{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as S
import Control.Monad.Logger (MonadLogger)

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

connectionCount :: Int
connectionCount = 10

withConnectionPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => (ConnectionPool -> m a) -> m a
withConnectionPool inner = do
    connStr <- liftIO $ S.readFile "config/connstr.txt"
    withPostgresqlPool connStr connectionCount inner

runConnectionPool :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> ConnectionPool -> m a
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
