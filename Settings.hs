{-# LANGUAGE CPP #-}
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
import Yesod (MonadCatchIO)

hamletFile :: FilePath -> Q Exp
#ifdef PRODUCTION
hamletFile x = H.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
#else
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = H.cassiusFile $ "cassius/" ++ x ++ ".cassius"
#else
cassiusFile x = H.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile x = H.juliusFile $ "julius/" ++ x ++ ".julius"
#else
juliusFile x = H.juliusFileDebug $ "julius/" ++ x ++ ".julius"
#endif

connStr :: String
#ifdef PRODUCTION
connStr = "user=luach password=luach host=localhost port=5432 dbname=luach"
#else
connStr = "user=luach password=luach host=localhost port=5432 dbname=luach"
#endif

connectionCount :: Int
connectionCount = 10

withConnectionPool :: MonadCatchIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr connectionCount

runConnectionPool :: MonadCatchIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

approot :: String
#ifdef PRODUCTION
approot = "http://www.snoyman.com/luach"
#else
approot = "http://localhost:3000"
#endif

staticroot :: String
staticroot = approot ++ "/static"

staticdir :: FilePath
staticdir = "static"
