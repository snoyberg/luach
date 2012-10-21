{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#if PRODUCTION
import App (withLuach)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Data.Text (pack)

main :: IO ()
main = do
    port <- fmap read $ getEnv "PORT"
    approot <- getEnv "APPROOT"
    withLuach (pack approot) $ run port
#else
import App (withLuach)
import System.IO (hPutStrLn, stderr)
--import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withLuach "http://10.0.0.3:3000" $ run port -- . debug
#endif
