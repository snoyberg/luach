{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
import App (withLuach)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)

main :: IO ()
main = do
    port <- fmap read $ getEnv "PORT"
    withLuach $ run port
