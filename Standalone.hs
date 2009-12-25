import App
import Hack.Handler.SimpleServer

main :: IO ()
main = putStrLn "Running..." >> app >>= run 3000
