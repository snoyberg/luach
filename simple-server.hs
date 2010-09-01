import App
import Network.Wai.Handler.SimpleServer

main :: IO ()
main = putStrLn "Running..." >> app >>= run 3000
