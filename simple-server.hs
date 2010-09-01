import App
import Network.Wai.Handler.SimpleServer

main :: IO ()
main = putStrLn "Running..." >> withLuach (run 3000)
