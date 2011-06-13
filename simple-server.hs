import App
import Network.Wai.Handler.Warp

main :: IO ()
main = putStrLn "Running..." >> withLuach (run 3000)
