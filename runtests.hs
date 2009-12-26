import Test.Framework (defaultMain)

import qualified Occurrence

main :: IO ()
main = defaultMain
    [ Occurrence.testSuite
    ]
