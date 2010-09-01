import Network.AWS.SimpleDB
import Data.Object
import Data.Object.Yaml

main = do
    let conn = amazonSimpleDBConnection "AKIAIZIKPTCXKAZRUKJA" "AniQ6noaucZ/W68/vBrbdpKqlEYfpN8pU3XsPufN"
    res <- select conn "select * from luach"
    encodeFile "luach.yaml" $ Mapping $ map go res

go (Item name attrs) = (name, Mapping $ map go' attrs)

go' (k := v) = (k, Scalar v)
