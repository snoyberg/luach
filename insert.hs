import Model
import Data.Object
import Data.Object.Yaml
import qualified Settings
import Control.Monad
import Database.Persist

main = do
    Mapping so <- join $ decodeFile "luach.yaml"
    Settings.withConnectionPool $ Settings.runConnectionPool $ do
        deleteWhere ([] :: [Filter Event])
        deleteWhere ([] :: [Filter User])

        let ids = filter filter1 so :: [(String, Object String String)]
        forM_ ids $ \(ident, Mapping [("feedId", Scalar feedId)]) ->
            insert (User ident feedId) >> return ()

        forM_ (filter (not . filter1) so) $ \(_, Mapping m) -> do
            Just (Scalar as) <- return $ lookup "afterSunset" m
            Just (Scalar t) <- return $ lookup "title" m
            Just (Scalar d) <- return $ lookup "day" m
            rems <- return $ map snd $ filter (\(x, _) -> x == "reminders") m
            Just (Scalar o) <- return $ lookup "owner" m
            let hebrew = Scalar "Hebrew" `elem` rems
                greg = Scalar "Gregorian" `elem` rems
            Just (uid, _) <- getBy $ UniqueUser o
            _ <- insert $ Event uid t (read d) greg hebrew (as == "true")
            return ()

filter1 ('h':'t':'t':'p':_, _) = True
filter1 _ = False
