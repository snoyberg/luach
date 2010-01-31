{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module App
    ( app
    ) where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Model hiding (uuid)
import Occurrence
import Data.Object.Yaml
import Control.Applicative
import Data.Attempt
import Control.Monad
import Data.Object.String
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Network.AWS.SimpleDB
import qualified Data.UUID as UUID
import Data.Time
import Web.Encodings
import Data.Object.Json

data Luach = Luach
    { dbInfo :: DBInfo
    , rpxnowKey :: String
    , theApproot :: String
    , luachTG :: TemplateGroup
    }

data InvalidLuach = InvalidLuach StringObject
    deriving (Show, Typeable)
instance Exception InvalidLuach

mkLuach :: FromAttempt m
        => StringObject
        -> TemplateGroup
        -> m Luach
mkLuach o tg = fa $ do
    m <- fromMapping o
    Luach <$> (DBInfo
                <$> (amazonSimpleDBConnection
                        <$> lookupObject "access-key" m
                        <*> lookupObject "secret-key" m
                    )
                <*> lookupObject "domain" m
              )
              <*> lookupObject "rpxnow-key" m
              <*> lookupObject "approot" m
              <*> pure tg

instance Yesod Luach where
    resources = [$mkResources|
/:
    Get: homepage
/auth/*: authHandler
/event:
    Get: getEventsH
    Put: putEventH
/event/$uuid:
    Get: getEventH
    Put: updateEventH
    Delete: deleteEventH
/static/*filepath: serveStatic'
/settings/feedid:
    Get: getFeedIdH
/feed/$feedId:
    Get: getFeedH
|]
instance YesodApproot Luach where
    approot = theApproot
instance YesodAuth Luach where
    rpxnowApiKey = Just . rpxnowKey
instance YesodTemplate Luach where
    getTemplateGroup = luachTG

homepage :: Handler Luach ChooseRep
homepage = do
    y <- getYesod
    template "index" (cs "FIXME") $ \_ -> return
        . setAttribute "approot" (toHtmlObject $ approot y)

getEventsHelper :: String -> Handler Luach JsonResponse
getEventsHelper i = do
    y <- getYesod
    es <- liftIO $ getEvents (dbInfo y) i
    os <- liftIO $ getOccurrencesIO es
    return $ JsonResponse $ cs
        [ ("events", Sequence $ map toHtmlObject es)
        , ("upcoming", toHtmlObject os)
        ]

-- FIXME put into yesod
newtype JsonResponse = JsonResponse HtmlObject
instance HasReps JsonResponse where
    chooseRep (JsonResponse ho) _ = return (TypeJson, cs $ unJsonDoc $ cs ho)

getEventsH :: Handler Luach JsonResponse
getEventsH = authIdentifier >>= getEventsHelper

-- FIXME move the following code to yesod
data Form x = Form ((ParamName -> [ParamValue]) -> Either [FormError] x)
instance Functor Form where
    fmap f (Form x) = Form $ \l -> fmap f (x l)
instance Applicative Form where
    pure x = Form $ \_ -> Right x
    (Form f') <*> (Form x') = Form $ \l -> case (f' l, x' l) of
        (Right f, Right x) -> Right $ f x
        (Left e1, Left e2) -> Left $ e1 ++ e2
        (Left e, _) -> Left e
        (_, Left e) -> Left e

type FormError = String

runForm :: Form x -> Handler y x
runForm (Form f) = do
    rr <- getRawRequest
    case f $ postParams rr of
        Left es -> invalidArgs $ map (\x -> ("FIXME", x)) es
        Right x -> return x

input :: ParamName -> Form [ParamValue]
input pn = Form $ \l -> Right $ l pn

applyForm :: (x -> Either [FormError] y) -> Form x -> Form y
applyForm f (Form x') = Form $ \l -> case x' l of
                            Left es -> Left es
                            Right x -> f x

required :: Form [ParamValue] -> Form ParamValue
required = applyForm $ \pvs -> case pvs of
                [x] -> Right x
                [] -> Left ["No value for required field"]
                _ -> Left ["Multiple values for required field"]

notEmpty :: Form ParamValue -> Form ParamValue
notEmpty = applyForm $ \pv ->
                if null pv
                    then Left ["Value required"]
                    else Right pv

checkDay :: Form ParamValue -> Form Day
checkDay = applyForm $ attempt (const (Left ["Invalid day"])) Right . ca

checkBool :: Form [ParamValue] -> Form Bool
checkBool = applyForm $ \pv -> Right $ case pv of
                                        [] -> False
                                        [""] -> False
                                        ["false"] -> False
                                        _ -> True

putEventHelper :: Maybe UUID.UUID -> Handler Luach JsonResponse
putEventHelper uuid = do
    o <- authIdentifier
    (t, d, g, h, s) <- runForm $ (,,,,)
                        <$> notEmpty (required $ input "title")
                        <*> checkDay (required $ input "day")
                        <*> checkBool (input "remindGreg")
                        <*> checkBool (input "remindHebrew")
                        <*> checkBool (input "afterSunset")
    let r = (if g then [Gregorian] else []) ++
            (if h then [Hebrew] else [])
    let e = Event t d r s uuid o
    y <- getYesod
    liftIO $ putEvent (dbInfo y) e
    getEventsHelper o

putEventH :: Handler Luach JsonResponse
putEventH = putEventHelper Nothing

updateEventH :: String -> Handler Luach JsonResponse
updateEventH uuid = do
    uuid' <- try $ UUID.fromString uuid
    putEventHelper $ Just uuid'

deleteEventH :: String -> Handler Luach JsonResponse
deleteEventH uuid = do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    liftIO $ deleteEvent (dbInfo y) e
    getEventsHelper i

getEventH :: String -> Handler Luach Event
getEventH uuid = do
    y <- getYesod
    e' <- liftIO $ getEvent (dbInfo y) uuid
    e <- case e' of
            Nothing -> notFound
            Just x -> return x
    i <- authIdentifier
    unless (i == owner e) permissionDenied
    return e

getFeedIdH :: Handler Luach JsonResponse
getFeedIdH = do
    i <- authIdentifier
    y <- getYesod
    rr <- getRawRequest
    let forceReset = case getParams rr "forceReset" of
                        [] -> False
                        _ -> True
    feedId <- liftIO $ getFeedId (dbInfo y) i forceReset
    return $ JsonResponse $ toHtmlObject
                [ ("feedId", feedId)
                , ("ident", i)
                , ("feedUrl", approot y ++ "feed/" ++ encodeUrl feedId ++ "/")
                ]

getFeedH :: String -> Handler Luach AtomFeedResponse
getFeedH feedId = do
    y <- getYesod
    ident' <- liftIO $ checkFeedId (dbInfo y) feedId
    ident <- case ident' of
                Nothing -> notFound
                Just x -> return x
    es <- liftIO $ getEvents (dbInfo y) ident
    now <- liftIO getCurrentTime
    os <- liftIO $ getOccurrencesIO es
    atomFeed $ AtomFeed "Your upcoming reminders"
                        (RelLoc $ "feed/" ++ encodeUrl feedId ++ "/")
                        (RelLoc "")
                        now
                        (map (helper now) os)
      where
        helper :: UTCTime -> (Day, [Occurrence]) -> AtomFeedEntry
        helper now (d, os) = AtomFeedEntry
                            (RelLoc $ "#" ++ cs d)
                            now
                            ("Reminders for " ++ prettyDate d)
                            $ Tag "ul" [] $ HtmlList $ map helper2 os
        helper2 :: Occurrence -> Html
        helper2 o = Tag "li" [] $ cs $ toHtmlObject o

serveStatic' :: Verb -> [String] -> Handler y [(ContentType, Content)]
serveStatic' = serveStatic $ fileLookupDir "static"

readLuach :: IO Luach
readLuach = do
    so <- decodeFile "settings.yaml"
    tg <- loadTemplateGroup "templates"
    mkLuach so tg


app :: IO Application
app = readLuach >>= toWaiApp
