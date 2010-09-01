{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Data.Time.Calendar
import Data.Time
import System.Locale
import Web.Encodings

mkPersist [$persist|
User
    ident String
    UniqueUser ident
Event
    user UserId
    title String
    day Day
    gregorian Bool
    hebrew Bool
    afterSunset Bool
|]

prettyDate' :: Day -> String
prettyDate' = formatTime defaultTimeLocale "%b %e, %Y"

prettyDate :: Day -> String
prettyDate = formatTime defaultTimeLocale "%A %B %e, %Y"

{-
instance ConvertSuccess Event HtmlObject where
    convertSuccess e = cs
        [ ("title", toHtmlObject $ title e)
        , ("rawtitle", toHtmlObject $ Html $ cs $ title e)
        , ("day", cs $ (cs :: Day -> String) $ day e)
        , ("prettyday", cs $ prettyDate' $ day e)
        , ("reminders", cs $ reminders e)
        , ("sunset", cs $ (cs :: Bool -> String) $ afterSunset e)
        , ("uuid", cs $ maybe "" UUID.toString $ Model.uuid e)
        ]
-}

eventToJson :: Event -> Json
eventToJson e = jsonMap
    [ ("title", jsonScalar $ encodeHtml $ eventTitle e)
    , ("rawtitle", jsonScalar $ eventTitle e)
    , ("day", jsonScalar $ show $ eventDay e)
    , ("prettyday", jsonScalar $ prettyDate' $ eventDay e)
    , ("reminders", jsonList $
        (if eventGregorian e then [jsonScalar "Gregorian"] else []) ++
        (if eventHebrew e then [jsonScalar "Hebrew"] else []))
    , ("sunset", jsonScalar $ if eventAfterSunset e then "true" else "false")
    ]
