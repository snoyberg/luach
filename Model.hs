{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import Yesod
import Data.Time.Calendar
import Data.Time
import System.Locale
import Data.Text (Text)

mkPersist [persist|
User
    ident Text
    feedId Text Update
    UniqueUser ident
    UniqueFeedId feedId
Event
    user UserId Eq
    title Text Asc
    day Day Asc
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
