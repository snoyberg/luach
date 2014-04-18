{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Yesod
import Data.Time.Calendar
import Data.Time
import System.Locale
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text (Text, append)
import qualified Data.Text as T
import Data.Char (isUpper)
import Database.Persist.Quasi
import Data.Time (UTCTime)
import Data.Typeable (Typeable)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings
        { psToDBName = \t ->
            if not (T.null t) && isUpper (T.head t)
                then "Luach__" `append` psToDBName upperCaseSettings t
                else psToDBName upperCaseSettings t
        } "config/models")

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
