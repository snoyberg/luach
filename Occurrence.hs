{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Occurrence
    ( Occurrence (..)
    , Occurrences
    , getOccurrencesIO
    , prettyOccurrence
    , occurrencesToJson
    , showCT
#if TEST
    , getOccurrences
    , testSuite
#endif
    ) where

import Yesod
import Model
import Data.Time.Calendar.Hebrew
import Data.List
import Data.Function
import Data.Time
import Data.Text (Text, unpack)
import qualified Data.Vector as V

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

data CalendarType = Gregorian | Hebrew
    deriving (Show, Eq)

data Occurrence = Occurrence
    { calendarType :: CalendarType
    , otitle :: Text
    , years :: Integer
    }
    deriving (Show, Eq)

showCT :: CalendarType -> String
showCT Hebrew = "Hebrew"
showCT Gregorian = "English"

type Occurrences = [(Day, [Occurrence])]

{-
instance ConvertSuccess Occurrences HtmlObject where
    convertSuccess = Sequence . map helper where
        helper (d, o) = toHtmlObject
            [ ("day", toHtmlObject $ prettyDate d)
            , ("o", Sequence $ map cs o)
            ]
-}

occurrencesToJson :: Occurrences -> Value
occurrencesToJson =
    Array . V.fromList . map go
  where
    go (d, o) = object
        [ "day" .= prettyDate d
        , "o" .= map occurrenceToJson o
        ]

occurrenceToJson :: Occurrence -> Value
occurrenceToJson o = object
    [ "title" .= otitle o
    , "years" .= show (years o)
    , "calendar" .= show (calendarType o)
    ]

prettyOccurrence :: Occurrence -> String
prettyOccurrence o = unpack (otitle o) ++ " - " ++ show (years o) ++ " on the " ++
                       show (calendarType o) ++ " calendar"

-- | Only does next 7 days.
getOccurrencesIO :: [Event] -> IO [(Day, [Occurrence])]
getOccurrencesIO es = do
    today <- utctDay <$> getCurrentTime
    let maxDay = addDays 7 today
    let os = getOccurrences today es
    return $ filter (\(d, _) -> d <= maxDay) os

getOccurrences :: Day -> [Event] -> [(Day, [Occurrence])]
getOccurrences gtoday =
    map hoist
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . concatMap (nos gtoday $ toHebrew gtoday)

hoist :: [(k, v)] -> (k, [v])
hoist pairs'@((k, _):_) = (k, map snd pairs')
hoist [] = error "Empty list to hoist"

-- | next occurences
nos :: Day -> HebrewDate -> Event -> [(Day, Occurrence)]
nos gd hd e =
    (if eventGregorian e then [nosGregorian] else []) ++
    (if eventHebrew e then [nosHebrew] else [])
  where
    nosGregorian =
                let (yOrig, m, da) = toGregorian $ eventDay e
                    (y, m', da') = toGregorian gd
                    y' = if m' > m || (m' == m && da' > da)
                            then y + 1
                            else y
                    gd' = fromGregorian y' m da
                    years' = y' - yOrig
                    o = Occurrence Gregorian (eventTitle e) years'
                 in (gd', o)
    nosHebrew =
                let day' = addDays (if eventAfterSunset e then 1 else 0) $ eventDay e
                    orig = toHebrew day'
                    hd' = nextAnniversary hd orig
                    years' = fromIntegral $ year hd' - year orig
                    o = Occurrence Hebrew (eventTitle e) years'
                 in (fromHebrew hd', o)

#if TEST
testSuite :: Test
testSuite = testGroup "Occurrence"
    [ testCase "occurences" caseOccurrences
    ]

caseOccurrences = do
    let today = fromGregorian 2010 1 11
    let myTitle = "my birthday"
        myBday = fromGregorian 1985 1 12
        myUuid = Nothing
        myUuidStr = ""
        myOwner = "michael"
    let es = [Event myTitle myBday [Gregorian] False myUuid myOwner]
    let expected = [(fromGregorian 2010 1 12,
                        [ Occurrence Gregorian myTitle myUuidStr 25
                        ]
                    )]
    expected @=? getOccurrences today es
#endif
