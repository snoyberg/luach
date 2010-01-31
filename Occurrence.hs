{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Occurrence
    ( Occurrence (..)
    , Occurrences
    , getOccurrencesIO
#if TEST
    , getOccurrences
    , testSuite
#endif
    ) where

import Model
import Control.Applicative
import Data.Time.Calendar.Hebrew
import Data.List
import Data.Function
import Data.UUID
import Data.Object.Html
import Data.Object.Text
import Data.Time

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

data Occurrence = Occurrence
    { calendarType :: CalendarType
    , otitle :: String
    , ouuid :: String
    , years :: Integer
    }
    deriving (Show, Eq)

type Occurrences = [(Day, [Occurrence])]

instance ConvertSuccess Occurrences HtmlObject where
    convertSuccess = Sequence . map helper where
        helper (d, o) = toHtmlObject
            [ ("day", toHtmlObject $ prettyDate d)
            , ("o", Sequence $ map cs o)
            ]
instance ConvertSuccess Occurrence HtmlObject where
    convertSuccess o = cs
        [ ("title", otitle o)
        , ("years", show $ years o)
        , ("calendar", show $ calendarType o)
        ]
instance ConvertSuccess Occurrence String where
    convertSuccess o = otitle o ++ " - " ++ show (years o) ++ " on the " ++
                       show (calendarType o) ++ " calendar"
instance ConvertSuccess Occurrence Text where
    convertSuccess = (cs :: String -> Text) . cs

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
hoist pairs@((k, _):_) = (k, map snd pairs)
hoist [] = error "Empty list to hoist"

-- | next occurences
nos :: Day -> HebrewDate -> Event -> [(Day, Occurrence)]
nos gd hd e = map nos' $ reminders e where
    nos' :: CalendarType -> (Day, Occurrence)
    nos' Gregorian =
                let (yOrig, m, da) = toGregorian $ day e
                    (y, m', da') = toGregorian gd
                    y' = if m' > m || (m' == m && da' > da)
                            then y + 1
                            else y
                    gd' = fromGregorian y' m da
                    years' = y' - yOrig
                    o = Occurrence Gregorian (title e) (showUUID e) years'
                 in (gd', o)
    nos' Hebrew =
                let day' = addDays (if afterSunset e then 1 else 0) $ day e
                    orig = toHebrew day'
                    hd' = nextAnniversary hd orig
                    years' = fromIntegral $ year hd' - year orig
                    o = Occurrence Hebrew (title e) (showUUID e) years'
                 in (fromHebrew hd', o)

showUUID :: Event -> String
showUUID e = case uuid e of
                Nothing -> ""
                Just u -> toString u

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
