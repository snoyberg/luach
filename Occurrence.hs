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
import Data.Time
import Data.Time.Calendar.Hebrew
import Data.List
import Data.Function
import Data.UUID
import Data.Object.Html
import Data.Object
import Control.Arrow
import Yesod

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
    convertSuccess = Mapping . map helper where
        helper = cs *** Sequence . map cs
instance ConvertSuccess Occurrence HtmlObject where
    convertSuccess o = cs $ otitle o ++ " (" ++ show (years o) ++ ", " ++
                            show (calendarType o) ++ ")"
instance HasReps Occurrences where
    reps = error "reps Occurrences"
    chooseRep = chooseRep . toHtmlObject

getOccurrencesIO :: [Event] -> IO [(Day, [Occurrence])]
getOccurrencesIO es = do
    today <- utctDay <$> getCurrentTime
    return $ getOccurrences today es

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
nos gd hd e =
    (if remindGreg e then [helperG] else []) ++
    (if remindHebrew e then [helperH] else [])
        where
            helperG =
                let (yOrig, m, da) = toGregorian $ day e
                    (y, m', da') = toGregorian gd
                    y' = if m' > m || (m' == m && da' > da)
                            then y + 1
                            else y
                    gd' = fromGregorian y' m da
                    years' = y' - yOrig
                    o = Occurrence Gregorian (title e) (showUUID e) years'
                 in (gd', o)
            helperH =
                let hd' = nextAnniversary hd $ toHebrew $ day e
                    years' = fromIntegral $ year hd' - year hd
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
    let es = [Event myTitle myBday True False myUuid myOwner]
    let expected = [(fromGregorian 2010 1 12,
                        [ Occurrence Gregorian myTitle myUuidStr 25
                        ]
                    )]
    expected @=? getOccurrences today es
#endif
