{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Business.Bookkeeping
  ( Transactions
  , Transaction
  , Year(..)
  , Month(..)
  , Day(..)
  , Description(..)
  , Amount(..)
  , Category
  , CategoryName(..)
  , CategoryType(..)
  , year
  , month
  , activity
  , dayTrans
  ) where

import Control.Monad.State (State, execState, modify)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)

type Transactions = State (DList Transaction) ()

type YearTransactions = State (DList (Year -> Transaction)) ()

type MonthTransactions = State (DList (Month -> Year -> Transaction)) ()

type DayTransactions = State (DList (Day -> Description -> Month -> Year -> Transaction)) ()

{-| A type representing a transaction.
 -}
data Transaction = Transaction
  { tYear :: Year
  , tMonth :: Month
  , tDay :: Day
  , tDescription :: Description
  , tCategory :: Category
  , tAmount :: Amount
  } deriving (Show, Read, Ord, Eq)

newtype Year = Year
  { unYear :: Int
  } deriving (Show, Read, Ord, Eq, Num)

newtype Month = Month
  { unMonth :: Int
  } deriving (Show, Read, Ord, Eq, Num)

newtype Day = Day
  { unDay :: Int
  } deriving (Show, Read, Ord, Eq, Num)

newtype Description = Description
  { unDescription :: Text
  } deriving (Show, Read, Ord, Eq)

instance IsString Description where
  fromString = Description . fromString

instance Monoid Description where
  mempty = Description mempty
  mappend (Description a) (Description b) = Description $ mappend a b

newtype Amount = Amount
  { unAmount :: Int
  } deriving (Show, Read, Ord, Eq, Num)

{-| A type representing an accounts title.
 -}
data Category = Category
  { cName :: CategoryName
  , cType :: CategoryType
  } deriving (Show, Read, Ord, Eq)

newtype CategoryName = CategoryName
  { unCategoryName :: Text
  } deriving (Show, Read, Ord, Eq)

instance IsString CategoryName where
  fromString = CategoryName . fromString

data CategoryType
  = Assets
  | Liabilities
  | Stock
  | Revenue
  | Expenses
  deriving (Show, Read, Ord, Eq, Enum)

-- =============
-- = Modifiers =
-- =============
{-| Convert from `YearTransactions` to `Transactions`.
-}
year :: Year -> YearTransactions -> Transactions
year y yts = modify . flip mappend $ fmap ($ y) fs
  where
    fs = execState yts mempty

{-| Convert from `MonthTransactions` to `YearTransactions`.
-}
month :: Month -> MonthTransactions -> YearTransactions
month m mts = modify . flip mappend $ fmap ($ m) fs
  where
    fs = execState mts mempty

{-| Convert from `DayTransactions` to `MonthTransactions`.
-}
activity :: Day -> Description -> DayTransactions -> MonthTransactions
activity d desc dts = modify . flip mappend $ fmap (($ desc) . ($ d)) fs
  where
    fs = execState dts mempty

dayTrans :: CategoryType
         -> CategoryName
         -> Description
         -> Amount
         -> DayTransactions
dayTrans categ name desc amount =
  modify $ flip mappend $
    DList.singleton $ \d desc' m y ->
      Transaction
      { tYear = y
      , tMonth = m
      , tDay = d
      , tDescription = desc' <> " " <> desc
      , tCategory = Category {cName = name, cType = categ}
      , tAmount = amount
      }


{- $setup

>>> :set -XOverloadedStrings
>>> :{
let
  sample =
    year 2015 $ do
      month 1 $ do
        activity 3 "予定1" $ do
          dayTrans Assets "預金" "最初の預金" 1000
          dayTrans Expenses "通信費" "切手代" 80
        activity 4 "予定2" $
          dayTrans Expenses "通信費" "携帯料金" 3000
      month 2 $
        activity 4 "予定3" $
          dayTrans Expenses "通信費" "携帯料金" 3010
:}
-}

{-|
>>> DList.toList $ execState sample mempty
[Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 1}, tDay = Day {unDay = 3}, tDescription = Description {unDescription = "\20104\23450\&1 \26368\21021\12398\38928\37329"}, tCategory = Category {cName = CategoryName {unCategoryName = "\38928\37329"}, cType = Assets}, tAmount = Amount {unAmount = 1000}},Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 1}, tDay = Day {unDay = 3}, tDescription = Description {unDescription = "\20104\23450\&1 \20999\25163\20195"}, tCategory = Category {cName = CategoryName {unCategoryName = "\36890\20449\36027"}, cType = Expenses}, tAmount = Amount {unAmount = 80}},Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 1}, tDay = Day {unDay = 4}, tDescription = Description {unDescription = "\20104\23450\&2 \25658\24111\26009\37329"}, tCategory = Category {cName = CategoryName {unCategoryName = "\36890\20449\36027"}, cType = Expenses}, tAmount = Amount {unAmount = 3000}},Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 2}, tDay = Day {unDay = 4}, tDescription = Description {unDescription = "\20104\23450\&3 \25658\24111\26009\37329"}, tCategory = Category {cName = CategoryName {unCategoryName = "\36890\20449\36027"}, cType = Expenses}, tAmount = Amount {unAmount = 3010}}]
-}
