{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Bookkeeping
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

import Control.Monad.State (State, execState, put)
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
year y yts = put $ fmap ($ y) fs
  where
    fs = execState yts mempty

{-| Convert from `MonthTransactions` to `YearTransactions`.
-}
month :: Month -> MonthTransactions -> YearTransactions
month m mts = put $ fmap ($ m) fs
  where
    fs = execState mts mempty

{-| Convert from `DayTransactions` to `MonthTransactions`.
-}
activity :: Day -> Description -> DayTransactions -> MonthTransactions
activity d desc dts = put $ fmap (($ desc) . ($ d)) fs
  where
    fs = execState dts mempty

dayTrans :: CategoryType
         -> CategoryName
         -> Description
         -> Amount
         -> DayTransactions
dayTrans categ name desc amount =
  put $
  DList.singleton $ \d desc' m y ->
    Transaction
    { tYear = y
    , tMonth = m
    , tDay = d
    , tDescription = desc' <> " " <> desc
    , tCategory = Category {cName = name, cType = categ}
    , tAmount = amount
    }
