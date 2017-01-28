{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , CategoryType
  , year
  , month
  , activity
  , assets
  ) where

import Control.Monad.State (State, execState, put)
import Data.Default (Default(def))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)

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
  } deriving (Show, Read, Ord, Eq, Default, Generic)

newtype Year = Year { unYear :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)
newtype Month = Month { unMonth :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)
newtype Day = Day { unDay :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)
newtype Description = Description { unDescription :: Text }
  deriving (Show, Read, Ord, Eq, Default, Generic)
instance IsString Description where
  fromString = Description . fromString
instance Monoid Description where
  mempty = Description mempty
  mappend (Description a) (Description b) = Description $ mappend a b
newtype Amount = Amount { unAmount :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)

{-| A type representing an accounts title.
 -}
data Category = Category
  { cName :: CategoryName
  , cType :: CategoryType
  } deriving (Show, Read, Ord, Eq, Default, Generic)

newtype CategoryName = CategoryName { unCategoryName :: Text }
  deriving (Show, Read, Ord, Eq, Default, Generic)
instance IsString CategoryName where
  fromString = CategoryName . fromString

data CategoryType
  = Assets
  | Liabilities
  | Stock
  | Revenue
  | Expenses
  deriving (Show, Read, Ord, Eq, Enum)

instance Default CategoryType where
  def = Assets

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

-- ====================
-- = Orphan instances =
-- ====================
instance Default Text where
  def = pack (def :: String)
