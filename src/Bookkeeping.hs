{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bookkeeping
  ( Transactions
  , Transaction
  , Category
  , CategoryType
  , year
  , month
  , activity
  ) where

import Control.Monad.State (State, execState, put)
import Data.Default (Default(def))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text, pack)
import GHC.Generics (Generic)

type Transactions = State (DList Transaction) ()
type YearTransactions = State (DList (Year -> Transaction)) ()
type MonthTransactions = State (DList (Month -> Year -> Transaction)) ()
type DayTransactions = State (DList (Day -> Text -> Month -> Year -> Transaction)) ()

{-| A type representing a transaction.
 -}
data Transaction = Transaction
  { tYear :: Year
  , tMonth :: Month
  , tDay :: Day
  , tDescription :: Text
  , tCategory :: Text
  , tAmount :: Int
  } deriving (Show, Read, Ord, Eq, Default, Generic)

newtype Year = Year { unYear :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)
newtype Month = Month { unMonth :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)
newtype Day = Day { unDay :: Int }
  deriving (Show, Read, Ord, Eq, Default, Generic)

{-| A type representing an accounts title.
 -}
data Category = Category
  { cName :: Text
  , cType :: CategoryType
  } deriving (Show, Read, Ord, Eq, Default, Generic)

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
activity :: Day -> Text -> DayTransactions -> MonthTransactions
activity d desc dts = put $ fmap (($ desc) . ($ d)) fs
  where
    fs = execState dts mempty

-- ====================
-- = Orphan instances =
-- ====================
instance Default Text where
  def = pack (def :: String)
