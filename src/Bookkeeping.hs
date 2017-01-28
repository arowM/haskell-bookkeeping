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
  ) where

import Control.Monad.State (State)
import Data.Default (Default(def))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text, pack)
import GHC.Generics (Generic)

type Transactions = State (DList Transaction) ()

{-| A type representing a transaction.
 -}
data Transaction = Transaction
  { tYear :: Year
  , tMonth :: Month
  , tDay :: Day
  , tDescription :: Text
  , tSubDesc :: Text
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

-- ====================
-- = Orphan instances =
-- ====================
instance Default Text where
  def = pack (def :: String)
