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
import Data.Text (Text, pack)
import GHC.Generics (Generic)

type Transactions = State Transaction ()

data Transaction = Transaction
  { tYear :: Int
  , tMonth :: Int
  , tDate :: Int
  , tDescription :: Text
  , tSubDesc :: Text
  , tCategory :: Text
  , tAmount :: Int
  } deriving (Show, Read, Ord, Eq, Default, Generic)

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
