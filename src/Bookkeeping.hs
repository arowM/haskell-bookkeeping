{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Bookkeeping
  ( Transactions
  , Transaction
  , Category
  , CategoryType
  ) where

import Control.Monad.State (State)
import Data.Text (Text)

type Transactions =
  State Transaction ()

data Transaction = Transaction
  { tYear        :: Int
  , tMonth       :: Int
  , tDate        :: Int
  , tDescription :: Text
  , tSubDesc     :: Text
  , tCategory    :: Text
  , tAmount      :: Int
  } deriving (Show, Read, Ord, Eq)


data Category = Category
  { cName :: Text
  , cType :: CategoryType
  } deriving (Show, Read, Ord, Eq)


data CategoryType
  = Assets
  | Liabilities
  | Stock
  | Revenue
  | Expenses
  deriving (Show, Read, Ord, Eq, Enum)
