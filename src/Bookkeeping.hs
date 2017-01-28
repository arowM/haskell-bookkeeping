{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Bookkeeping
  (
  ) where

import Control.Monad.State (State)

newtype Transactions m a =
  Transactions { unTransactions :: StateT Transaction m a }

data Transaction = Transaction
  { tYear        :: Int
  , tMonth       :: Int
  , tDate        :: Int
  , tDescription :: Text
  , tSubDesc     :: Text
  , tCategory    :: Text
  , tAmount      :: Int
  } deriving (Show, Read, Ord, Eq, Enum)


data Category = Category
  { cName :: Text
  , cType :: CategoryType
  } deriving (Show, Read, Ord, Eq, Enum)


data CategoryType
  = Assets
  | Liabilities
  | Stock
  | Revenue
  | Expenses
  deriving (Show, Read, Ord, Eq, Enum, Functor)
