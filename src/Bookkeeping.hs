{-# LANGUAGE DeriveFunctor #-}
module Bookkeeping
  (
  ) where

import Control.Monad.State (State)

newtype Transactions m a =
  Transactions { unTransactions :: StateT Transaction m a }

data Transaction = Transaction
  { tYear        :: {-# UNPACK #-} !Int
  , tMonth       :: {-# UNPACK #-} !Int
  , tDate        :: {-# UNPACK #-} !Int
  , tDescription :: {-# UNPACK #-} !Text
  , tSubDesc     :: {-# UNPACK #-} !Text
  , tCategory    :: {-# UNPACK #-} !Text
  , tAmount      :: {-# UNPACK #-} !Int
  } deriving (Show, Read, Ord, Eq, Enum)


data Category = Category
  { name :: {-# UNPACK #-} !Text
  , categoryType :: {-# UNPACK #-} !CategoryType
  } deriving (Show, Read, Ord, Eq, Enum)


data CategoryType
  = Assets
  | Liabilities
  | Stock
  | Revenue
  | Expenses
  deriving (Show, Read, Ord, Eq, Enum, Functor)
