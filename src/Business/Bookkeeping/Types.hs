{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Business.Bookkeeping.Types
  ( Transactions
  , YearTransactions
  , MonthTransactions
  , DateTransactions
  , Transaction(..)
  , Year(..)
  , Month(..)
  , Date(..)
  , Description(..)
  , Amount(..)
  , Category(..)
  , CategoryName(..)
  , CategoryType(..)
  , DebitCategory(..)
  , CreditCategory(..)
  ) where

import Control.Monad.State (State)
import Data.DList (DList)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time.Calendar (Day)

{-| An alias for handling `Transaction` values with `State` monad.
 -}
type Transactions = State (DList Transaction) ()

type YearTransactions = State (DList (Year -> Transaction)) ()

type MonthTransactions = State (DList (Month -> Year -> Transaction)) ()

type DateTransactions = State (DList (Date -> Description -> Month -> Year -> Transaction)) ()

{-| A type representing a transaction.
 -}
data Transaction = Transaction
  { tDay :: Day
  , tDescription :: Description
  , tDebit :: DebitCategory
  , tCredit :: CreditCategory
  , tAmount :: Amount
  } deriving (Show, Read, Ord, Eq)

newtype Year = Year
  { unYear :: Integer
  } deriving (Show, Read, Ord, Eq, Num)

newtype Month = Month
  { unMonth :: Int
  } deriving (Show, Read, Ord, Eq, Num)

newtype Date = Date
  { unDate :: Int
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

newtype DebitCategory = DebitCategory
  { unDebitCategory :: Category
  } deriving (Show, Read, Ord, Eq)

newtype CreditCategory = CreditCategory
  { unCreditCategory :: Category
  } deriving (Show, Read, Ord, Eq)

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
