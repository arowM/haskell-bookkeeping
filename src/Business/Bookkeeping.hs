{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Business.Bookkeeping

Copyright   :  Kadzuya Okamoto 2017
License     :  MIT

Stability   :  experimental
Portability :  unknown

This module exports core functions and types for bookkeeping.
-}
module Business.Bookkeeping
  (
  -- * Usage examples
  -- $setup
  -- * Constructors
    year
  , month
  , activity
  , dateTrans

  -- * Converters
  , runTransactions

  -- * Pritty printers
  , ppr

  -- * Types
  , Transactions
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

import Control.Monad.State (State, execState, modify)
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (Day, fromGregorian)

{- $setup
>>> :{
let
  advance :: CategoryName -> Description -> Amount -> DateTransactions
  advance name = dateTrans
    (DebitCategory $ Category name Expenses)
    (CreditCategory $ Category "Deposit" Assets)
  sample =
    year 2015 $ do
      month 1 $ do
        activity 1 "Constant expenses --" $
          advance "Communication" "Mobile phone" 3000
        activity 3 "Mail a contract --" $ do
          advance "Communication" "Stamp" 50
          advance "Office supplies" "Envelope" 100
      month 2 $
        activity 1 "Constant expenses --" $
          advance "Communication" "Mobile phone" 3000
:}
-}
{-| Convert from 'YearTransactions' to 'Transactions'.
-}
year :: Year -> YearTransactions -> Transactions
year y =
  modify . flip mappend . fmap ($ y) . toDList

{-| Convert from 'MonthTransactions' to 'YearTransactions'.
-}
month :: Month -> MonthTransactions -> YearTransactions
month m =
  modify . flip mappend . fmap ($ m) . toDList

{-| Convert from 'DateTransactions' to 'MonthTransactions'.
-}
activity :: Date -> Description -> DateTransactions -> MonthTransactions
activity d desc =
  modify . flip mappend . fmap (($ desc) . ($ d)) . toDList

dateTrans :: DebitCategory
          -> CreditCategory
          -> Description
          -> Amount
          -> DateTransactions
dateTrans debit credit desc amount =
  modify . flip mappend . DList.singleton $ \d desc' m y ->
    Transaction
    { tDay = fromGregorian (unYear y) (unMonth m) (unDate d)
    , tDescription = desc' <> " " <> desc
    , tDebit = debit
    , tCredit = credit
    , tAmount = amount
    }

{-| Take list of `Transaction` out from 'Transactions'.
-}
runTransactions :: Transactions -> [Transaction]
runTransactions = DList.toList . toDList

toDList :: Trans a -> DList a
toDList ts = execState ts mempty

{-| A pretty printer for `Transactions`.

>>> ppr sample
tDay: 2015-01-01
tDescription: Constant expenses -- Mobile phone
tDebit: Communication (Expenses)
tCredit: Deposit (Assets)
tAmount: 3000
<BLANKLINE>
tDay: 2015-01-03
tDescription: Mail a contract -- Stamp
tDebit: Communication (Expenses)
tCredit: Deposit (Assets)
tAmount: 50
<BLANKLINE>
tDay: 2015-01-03
tDescription: Mail a contract -- Envelope
tDebit: Office supplies (Expenses)
tCredit: Deposit (Assets)
tAmount: 100
<BLANKLINE>
tDay: 2015-02-01
tDescription: Constant expenses -- Mobile phone
tDebit: Communication (Expenses)
tCredit: Deposit (Assets)
tAmount: 3000
<BLANKLINE>
-}
ppr :: Transactions -> IO ()
ppr = T.putStr . T.unlines . map format . runTransactions
  where
    format :: Transaction -> T.Text
    format Transaction {..} =
      T.unlines
        [ "tDay: " <> (T.pack . show) tDay
        , "tDescription: " <> unDescription tDescription
        , "tDebit: " <> (unCategoryName . cName . unDebitCategory) tDebit <>
          " (" <>
          (T.pack . show . cType . unDebitCategory) tDebit <>
          ")"
        , "tCredit: " <> (unCategoryName . cName . unCreditCategory) tCredit <>
          " (" <>
          (T.pack . show . cType . unCreditCategory) tCredit <>
          ")"
        , "tAmount: " <> (T.pack . show . unAmount) tAmount
        ]


{- ==============
 -     Types
 - ============== -}

type Trans a = State (DList a) ()

{-| A type for handling `Transaction` values.
 -}
type Transactions = Trans Transaction

type YearTransactions = Trans (Year -> Transaction)

type MonthTransactions = Trans (Month -> Year -> Transaction)

type DateTransactions = Trans (Date -> Description -> Month -> Year -> Transaction)

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
