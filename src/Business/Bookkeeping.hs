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
  advance categ name = dateTrans
    (DebitCategory $ Category name categ)
    (CreditCategory $ Category "事業主借" Liabilities)
  sample =
    year 2015 $ do
      month 1 $ do
        activity 3 "予定1" $ do
          advance Assets "預金" "最初の預金" 1000
          advance Expenses "通信費" "切手代" 80
        activity 4 "予定2" $
          advance Expenses "通信費" "携帯料金" 3000
      month 2 $
        activity 4 "予定3" $
          advance Expenses "通信費" "携帯料金" 3010
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
tDay: 2015-01-03
tDescription: 予定1 最初の預金
tDebit: 預金 (Assets)
tCredit: 事業主借 (Liabilities)
tAmount: 1000
<BLANKLINE>
tDay: 2015-01-03
tDescription: 予定1 切手代
tDebit: 通信費 (Expenses)
tCredit: 事業主借 (Liabilities)
tAmount: 80
<BLANKLINE>
tDay: 2015-01-04
tDescription: 予定2 携帯料金
tDebit: 通信費 (Expenses)
tCredit: 事業主借 (Liabilities)
tAmount: 3000
<BLANKLINE>
tDay: 2015-02-04
tDescription: 予定3 携帯料金
tDebit: 通信費 (Expenses)
tCredit: 事業主借 (Liabilities)
tAmount: 3010
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
