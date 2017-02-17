module Business.Bookkeeping
  ( year
  , month
  , activity
  , dateTrans
  , runTransactions
  , module X
  ) where

import Control.Monad.State (execState, modify)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Data.Time.Calendar (fromGregorian)

import Business.Bookkeeping.Types
import qualified Business.Bookkeeping.Types as X

{-| Convert from 'YearTransactions' to 'Transactions'.
-}
year :: Year -> YearTransactions -> Transactions
year y yts = modify . flip mappend $ fmap ($ y) fs
  where
    fs = execState yts mempty

{-| Convert from 'MonthTransactions' to 'YearTransactions'.
-}
month :: Month -> MonthTransactions -> YearTransactions
month m mts = modify . flip mappend $ fmap ($ m) fs
  where
    fs = execState mts mempty

{-| Convert from 'DateTransactions' to 'MonthTransactions'.
-}
activity :: Date -> Description -> DateTransactions -> MonthTransactions
activity d desc dts = modify . flip mappend $ fmap (($ desc) . ($ d)) fs
  where
    fs = execState dts mempty

dateTrans :: DebitCategory
          -> CreditCategory
          -> Description
          -> Amount
          -> DateTransactions
dateTrans debit credit desc amount =
  modify $
  flip mappend $
  DList.singleton $ \d desc' m y ->
    Transaction
    { tDay = fromGregorian (unYear y) (unMonth m) (unDate d)
    , tDescription = desc' <> " " <> desc
    , tDebit = debit
    , tCredit = credit
    , tAmount = amount
    }

runTransactions :: Transactions -> [Transaction]
runTransactions ts = DList.toList $ execState ts mempty

{- $setup

>>> :set -XOverloadedStrings
>>> import qualified Data.Text as T
>>> import qualified Data.Text.IO as T
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
  format :: Transaction -> T.Text
  format (Transaction {..}) = T.unlines
    [ "tDay: " <> (T.pack . show) tDay
    , "tDescription: " <> unDescription tDescription
    , "tDebit: " <> (unCategoryName . cName . unDebitCategory) tDebit
    , "tCredit: " <> (unCategoryName . cName . unCreditCategory) tCredit
    , "tAmount: " <> (T.pack . show . unAmount) tAmount
    ]
:}
-}
{-|
>>> T.putStr $ T.unlines $ map format $ runTransactions sample
tDay: 2015-01-03
tDescription: 予定1 最初の預金
tDebit: 預金
tCredit: 事業主借
tAmount: 1000
<BLANKLINE>
tDay: 2015-01-03
tDescription: 予定1 切手代
tDebit: 通信費
tCredit: 事業主借
tAmount: 80
<BLANKLINE>
tDay: 2015-01-04
tDescription: 予定2 携帯料金
tDebit: 通信費
tCredit: 事業主借
tAmount: 3000
<BLANKLINE>
tDay: 2015-02-04
tDescription: 予定3 携帯料金
tDebit: 通信費
tCredit: 事業主借
tAmount: 3010
<BLANKLINE>
-}
