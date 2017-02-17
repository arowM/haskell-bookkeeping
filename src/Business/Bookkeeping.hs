module Business.Bookkeeping
  ( year
  , month
  , activity
  , dayTrans
  , runTransactions
  , module X
  ) where

import Control.Monad.State (execState, modify)
import qualified Data.DList as DList
import Data.Monoid ((<>))

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

{-| Convert from 'DayTransactions' to 'MonthTransactions'.
-}
activity :: Day -> Description -> DayTransactions -> MonthTransactions
activity d desc dts = modify . flip mappend $ fmap (($ desc) . ($ d)) fs
  where
    fs = execState dts mempty

dayTrans :: DebitCategory
         -> CreditCategory
         -> Description
         -> Amount
         -> DayTransactions
dayTrans debit credit desc amount =
  modify $
  flip mappend $
  DList.singleton $ \d desc' m y ->
    Transaction
    { tYear = y
    , tMonth = m
    , tDay = d
    , tDescription = desc' <> " " <> desc
    , tDebit = debit
    , tCredit = credit
    , tAmount = amount
    }

runTransactions :: Transactions -> [Transaction]
runTransactions ts = DList.toList $ execState ts mempty

{- $setup

>>> :set -XOverloadedStrings
>>> :{
let
  advance categ name = dayTrans
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
{-|
>>> DList.toList $ execState sample mempty
[Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 1}, tDay = Day {unDay = 3}, tDescription = Description {unDescription = "\20104\23450\&1 \26368\21021\12398\38928\37329"}, tDebit = DebitCategory {unDebitCategory = Category {cName = CategoryName {unCategoryName = "\38928\37329"}, cType = Assets}}, tCredit = CreditCategory {unCreditCategory = Category {cName = CategoryName {unCategoryName = "\20107\26989\20027\20511"}, cType = Liabilities}}, tAmount = Amount {unAmount = 1000}},Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 1}, tDay = Day {unDay = 3}, tDescription = Description {unDescription = "\20104\23450\&1 \20999\25163\20195"}, tDebit = DebitCategory {unDebitCategory = Category {cName = CategoryName {unCategoryName = "\36890\20449\36027"}, cType = Expenses}}, tCredit = CreditCategory {unCreditCategory = Category {cName = CategoryName {unCategoryName = "\20107\26989\20027\20511"}, cType = Liabilities}}, tAmount = Amount {unAmount = 80}},Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 1}, tDay = Day {unDay = 4}, tDescription = Description {unDescription = "\20104\23450\&2 \25658\24111\26009\37329"}, tDebit = DebitCategory {unDebitCategory = Category {cName = CategoryName {unCategoryName = "\36890\20449\36027"}, cType = Expenses}}, tCredit = CreditCategory {unCreditCategory = Category {cName = CategoryName {unCategoryName = "\20107\26989\20027\20511"}, cType = Liabilities}}, tAmount = Amount {unAmount = 3000}},Transaction {tYear = Year {unYear = 2015}, tMonth = Month {unMonth = 2}, tDay = Day {unDay = 4}, tDescription = Description {unDescription = "\20104\23450\&3 \25658\24111\26009\37329"}, tDebit = DebitCategory {unDebitCategory = Category {cName = CategoryName {unCategoryName = "\36890\20449\36027"}, cType = Expenses}}, tCredit = CreditCategory {unCreditCategory = Category {cName = CategoryName {unCategoryName = "\20107\26989\20027\20511"}, cType = Liabilities}}, tAmount = Amount {unAmount = 3010}}]
-}
