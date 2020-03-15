module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

main :: IO ()
main = print "tempest-cart-sessions!!!"

{-# LANGUAGE OverloadedRecordFields #-}

-- To build on save...
-- stack exec -- ghcid -c "stack ghci tempest-cart-session"

data CartSession = CartSession
    { cartSessionId :: Maybe Int
    , uID :: Maybe Double
    , customerid :: Maybe Int
    , orderdate :: Maybe UTCTime
    , deliveryMethod :: Maybe Int
    , totalTax :: Maybe Double
    , totalDelivery :: Maybe Double 
    , totalSub :: Maybe Double 
    , totalFinal :: Maybe Double 
    , countryCode :: Maybe String
    , currencyCode :: Maybe String
    , orderNumber :: Maybe String
    , discountSystem_Triggers :: Maybe String
    , discountSystem_Tracking :: Maybe String
    , discountSystem_Coupons :: Maybe String
    , discountSystem_Total :: Maybe Double
    , discountSystem_Rewards :: Maybe String 
    , hashCode :: Maybe String 
    , gUID :: Maybe String 
    , abandonedCartEmailSend :: Maybe String 
    , postCode :: Maybe String 
    , deliveryMethodSetByUser :: Maybe Bool
    , hasEstimatedDelivery :: Maybe Bool
    , updateLock :: Maybe Int
    , updateLockId :: Maybe String 
    , hasValidVatNumber :: Maybe Bool
}

instance Show CartSession where
   show cs = mconcat [ 
                       nullableShow $ cartSessionId cs
                     , "\n"
                     , nullableShow $ uID cs
                     , "\n"
                     , nullableShow $ customerid cs
                     , "\n"
                     , nullableShow $ orderdate cs
                     , "\n"
                     , nullableShow $ deliveryMethod cs
                     , "\n"
                     , nullableShow $ totalTax cs
                     , "\n"
                     , nullableShow $ totalDelivery cs
                     , "\n"
                     , nullableShow $ totalSub cs
                     , "\n"
                     , nullableShow $ totalFinal cs
                     , "\n"
                     , nullableShow $ countryCode cs
                     , "\n"
                     , nullableShow $ currencyCode cs
                     , "\n"
                     , nullableShow $ orderNumber cs
                     , "\n"
                     , nullableShow $ discountSystem_Triggers cs
                     , "\n"
                     , nullableShow $ discountSystem_Tracking cs
                     , "\n"
                     , nullableShow $ discountSystem_Coupons cs
                     , "\n"
                     , nullableShow $ discountSystem_Total cs
                     , "\n"
                     , nullableShow $ discountSystem_Rewards cs
                     , "\n"
                     , nullableShow $ hashCode cs
                     , "\n"
                     , nullableShow $ gUID cs
                     , "\n"
                     , nullableShow $ abandonedCartEmailSend cs
                     , "\n"
                     , nullableShow $ postCode cs
                     , "\n"
                     , nullableShow $ deliveryMethodSetByUser cs
                     , "\n"
                     , nullableShow $ hasEstimatedDelivery cs
                     , "\n"
                     , nullableShow $ updateLock cs
                     , "\n"
                     , nullableShow $ updateLockId cs
                     , "\n"
                     , nullableShow $ hasValidVatNumber cs
                     , "\n"]


nullableShow :: Show a => Maybe a -> String
nullableShow Nothing = "Null"
nullableShow (Just v) = show v   

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn

instance FromRow CartSession where
   fromRow = CartSession <$> field 
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field

printCartSessions :: IO ()
printCartSessions = withConn "./data/shop-db.sqlite" $
             \conn ->  do
               -- resp <- query_ conn "SELECT id, uId, customerid, orderdate, discountSystem_Total, abandonedCartEmailSend FROM tbl_cartSessions LIMIT 1;" :: IO [CartSession]
               resp <- query_ conn "SELECT * FROM tbl_cartSessions LIMIT 1;" :: IO [CartSession]
            --    resp <- query_ conn "SELECT * FROM tbl_cartSessions LIMIT 1;" :: IO [CartSession]
               mapM_ print resp
                       
