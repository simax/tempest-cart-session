module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

main :: IO ()
main = print "tempest-cart-sessions!!!"

{-# LANGUAGE OverloadedRecordFields #-}

data CartSession = CartSession
    { cartSessionId :: Int
    , uID :: Double
    , customerid :: Int
    , orderdate :: UTCTime
    , deliveryMethod :: Int
    , totalTax :: Double
    , totalDelivery :: Double 
    , totalSub :: Double 
    , totalFinal :: Double 
    , countryCode :: String
    , currencyCode :: String
    , orderNumber :: String
    , discountSystem_Triggers :: String
    , discountSystem_Tracking :: String
    , discountSystem_Coupons :: String
    , discountSystem_Total :: Double
    , discountSystem_Rewards :: String 
    , hashCode :: String 
    , gUID :: String 
    , abandonedCartEmailSend :: String 
    , postCode :: String 
    , deliveryMethodSetByUser :: Bool
    , hasEstimatedDelivery :: Bool
    , updateLock :: Int
    , updateLockId :: String 
    , hasValidVatNumber :: Bool
}

instance Show CartSession where
   show cs = mconcat [ 
                       show $ cartSessionId cs
                     , "\n"
                     , show $ uID cs
                     , "\n"
                     , show $ customerid cs
                     , "\n"
                     , show $ orderdate cs
                     , "\n"
                     , show $ deliveryMethod cs
                     , "\n"
                     , show $ totalTax cs
                     , "\n"
                     , show $ totalDelivery cs
                     , "\n"
                     , show $ totalSub cs
                     , "\n"
                     , show $ totalFinal cs
                     , "\n"
                     , countryCode cs
                     , "\n"
                     , currencyCode cs
                     , "\n"
                     , orderNumber cs
                     , "\n"
                     , discountSystem_Triggers cs
                     , "\n"
                     , discountSystem_Tracking cs
                     , "\n"
                     , discountSystem_Coupons cs
                     , "\n"
                     , show $ discountSystem_Total cs
                     , "\n"
                     , discountSystem_Rewards cs
                     , "\n"
                     , hashCode cs
                     , "\n"
                     , gUID cs
                     , "\n"
                     , abandonedCartEmailSend cs
                     , "\n"
                     , postCode cs
                     , "\n"
                     , show $ deliveryMethodSetByUser cs
                     , "\n"
                     , show $ hasEstimatedDelivery cs
                     , "\n"
                     , show $ updateLock cs
                     , "\n"
                     , updateLockId cs
                     , "\n"
                     , show $ hasValidVatNumber cs
                     , "\n"]


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
printCartSessions = withConn "./../data/shop-db.sqlite" $
             \conn ->  do
            --    resp <- query_ conn "SELECT id, uId, customerid, orderdate FROM tbl_cartSessions LIMIT 1;" :: IO [CartSession]
               resp <- query_ conn "SELECT * FROM tbl_cartSessions LIMIT 1;" :: IO [CartSession]
               mapM_ print resp
                       
