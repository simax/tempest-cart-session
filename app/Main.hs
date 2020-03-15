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
                       maybeShow $ cartSessionId cs
                     , "\n"
                     , maybeShow $ uID cs
                     , "\n"
                     , maybeShow $ customerid cs
                     , "\n"
                     , maybeShow $ orderdate cs
                     , "\n"
                     , maybeShow $ deliveryMethod cs
                     , "\n"
                     , maybeShow $ totalTax cs
                     , "\n"
                     , maybeShow $ totalDelivery cs
                     , "\n"
                     , maybeShow $ totalSub cs
                     , "\n"
                     , maybeShow $ totalFinal cs
                     , "\n"
                     , maybeShow $ countryCode cs
                     , "\n"
                     , maybeShow $ currencyCode cs
                     , "\n"
                     , maybeShow $ orderNumber cs
                     , "\n"
                     , maybeShow $ discountSystem_Triggers cs
                     , "\n"
                     , maybeShow $ discountSystem_Tracking cs
                     , "\n"
                     , maybeShow $ discountSystem_Coupons cs
                     , "\n"
                     , maybeShow $ discountSystem_Total cs
                     , "\n"
                     , maybeShow $ discountSystem_Rewards cs
                     , "\n"
                     , maybeShow $ hashCode cs
                     , "\n"
                     , maybeShow $ gUID cs
                     , "\n"
                     , maybeShow $ abandonedCartEmailSend cs
                     , "\n"
                     , maybeShow $ postCode cs
                     , "\n"
                     , maybeShow $ deliveryMethodSetByUser cs
                     , "\n"
                     , maybeShow $ hasEstimatedDelivery cs
                     , "\n"
                     , maybeShow $ updateLock cs
                     , "\n"
                     , maybeShow $ updateLockId cs
                     , "\n"
                     , maybeShow $ hasValidVatNumber cs
                     , "\n"]


maybeShow :: Show a => Maybe a -> String
maybeShow Nothing = "Null"
maybeShow (Just v) = show v   

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
                       
