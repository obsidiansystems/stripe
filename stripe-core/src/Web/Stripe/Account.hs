{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Account
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#account >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Account
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config getAccountDetails
--   case result of
--     Right account    -> print account
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Account where

import qualified Data.ByteString as BS
import Data.Default.Class
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Web.Stripe.StripeRequest
import Web.Stripe.Types
import Web.Stripe.Util

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
data GetAccountDetails
type instance StripeReturn GetAccountDetails = Account
getAccountDetails :: StripeRequest GetAccountDetails
getAccountDetails = request
  where request = mkStripeRequest GET url params
        url     = "account"
        params  = []

------------------------------------------------------------------------------
-- | Create a connected account

data AccountType = AccountType_Managed
                 | AccountType_Standalone
  deriving (Eq, Ord, Show, Read)

--TODO: Support more parameters for NewAccount
data NewAccount = NewAccount
       { _newAccount_managed :: Maybe AccountType
       , _newAccount_email :: Maybe Email
       , _newAccount_country :: Maybe Country
       , _newAccount_external_account :: Maybe TokenId
       , _newAccount_legal_entity_business_name :: Maybe Text
       , _newAccount_legal_entity_business_tax_id :: Maybe Text
       , _newAccount_legal_entity_dob_day :: Maybe Int
       , _newAccount_legal_entity_dob_month :: Maybe Int
       , _newAccount_legal_entity_dob_year :: Maybe Int
       , _newAccount_legal_entity_first_name :: Maybe Text
       , _newAccount_legal_entity_last_name :: Maybe Text
       , _newAccount_tos_acceptance_date :: Maybe Int
       , _newAccount_tos_acceptance_ip :: Maybe Text
       }
  deriving (Eq, Ord, Show, Read)

instance ToStripeParam NewAccount where
  toStripeParam n =
    let lp = ("legal_entity" <>) . BS.concat . map (\t -> BS.concat ["[", t, "]"])
        tos = ("tos_acceptance" <>) . BS.concat . map (\t -> BS.concat ["[", t, "]"])
    in (getParams
          [ ("managed", _newAccount_managed n <&> \case
               AccountType_Managed -> "true"
               AccountType_Standalone -> "false")
          , ("email", _newAccount_email n <&> \(Email e) -> e)
          , ("country", _newAccount_country n <&> \(Country c) -> c)
          , ("external_account", _newAccount_external_account n <&> \(TokenId tid) -> tid)
          , (lp ["type"], Just "company")
          , (lp ["business_name"], _newAccount_legal_entity_business_name n)
          , (lp ["business_tax_id"], _newAccount_legal_entity_business_tax_id n)
          , (lp ["dob", "day"], _newAccount_legal_entity_dob_day n <&> T.pack . show)
          , (lp ["dob", "month"], _newAccount_legal_entity_dob_month n <&> T.pack . show)
          , (lp ["dob", "year"], _newAccount_legal_entity_dob_year n <&> T.pack . show)
          , (lp ["first_name"], _newAccount_legal_entity_first_name n)
          , (lp ["last_name"], _newAccount_legal_entity_last_name n)
          , (tos ["date"], _newAccount_tos_acceptance_date n <&> T.pack . show)
          , (tos ["ip"], _newAccount_tos_acceptance_ip n)
          ] ++)

instance Default NewAccount where
  def = NewAccount Nothing Nothing Nothing
                   Nothing Nothing Nothing
                   Nothing Nothing Nothing
                   Nothing Nothing Nothing
                   Nothing

data CreateAccount
createAccount :: NewAccount -> StripeRequest CreateAccount
createAccount t = mkStripeRequest POST url params
  where url = "accounts"
        params = toStripeParam t
               $ []
type instance StripeReturn CreateAccount = CreatedAccount

------------------------------------------------------------------------------
-- | Update a connected account

--TODO: Support more parameters for UpdateAccount
--TODO: Support external account dictionaries as well as tokens
data UpdateAccountParams = UpdateAccountParams
       { _updateAccount_external_account :: Maybe TokenId
       , _updateAccount_legal_entity_business_name :: Maybe Text
       , _updateAccount_legal_entity_business_tax_id :: Maybe Text
       , _updateAccount_legal_entity_dob_day :: Maybe Int
       , _updateAccount_legal_entity_dob_month :: Maybe Int
       , _updateAccount_legal_entity_dob_year :: Maybe Int
       , _updateAccount_legal_entity_first_name :: Maybe Text
       , _updateAccount_legal_entity_last_name :: Maybe Text
       , _updateAccount_tos_acceptance_date :: Maybe Int
       , _updateAccount_tos_acceptance_ip :: Maybe Text
       }

instance ToStripeParam UpdateAccountParams where
  toStripeParam u =
    let lp = ("legal_entity" <>) . BS.concat . map (\t -> BS.concat ["[", t, "]"])
        tos = ("tos_acceptance" <>) . BS.concat . map (\t -> BS.concat ["[", t, "]"])
    in (getParams
          [ ("external_account", _updateAccount_external_account u <&> \(TokenId tid) -> tid)
          , (lp ["type"], Just "company")
          , (lp ["business_name"], _updateAccount_legal_entity_business_name u)
          , (lp ["business_tax_id"], _updateAccount_legal_entity_business_tax_id u)
          , (lp ["dob", "day"], _updateAccount_legal_entity_dob_day u <&> T.pack . show)
          , (lp ["dob", "month"], _updateAccount_legal_entity_dob_month u <&> T.pack . show)
          , (lp ["dob", "year"], _updateAccount_legal_entity_dob_year u <&> T.pack . show)
          , (lp ["first_name"], _updateAccount_legal_entity_first_name u)
          , (lp ["last_name"], _updateAccount_legal_entity_last_name u)
          , (tos ["date"], _updateAccount_tos_acceptance_date u <&> T.pack . show)
          , (tos ["ip"], _updateAccount_tos_acceptance_ip u)
          ] ++)

instance Default UpdateAccountParams where
  def = UpdateAccountParams Nothing Nothing Nothing
                            Nothing Nothing Nothing
                            Nothing Nothing Nothing
                            Nothing -- NOTHING!!!!!

data UpdateAccount
updateAccount :: AccountId -> UpdateAccountParams -> StripeRequest UpdateAccount
updateAccount (AccountId aid) u = mkStripeRequest POST url params
  where url = "accounts" </> aid
        params = toStripeParam u
               $ []

type instance StripeReturn UpdateAccount = Account
