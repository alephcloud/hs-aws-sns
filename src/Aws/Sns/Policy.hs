-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Aws.Sns.Policy
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_SpecialInfo.html>
--
module Aws.Sns.Policy
( SnsPolicy

#if ! UNTYPED_POLICY
, SnsConditionKey(..)
#endif

-- TODO reexport everything that is needed to write and parse SNS Policies
) where

import Data.Aeson

#if ! UNTYPED_POLICY
import Aws.General
import Aws.Sns.Policy.Internal

import Control.Applicative

import Data.Typeable

import Text.Parser.Combinators
import Text.Parser.Char
#endif

-- -------------------------------------------------------------------------- --
-- SNS Policy

#if UNTYPED_POLICY
type SnsPolicy = Value
#else
type SnsPolicy = ServicePolicy SnsConditionKey
#endif

-- -------------------------------------------------------------------------- --
-- SNS Policy Key

#if ! UNTYPED_POLICY
data SnsConditionKey (a ∷ AwsConditionType) where
    SNSProtocol ∷ SnsConditionKey AwsString
    SNSEndpoint ∷ SnsConditionKey AwsString

deriving instance Show (SnsConditionKey a)
deriving instance Eq (SnsConditionKey a)
deriving instance Ord (SnsConditionKey a)
deriving instance Typeable SnsConditionKey

instance Typeable a ⇒ AwsType (SnsConditionKey a) where
    toText SNSProtocol = "sns:Protocol"
    toText SNSEndpoint = "sns:Endpoint"

    parse = p >>= maybe (unexpected "type error") return
      where
        p ∷ (Monad m, CharParsing m, Typeable a) ⇒ m (Maybe (SnsConditionKey a))
        p = c SNSProtocol <$ text "sns:Protocol"
            <|> c SNSEndpoint <$ text "sns:Endpoint"
            <?> "SnsConditionKey"
        c ∷ Typeable b ⇒ SnsConditionKey b → Maybe (SnsConditionKey a)
        c = cast

instance Typeable a ⇒ FromJSON (SnsConditionKey a) where
    parseJSON = withText "SnsConditionKey"
        $ either fail return . fromText

instance ConditionKey SnsConditionKey
#endif

{-
-- -------------------------------------------------------------------------- --
-- Policy Action

-- | TODO
--
data SnsPolicyAction
    = SnsPolicyActionAny
    | SnsPolicyAction [SnsAction]
    deriving (Show, Read, Eq, Ord, Typeable)

instance ToJSON SnsPolicyAction where
    toJSON SnsPolicyActionAny = toJSON ["*"]
    toJSON (SnsPolicyAction l) = toJSON l

instance FromJSON SnsPolicyAction where
    parseJSON = withArray "Action" $ V.foldM f (SnsPolicyAction [])
      where
        f SnsPolicyActionAny _ = return SnsPolicyActionAny
        f (SnsPolicyAction t) x = withText "SnsAction" $ \case
            "*" → pure SnsPolicyActionAny
            x → SnsPolicyAction . (:t) <$> parseJSON x
-}

