{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module: Aws.Sns.Types
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/Welcome.html>
--
module Aws.Sns.Types
(
-- * Delivery Policy
  DeliveryPolicy

-- * SubscriptionAttributes
, SubscriptionAttributes(..)
) where

import Aws.General

import Data.Aeson
import qualified Data.Text as T
import Data.Typeable

-- -------------------------------------------------------------------------- --
-- Delivery Policy

type DeliveryPolicy = Value

-- -------------------------------------------------------------------------- --
-- Subscription Attributes

-- | Subscription Attributes
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/api/API_GetSubscriptionAttributes.html>
--
-- TODO find out which of the fields in the structure are optional and
-- which are required.
--
data SubscriptionAttributes = SubscriptionAttributes
    { subscriptionAttrSubscriptionArn :: !(Maybe Arn)
    -- ^ the subscription's ARN
    , subscriptionAttrTopicArn :: !(Maybe Arn)
    -- ^ the topic ARN that the subscription is associated with
    , subscriptionAttrOwner :: !(Maybe AccountId)
    -- ^ the AWS account ID of the subscription's owner
    , subscriptionAttrConfirmationWasAuthenticated :: !Bool
    -- ^ 'True' if the subscription confirmation request was authenticated
    , subscriptionAttrDeliveryPolicy :: !(Maybe DeliveryPolicy)
    -- ^ the JSON serialization of the subscription's delivery policy
    , subscriptionAttrEffectiveDeliveryPolicy :: !(Maybe DeliveryPolicy)
    -- ^ the JSON serialization of the effective delivery policy that takes into
    -- account the topic delivery policy and account system defaults
    }
    deriving (Show, Eq, Typeable)

