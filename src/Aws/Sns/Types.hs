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

-- * Topic Attributes
, TopicAttributes(..)
) where

import Aws.General
import Aws.Sns.Policy

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

-- -------------------------------------------------------------------------- --
-- Topic Attributes

-- | A map of topic's attributes.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/api/API_GetTopicAttributesResult.html>
--
data TopicAttributes = TopicAttributes
    { topicAttributeTopicArn :: !(Maybe Arn)
      -- ^ the topic's ARN
    , topicAttributeOwner :: !(Maybe AccountId)
      -- ^ the AWS account ID of the topic's owner
    , topicAttributePolicy :: !(Maybe SnsPolicy)
      -- ^ the JSON serialization of the topic's access control policy
    , topicAttributeDisplayName :: !(Maybe T.Text)
      -- ^ the human-readable name used in the "From" field for notifications to email and email-json endpoints
    , topicAttributeSubscriptionsPending :: !(Maybe Int)
      -- ^ the number of subscriptions pending confirmation on this topic
    , topicAttributeSubscriptionsConfirmed :: !(Maybe Int)
      -- ^ the number of confirmed subscriptions on this topic
    , topicAttributeSubscriptionsDeleted :: !(Maybe Int)
      -- ^ the number of deleted subscriptions on this topic
    , topicAttributeDeliveryPolicy :: !(Maybe DeliveryPolicy)
      -- ^ the JSON serialization of the topic's delivery policy
    , topicAttributeEffectiveDeliveryPolicy :: !(Maybe DeliveryPolicy)
      -- ^ the JSON serialization of the effective delivery policy that takes into account system defaults
    }
    deriving (Show, Eq, Typeable)

