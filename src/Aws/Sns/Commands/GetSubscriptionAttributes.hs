{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Aws.Sns.Commands.GetSubscriptionAttributes
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Returns all of the properties of a subscription.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/api/API_GetSubscriptionAttributes.html>
--
module Aws.Sns.Commands.GetSubscriptionAttributes
( SubscriptionAttributes(..)
, GetSubscriptionAttributes(..)
, GetSubscriptionAttributesResponse(..)
, GetSubscriptionAttributesErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Internal
import Aws.Sns.Core

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Resource (throwM)

import qualified Data.Text as T
import qualified Data.Traversable as TR
import Data.Typeable

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

type DeliveryPolicy = T.Text

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
    deriving (Show, Read, Eq, Ord, Typeable)

-- -------------------------------------------------------------------------- --
-- GetSubscriptionAttributes

getSubscriptionAttributesAction :: SnsAction
getSubscriptionAttributesAction = SnsActionGetSubscriptionAttributes

data GetSubscriptionAttributes = GetSubscriptionAttributes
    { getSubscriptionAttributesSubscriptionArn :: !Arn
    -- ^ The ARN of the subscription whose properties you want to get.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    { getSubscriptionAttributesResAttributes :: !SubscriptionAttributes
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r GetSubscriptionAttributesResponse where
    type ResponseMetadata GetSubscriptionAttributesResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p :: CU.Cursor -> Response (ResponseMetadata GetSubscriptionAttributesResponse) GetSubscriptionAttributesResponse
        p el = either throwM return $ do
            entries <- fmap parseXmlEntryMap $ force "Missing Attributes" $ el
                $// CU.laxElement "GetSubscriptionAttributesResult"
                &/ CU.laxElement "Attributes"
            fmapL (toException . XmlException) . fmap GetSubscriptionAttributesResponse $ SubscriptionAttributes
                <$> TR.mapM fromText (lookup "SubscriptionArn" entries)
                <*> TR.mapM fromText (lookup "TopicArn" entries)
                <*> (pure $ fmap AccountId (lookup "Owner" entries))
                <*> pure (maybe False (== "true") (lookup "ConfirmationWasAuthenticated" entries))
                <*> pure (lookup "DeliveryPolicy" entries)
                <*> pure (lookup "EffectiveDeliveryPolicy" entries)

instance SignQuery GetSubscriptionAttributes where
    type ServiceConfiguration GetSubscriptionAttributes = SnsConfiguration
    signQuery GetSubscriptionAttributes{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = getSubscriptionAttributesAction
        , snsQueryParameters = [("SubscriptionArn", Just $ toText getSubscriptionAttributesSubscriptionArn)]
        , snsQueryBody = Nothing
        }

instance Transaction GetSubscriptionAttributes GetSubscriptionAttributesResponse

instance AsMemoryResponse GetSubscriptionAttributesResponse where
    type MemoryResponse GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data GetSubscriptionAttributesErrors
    = GetSubscriptionAttributesAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | GetSubscriptionAttributesInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | GetSubscriptionAttributesInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | GetSubscriptionAttributesNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

