{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.ConfirmSubscription
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Verifies an endpoint owner's intent to receive messages by validating the
-- token sent to the endpoint by an earlier Subscribe action. If the token is
-- valid, the action creates a new subscription and returns its Amazon Resource
-- Name (ARN). This call requires an AWS signature only when the
-- AuthenticateOnUnsubscribe flag is set to "true".
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_ConfirmSubscription.html>
--
module Aws.Sns.Commands.ConfirmSubscription
( ConfirmSubscription(..)
, ConfirmSubscriptionResponse(..)
, ConfirmSubscriptionErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Control.Applicative
import Control.Monad.Trans.Resource (throwM)

import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Typeable

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

confirmSubscriptionAction :: SnsAction
confirmSubscriptionAction = SnsActionConfirmSubscription

-- -------------------------------------------------------------------------- --
-- Subscription Confirmation Token

newtype SubscriptionConfirmationToken = SubscriptionConfirmationToken
    { subscriptionConfirmationTokenText :: T.Text
    }
    deriving (Show, Read, Eq, Ord, Monoid, IsString, Typeable)

-- -------------------------------------------------------------------------- --
-- Confirm Subscription

data ConfirmSubscription = ConfirmSubscription
    { confirmSubscriptionAuthenticateOnUnsubscribe:: !Bool
    -- ^ Disallows unauthenticated unsubscribes of the subscription. If the
    -- value of this parameter is true and the request has an AWS signature,
    -- then only the topic owner and the subscription owner can unsubscribe the
    -- endpoint. The unsubscribe action requires AWS authentication.

    , confirmSubscriptionToken :: !SubscriptionConfirmationToken
    -- ^ Short-lived token sent to an endpoint during the Subscribe action.

    , confirmSubscriptionTopicArn :: Arn
    -- ^ The ARN of the topic for which you wish to confirm a subscription.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    { confirmSubscriptionResSubscriptionArn :: !Arn
    -- ^ The ARN of the created subscription.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r ConfirmSubscriptionResponse where
    type ResponseMetadata ConfirmSubscriptionResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = ConfirmSubscriptionResponse <$> arn el
        arn el = do
            t <- force "Missing topic ARN" $ el
                $// CU.laxElement "ConfirmSubscriptionResult"
                &/ CU.laxElement "SubscriptionArn"
                &/ CU.content
            case fromText t of
                Right a -> return a
                Left e -> throwM $ SnsResponseDecodeError $
                    "failed to parse subscription ARN (" <> t <> "): " <> (T.pack . show) e

instance SignQuery ConfirmSubscription where
    type ServiceConfiguration ConfirmSubscription = SnsConfiguration
    signQuery ConfirmSubscription{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = confirmSubscriptionAction
        , snsQueryParameters = authOnUnsubscribe <>
            [ ("Token", Just $ subscriptionConfirmationTokenText confirmSubscriptionToken)
            , ("TopicArn", Just $ toText confirmSubscriptionTopicArn)
            ]
        , snsQueryBody = Nothing
        }
      where
        authOnUnsubscribe = if confirmSubscriptionAuthenticateOnUnsubscribe
            then [ ("AuthenticateOnUnsubscribe", Just "true") ]
            else []

instance Transaction ConfirmSubscription ConfirmSubscriptionResponse

instance AsMemoryResponse ConfirmSubscriptionResponse where
    type MemoryResponse ConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data ConfirmSubscriptionErrors
    = ConfirmSubscriptionAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | ConfirmSubscriptionInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | ConfirmSubscriptionInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | ConfirmSubscriptionNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/

    | ConfirmSubscriptionSubscriptionLimitExceeded
    -- ^ Indicates that the customer already owns the maximum allowed number of subscriptions.
    --
    -- /Code 403/

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)


