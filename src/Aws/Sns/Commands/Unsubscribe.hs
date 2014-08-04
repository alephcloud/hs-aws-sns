{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.Unsubscribe
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Deletes a subscription. If the subscription requires authentication for
-- deletion, only the owner of the subscription or the topic's owner can
-- unsubscribe, and an AWS signature is required. If the Unsubscribe call does
-- not require authentication and the requester is not the subscription owner,
-- a final cancellation message is delivered to the endpoint, so that the
-- endpoint owner can easily resubscribe to the topic if the Unsubscribe
-- request was unintended.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_Unsubscribe.html>
--
module Aws.Sns.Commands.Unsubscribe
( Unsubscribe(..)
, UnsubscribeResponse(..)
, UnsubscribeErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Data.Typeable

unsubscribeAction :: SnsAction
unsubscribeAction = SnsActionUnsubscribe

data Unsubscribe = Unsubscribe
    { unsubscribeSubscriptionArn :: !Arn
    -- ^ The ARN of the subscription to be deleted.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data UnsubscribeResponse = UnsubscribeResponse
    {}
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r UnsubscribeResponse where
    type ResponseMetadata UnsubscribeResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer $ \_ ->
        return UnsubscribeResponse

instance SignQuery Unsubscribe where
    type ServiceConfiguration Unsubscribe = SnsConfiguration
    signQuery Unsubscribe{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = unsubscribeAction
        , snsQueryParameters = [("SubscriptionArn", Just $ toText unsubscribeSubscriptionArn)]
        , snsQueryBody = Nothing
        }

instance Transaction Unsubscribe UnsubscribeResponse

instance AsMemoryResponse UnsubscribeResponse where
    type MemoryResponse UnsubscribeResponse = UnsubscribeResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data UnsubscribeErrors
    = UnsubscribeAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | UnsubscribeInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | UnsubscribeInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | UnsubscribeNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 403/
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

