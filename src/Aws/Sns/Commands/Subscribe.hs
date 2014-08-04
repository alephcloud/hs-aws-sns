{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.Subscribe
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Prepares to subscribe an endpoint by sending the endpoint a confirmation
-- message. To actually create a subscription, the endpoint owner must call the
-- ConfirmSubscription action with the token from the confirmation message.
-- Confirmation tokens are valid for three days.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_Subscribe.html>
--
module Aws.Sns.Commands.Subscribe
( Subscribe(..)
, SubscribeResponse(..)
, SubscribeErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Data.Traversable as TR
import Control.Applicative
import Control.Monad.Trans.Resource (throwM)

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Typeable

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

subscribeAction :: SnsAction
subscribeAction = SnsActionSubscribe

data Subscribe = Subscribe
    { subscribeEndpoint :: !(Maybe SnsEndpoint)
    -- ^ The endpoint that you want to receive notifications.
    -- This must match with the corresponding protocol
    -- (see 'SnsEndPoint' for details).

    , subscribeProtocol :: !SnsProtocol
    -- ^ The protocol you want to use.

    , subscribeTopicArn :: !Arn
    -- ^ The ARN of the topic you want to subscribe to.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data SubscribeResponse = SubscribeResponse
    { subscribeResSubscriptionArn :: !(Maybe Arn)
    -- ^ The ARN of the subscription, if the service was able to create a
    -- subscription immediately (without requiring endpoint owner confirmation).
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r SubscribeResponse where
    type ResponseMetadata SubscribeResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = SubscribeResponse <$> arn el
        arn el = do
            let t = listToMaybe . filter (/= "pending confirmation") $ el
                    $// CU.laxElement "SubscribeResult"
                    &/ CU.laxElement "SubscriptionArn"
                    &/ CU.content
            TR.forM t $ \t_ -> case fromText t_ of
                Right a -> return a
                Left e -> throwM $ SnsResponseDecodeError $
                    "failed to parse subscription ARN (" <> t_ <> "): " <> (T.pack . show) e

instance SignQuery Subscribe where
    type ServiceConfiguration Subscribe = SnsConfiguration
    signQuery Subscribe{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = subscribeAction
        , snsQueryParameters = catMaybes
            [ ("Endpoint",) . Just <$> subscribeEndpoint
            , Just ("Protocol", Just . toText $ subscribeProtocol)
            , Just ("TopicArn", Just . toText $ subscribeTopicArn)
            ]
        , snsQueryBody = Nothing
        }

instance Transaction Subscribe SubscribeResponse

instance AsMemoryResponse SubscribeResponse where
    type MemoryResponse SubscribeResponse = SubscribeResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data SubscribeErrors
    = SubscribeAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | SubscribeInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | SubscribeInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | SubscribeNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/

    | SubscribeTopicLimitExceeded
    -- ^ Indicates that the customer already owns the maximum allowed number of subscriptions.
    --
    -- /Code 403/
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)


