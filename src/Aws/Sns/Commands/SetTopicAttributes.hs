{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Aws.Sns.Commands.SetTopicAttributes
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Allows a topic owner to set an attribute of the topic to a new value.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/api/API_SetTopicAttributes.html>
--
module Aws.Sns.Commands.SetTopicAttributes
( MutableTopicAttribute
, SetTopicAttributes(..)
, SetTopicAttributesResponse(..)
, SetTopicAttributesErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core
import Aws.Sns.Types
import Aws.Sns.Policy

import Control.Applicative

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable

import qualified Network.HTTP.Types as HTTP

-- -------------------------------------------------------------------------- --
-- Mutable Topic Attribute

data MutableTopicAttribute
    = MutableTopicAttributePolicy (Maybe SnsPolicy)
    | MutableTopicAttributeDisplayName (Maybe T.Text)
    | MutableTopicAttributeDeliveryPolicy (Maybe DeliveryPolicy)
    deriving (Show, Eq, Typeable)

mutableTopicAttributeQuery :: MutableTopicAttribute -> HTTP.QueryText
mutableTopicAttributeQuery (MutableTopicAttributePolicy v) =
    [ ("AttributeName", Just "Policy")
    , ("AttributeValue", fmap encodeTextStrict v)
    ]
mutableTopicAttributeQuery (MutableTopicAttributeDisplayName v) =
    [ ("AttributeName", Just "DisplayName")
    , ("AttributeValue", v)
    ]
mutableTopicAttributeQuery (MutableTopicAttributeDeliveryPolicy v) =
    [ ("AttributeName", Just "DeliveryPolicy")
    , ("AttributeValue", fmap encodeTextStrict v)
    ]

encodeTextStrict :: ToJSON a => a -> T.Text
encodeTextStrict = T.decodeUtf8 . LB.toStrict . encode

-- -------------------------------------------------------------------------- --
-- SetTopicAttributes

setTopicAttributesAction :: SnsAction
setTopicAttributesAction = SnsActionSetTopicAttributes

data SetTopicAttributes = SetTopicAttributes
    { setTopicAttributesAttribute :: !MutableTopicAttribute
    , setTopicAttributesTopicArn :: !Arn
    }
    deriving (Show, Eq, Typeable)

data SetTopicAttributesResponse = SetTopicAttributesResponse
    {}
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r SetTopicAttributesResponse where
    type ResponseMetadata SetTopicAttributesResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer (const $ pure SetTopicAttributesResponse)

instance SignQuery SetTopicAttributes where
    type ServiceConfiguration SetTopicAttributes = SnsConfiguration
    signQuery SetTopicAttributes{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = setTopicAttributesAction
        , snsQueryParameters
            = ("TopicArn", Just $ toText setTopicAttributesTopicArn)
            : mutableTopicAttributeQuery setTopicAttributesAttribute
        , snsQueryBody = Nothing
        }

instance Transaction SetTopicAttributes SetTopicAttributesResponse

instance AsMemoryResponse SetTopicAttributesResponse where
    type MemoryResponse SetTopicAttributesResponse = SetTopicAttributesResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data SetTopicAttributesErrors
    = SetTopicAttributesAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | SetTopicAttributesInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | SetTopicAttributesInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | SetTopicAttributesNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)


