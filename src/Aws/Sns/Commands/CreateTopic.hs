{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.CreateTopic
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Creates a topic to which notifications can be published. Users can create at
-- most 3000 topics. For more information, see <http://aws.amazon.com/sns>. This
-- action is idempotent, so if the requester already owns a topic with the
-- specified name, that topic's ARN is returned without creating a new topic.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_CreateTopic.html>
--
module Aws.Sns.Commands.CreateTopic
( CreateTopic(..)
, CreateTopicResponse(..)
, CreateTopicErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Control.Applicative
import Control.Monad.Trans.Resource (throwM)

import Data.Monoid
import qualified Data.Text as T
import Data.Typeable

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

createTopicAction :: SnsAction
createTopicAction = SnsActionCreateTopic

data CreateTopic = CreateTopic
    { createTopicName :: !T.Text
    -- ^ The name of the topic you want to create. Topic names must be made up
    -- of only uppercase and lowercase ASCII letters, numbers, underscores, and
    -- hyphens, and must be between 1 and 256 characters long
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data CreateTopicResponse = CreateTopicResponse
    { createTopicResTopicArn :: !Arn
    -- ^ The Amazon Resource Name (ARN) assigned to the created topic.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r CreateTopicResponse where
    type ResponseMetadata CreateTopicResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = CreateTopicResponse <$> arn el
        arn el = do
            t <- force "Missing topic ARN" $ el
                $// CU.laxElement "CreateTopicResult"
                &/ CU.laxElement "TopicArn"
                &/ CU.content
            case fromText t of
                Right a -> return a
                Left e -> throwM $ SnsResponseDecodeError $
                    "failed to parse topic ARN (" <> t <> "): " <> (T.pack . show) e

instance SignQuery CreateTopic where
    type ServiceConfiguration CreateTopic = SnsConfiguration
    signQuery CreateTopic{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = createTopicAction
        , snsQueryParameters = [("Name", Just createTopicName)]
        , snsQueryBody = Nothing
        }

instance Transaction CreateTopic CreateTopicResponse

instance AsMemoryResponse CreateTopicResponse where
    type MemoryResponse CreateTopicResponse = CreateTopicResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data CreateTopicErrors
    = CreateTopicAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | CreateTopicInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | CreateTopicInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | CreateTopicTopicLimitExceeded
    -- ^ Indicates that the customer already owns the maximum allowed number of topics.
    --
    -- /Code 403/
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

