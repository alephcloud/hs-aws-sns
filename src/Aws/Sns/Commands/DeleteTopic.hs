{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.DeleteTopic
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Deletes a topic and all its subscriptions. Deleting a topic might prevent
-- some messages previously sent to the topic from being delivered to
-- subscribers. This action is idempotent, so deleting a topic that does not
-- exist does not result in an error.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_DeleteTopic.html>
--
module Aws.Sns.Commands.DeleteTopic
( DeleteTopic(..)
, DeleteTopicResponse(..)
, DeleteTopicErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Data.Typeable

deleteTopicAction :: SnsAction
deleteTopicAction = SnsActionDeleteTopic

data DeleteTopic = DeleteTopic
    { deleteTopicArn :: !Arn
    -- ^ The ARN of the topic you want to delete.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data DeleteTopicResponse = DeleteTopicResponse
    {}
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r DeleteTopicResponse where
    type ResponseMetadata DeleteTopicResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer $ \_ ->
        return DeleteTopicResponse

instance SignQuery DeleteTopic where
    type ServiceConfiguration DeleteTopic = SnsConfiguration
    signQuery DeleteTopic{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = deleteTopicAction
        , snsQueryParameters = [("TopicArn", Just $ toText deleteTopicArn)]
        , snsQueryBody = Nothing
        }

instance Transaction DeleteTopic DeleteTopicResponse

instance AsMemoryResponse DeleteTopicResponse where
    type MemoryResponse DeleteTopicResponse = DeleteTopicResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data DeleteTopicErrors
    = DeleteTopicAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | DeleteTopicInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | DeleteTopicInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | DeleteTopicTopicNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

