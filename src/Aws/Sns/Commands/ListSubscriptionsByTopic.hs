{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.ListSubscriptionsByTopic
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Returns a list of the subscriptions to a specific topic. Each call returns a
-- limited list of subscriptions, up to 100. If there are more subscriptions, a
-- NextToken is also returned. Use the NextToken parameter in a new
-- ListSubscriptionsByTopic call to get further results.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_ListSubscriptionsByTopic.html>
--
module Aws.Sns.Commands.ListSubscriptionsByTopic
( Subscription(..)
, ListSubscriptionsByTopicNextToken
, ListSubscriptionsByTopic(..)
, ListSubscriptionsByTopicResponse(..)
, ListSubscriptionsByTopicErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Resource (throwM)

import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Traversable as TR
import Data.Typeable

import Text.XML.Cursor (($//), ($/), (&/), (&|))
import qualified Text.XML.Cursor as CU

-- -------------------------------------------------------------------------- --
-- Subscription

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_Subscription.html>
--
data Subscription = Subscription
    { subscriptionEndpoint :: !(Maybe SnsEndpoint)
    -- ^ The subscription's endpoint (format depends on the protocol).
    , subscriptionOwner :: !(Maybe T.Text) -- FIXME I think this is actually an account ID
    -- ^ The subscription's owner.
    , subscriptionProtocol :: !(Maybe SnsProtocol)
    -- ^ The subscription's protocol.
    , subscriptionSubscriptionArn :: !(Maybe Arn)
    -- ^ The subscription's ARN.
    , subscriptionTopicArn :: !(Maybe Arn)
    -- ^ The ARN of the subscription's topic.
    }
    deriving (Show, Read, Eq, Ord)

subscriptionParseXml :: CU.Cursor -> Either String Subscription
subscriptionParseXml el = Subscription
    <$> (pure . listToMaybe $ el $/ CU.laxElement "Endpoint" &/ CU.content)
    <*> (pure . listToMaybe $ el $/ CU.laxElement "Owner" &/ CU.content)
    <*> (TR.mapM fromText . listToMaybe $ el $/ CU.laxElement "Protocol" &/ CU.content)
    <*> (subArnFromText . listToMaybe $ el $/ CU.laxElement "SubscriptionArn" &/ CU.content)
    <*> (TR.mapM fromText . listToMaybe $ el $/ CU.laxElement "TopicArn" &/ CU.content)
  where
    subArnFromText :: Maybe T.Text -> Either String (Maybe Arn)
    subArnFromText (Just "PendingConfirmation") = Right Nothing
    subArnFromText t = TR.mapM fromText t

-- -------------------------------------------------------------------------- --
-- List Subscriptions By Topic

listSubscriptionsByTopicAction :: SnsAction
listSubscriptionsByTopicAction = SnsActionListSubscriptionsByTopic

newtype ListSubscriptionsByTopicNextToken = ListSubscriptionsByTopicNextToken
    { listSubscriptionsByTopicNextTokenText :: T.Text
    }
    deriving (Show, Read, Eq, Ord, Monoid, IsString)

data ListSubscriptionsByTopic = ListSubscriptionsByTopic
    { listSubscriptionsByTopicNextToken :: !(Maybe ListSubscriptionsByTopicNextToken)
    -- ^ Token returned by the previous 'ListSubscriptionsByTopic' request.
    , listSubscriptionsByTopicArn :: !Arn
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse
    { listSubscriptionsByTopicResponseNextToken :: !(Maybe ListSubscriptionsByTopicNextToken)
    -- ^ Token to pass along to the next ListSubscriptionsByTopic request. This element is
    -- returned if there are additional topics to retrieve.

    , listSubscriptionsByTopicResponseSubscriptions :: ![Subscription]
    -- ^ A list of topic ARNs.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r ListSubscriptionsByTopicResponse where
    type ResponseMetadata ListSubscriptionsByTopicResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = ListSubscriptionsByTopicResponse (nextToken el) <$> subs el
        subs el = do
            let t = el
                    $// CU.laxElement "ListSubscriptionsByTopicResult"
                    &/ CU.laxElement "Subscriptions"
                    &/ CU.laxElement "member"
                    &| subscriptionParseXml
            forM t $ \i -> case i of
                Right a -> return a
                Left e -> throwM $ SnsResponseDecodeError
                    $ "failed to parse subscriptions: " <> T.pack e
        nextToken el = fmap ListSubscriptionsByTopicNextToken . listToMaybe $ el
            $// CU.laxElement "ListSubscriptionsByTopicResult"
            &/ CU.laxElement "NextToken"
            &/ CU.content

instance SignQuery ListSubscriptionsByTopic where
    type ServiceConfiguration ListSubscriptionsByTopic = SnsConfiguration
    signQuery ListSubscriptionsByTopic{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = listSubscriptionsByTopicAction
        , snsQueryParameters = nextToken <>
            [ ("TopicArn", Just $ toText listSubscriptionsByTopicArn)
            ]
        , snsQueryBody = Nothing
        }
      where
        nextToken = case listSubscriptionsByTopicNextToken of
            Nothing -> []
            Just _ -> [("NextToken", tokParam)]
        tokParam = listSubscriptionsByTopicNextTokenText <$> listSubscriptionsByTopicNextToken

instance Transaction ListSubscriptionsByTopic ListSubscriptionsByTopicResponse

instance AsMemoryResponse ListSubscriptionsByTopicResponse where
    type MemoryResponse ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse
    loadToMemory = return

instance ListResponse ListSubscriptionsByTopicResponse Subscription where
    listResponse (ListSubscriptionsByTopicResponse _ subs) = subs

instance IteratedTransaction ListSubscriptionsByTopic ListSubscriptionsByTopicResponse where
    nextIteratedRequest query ListSubscriptionsByTopicResponse{..} =
        query
            { listSubscriptionsByTopicNextToken = listSubscriptionsByTopicResponseNextToken
            }
        <$ listSubscriptionsByTopicResponseNextToken

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data ListSubscriptionsByTopicErrors
    = ListSubscriptionsByTopicAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | ListSubscriptionsByTopicInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | ListSubscriptionsByTopicInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | ListSubscriptionsByTopicNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)



