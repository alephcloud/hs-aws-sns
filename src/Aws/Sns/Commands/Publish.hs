{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.Publish
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Sends a message to all of a topic's subscribed endpoints. When a messageId
-- is returned, the message has been saved and Amazon SNS will attempt to
-- deliver it to the topic's subscribers shortly. The format of the outgoing
-- message to each subscribed endpoint depends on the notification protocol
-- selected.
--
-- To use the Publish action for sending a message to a mobile endpoint, such
-- as an app on a Kindle device or mobile phone, you must specify the
-- EndpointArn. The EndpointArn is returned when making a call with the
-- CreatePlatformEndpoint action. The second example below shows a request and
-- response for publishing to a mobile endpoint.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_Publish.html>
--
module Aws.Sns.Commands.Publish
( SnsMessage(..)
, MessageId(..)
, snsMessage
, SqsNotification(..)
, Publish(..)
, PublishResponse(..)
, PublishErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core
import Aws.Sns.Internal

import Control.Applicative

import Data.Aeson (ToJSON(..), FromJSON(..), (.:), withObject, encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Typeable

import qualified Network.HTTP.Types as HTTP

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

publishAction :: SnsAction
publishAction = SnsActionPublish

-- -------------------------------------------------------------------------- --
-- SNS Messages

data SnsMessage = SnsMessage
    { snsMessageDefault :: !T.Text
    , snsMessageMap :: !(M.Map SnsProtocol T.Text)
    }
    deriving (Show, Read, Eq, Ord, Typeable)

snsMessageParameters :: SnsMessage -> (Method, HTTP.QueryText)
snsMessageParameters SnsMessage{..} = if M.null snsMessageMap
    then (Get,)
        [ ("Message", Just snsMessageDefault)
        ]
    else (PostQuery,)
        [ ("MessageStructure", Just "json")
        , ("Message", (Just . T.decodeUtf8 . LB.toStrict) msg)
        ]
  where
    msg = encode
        . M.insert "default" snsMessageDefault
        . M.mapKeys (toText :: SnsProtocol -> String)
        $ snsMessageMap

snsMessage :: T.Text -> SnsMessage
snsMessage t = SnsMessage t M.empty

-- | Unique identifier assigned to a published message.
--
-- Length Constraint: Maximum 100 characters
--
newtype MessageId = MessageId { messageIdText :: T.Text }
    deriving (Show, Read, Eq, Ord, Monoid, IsString, Typeable, FromJSON, ToJSON)

-- -------------------------------------------------------------------------- --
-- SQS Notification Message

-- | The format of messages used with 'SnsProtocolSqs'
--
-- The format is described informally at
--
-- <http://docs.aws.amazon.com/sns/latest/dg/SendMessageToSQS.html>
--
data SqsNotification = SqsNotification
    { sqsNotificationMessageId :: !MessageId
    , sqsNotificationTopicArn :: !Arn
    , sqsNotificationSubject :: !(Maybe T.Text)
    , sqsNotificationMessage :: !T.Text
    , sqsNotificationTimestamp :: !UTCTime
    , sqsNotificationSignatureVersion :: !T.Text
    , sqsNotificationSignature :: !T.Text
    , sqsNotificationSigningCertURL :: !T.Text
    , sqsNotificationUnsubscribeURL :: !T.Text
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance FromJSON SqsNotification where
    parseJSON = withObject "SqsNotification" $ \o -> SqsNotification
        <$> o .: "MessageId"
        <*> o .: "TopicArn"
        <*> o .: "Subject"
        <*> o .: "Message"
        <*> o .: "Timestamp"
        <*> o .: "SignatureVersion"
        <*> o .: "Signature"
        <*> o .: "SigningCertURL"
        <*> o .: "UnsubscribeURL"
        <* (o .: "Type" >>= expectValue ("Notification" :: T.Text))

-- -------------------------------------------------------------------------- --
-- Publish

data Publish = Publish
    { publishMessage :: !SnsMessage
    -- ^ The message you want to send to the topic.
    --
    -- If you want to send the same message to all transport protocols, include
    -- the text of the message as a String value.
    --
    -- If you want to send different messages for each transport protocol add
    -- these to the 'snsMessageMap' map.
    --
    -- Constraints: Messages must be UTF-8 encoded strings at most 256 KB in
    -- size (262144 bytes, not 262144 characters).
    --

    , publishMessageAttributes_entry_N :: Maybe () -- TODO what's that?
    -- ^ Message attributes for Publish action.

    , publishSubject :: !(Maybe T.Text)
    -- ^ Optional parameter to be used as the "Subject" line when the message
    -- is delivered to email endpoints. This field will also be included, if
    -- present, in the standard JSON messages delivered to other endpoints.
    --
    -- Constraints: Subjects must be ASCII text that begins with a letter,
    -- number, or punctuation mark; must not include line breaks or control
    -- characters; and must be less than 100 characters long.
    --

    , publishArn :: !(Either Arn Arn)
    -- ^ Either TopicArn (left) or EndpointArn (right).

    }
    deriving (Show, Read, Eq, Ord, Typeable)

data PublishResponse = PublishResponse
    { publishMessageId :: !MessageId
    -- ^ Unique identifier assigned to the published message.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r PublishResponse where
    type ResponseMetadata PublishResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = PublishResponse . MessageId <$> arn el
        arn el = force "Missing Message Id" $ el
            $// CU.laxElement "PublishResult"
            &/ CU.laxElement "MessageId"
            &/ CU.content

instance SignQuery Publish where
    type ServiceConfiguration Publish = SnsConfiguration
    signQuery Publish{..} = snsSignQuery SnsQuery
        { snsQueryMethod = method
        , snsQueryAction = publishAction
        , snsQueryParameters = msgQuery <> subject <> arn <> entry
        , snsQueryBody = Nothing
        }
      where
        (method, msgQuery) = snsMessageParameters publishMessage
        subject = maybe [] (\x -> [("Subject", Just x)]) publishSubject
        arn = case publishArn of
            Left a -> [("TopicArn", Just $ toText a)]
            Right a -> [("TargetArn", Just $ toText a)]
        entry = [] -- TODO

instance Transaction Publish PublishResponse

instance AsMemoryResponse PublishResponse where
    type MemoryResponse PublishResponse = PublishResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data PublishErrors
    = PublishAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | PublishInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | PublishInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | PublishEndpointDisabled
    -- ^ Exception error indicating endpoint disabled.
    --
    -- /Code 400/

    | PublishInvalidParameterValue
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    | PublishNotFound
    -- ^ Indicates that the requested resource does not exist.
    --
    -- /Code 404/

    | PublishApplicationDisabled
    -- ^ Exception error indicating platform application disabled.
    --
    -- /Code 400/
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)


