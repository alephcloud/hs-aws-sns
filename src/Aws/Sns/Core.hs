{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Aws.Sns.Core
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/Welcome.html>
--
module Aws.Sns.Core
(
  SnsVersion(..)

-- * SNS Client Configuration
, SnsConfiguration(..)

-- * SNS Client Metadata
, SnsMetadata(..)

-- * SNS Exceptions
, SnsErrorResponse(..)

-- * SNS Subscription Protocols
, SnsProtocol(..)
, snsProtocolToText
, parseSnsProtocol

-- * SNS Subscription Endpoints
, SnsEndpoint

-- * Internal

-- ** SNS Actions
, SnsAction(..)
, snsActionToText
, parseSnsAction

-- ** SNS AWS Service Endpoints
, snsServiceEndpoint

-- ** SNS Queries
, SnsQuery(..)
, snsSignQuery

-- ** SNS Response Consumers
, snsResponseConsumer
, snsXmlResponseConsumer
, snsErrorResponseConsumer

-- ** SNS Errors and Common Parameters
, SnsError(..)
, SnsCommonParameters(..)
, SnsCommonError(..)
) where

import Aws.Core
import Aws.General
import Aws.SignatureV4

import qualified Blaze.ByteString.Builder as BB

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (throwM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Conduit (($$+-))
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Time.Clock
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import qualified Test.QuickCheck as Q

import qualified Text.Parser.Char as P
import Text.Parser.Combinators ((<?>))
import qualified Text.XML as XML
import qualified Text.XML.Cursor as CU
import Text.XML.Cursor (($//))

data SnsVersion
    = SnsVersion_2013_03_31

-- -------------------------------------------------------------------------- --
-- SNS Actions

data SnsAction
    = SnsActionAddPermission
    | SnsActionConfirmSubscription
    | SnsActionCreatePlatformApplication
    | SnsActionCreatePlatformEndpoint
    | SnsActionCreateTopic
    | SnsActionDeleteEndPoint
    | SnsActionDeletePlatformApplication
    | SnsActionDeleteTopic
    | SnsActionGetEndpointAttributes
    | SnsActionGetPlatformApplicationAttribute
    | SnsActionGetSubscriptionAttributes
    | SnsActionGetTopicAttributes
    | SnsActionListEndpointsByPlatformApplication
    | SnsActionListPlatformApplications
    | SnsActionListSubscriptions
    | SnsActionListSubscriptionsByTopic
    | SnsActionListTopics
    | SnsActionPublish
    | SnsActionRemovePermission
    | SnsActionSetEndpointAttributes
    | SnsActionSetPlatformApplicationAttributes
    | SnsActionSetSubscriptionAttributes
    | SnsActionSetTopicAttributes
    | SnsActionSubscribe
    | SnsActionUnsubscribe
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

snsActionToText :: IsString a => SnsAction -> a
snsActionToText SnsActionAddPermission = "AddPermission"
snsActionToText SnsActionConfirmSubscription = "ConfirmSubscription"
snsActionToText SnsActionCreatePlatformApplication = "CreatePlatformApplication"
snsActionToText SnsActionCreatePlatformEndpoint = "CreatePlatformEndpoint"
snsActionToText SnsActionCreateTopic = "CreateTopic"
snsActionToText SnsActionDeleteEndPoint = "DeleteEndPoint"
snsActionToText SnsActionDeletePlatformApplication = "DeletePlatformApplication"
snsActionToText SnsActionDeleteTopic = "DeleteTopic"
snsActionToText SnsActionGetEndpointAttributes = "GetEndpointAttributes"
snsActionToText SnsActionGetPlatformApplicationAttribute = "GetPlatformApplicationAttribute"
snsActionToText SnsActionGetSubscriptionAttributes = "GetSubscriptionAttributes"
snsActionToText SnsActionGetTopicAttributes = "GetTopicAttributes"
snsActionToText SnsActionListEndpointsByPlatformApplication = "ListEndpointsByPlatformApplication"
snsActionToText SnsActionListPlatformApplications = "ListPlatformApplications"
snsActionToText SnsActionListSubscriptions = "ListSubscriptions"
snsActionToText SnsActionListSubscriptionsByTopic = "ListSubscriptionsByTopic"
snsActionToText SnsActionListTopics = "ListTopics"
snsActionToText SnsActionPublish = "Publish"
snsActionToText SnsActionRemovePermission = "RemovePermission"
snsActionToText SnsActionSetEndpointAttributes = "SetEndpointAttributes"
snsActionToText SnsActionSetPlatformApplicationAttributes = "SetPlatformApplicationAttributes"
snsActionToText SnsActionSetSubscriptionAttributes = "SetSubscriptionAttributes"
snsActionToText SnsActionSetTopicAttributes = "SetTopicAttributes"
snsActionToText SnsActionSubscribe = "Subscribe"
snsActionToText SnsActionUnsubscribe = "Unsubscribe"

parseSnsAction :: P.CharParsing m => m SnsAction
parseSnsAction =
        SnsActionAddPermission <$ P.text "AddPermission"
    <|> SnsActionConfirmSubscription <$ P.text "ConfirmSubscription"
    <|> SnsActionCreatePlatformApplication <$ P.text "CreatePlatformApplication"
    <|> SnsActionCreatePlatformEndpoint <$ P.text "CreatePlatformEndpoint"
    <|> SnsActionCreateTopic <$ P.text "CreateTopic"
    <|> SnsActionDeleteEndPoint <$ P.text "DeleteEndPoint"
    <|> SnsActionDeletePlatformApplication <$ P.text "DeletePlatformApplication"
    <|> SnsActionDeleteTopic <$ P.text "DeleteTopic"
    <|> SnsActionGetEndpointAttributes <$ P.text "GetEndpointAttributes"
    <|> SnsActionGetPlatformApplicationAttribute <$ P.text "GetPlatformApplicationAttribute"
    <|> SnsActionGetSubscriptionAttributes <$ P.text "GetSubscriptionAttributes"
    <|> SnsActionGetTopicAttributes <$ P.text "GetTopicAttributes"
    <|> SnsActionListEndpointsByPlatformApplication <$ P.text "ListEndpointsByPlatformApplication"
    <|> SnsActionListPlatformApplications <$ P.text "ListPlatformApplications"
    <|> SnsActionListSubscriptions <$ P.text "ListSubscriptions"
    <|> SnsActionListSubscriptionsByTopic <$ P.text "ListSubscriptionsByTopic"
    <|> SnsActionListTopics <$ P.text "ListTopics"
    <|> SnsActionPublish <$ P.text "Publish"
    <|> SnsActionRemovePermission <$ P.text "RemovePermission"
    <|> SnsActionSetEndpointAttributes <$ P.text "SetEndpointAttributes"
    <|> SnsActionSetPlatformApplicationAttributes <$ P.text "SetPlatformApplicationAttributes"
    <|> SnsActionSetSubscriptionAttributes <$ P.text "SetSubscriptionAttributes"
    <|> SnsActionSetTopicAttributes <$ P.text "SetTopicAttributes"
    <|> SnsActionSubscribe <$ P.text "Subscribe"
    <|> SnsActionUnsubscribe <$ P.text "Unsubscribe"
    <?> "SnsAction"

instance AwsType SnsAction where
    toText = snsActionToText
    parse = parseSnsAction

instance Q.Arbitrary SnsAction where
    arbitrary = Q.elements [minBound..maxBound]

-- -------------------------------------------------------------------------- --
-- SNS AWS Service Endpoints

-- | SNS Endpoints as specified in AWS General API version 0.1
--
-- <http://docs.aws.amazon.com/general/1.0/gr/rande.html#sns_region>
--
-- This are the endpoints of the AWS SNS Service. This must not be
-- confused with an SNS endpoint that is a client that subscribes
-- to receive SNS notifications (see 'SnsEndpoint').
--
snsServiceEndpoint :: Region -> B8.ByteString
snsServiceEndpoint ApNortheast1 = "sns.ap-northeast-1.amazonaws.com"
snsServiceEndpoint ApSoutheast1 = "sns.ap-southeast-1.amazonaws.com"
snsServiceEndpoint ApSoutheast2 = "sns.ap-southeast-2.amazonaws.com"
snsServiceEndpoint EuWest1 = "sns.eu-west-1.amazonaws.com"
snsServiceEndpoint SaEast1 = "sns.sa-east-1.amazonaws.com"
snsServiceEndpoint UsEast1 = "sns.us-east-1.amazonaws.com"
snsServiceEndpoint UsWest1 = "sns.us-west-1.amazonaws.com"
snsServiceEndpoint UsWest2 = "sns.us-west-2.amazonaws.com"

-- -------------------------------------------------------------------------- --
-- SNS Metadata

data SnsMetadata = SnsMetadata
    { snsMAmzId2 :: Maybe T.Text
    , snsMRequestId :: Maybe T.Text
    }
    deriving (Show)

instance Loggable SnsMetadata where
    toLogText (SnsMetadata rid id2) =
        "SNS: request ID=" <> fromMaybe "<none>" rid
        <> ", x-amz-id-2=" <> fromMaybe "<none>" id2

instance Monoid SnsMetadata where
    mempty = SnsMetadata Nothing Nothing
    SnsMetadata id1 r1 `mappend` SnsMetadata id2 r2 = SnsMetadata (id1 <|> id2) (r1 <|> r2)

-- -------------------------------------------------------------------------- --
-- SNS Configuration

data SnsConfiguration qt = SnsConfiguration
    { snsConfProtocol :: Protocol
    , snsConfRegion :: Region
    }
    deriving (Show)

-- -------------------------------------------------------------------------- --
-- SNS Query

data SnsQuery = SnsQuery
    { snsQueryMethod :: !Method
    , snsQueryAction :: !SnsAction
    , snsQueryParameters :: !HTTP.QueryText
    , snsQueryBody :: !(Maybe B.ByteString)
    }
    deriving (Show, Eq)

-- | Creates a signed query.
--
-- Uses AWS Signature V4. All requests expect for publish
-- are GET requests with the signature embedded into the URI.
--
-- FIXME eliminate usage of 'error'. Either statically elimintate
-- possibility of failures or change type to Either.
--
snsSignQuery :: SnsQuery -> SnsConfiguration qt -> SignatureData -> SignedQuery
snsSignQuery query conf sigData = SignedQuery
    { sqMethod = method
    , sqProtocol = snsConfProtocol conf
    , sqHost = host
    , sqPort = port
    , sqPath = BB.toByteString $ HTTP.encodePathSegments path
    , sqQuery = HTTP.queryTextToQuery signedQuery
    , sqDate = Nothing
    , sqAuthorization = authorization
    , sqContentType = contentType
    , sqContentMd5 = Nothing
    , sqAmzHeaders = amzHeaders
    , sqOtherHeaders = [] -- headers -- we put everything into amzHeaders
    , sqBody = HTTP.RequestBodyBS <$> body
    , sqStringToSign = mempty -- Let me know if you really need this...
    }
  where
    -- values that don't depend on the signature
    action = snsQueryAction query
    path = []
    host = snsServiceEndpoint $ snsConfRegion conf
    headers = [("host", host)]
    port = case snsConfProtocol conf of
        HTTP -> 80
        HTTPS -> 443
    contentType = case snsQueryMethod query of
        Post -> Just "application/json"
        Get -> Nothing
        PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"
        -- The following cases are currently not supported
        Put -> Just "application/json"
        Delete -> Nothing
        Head -> Nothing

    -- The following is somewhat hacky for dealing with method PostQuery
    -- TODO it may be better to have the commands take care of this...
    --
    -- Alternatively we may have the signing function decide what to do
    -- based on the method and always return the updated query and headers.
    --
    method = case snsQueryMethod query of
        PostQuery -> Post
        x -> x

    body = case snsQueryMethod query of
        PostQuery -> Just $ BB.toByteString $ HTTP.renderQueryText False
            $ ("Action", Just . toText $ action) : snsQueryParameters query
        _ -> snsQueryBody query

    unsignedQuery = case snsQueryMethod query of
        PostQuery -> []
        _ -> ("Action", Just . toText $ action) : snsQueryParameters query

    -- Values that depend on the signature
    (signedQuery, amzHeaders, authorization) = case method of
        Get -> (getQuery, getAmzHeaders, getAuthorization)
        Head -> (getQuery, getAmzHeaders, getAuthorization)
        Delete -> (getQuery, getAmzHeaders, getAuthorization)
        Post -> (postQuery, postAmzHeaders, postAuthorization)
        PostQuery -> (postQuery, postAmzHeaders, postAuthorization)
        Put -> (postQuery, postAmzHeaders, postAuthorization)

    -- signatue dependend values for POST request
    postAmzHeaders = filter ((/= "Authorization") . fst) postSignature
    postAuthorization = return <$> lookup "authorization" postSignature
    postQuery = unsignedQuery
    postSignature = either error id $ signPostRequest
            (cred2cred $ signatureCredentials sigData)
            (snsConfRegion conf)
            ServiceNamespaceSns
            (signatureTime sigData)
            (httpMethod method)
            path
            unsignedQuery
            headers
            (fromMaybe "" body)

    -- signature dependend values for GET request
    getAmzHeaders = headers
    getAuthorization = Nothing
    getQuery = getSignature
    getSignature = either error id $ signGetRequest
            (cred2cred $ signatureCredentials sigData)
            (snsConfRegion conf)
            ServiceNamespaceSns
            (signatureTime sigData)
            (httpMethod method)
            path
            unsignedQuery
            headers
            (fromMaybe "" body)

    cred2cred (Credentials a b c) = SignatureV4Credentials a b c

-- -------------------------------------------------------------------------- --
-- SNS Response Consumer

snsResponseConsumer
    :: HTTPResponseConsumer a
    -> IORef SnsMetadata
    -> HTTPResponseConsumer a
snsResponseConsumer inner metadata resp = do

    let headerString = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders resp)
        amzId2 = headerString "x-amz-id-2"
        requestId = headerString "x-amz-request-id"
        m = SnsMetadata { snsMAmzId2 = amzId2, snsMRequestId = requestId }

    liftIO $ tellMetadataRef metadata m

    if HTTP.responseStatus resp >= HTTP.status400
        then snsErrorResponseConsumer resp
        else inner resp

-- | Parse XML Responses
--
snsXmlResponseConsumer
    :: (CU.Cursor -> Response SnsMetadata a)
    -> IORef SnsMetadata
    -> HTTPResponseConsumer a
snsXmlResponseConsumer p metadataRef =
    snsResponseConsumer (xmlCursorConsumer p metadataRef) metadataRef

-- | Parse Error Responses
--
snsErrorResponseConsumer :: HTTPResponseConsumer a
snsErrorResponseConsumer resp = do
    doc <- HTTP.responseBody resp $$+- XML.sinkDoc XML.def
    case parseError (CU.fromDocument doc) of
        Right err -> liftIO $ throwM err
        Left otherErr -> do
            -- doc <- HTTP.responseBody resp $$+- consume
            -- liftIO $ print $ B8.concat $ doc
            liftIO $ throwM otherErr
  where
    parseError root = SnsErrorResponse
        <$> pure (HTTP.responseStatus resp)
        <*> (force "Missing error Code" $ root $// elContent "Code")
        <*> (force "Missing error Message" $ root $// elContent "Message")
        <*> pure (listToMaybe $ root $// elContent "Resource")
        <*> pure (listToMaybe $ root $// elContent "HostId")
        <*> pure (listToMaybe $ root $// elContent "AWSAccessKeyId")
        <*> (pure $ do
            unprocessed <- listToMaybe $ root $// elCont "StringToSignBytes"
            B.pack <$> mapM readHex2 (words unprocessed))

-- -------------------------------------------------------------------------- --
-- SNS Errors

-- |
--
data SnsError a
    = SnsErrorCommon SnsCommonError
    | SnsErrorCommand a
    deriving (Show, Read, Eq, Ord, Typeable)

-- | TODO use type SnsError for snsErrorCode.
--
data SnsErrorResponse
    = SnsErrorResponse
        { snsErrorStatusCode :: !HTTP.Status
        , snsErrorCode :: !T.Text
        , snsErrorMessage :: !T.Text
        , snsErrorResource :: !(Maybe T.Text)
        , snsErrorHostId :: !(Maybe T.Text)
        , snsErrorAccessKeyId :: !(Maybe T.Text)
        , snsErrorStringToSign :: !(Maybe B.ByteString)
        }
    | SnsResponseDecodeError T.Text
    deriving (Show, Eq, Ord, Typeable)

instance Exception SnsErrorResponse

-- | Common SNS Errors
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/CommonErrors.html>
--
-- TODO add function to provide info about the error (content of haddock comments)
--
data SnsCommonError

    -- | The request signature does not conform to AWS standards. /Code 400/
    --
    = ErrorIncompleteSignature

    -- | The request processing has failed because of an unknown error,
    -- exception or failure.
    --
    -- /Code 500/
    --
    | ErrorInternalFailure

    -- | The action or operation requested is invalid. Verify that the action
    -- is typed correctly.
    --
    -- /Code 400/
    --
    | ErrorInvalidAction

    -- | The X.509 certificate or AWS access key ID provided does not exist in
    -- our records.
    --
    -- /Code 403/
    --
    | ErrorInvalidClientTokenId

    -- | Parameters that must not be used together were used together.
    --
    -- /Code 400/
    --
    | ErrorInvalidParameterCombination

    -- | An invalid or out-of-range value was supplied for the input parameter.
    --
    -- /Code 400/
    --
    | ErrorInvalidParameterValue

    -- | The AWS query string is malformed or does not adhere to AWS standards.
    --
    -- /Code 400/
    --
    | ErrorInvalidQueryParamter

    -- | The query string contains a syntax error.
    --
    -- /Code 404/
    --
    | ErrorMalformedQueryString

    -- | The request is missing an action or a required parameter.
    --
    -- /Code 400/
    --
    | ErrorMissingAction

    -- | The request must contain either a valid (registered) AWS access key ID
    -- or X.509 certificate.
    --
    -- /Code 403/
    --
    | ErrorMissingAuthenticationToken

    -- | A required parameter for the specified action is not supplied.
    --
    -- /Code 400/
    --
    | ErrorMissingParameter

    -- | The AWS access key ID needs a subscription for the service.
    --
    -- /Code 403/
    --
    | ErrorOptInRequired

    -- | The request reached the service more than 15 minutes after the date
    -- stamp on the request or more than 15 minutes after the request
    -- expiration date (such as for pre-signed URLs), or the date stamp on the
    -- request is more than 15 minutes in the future.
    --
    -- /Code 400/
    --
    | ErrorRequestExpired

    -- | The request has failed due to a temporary failure of the server.
    --
    -- /Code 503/
    --
    | ErrorServiceUnavailable

    -- | The request was denied due to request throttling.
    --
    -- /Code 400/
    --
    | ErrorThrottling

    -- | The input fails to satisfy the constraints specified by an AWS
    -- service.
    --
    -- /Code 400/
    --
    | ErrorValidationError
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

-- -------------------------------------------------------------------------- --
-- SNS Protocols

data SnsProtocol
    = SnsProtocolHttp
    | SnsProtocolHttps
    | SnsProtocolEmail
    | SnsProtocolEmailJson
    | SnsProtocolSms
    | SnsProtocolSqs
    | SnsProtocolApplication
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

snsProtocolToText :: IsString a => SnsProtocol -> a
snsProtocolToText SnsProtocolHttp = "http"
snsProtocolToText SnsProtocolHttps = "https"
snsProtocolToText SnsProtocolEmail = "email"
snsProtocolToText SnsProtocolEmailJson = "email-json"
snsProtocolToText SnsProtocolSms = "sms"
snsProtocolToText SnsProtocolSqs = "sqs"
snsProtocolToText SnsProtocolApplication = "application"

parseSnsProtocol :: P.CharParsing m => m SnsProtocol
parseSnsProtocol =
        SnsProtocolHttp <$ P.text "http"
    <|> SnsProtocolHttps <$ P.text "https"
    <|> SnsProtocolEmail <$ P.text "email"
    <|> SnsProtocolEmailJson <$ P.text "email-json"
    <|> SnsProtocolSms <$ P.text "sms"
    <|> SnsProtocolSqs <$ P.text "sqs"
    <|> SnsProtocolApplication <$ P.text "application"
    <?> "SnsProtocol"

instance AwsType SnsProtocol where
    toText = snsProtocolToText
    parse = parseSnsProtocol

instance Q.Arbitrary SnsProtocol where
    arbitrary = Q.elements [minBound..maxBound]

-- | An SNS endpoint is a client that subscribes to receive notifications
-- through the SNS service.
--
-- This must not be confused with the SNS AWS Service endpoints that are
-- the domain names to which API requests are made (see 'snsServiceEndpoint').
--
-- Endpoints vary by protocol:
--
-- * For the http protocol, the endpoint is an URL beginning with "http://"
-- * For the https protocol, the endpoint is a URL beginning with "https://"
-- * For the email protocol, the endpoint is an email address
-- * For the email-json protocol, the endpoint is an email address
-- * For the sms protocol, the endpoint is a phone number of an SMS-enabled device
-- * For the sqs protocol, the endpoint is the ARN of an Amazon SQS queue
-- * For the application protocol, the endpoint is the EndpointArn of a mobile app and device.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_Subscribe.html>
--
type SnsEndpoint = T.Text

-- -------------------------------------------------------------------------- --
-- Common Parameters

-- | Common SNS Parameters
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/CommonParameters.html>
--
-- The user of this API hardy needs to deal with the data type directly.
--
-- This API supports only signature version 4 with signature method @AWS4-HMAC-SHA256@.
--
-- /This is not currently used for computing the requests, but serves as
-- documentation and reference for the implementation of yet missing features./
--
data SnsCommonParameters = SnsCommonParameters
    { snsAction :: !SnsAction
    -- ^ The action to be performed.

    , snsParams :: () -- !(Maybe ConditionalRequestAuthParameters)
    -- ^ The parameters that are required to authenticate a Conditional request.

    , snsAWSAccessKeyId :: !B8.ByteString
    -- ^ The access key ID that corresponds to the secret access key that you used to sign the request.

    , snsExpires :: !UTCTime
    -- ^ The date and time when the request signature expires.
    -- Precisely one of snsExpires or snsTimestamp must be present.
    --
    -- format: @YYYY-MM-DDThh:mm:ssZ@ (ISO 8601)

    , snsTimestamp :: !UTCTime
    -- ^ The date and time of the request.
    -- Precisely one of snsExpires or snsTimestamp must be present.
    --
    -- format: @YYYY-MM-DDThh:mm:ssZ@ (ISO 8601)

    , snsSecurityToken :: () -- !(Maybe SecurityToken)
    -- ^ TODO

    , snsSignature :: !Signature
    -- ^ The digital signature that you created for the request. For
    -- information about generating a signature, go to the service's developer
    -- documentation.

    , snsSignatureMethod :: !SignatureMethod
    -- ^ The hash algorithm that you used to create the request signature.

    , snsSignatureVersion :: !SignatureVersion
    -- ^ The signature version you use to sign the request. Set this to the value that is recommended for your service.

    , snsVersion :: SnsVersion
    -- ^ The API version that the request is written for.
    }

