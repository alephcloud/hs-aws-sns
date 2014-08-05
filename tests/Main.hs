{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Tests for Haskell SNS bindings
--

import Aws
import Aws.Core
import Aws.General
import Aws.Sns
import qualified Aws.Sqs as SQS

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (encode, object, (.=), eitherDecode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Tasty

import System.Environment
import System.Exit
import System.IO

import Utils

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = do
    args <- getArgs
    runMain args $ map (second tail . span (/= '=')) args
  where
    runMain :: [String] -> [(String,String)] -> IO ()
    runMain args argsMap
        | any (`elem` helpArgs) args = defaultMain (tests "")
        | "--run-with-aws-credentials" `elem` args =
            case lookup "--test-email" argsMap of
                Nothing -> do
                    hPutStrLn stderr "Command line option --test-email=<email> is missing"
                    hPutStrLn stderr help
                    exitFailure
                Just email ->
                    withArgs (tastyArgs args) . defaultMain $ tests (T.pack email)
        | otherwise = putStrLn help >> exitFailure

    helpArgs = ["--help", "-h"]
    mainArgs =
        [ "--run-with-aws-credentials"
        , "--test-email"
        ]
    tastyArgs args = flip filter args $ \x -> not
        $ any (`L.isPrefixOf` x) mainArgs


help :: String
help = L.intercalate "\n"
    [ ""
    , "NOTE"
    , ""
    , "This test suite accesses the AWS account that is associated with"
    , "the default credentials from the credential file ~/.aws-keys."
    , ""
    , "By running the tests in this test-suite costs for usage of AWS"
    , "services may incur."
    , ""
    , "In order to actually excute the tests in this test-suite you must"
    , "provide the command line options:"
    , ""
    , "    --test-email=<email-address>"
    , "    --run-with-aws-credentials"
    , ""
    , "When running this test-suite through cabal you may use the following"
    , "command:"
    , ""
    , "    cabal test SNS-tests --test-option=--run-with-aws-credentials \\"
    , "                         --test-option=--test-email=<email-address>"
    , ""
    ]

tests
    :: T.Text -- email address
    -> TestTree
tests email = testGroup "SNS Tests"
    [ test_createTopic
    , test_topic1 email
    , test_topicSqs
    ]

-- -------------------------------------------------------------------------- --
-- Static Test parameters
--
-- TODO make these configurable

testProtocol :: Protocol
testProtocol = HTTP

-- | SQS endpoint
testSqsEndpoint :: SQS.Endpoint
testSqsEndpoint = SQS.sqsEndpointUsWest2

defaultTopicName :: T.Text
defaultTopicName = "test-topic"

defaultQueueName :: T.Text
defaultQueueName = "test-queue"

-- -------------------------------------------------------------------------- --
-- SNS Utils

snsConfiguration :: SnsConfiguration qt
snsConfiguration = SnsConfiguration testProtocol testRegion

simpleSns
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SnsConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleSns command = do
    c <- baseConfiguration
    simpleAws c snsConfiguration command

simpleSnsT
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SnsConfiguration, MonadIO m)
    => r
    -> EitherT T.Text m (MemoryResponse a)
simpleSnsT = tryT . simpleSns

-- |
--
withTopic
    :: T.Text -- ^ Topic name
    -> (Arn -> IO a) -- ^ test function
    -> IO a
withTopic topicName = bracket createTopic deleteTopic
  where
    createTopic = do
        CreateTopicResponse arn <- simpleSns $ CreateTopic topicName
        return arn
    deleteTopic arn = simpleSns (DeleteTopic arn) >> return ()

withTopicTest
    :: T.Text -- ^ Topic name
    -> (IO Arn -> TestTree) -- ^ test tree
    -> TestTree
withTopicTest topic = withResource createTopic deleteTopic
  where
    createTopic = do
        CreateTopicResponse arn <- simpleSns $ CreateTopic tTopic
        return arn
    deleteTopic arn = void . simpleSns $ DeleteTopic arn
    tTopic = testData topic

-- -------------------------------------------------------------------------- --
-- SQS Utils

sqsArn
    :: Region
    -> AccountId
    -> T.Text -- ^ Queue Name
    -> Arn
sqsArn region accountId queueName = Arn
    { arnService = ServiceNamespaceSqs
    , arnRegion = Just region
    , arnAccount = Just accountId
    , arnResource = [queueName]
    }

testSqsArn :: T.Text -> Arn
testSqsArn url = Arn
    { arnService = ServiceNamespaceSqs
    , arnRegion = Just testRegion
    , arnAccount = Just $ AccountId (sqsAccountIdText url)
    , arnResource = [sqsQueueNameText url]
    }

sqsQueueName :: T.Text -> SQS.QueueName
sqsQueueName url = SQS.QueueName (sqsQueueNameText url) (sqsAccountIdText url)

sqsQueueNameText :: T.Text -> T.Text
sqsQueueNameText url = T.split (== '/') url !! 4

sqsAccountIdText :: T.Text -> T.Text
sqsAccountIdText url = T.split (== '/') url !! 3

sqsConfiguration :: SQS.SqsConfiguration qt
sqsConfiguration = SQS.SqsConfiguration
    { SQS.sqsProtocol = testProtocol
    , SQS.sqsEndpoint = testSqsEndpoint
    , SQS.sqsPort = 80
    , SQS.sqsUseUri = False
    , SQS.sqsDefaultExpiry = 180
    }

simpleSqs
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleSqs command = do
    c <- baseConfiguration
    simpleAws c sqsConfiguration command

simpleSqsT
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration, MonadIO m)
    => r
    -> EitherT T.Text m (MemoryResponse a)
simpleSqsT = tryT . simpleSqs

-- |
--
-- Throws an exception if the URL returned by 'CreateQueue' doesn't
-- include the HTTP protocol or the account ID as first path component.
--
withSqsQueue
    :: T.Text
    -- ^ queue name
    -> (T.Text -> SQS.QueueName -> Arn -> IO a)
    -- ^ the first argument is the queue URL,
    -- the second is the 'SQS.QueueName', and
    -- the third the queue 'Arn'.
    -> IO a
withSqsQueue queueName f = bracket createQueue deleteQueue $ \url ->
    f url (sqsQueueName url) (testSqsArn url)
  where
    createQueue = do
        SQS.CreateQueueResponse url <- simpleSqs $ SQS.CreateQueue Nothing queueName
        return url
    deleteQueue url = void $ simpleSqs (SQS.DeleteQueue (sqsQueueName url))

withQueueTest
    :: T.Text -- ^ Queue name
    -> (IO (T.Text, SQS.QueueName, Arn) -> TestTree) -- ^ test tree
    -> TestTree
withQueueTest queueName f = withResource createQueue deleteQueue $ \getQueueUrl -> do
    let getQueueParams = do
            url <- getQueueUrl
            return (url, sqsQueueName url, testSqsArn url)
    f getQueueParams
  where
    createQueue = do
        SQS.CreateQueueResponse url <- simpleSqs $ SQS.CreateQueue Nothing queueName
        return url
    deleteQueue url = void $ simpleSqs (SQS.DeleteQueue (sqsQueueName url))

-- | Set queue policy attribute that allows an SNS Topic
-- to send notification to an SQS queue.
--
sqsAllowTopicAttribute
    :: Arn -- ^ Queue ARN
    -> T.Text -- ^ policy ID
    -> Arn -- ^ topic ARN
    -> SQS.SetQueueAttributes
sqsAllowTopicAttribute queueArn policyId topicArn = SQS.SetQueueAttributes
    { SQS.sqaAttribute = SQS.Policy
    , SQS.sqaValue = T.decodeUtf8 . LB.toStrict . encode $ object
        [ "Version" .= ("2012-10-17" :: T.Text)
        , "Statement" .= [object
            [ "Resource" .= queueArn
            , "Sid" .= policyId
            , "Effect" .= ("Allow" :: T.Text)
            , "Principal" .= object [ "AWS" .= ("*" :: T.Text) ]
            , "Action" .= ("sqs:SendMessage" :: T.Text)
            , "Condition" .= object
                [ "ArnEquals" .= object [ "aws:SourceArn" .= topicArn ]
                ]
            ]]
        ]
    , SQS.sqaQueueName = queueId
    }
  where
    queueId = SQS.QueueName
        { SQS.qAccountNumber = case arnAccount queueArn of
            Nothing -> error $ "Malformed SQS queue ARN: " <> toText queueArn
            Just (AccountId t) -> t
        , SQS.qName = if length (arnResource queueArn) /= 1
            then error $ "Malformed SQS queue ARN: " <> toText queueArn
            else head $ arnResource queueArn
        }

-- -------------------------------------------------------------------------- --
-- Topic Creation Tests

test_createTopic :: TestTree
test_createTopic = testGroup "Topic creation"
    [ eitherTOnceTest1 "create list delete topic" prop_topicCreateListDelete
    ]

-- |
--
-- TODO:
--
-- * use 'awsIteratedList' for parsing the topics list
--
prop_topicCreateListDelete
    :: T.Text -- ^ topic name
    -> EitherT T.Text IO ()
prop_topicCreateListDelete topicName = do
    CreateTopicResponse topicArn <- simpleSnsT $ CreateTopic tTopicName
    handleT (\e -> deleteTopic topicArn >> left e) $ do
        ListTopicsResponse _ allTopics <- simpleSnsT (ListTopics Nothing)
        unless (topicArn `elem` allTopics)
            . left $ "topic " <> toText topicArn <> " not listed"
        deleteTopic topicArn
  where
    tTopicName = testData topicName
    deleteTopic arn = void $ simpleSnsT (DeleteTopic arn)

-- -------------------------------------------------------------------------- --
-- Topic Tests

test_topic1
    :: T.Text -- ^ email address
    -> TestTree
test_topic1 email = withTopicTest defaultTopicName $ \getTopicArn ->
    testGroup "Perform a series of tests on a single topic"
        [ eitherTOnceTest0 "email subscribe"
            $ liftIO getTopicArn >>= \t -> prop_emailSubscribe t email
        ]

-- | Subscribe an email endpoint (don't wait for confirmation).
--
prop_emailSubscribe
    :: Arn -- ^ topic arn
    -> SnsEndpoint -- ^ email addresss
    -> EitherT T.Text IO ()
prop_emailSubscribe topicArn email = do
    SubscribeResponse maybeSubArn <- simpleSnsT $ Subscribe (Just email) SnsProtocolEmail topicArn
    case maybeSubArn of
        Nothing -> return ()
        Just subArn -> do
            let e = "unexpected subscription arn when 'confirmation pending' is expected"
            void . handleT (\e2 -> left (e <> " and " <> e2))
                . simpleSnsT $ Unsubscribe subArn
            left e

-- -------------------------------------------------------------------------- --
-- SQS Integration Tests

test_topicSqs :: TestTree
test_topicSqs = withTopicTest defaultTopicName $ \getTopicArn ->
    withQueueTest defaultQueueName $ \getQueueParams -> testGroup "SQS Integration Tests"
        [ eitherTOnceTest0 "SQS subscribe publish unsubscribe" $ do
            topicArn <- liftIO getTopicArn
            (_, queueId, queueArn) <- liftIO getQueueParams
            prop_sqsSubscribePublishUnsubscribe topicArn queueId queueArn
        ]

-- | Subscribe an SQS queue to an SNS topic
--
prop_sqsSubscribePublishUnsubscribe
    :: Arn -- ^ topic arn
    -> SQS.QueueName -- ^ queue id
    -> Arn -- queue Arn
    -> EitherT T.Text IO ()
prop_sqsSubscribePublishUnsubscribe topicArn queueId queueArn = do

    -- Add permission to send messages from SNS topic (identified by ARN) to queue
    void . simpleSqsT $ sqsAllowTopicAttribute queueArn sqsPermissionId topicArn

    -- subscribe Queue to SNS topci
    SubscribeResponse maybeSubArn <- simpleSnsT $ Subscribe (Just $ toText queueArn) SnsProtocolSqs topicArn
    subArn <- maybeSubArn ?? "Subscription failed: subscription Arn is missing probably because the confirmation is yet pending"

    -- Let's wait 5 seconds, just be on the safe side ...
    liftIO $ threadDelay (5*1000000)

    -- publish to topic
    PublishResponse msgId0 <- simpleSnsT $ Publish msg Nothing (Just subj) (Left topicArn)

    -- receive messages
#if MIN_VERSION_aws(100,0,0)
    let numRetry = 3
#else
    let numRetry = 6
#endif
    sqsMsg <- retryT numRetry $ do
        SQS.ReceiveMessageResponse msgs <- simpleSqsT $ SQS.ReceiveMessage
            { SQS.rmVisibilityTimeout = Nothing
            , SQS.rmAttributes = []
            , SQS.rmMaxNumberOfMessages = Just 1
            , SQS.rmQueueName = queueId
#if MIN_VERSION_aws(100,0,0)
            , SQS.rmWaitTimeSeconds = Just 20
#endif
            }
        when (length msgs < 1) $ left
            $ "unexpected number of messages in queue; expected 1, got " <> sshow (length msgs)
        return $ head msgs

    -- parse notification
    notification :: SqsNotification <- fmapLT T.pack . hoistEither
        . eitherDecode . LB.fromStrict . T.encodeUtf8 $ SQS.mBody sqsMsg

    -- check result
    when (sqsNotificationMessageId notification /= msgId0) $ left
        $ "message IDs don't match; epxected " <> q (messageIdText msgId0)
        <> ", got " <> q (messageIdText $ sqsNotificationMessageId notification)
    when (sqsNotificationMessage notification /= snsMessageDefault msg) $ left
        $ "messages don't match; expected " <> q (snsMessageDefault msg)
        <> ", got " <> q (sqsNotificationMessage notification)

    -- unsubscribe queue
    void $ simpleSnsT $ Unsubscribe subArn
  where
    q t = "\"" <> t <> "\""
    sqsPermissionId = testData . head . arnResource $ topicArn
    msg = snsMessage "message abc"
    subj = "subject abc"

