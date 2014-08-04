{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Aws.Sns
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/Welcome.html>
--
-- The types and functions of this package are supposed to be
-- use with the machinery from the
-- <https://hackage.haskell.org/package/aws aws package>.
--
-- Here is a very simple example for making a single request to AWS SNS.
--
-- > import Aws
-- > import Aws.Core
-- > import Aws.General
-- > import Aws.Sns
-- > import Data.IORef
-- >
-- > cfg <- Aws.baseConfiguration
-- > creds <- Credentials "access-key-id" "secret-access-key" `fmap` newIORef []
-- > let snsCfg = SnsConfiguration HTTPS UsWest2
-- > simpleAws cfg snsCfg $ ListTopics Nothing
--
-- In order to run the example you must replace @"access-key-id"@ and
-- @"secret-access-key"@ with the respective values for your AWS account.
--
module Aws.Sns
( module Aws.Sns.Core
, module Aws.Sns.Commands.CreateTopic
, module Aws.Sns.Commands.DeleteTopic
, module Aws.Sns.Commands.ListTopics
, module Aws.Sns.Commands.Subscribe
, module Aws.Sns.Commands.Unsubscribe
, module Aws.Sns.Commands.Publish
, module Aws.Sns.Commands.ConfirmSubscription
, module Aws.Sns.Commands.ListSubscriptionsByTopic
, module Aws.Sns.Commands.GetSubscriptionAttributes
) where

import Aws.Sns.Core
import Aws.Sns.Commands.CreateTopic
import Aws.Sns.Commands.DeleteTopic
import Aws.Sns.Commands.ListTopics
import Aws.Sns.Commands.Subscribe
import Aws.Sns.Commands.Unsubscribe
import Aws.Sns.Commands.Publish
import Aws.Sns.Commands.ConfirmSubscription
import Aws.Sns.Commands.ListSubscriptionsByTopic
import Aws.Sns.Commands.GetSubscriptionAttributes

