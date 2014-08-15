-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module: Aws.Sns.Policy.Internal
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_SpecialInfo.html>
--
-- This module contains the SNS independent aspects of the access policy
-- language. It seems that this is shared with the respective aspects of the
-- SQS policy language. Also it seesm that (functionally) it is a strict subset
-- of the IAM policy language.
--
module Aws.Sns.Policy.Internal
(
-- * Misc Types
  PrincipalType
, IPAddress

-- * Policy
, PolicyId
, PolicyVersion(..)
, Policy(..)
, ServicePolicy(..)

-- * Statement
, Effect
, Principal
, PolicyAction
, StatementId
, Statement(..)
, ServiceStatement(..)

-- * Condition Key
, ConditionKey(..)
, SomeConditionKey(..)

-- * AWS Condition Key
, AwsConditionType(..)
, AwsConditionKey(..)

-- * Condition
, Condition(..)
, ServiceCondition(..)
) where

import Aws.General

import Control.Applicative
import Control.Arrow (first)
import Control.Monad

import Data.Aeson hiding (Null)
import Data.Aeson.Types hiding (Null)
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Typeable
import Data.UUID hiding (fromString)
import qualified Data.Vector as V
import Data.IP

import GHC.Generics

import System.Locale

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Read hiding (String)

-- -------------------------------------------------------------------------- --
-- Misc Types

-- | FIXME: where is this documented? Is it free style?
--
-- For now we use a textual representation which supports
-- easy usage with string conditions.
--
type PrincipalType = T.Text

-- | IP Addresses
--
-- IP address conditions let you constrain based on IP address matching rules.
-- You use these with the aws:SourceIp key. The value must be in the standard
-- CIDR format (for example, 10.52.176.0/24). For more information, go to RFC
-- 4632.
--
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_ElementDescriptions.html#Conditions_IPAddress>
--
type IPAddress = AddrRange IPv4

instance AwsType IPAddress where
    toText = fromString . show
    parse = many anyChar >>= either fail return . readEither

instance ToJSON IPAddress where
    toJSON = toJSON . (toText ∷ IPAddress → T.Text)

instance FromJSON IPAddress where
    parseJSON = withText "IPAddress"
        $ either fail return . fromText

instance ToJSON AccountId where
    toJSON = toJSON . (toText ∷ AccountId → T.Text)

instance FromJSON AccountId where
    parseJSON = withText "AccountId" $ either fail return . fromText

-- -------------------------------------------------------------------------- --
-- Policy

-- |The Id is an optional identifier for the policy. We recommend you use a
-- UUID for the value, or incorporate a UUID as part of the ID to ensure
-- uniqueness.
--
-- /IMPORTANT/
--
-- The AWS service (e.g., Amazon SNS) implementing the access policy language
-- might require this element and have uniqueness requirements for it. For
-- service-specific information about writing policies, see Special Information
-- for Amazon SNS Policies.
--
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_ElementDescriptions.html>
--
type PolicyId = UUID

instance ToJSON PolicyId where
    toJSON = toJSON . show

instance FromJSON PolicyId where
    parseJSON = withText "PolicyId" $ either fail return . readEither . T.unpack

-- | The Version element specifies the access policy language version.
--
-- If you do not include a Version element, the value defaults to 2008-10-17.
-- However, it is a good practice to always include a Version element and set
-- it to 2012-10-17.
--
-- /NOTE/
--
-- If your policy includes policy variables, you must include a Version element
-- and set it to 2012-10-17. If you don't include a Version element set to
-- 2012-10-17, variables such as ${aws:username} won't be recognized as
-- variables and will instead be treated as literal strings in the policy.
--
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_ElementDescriptions.html>
--
data PolicyVersion
    = PolicyVersion_2008_10_17
    -- ^ This was an earlier version of the policy language. You might see this
    -- version on existing policies. Do not use this version for any new
    -- policies or any existing policies that you are updating.

    | PolicyVersion_2012_10_17
    -- ^ This is the current version of the policy language, and you should use
    -- this version number for all policies.

    deriving (Show, Read, Eq, Ord, Enum, Typeable, Generic)

instance ToJSON PolicyVersion where
    toJSON PolicyVersion_2008_10_17 = "2008-10-17"
    toJSON PolicyVersion_2012_10_17 = "2012-10-17"

instance FromJSON PolicyVersion where
    parseJSON = withText "PolicyVersion" $ \case
        "2008-10-17" → pure PolicyVersion_2008_10_17
        "2012-10-17" → pure PolicyVersion_2012_10_17
        t → fail $ "unexpected policy version: " <> show t

-- | Restrictions for AWS SNS policies:
--
-- * each policy must have a unique 'policyId'
--
-- * each statement must have a unique 'statementSid'
--
-- * each policy must cover only a single topic or queue
--
-- Limits for SNS policies:
--
-- * 20KB, 20 Statements, 20 Principals, 1 Resource where the
--   ARN matches the ARN of the policy's topic.
--
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_SpecialInfo.html>
--
data Policy = Policy
    { policyVersion ∷ !PolicyVersion
    , policyId ∷ !PolicyId
    , policyStatement ∷ ![Statement]
    }
    -- deriving (Show, Eq, Ord, Typeable)
    deriving (Show, Eq, Ord, Typeable)

instance ToJSON Policy where
    toJSON Policy{..} = object
        [ "Version" .= policyVersion
        , "Id" .= policyId
        , "Statement" .= policyStatement
        ]

-- -------------------------------------------------------------------------- --
-- ServicePolicy newtype wrapper for JSON parsing

newtype ServicePolicy (s ∷ AwsConditionType → *) = ServicePolicy Policy
    deriving (Show, Eq, Ord, Typeable)

instance ConditionKey s ⇒ FromJSON (ServicePolicy s) where
    parseJSON v = ServicePolicy <$> p v
      where
        p = withObject "Policy" $ \o → Policy
            <$> o .: "Version"
            <*> o .: "Id"
            <*> (map (\(ServiceStatement s ∷ ServiceStatement s) → s) <$> o .: "Statement")

instance ConditionKey s ⇒ ToJSON (ServicePolicy s) where
    toJSON (ServicePolicy p) = toJSON p

-- -------------------------------------------------------------------------- --
-- Statement

-- | The Sid (statement ID) is an optional identifier you provide for the
-- policy statement. Essentially it is just a sub-ID of the policy document's
-- ID.
--
-- /IMPORTANT/
--
-- The AWS service (e.g., Amazon SNS) implementing the access policy language
-- might require this element and have uniqueness requirements for it. For
-- service-specific information about writing policies, see Special Information
-- for Amazon SNS Policies.
--
type StatementId = T.Text

data Effect
    = EffectAllow
    | EffectDeny
    deriving (Show, Read, Eq, Ord, Enum, Typeable)

instance ToJSON Effect where
    toJSON EffectAllow = "Allow"
    toJSON EffectDeny = "Deny"

instance FromJSON Effect where
    parseJSON = withText "Effect" $ \case
        "Allow" → pure EffectAllow
        "Deny" → pure EffectDeny
        t → fail $ "unexpected effect: " <> show t

-- | The Principal is the person or persons who receive or are denied
-- permission according to the policy.
--
-- You must specify the principal by using the principal's AWS account ID
-- (e.g., 1234-5678-9012, with or without the hyphens). You can specify
-- multiple principals, or a wildcard (*) to indicate all possible users.
--
-- NOTE that the documentation at
-- <http://docs.aws.amazon.com/sns/latest/dg/AccessPolicyLanguage_ElementDescriptions.html>
-- is not clear about the format. Also it is not clear of accounts are given only
-- by account ID or full ARNs.
--
data Principal
    = PrincipalAccountIds [AccountId]
    | PrincipalAny
    deriving (Show, Read, Eq, Ord, Typeable)

instance ToJSON Principal where
    toJSON (PrincipalAccountIds l) = toJSON l
    toJSON PrincipalAny = toJSON ["*" ∷ T.Text]

instance FromJSON Principal where
    parseJSON = withArray "Principal" $ V.foldM f (PrincipalAccountIds [])
      where
        f PrincipalAny = const $ return PrincipalAny
        f (PrincipalAccountIds t) = withText "AccountId" $ \case
            "*" → pure PrincipalAny
            x → PrincipalAccountIds . (:t) <$> parseJSON (String x)

-- TODO replace this with a typeclass
type PolicyAction = T.Text

{-
-- | TODO
--
data SnsPolicyAction
    = SnsPolicyActionAny
    | SnsPolicyAction [SnsAction]
    deriving (Show, Read, Eq, Ord, Typeable)

instance ToJSON SnsPolicyAction where
    toJSON SnsPolicyActionAny = toJSON ["*"]
    toJSON (SnsPolicyAction l) = toJSON l

instance FromJSON SnsPolicyAction where
    parseJSON = withArray "Action" $ V.foldM f (SnsPolicyAction [])
      where
        f SnsPolicyActionAny _ = return SnsPolicyActionAny
        f (SnsPolicyAction t) x = withText "SnsAction" $ \case
            "*" → pure SnsPolicyActionAny
            x → SnsPolicyAction . (:t) <$> parseJSON x
-}

-- | Each statement includes the core information about a single permission. If
-- a policy includes multiple statements, we apply a logical OR across the
-- statements at evaluation time. If multiple policies are applicable to a
-- request, we apply a logical OR across the policies at evaluation time.
--
-- <http://docs.aws.amazon.com/sns/2013-/dg/BasicStructure.html>
--
data Statement = Statement
    { statementSid ∷ !StatementId
    -- ^ The Sid (statement ID) is an optional identifier you provide for the
    -- policy statement. Essentially it is just a sub-ID of the policy
    -- document's ID.

    , statementEffect ∷ !Effect
    -- ^ The Effect is a required element that indicates whether you want the
    -- statement to result in an allow or an explicit deny (for more
    -- information, see Explicit Deny).

    , statementPrincipal ∷ !Principal
    -- ^ The Principal is the person or persons who receive or are denied
    -- permission according to the policy.

    , statementNotPrincipal ∷ !Principal
    -- ^ The NotPrincipal element lets you specify an exception to a list of
    -- principals. For example, you can use this to prevent all AWS accounts
    -- except a specific account from accessing a resource. Conversely, you can
    -- deny access to all principals except the one named in the NotPrincipal
    -- element. As with Principal, you specify the user or account that should
    -- be allowed or denied permission; the difference is that NotPrincipal
    -- translates to everyone except that person or account.

    , statementAction ∷ !PolicyAction
    -- ^ The Action is the specific type or types of access allowed or denied
    -- (for example, read or write). You can specify multiple values for this
    -- element. The values are free-form but must match values the AWS service
    -- expects (for more information, see Special Information for Amazon SNS
    -- Policies). You can use a wildcard (*) to give the principal access to
    -- all the actions the specific AWS service lets you share with other
    -- developers.
    --
    -- The prefix and the action name are case insensitive. For example,
    -- sns:Subscribe is equivalent to SNS:subscribe.

    , statementNotAction ∷ !PolicyAction
    -- ^ The NotAction element matches everything except the specified action.
    -- This is useful if you want to make an exception to a list of actions
    -- being allowed or denied. The example below matches any action, except
    -- Publish.

    , statementResource ∷ ![Arn]
    -- ^ The Resource is the object or objects the policy covers. You specify
    -- the resource using the following Amazon Resource Name (ARN) format.

    , statementNotResource ∷ ![Arn]
    -- ^ The NotResource element is useful if you want to make an exception to
    -- a list of resources. You could use this, for example, if you want your
    -- users to be able to access a specific Amazon SNS topic belonging to the
    -- AWS account. If the AWS account were to create a new topic for the
    -- company, an admin wouldn't have to update the policy with the new
    -- topic's name in order to prevent users from being able to use the topic.
    -- By default, the users wouldn't be able to use it.
    --
    -- The following example refers to all resources other than your company's
    -- topic called my_corporate_topic. You would use this in a policy with
    -- @"Effect":"Deny"@ to keep users from accessing any queue besides
    -- @my_corporate_topic@.
    --
    -- > "NotResource":"arn:aws:sqs:*:123456789012:my_corporate_topic"

    , statementCondition ∷ ![Condition]
    -- ^ conditions are joined via /and/
    }
    -- deriving (Show, Eq, Ord, Typeable, Generic)
    deriving (Show, Eq, Ord, Typeable)

instance ToJSON Statement where
    toJSON Statement{..} = object
        [ "Sid" .= statementSid
        , "Effect" .= statementEffect
        , "Principal" .= statementPrincipal
        , "NotPrincipal" .= statementNotPrincipal
        , "Action" .= statementAction
        , "NotAction" .= statementNotAction
        , "Resource" .= statementResource
        , "NotResource" .= statementNotResource
        , "Condition" .= foldBlock statementCondition
        ]
      where
        foldBlock ∷ [Condition] → Value
        foldBlock = undefined -- L.foldl' (\a b → HM.unionWith foldKey a (toJSON b)) HM.empty
        foldKey ∷ HM.HashMap T.Text [a] → HM.HashMap T.Text [a] → HM.HashMap T.Text [a]
        foldKey = HM.unionWith (<>)

-- -------------------------------------------------------------------------- --
-- ServiceStatement newtype wrapper for JSON parsing

newtype ServiceStatement (s ∷ AwsConditionType → *) = ServiceStatement Statement
    deriving (Show, Eq, Ord, Typeable)

instance ConditionKey s ⇒ FromJSON (ServiceStatement s) where
    parseJSON v = ServiceStatement <$> p v
      where
        p = withObject "Statement" $ \o → Statement
            <$> o .: "Sid"
            <*> o .: "Effect"
            <*> o .: "Principal"
            <*> o .: "NotPrincipal"
            <*> o .: "Action"
            <*> o .: "NotAction"
            <*> o .: "Resource"
            <*> o .: "NotResource"
            <*> (unfoldCondition =<< o .: "Condition")
          where
            unfoldCondition ∷ Value → Parser [Condition]
            unfoldCondition = withObject "Condition" $ \o →
                forM (HM.toList o) $ \(k, val) → do
                    ServiceCondition c ∷ ServiceCondition s ← parseJSON $ Object (HM.singleton k val)
                    return c

-- -------------------------------------------------------------------------- --
-- Condition Key Class

data SomeConditionKey
    = ∀ k a . (Typeable a, ConditionKey k, Ord (k a), Show (k a))
    ⇒ SomeConditionKey (k a)

deriving instance Typeable SomeConditionKey

instance Ord SomeConditionKey where
    compare (SomeConditionKey a) (SomeConditionKey b) = case cast a of
        Just a_ → compare a_ b
        Nothing → compare (typeRep a) (typeRep b)

instance Eq SomeConditionKey where
    a == b = compare a b == EQ

instance Show SomeConditionKey where
    show (SomeConditionKey k) = show k

class
    (Typeable k
    , FromJSON (k 'AwsString)
    , FromJSON (k 'AwsNumeric)
    , FromJSON (k 'AwsDateTime)
    , FromJSON (k 'AwsBoolean)
    , FromJSON (k 'AwsIPAddress)
    , FromJSON (k 'AwsArn)

    , Show (k 'AwsString)
    , Show (k 'AwsNumeric)
    , Show (k 'AwsDateTime)
    , Show (k 'AwsBoolean)
    , Show (k 'AwsIPAddress)
    , Show (k 'AwsArn)

    , Ord (k 'AwsString)
    , Ord (k 'AwsNumeric)
    , Ord (k 'AwsDateTime)
    , Ord (k 'AwsBoolean)
    , Ord (k 'AwsIPAddress)
    , Ord (k 'AwsArn)
    )
    ⇒ ConditionKey (k ∷ AwsConditionType → *)
  where
    toConditionKey ∷ (Typeable a, Show (k a), Ord (k a)) ⇒ k a → SomeConditionKey
    fromConditionKey ∷ (Typeable a) ⇒ SomeConditionKey → Maybe (k a)

    toConditionKey = SomeConditionKey
    fromConditionKey (SomeConditionKey k) = cast k

-- -------------------------------------------------------------------------- --
-- AWS Condition Key

data AwsConditionType
    = AwsString
    | AwsNumeric
    | AwsBoolean
    | AwsDateTime
    | AwsIPAddress
    | AwsArn
    | AwsNull
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

deriving instance Typeable 'AwsString
deriving instance Typeable 'AwsNumeric
deriving instance Typeable 'AwsBoolean
deriving instance Typeable 'AwsDateTime
deriving instance Typeable 'AwsIPAddress
deriving instance Typeable 'AwsArn
deriving instance Typeable 'AwsNull

-- | Common Condition Keys
--
data AwsConditionKey (a ∷ AwsConditionType) where
    AWSCurrentTime ∷ AwsConditionKey AwsDateTime
    -- ^ For date/time conditions.
    --
    -- /Common to all AWS Services/

    AWSEpocheTime ∷ AwsConditionKey AwsDateTime
    -- ^ The date in epoch or UNIX time, for use with date/time conditions.
    --
    -- Internally we represent this in the same way as 'AWSCurrentTime'.
    --
    -- /Common to all AWS Services/

    AWSMultiFactorAuthAge ∷ AwsConditionKey AwsNumeric
    -- ^ Provides a numeric value indicating how long ago (in whole seconds) the
    -- MFA-validated security credentials making the request were issued using
    -- Multi-Factor Authentication (MFA). Unlike other keys, if MFA is not used
    -- successfully, this key is not present (see Existence of Condition Keys,
    -- Numeric Conditions and Using Multi-Factor Authentication (MFA) Devices with
    -- AWS).
    --
    -- /Common to all AWS Services/

    AWSPrincipalType ∷ AwsConditionKey AwsString
    -- ^ To check the type of principal (user, account, federated user, etc.)
    -- for the current request.
    --
    -- /Common to all AWS Services/

    AWSSecureTransport ∷ AwsConditionKey AwsBoolean
    -- ^ Boolean representing whether the request was sent using SSL.
    --
    -- /Common to all AWS Services/

    AWSSourceArn ∷ AwsConditionKey AwsArn
    -- ^ The Amazon Resource Name (ARN) of the source.
    --
    -- /Common to all AWS Services/

    AWSSourceIp ∷ AwsConditionKey AwsIPAddress
    -- ^ The requester's IP address, for use with IP address conditions.
    --
    -- this is always the public IP address

    AWSUserAgent ∷ AwsConditionKey AwsString
    -- ^ Information about the requester's client application, for use with
    -- string conditions.
    --
    -- /Common to all AWS Services/

    AWSUserId ∷ AwsConditionKey AwsString
    -- ^ To check the requester's user ID.
    --
    -- /Common to all AWS Services/

    AWSUserName ∷ AwsConditionKey AwsString
    -- ^ To check the requester's user name.
    --
    -- /Common to all AWS Services/

deriving instance Show (AwsConditionKey a)
deriving instance Eq (AwsConditionKey a)
deriving instance Ord (AwsConditionKey a)
deriving instance Typeable AwsConditionKey

instance Typeable a ⇒ AwsType (AwsConditionKey a) where
    toText AWSCurrentTime = "aws:CurrentTime"
    toText AWSEpocheTime = "aws:EpocheTime"
    toText AWSMultiFactorAuthAge = "aws:MultiFactorAuthAge"
    toText AWSPrincipalType = "aws:PrincipalType"
    toText AWSSecureTransport = "aws:SecureTransport"
    toText AWSSourceArn = "aws:SourceArn"
    toText AWSSourceIp = "aws:SourceIp"
    toText AWSUserAgent = "aws:UserAgent"
    toText AWSUserId = "aws:UserId"
    toText AWSUserName = "aws:UserName"

    parse = p >>= maybe (unexpected "type error") return
      where
        p ∷ (Monad m, CharParsing m, Typeable a) ⇒ m (Maybe (AwsConditionKey a))
        p = c AWSCurrentTime <$ text "aws:CurrentTime"
            <|> c AWSCurrentTime <$ text "aws:EpocheTime"
            <|> c AWSMultiFactorAuthAge <$ text "aws:MultiFactorAuthAge"
            <|> c AWSPrincipalType <$ text "aws:PrincipalType"
            <|> c AWSSourceArn <$ text "aws:SourceArn"
            <|> c AWSSourceIp <$ text "aws:SourceIp"
            <|> c AWSSourceArn <$ text "aws:SourceArn"
            <|> c AWSUserAgent <$ text "aws:UserAgent"
            <|> c AWSUserId <$ text "aws:UserId"
            <|> c AWSUserName <$ text "aws:UserName"
            <?> "AwsConditionKey"
        c ∷ Typeable b ⇒ AwsConditionKey b → Maybe (AwsConditionKey a)
        c = cast

instance Typeable a ⇒ FromJSON (AwsConditionKey a) where
    parseJSON = withText "SnsConditionKey"
        $ either fail return . fromText

instance ConditionKey AwsConditionKey

-- -------------------------------------------------------------------------- --
-- Condition

-- | Values are joined via /or/.
--
-- We choose a slightly different syntax for conditions
-- compared to the JSON representation that is used
-- by the AWS API.
--
-- Our representation is more modular, more compositional, and provides more
-- type safety. The 'FromJSON' and 'ToJSON' instances translate between both
-- representations.
--
data Condition where
    StringEquals ∷ ConditionKey k ⇒ k AwsString → [T.Text] → Condition
    StringNotEquals ∷ ConditionKey k ⇒ k AwsString → [T.Text] → Condition
    StringEqualsI ∷ ConditionKey k ⇒ k AwsString → [T.Text] → Condition
    StringNotEqualsI ∷ ConditionKey k ⇒ k AwsString → [T.Text] → Condition
    StringLike ∷ ConditionKey k ⇒ k AwsString → [T.Text] → Condition
    StringNotLike ∷ ConditionKey k ⇒ k AwsString → [T.Text] → Condition

    NumericEquals ∷ ConditionKey k ⇒ k AwsNumeric → [Integer] → Condition
    NumericNotEquals ∷ ConditionKey k ⇒ k AwsNumeric → [Integer] → Condition
    NumericLessThan ∷ ConditionKey k ⇒ k AwsNumeric → [Integer] → Condition
    NumericLessThanEquals ∷ ConditionKey k ⇒ k AwsNumeric → [Integer] → Condition
    NumericGreaterThan ∷ ConditionKey k ⇒ k AwsNumeric → [Integer] → Condition
    NumericGreaterThanEquals ∷ ConditionKey k ⇒ k AwsNumeric → [Integer] → Condition

    DateEquals ∷ ConditionKey k ⇒ k AwsDateTime → [UTCTime] → Condition
    DateNotEquals ∷ ConditionKey k ⇒ k AwsDateTime → [UTCTime] → Condition
    DateLessThan ∷ ConditionKey k ⇒ k AwsDateTime → [UTCTime] → Condition
    DateLessThanEquals ∷ ConditionKey k ⇒ k AwsDateTime → [UTCTime] → Condition
    DateGreaterThan ∷ ConditionKey k ⇒ k AwsDateTime → [UTCTime] → Condition
    DateGreaterThanEquals ∷ ConditionKey k ⇒ k AwsDateTime → [UTCTime] → Condition

    Boolean ∷ ConditionKey k ⇒ k AwsBoolean → Bool → Condition

    IpAddress ∷ ConditionKey k ⇒ k AwsIPAddress → [IPAddress] → Condition
    NotIpAddress ∷ ConditionKey k ⇒ k AwsIPAddress → [IPAddress] → Condition

    ArnEquals ∷ ConditionKey k ⇒ k AwsArn → [Arn] → Condition
    ArnNotEquals ∷ ConditionKey k ⇒ k AwsArn → [Arn] → Condition
    ArnLike ∷ ConditionKey k ⇒ k AwsArn → [Arn] → Condition
    ArnNotLike ∷ ConditionKey k ⇒ k AwsArn → [Arn] → Condition

    Null ∷ SomeConditionKey → Bool → Condition
    -- ^ Check for existence ('True') / non-existence ('False') of a key

deriving instance Show Condition
deriving instance Typeable Condition

instance Ord Condition where

    -- String
    compare (StringEquals a0 b0) (StringEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare StringEquals{} _ = GT
    compare _ StringEquals{} = LT

    compare (StringNotEquals a0 b0) (StringNotEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare StringNotEquals{} _ = GT
    compare _ StringNotEquals{} = LT

    compare (StringEqualsI a0 b0) (StringEqualsI a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare StringEqualsI{} _ = GT
    compare _ StringEqualsI{} = LT

    compare (StringNotEqualsI a0 b0) (StringNotEqualsI a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare StringNotEqualsI{} _ = GT
    compare _ StringNotEqualsI{} = LT

    compare (StringLike a0 b0) (StringLike a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare StringLike{} _ = GT
    compare _ StringLike{} = LT

    compare (StringNotLike a0 b0) (StringNotLike a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare StringNotLike{} _ = GT
    compare _ StringNotLike{} = LT

    -- Numeric
    compare (NumericEquals a0 b0) (NumericEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NumericEquals{} _ = GT
    compare _ NumericEquals{} = LT

    compare (NumericNotEquals a0 b0) (NumericNotEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NumericNotEquals{} _ = GT
    compare _ NumericNotEquals{} = LT

    compare (NumericLessThan a0 b0) (NumericLessThan a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NumericLessThan{} _ = GT
    compare _ NumericLessThan{} = LT

    compare (NumericLessThanEquals a0 b0) (NumericLessThanEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NumericLessThanEquals{} _ = GT
    compare _ NumericLessThanEquals{} = LT

    compare (NumericGreaterThan a0 b0) (NumericGreaterThan a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NumericGreaterThan{} _ = GT
    compare _ NumericGreaterThan{} = LT

    compare (NumericGreaterThanEquals a0 b0) (NumericGreaterThanEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NumericGreaterThanEquals{} _ = GT
    compare _ NumericGreaterThanEquals{} = LT

    -- Date
    compare (DateEquals a0 b0) (DateEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare DateEquals{} _ = GT
    compare _ DateEquals{} = LT

    compare (DateNotEquals a0 b0) (DateNotEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare DateNotEquals{} _ = GT
    compare _ DateNotEquals{} = LT

    compare (DateLessThan a0 b0) (DateLessThan a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare DateLessThan{} _ = GT
    compare _ DateLessThan{} = LT

    compare (DateLessThanEquals a0 b0) (DateLessThanEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare DateLessThanEquals{} _ = GT
    compare _ DateLessThanEquals{} = LT

    compare (DateGreaterThan a0 b0) (DateGreaterThan a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare DateGreaterThan{} _ = GT
    compare _ DateGreaterThan{} = LT

    compare (DateGreaterThanEquals a0 b0) (DateGreaterThanEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare DateGreaterThanEquals{} _ = GT
    compare _ DateGreaterThanEquals{} = LT

    -- Boolean
    compare (Boolean a0 b0) (Boolean a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare Boolean{} _ = GT
    compare _ Boolean{} = LT

    -- IpAddress
    compare (IpAddress a0 b0) (IpAddress a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare IpAddress{} _ = GT
    compare _ IpAddress{} = LT

    compare (NotIpAddress a0 b0) (NotIpAddress a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare NotIpAddress{} _ = GT
    compare _ NotIpAddress{} = LT

    -- Arn
    compare (ArnEquals a0 b0) (ArnEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare ArnEquals{} _ = GT
    compare _ ArnEquals{} = LT

    compare (ArnNotEquals a0 b0) (ArnNotEquals a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare ArnNotEquals{} _ = GT
    compare _ ArnNotEquals{} = LT

    compare (ArnLike a0 b0) (ArnLike a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare ArnLike{} _ = GT
    compare _ ArnLike{} = LT

    compare (ArnNotLike a0 b0) (ArnNotLike a1 b1) = case cast a0 of
        Just a0_ → compare (a0_, b0) (a1, b1)
        Nothing → compare (typeRep a0, b0) (typeRep a1, b1)
    compare ArnNotLike{} _ = GT
    compare _ ArnNotLike{} = LT

    -- Null
    compare (Null a0 b0) (Null a1 b1) = compare (a0,b0) (a1,b1)

instance Eq Condition where
    a == b = compare a b == EQ

instance ToJSON Condition where
    toJSON = \case
        (StringEquals k v) → f "StringEquals" k v
        (StringNotEquals k v) → f "StringNotEquals" k v
        (StringEqualsI k v) → f "StringEqualsI" k v
        (StringNotEqualsI k v) → f "StringNotEqualsI" k v
        (StringLike k v) → f "StringLike" k v
        (StringNotLike k v) → f "StringNotLike" k v

        (NumericEquals k v) → f "NumericEquals" k v
        (NumericNotEquals k v) → f "NumericNotEquals" k v
        (NumericLessThan k v) → f "NumericLessThan" k v
        (NumericLessThanEquals k v) → f "NumericLessThanEquals" k v
        (NumericGreaterThan k v) → f "NumericGreaterThan" k v
        (NumericGreaterThanEquals k v) → f "NumericGreaterThanEquals" k v

        (DateEquals k v) → f "DateEquals" k $ map formatW3CIso8601Date v
        (DateNotEquals k v) → f "DateNotEquals" k $ map formatW3CIso8601Date v
        (DateLessThan k v) → f "DateLessThan" k $ map formatW3CIso8601Date v
        (DateLessThanEquals k v) → f "DateLessThanEquals" k $ map formatW3CIso8601Date v
        (DateGreaterThan k v) → f "DateGreaterThan" k $ map formatW3CIso8601Date v
        (DateGreaterThanEquals k v) → f "DateGreaterThanEquals" k $ map formatW3CIso8601Date v

        (Boolean k v) → f "Bool" k v

        (IpAddress k v) → f "IpAddress" k v
        (NotIpAddress k v) → f "NotIpAddress" k v

        (ArnEquals k v) → f "ArnEquals" k v
        (ArnNotEquals k v) → f "ArnNotEquals" k v
        (ArnLike k v) → f "ArnLike" k v
        (ArnNotLike k v) → f "ArnNotLike" k v

        (Null k v) → f "Null" k v

      where
        f ∷ (ToJSON v, Show k) ⇒ T.Text → k → v → Value
        f n k v = object [ n .= object [ (T.pack . show) k .= toJSON v ] ]

-- -------------------------------------------------------------------------- --
-- ServiceCondition newtype wrapper for JSON parsing

newtype ServiceCondition (s ∷ AwsConditionType → *) = ServiceCondition Condition
    deriving (Show, Eq, Ord, Typeable)

instance (ConditionKey s) ⇒ FromJSON (ServiceCondition s) where

    -- Check that the condition block contains only a single condition
    parseJSON = withObject "Condition" $ \o → case HM.toList o of
        [] → fail "failed to parse condition value: the condition object is empty"
        t@(_:_:_) → fail
            $ "failed to parse condition value: the condition object must contain exactly one condition."
            <> " The given object contains the following conditions: " <> show (map fst t)
        [(k,v)] → withObject "ConditionBody" (g k . map (first String) . HM.toList) v

      where
        pk
            ∷ ∀ b a . (FromJSON b, Typeable a, FromJSON (s a), ConditionKey s)
            ⇒ (∀ k . (ConditionKey k) ⇒ k a → b → Condition)
            → Value
            → Parser b
            → Parser (ServiceCondition s)
        pk c k v = fmap ServiceCondition $
                c <$> (parseJSON k ∷ Parser (AwsConditionKey a)) <*> v
            <|> c <$> (parseJSON k ∷ Parser (s a)) <*> v

        nullpk
            ∷ Value
            → Parser Bool
            → Parser (ServiceCondition s)
        nullpk k v = fmap ServiceCondition $
                Null <$> fmap toConditionKey (parseJSON k ∷ Parser (AwsConditionKey AwsString)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (s AwsString)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (AwsConditionKey AwsNumeric)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (s AwsNumeric)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (AwsConditionKey AwsDateTime)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (s AwsDateTime)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (AwsConditionKey AwsBoolean)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (s AwsBoolean)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (AwsConditionKey AwsIPAddress)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (s AwsIPAddress)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (AwsConditionKey AwsArn)) <*> v
            <|> Null <$> fmap toConditionKey (parseJSON k ∷ Parser (s AwsArn)) <*> v


        -- Make sure that the condition has only a single key
        g _ [] = fail "failed to parse condition value: the condition object is empty"
        g _ t@(_:_:_) = fail
            $ "failed to parse condition value: the condition object must contain exactly one key."
            <> " The given object contains the following keys: " <> show (map fst t)

        -- String conditions
        g "StringEquals" [(k,v)] = pk StringEquals k (parseJSON v)
        g "StringNotEquals" [(k,v)] = pk StringNotEquals k (parseJSON v)
        g "StringEqualsI" [(k,v)] = pk StringEqualsI k (parseJSON v)
        g "StringNotEqualsI" [(k,v)] = pk StringNotEqualsI k (parseJSON v)
        g "StringLike" [(k,v)] = pk StringLike k (parseJSON v)
        g "StringNotLike" [(k,v)] = pk StringNotLike k (parseJSON v)

        -- Numeric conditions
        g "NumericEquals" [(k,v)] = pk NumericEquals k (parseJSON v)
        g "NumericNotEquals" [(k,v)] = pk NumericNotEquals k (parseJSON v)
        g "NumericLessThan" [(k,v)] = pk NumericLessThan k (parseJSON v)
        g "NumericLessThanEquals" [(k,v)] = pk NumericLessThanEquals k (parseJSON v)
        g "NumericGreaterThan" [(k,v)] = pk NumericGreaterThan k (parseJSON v)
        g "NumericGreaterThanEquals" [(k,v)] = pk NumericGreaterThanEquals k (parseJSON v)

        -- date conditions
        g "DateEquals" [(k,v)] = pk DateEquals k (parseDates v)
        g "DateNotEquals" [(k,v)] = pk DateNotEquals k (parseDates v)
        g "DateLessThan" [(k,v)] = pk DateLessThan k (parseDates v)
        g "DateLessThanEquals" [(k,v)] = pk DateLessThanEquals k (parseDates v)
        g "DateGreaterThan" [(k,v)] = pk DateGreaterThan k (parseDates v)
        g "DateGreaterThanEquals" [(k,v)] = pk DateGreaterThanEquals k (parseDates v)

        -- boolean conditions
        g "Bool" [(k,v)] = pk Boolean k (parseJSON v)

        -- IP address conditions
        g "IpAddress" [(k,v)] = pk IpAddress k (parseJSON v)
        g "NotIpAddress" [(k,v)] = pk NotIpAddress k (parseJSON v)

        -- Arn conditions
        g "ArnEquals" [(k,v)] = pk ArnEquals k (parseJSON v)
        g "ArnNotEquals" [(k,v)] = pk ArnNotEquals k (parseJSON v)
        g "ArnLike" [(k,v)] = pk ArnLike k (parseJSON v)
        g "ArnNotLike" [(k,v)] = pk ArnNotLike k (parseJSON v)

        -- Key existence condition
        g "Null" [(k,v)] = nullpk k (parseJSON v)

        g s [_] = fail $ "fialed to parse condition value: unknown condition type: " <> T.unpack s

        parseDate ∷ Value → Parser UTCTime
        parseDate = withText "W3C Iso8601 Date" $ either (fail . T.unpack) return . parseW3CIso8601Date

        parseDates ∷ Value → Parser [UTCTime]
        parseDates = withArray "List Dates" $ mapM parseDate . V.toList

-- -------------------------------------------------------------------------- --
-- Dates
--
-- W3C implementation of ISO 8601 date formats (<http://www.w3.org/TR/NOTE-datetime>).
--
-- @
--   Year:
--      YYYY (eg 1997)
--   Year and month:
--      YYYY-MM (eg 1997-07)
--   Complete date:
--      YYYY-MM-DD (eg 1997-07-16)
--   Complete date plus hours and minutes:
--      YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
--   Complete date plus hours, minutes and seconds:
--      YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
--   Complete date plus hours, minutes, seconds and a decimal fraction of a second
--      YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
-- @
--
--where:
--
-- @
--     YYYY = four-digit year
--     MM   = two-digit month (01=January, etc.)
--     DD   = two-digit day of month (01 through 31)
--     hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
--     mm   = two digits of minute (00 through 59)
--     ss   = two digits of second (00 through 59)
--     s    = one or more digits representing a decimal fraction of a second
--     TZD  = time zone designator (Z or +hh:mm or -hh:mm)
-- @
--
parseW3CIso8601Date ∷ T.Text → Either T.Text UTCTime
parseW3CIso8601Date t = maybe (Left $ "failed to parse date string " <> t) Right $
        p "%0Y"
    <|> p "%0Y-%m"
    <|> p "%0Y-%m-%d"
    <|> p "%0Y-%m-%dT%H:%MZ"
    <|> p "%0Y-%m-%dT%H:%M%z"
    <|> p "%0Y-%m-%dT%H:%M:%SZ"
    <|> p "%0Y-%m-%dT%H:%M:%S%z"
    <|> p "%0Y-%m-%dT%H:%M:%S:%QZ"
    <|> p "%0Y-%m-%dT%H:%M:%S:%Q%z"
  where
    p format = parseTime defaultTimeLocale format $ T.unpack t

formatW3CIso8601Date ∷ UTCTime → T.Text
formatW3CIso8601Date = T.pack . formatTime defaultTimeLocale "%0y-%m-%dT%H:%M:%S:%Q%Z"

