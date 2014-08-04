{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Aws.Sns.Commands.ListTopics
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Returns a list of the requester's topics. Each call returns a limited list
-- of topics, up to 100. If there are more topics, a NextToken is also
-- returned. Use the NextToken parameter in a new ListTopics call to get
-- further results.
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_ListTopics.html>
--
module Aws.Sns.Commands.ListTopics
( ListTopicsNextToken
, ListTopics(..)
, ListTopicsResponse(..)
, ListTopicsErrors(..)
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
import Data.Typeable

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

listTopicsAction :: SnsAction
listTopicsAction = SnsActionListTopics

newtype ListTopicsNextToken = ListTopicsNextToken { listTopicsNextTokenText :: T.Text }
    deriving (Show, Read, Eq, Ord, Monoid, IsString)

data ListTopics = ListTopics
    { listTopicsNextToken :: !(Maybe ListTopicsNextToken)
    -- ^ Token returned by the previous 'ListTopics' request.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data ListTopicsResponse = ListTopicsResponse
    { listTopicsResponseNextToken :: !(Maybe ListTopicsNextToken)
    -- ^ Token to pass along to the next ListTopics request. This element is
    -- returned if there are additional topics to retrieve.
    --

    , listTopicsResponseTopics :: ![Arn]
    -- ^ A list of topic ARNs.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r ListTopicsResponse where
    type ResponseMetadata ListTopicsResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = ListTopicsResponse (nextToken el) <$> arns el
        arns el = do
            let t = el
                    $// CU.laxElement "ListTopicsResult"
                    &/ CU.laxElement "Topics"
                    &/ CU.laxElement "member"
                    &/ CU.laxElement "TopicArn"
                    &/ CU.content
            forM t $ \i -> case fromText i of
                Right a -> return a
                Left e -> throwM $ SnsResponseDecodeError
                    $ "failed to parse topic ARN (" <> i <> "): " <> (T.pack . show) e
        nextToken el = fmap ListTopicsNextToken . listToMaybe $ el
            $// CU.laxElement "ListTopicsResult"
            &/ CU.laxElement "NextToken"
            &/ CU.content

instance SignQuery ListTopics where
    type ServiceConfiguration ListTopics = SnsConfiguration
    signQuery ListTopics{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = listTopicsAction
        , snsQueryParameters = case listTopicsNextToken of
            Nothing -> []
            Just _ -> [("NextToken", listTopicsNextTokenText <$> listTopicsNextToken)]
        , snsQueryBody = Nothing
        }

instance Transaction ListTopics ListTopicsResponse

instance AsMemoryResponse ListTopicsResponse where
    type MemoryResponse ListTopicsResponse = ListTopicsResponse
    loadToMemory = return

instance ListResponse ListTopicsResponse Arn where
    listResponse (ListTopicsResponse _ arns) = arns

instance IteratedTransaction ListTopics ListTopicsResponse where
    nextIteratedRequest _ ListTopicsResponse{..} =
        ListTopics listTopicsResponseNextToken <$ listTopicsResponseNextToken

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data ListTopicsErrors
    = ListTopicsAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | ListTopicsInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | ListTopicsInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)


