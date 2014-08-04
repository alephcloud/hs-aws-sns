{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Aws.Sns.Internal
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
module Aws.Sns.Internal
( parseXmlEntryMap
, forceE
, fmapL
, expectValue
) where

import Control.Applicative
import Control.Exception

import Control.Monad
import Data.Monoid
import qualified Data.Text as T

import Text.XML.Cursor ((&/), ($/), (&|))
import qualified Text.XML.Cursor as CU

parseXmlEntryMap :: CU.Cursor -> [(T.Text, T.Text)]
parseXmlEntryMap cur = concat $ cur
    $/ CU.laxElement "entry"
    &| \c -> (,)
        <$> (c $/ CU.laxElement "key" &/ CU.content)
        <*> (c $/ CU.laxElement "value" &/ CU.content)

forceE :: (Exception e) => e -> [a] -> Either e a
forceE e [] = Left e
forceE _ (x:_) = Right x

fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f (Left e) = Left (f e)
fmapL _ (Right a) = Right a

expectValue :: (Monad m, Eq a, Show a) => a -> a -> m ()
expectValue expected got = when (got /= expected) . fail
    $ "unexpected value; got " <> show got <> "; expected " <> show expected

