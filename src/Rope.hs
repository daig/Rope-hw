--------------------------------------------------------------------
-- |
-- Module    :  Rope
-- Copyright :  (c) <David Girardo> 2017
-- License   :  MIT
-- Maintainer:  <david.girardo@mit.edu>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language MagicHash #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
module Rope where

import qualified Data.FingerTree as F
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (length,splitAt)


-- | A @Rope@ is like a @Seq Char@ from @Data.Container@ but optimized with packed bytestring chunks.
--   It can be considered as an alternative to @ByteString@ that supports efficient (O(log(min(m,n)))) merging as well as splitting (Unlike bytestring @Builder@s). Ropes are persistent, and so trivially support efficient arbitrary undos (unlike a mutable buffer).
type Rope = F.FingerTree Length Chunk

-- | The cumulative length measure of the rope so far.
newtype Length = Length {getLength :: Int} deriving (Eq,Ord,Show,Num)

-- | A bytestring chunk strung together in the rope, measured by Length. Newtyped to avoid orphan instances
newtype Chunk = Chunk {getChunk :: ByteString} deriving (Eq,Show)
-- | Lengths are additive
instance Monoid Length where
  mempty = 0
  mappend = (+)

-- | A Chunk is measured by its ByteString Length
instance F.Measured Length Chunk where measure = Length . BS.length . getChunk


-- | Deconstruct a rightmost ByteString from a Rope
viewr :: Rope -> (Rope,ByteString)
viewr r = case F.viewr r of
  F.EmptyR -> (r,"")
  r' F.:> Chunk bs -> (r',bs)
-- | Deconstruct a leftmost ByteString from a Rope
viewl :: Rope -> (ByteString,Rope)
viewl r = case F.viewl r of
  F.EmptyL -> ("",r)
  Chunk bs F.:< r' -> (bs,r')
-- | Append a @ByteString@ to the end of a @Rope@ or pattern match on the last chunk.
pattern (:>) :: Rope -> ByteString -> Rope
pattern r :> bs <- (viewr -> (r,bs)) where
  r :> bs = if BS.null bs then r else r F.|> Chunk bs
-- | Prepend a @ByteString@ to the beginning of a @Rope@ or pattern match on the first chunk.
pattern (:<) :: ByteString -> Rope -> Rope
pattern bs :< r <- (viewl -> (bs,r)) where
  bs :< r = if BS.null bs then r else Chunk bs F.<| r

-- | The total size of a @Rope@
length :: Rope -> Int
length = getLength . F.measure

-- | Split a @Rope@ at a given @Char@ index, splitting along an inner bytestring if needed.
splitAt :: Int -> Rope -> (Rope,Rope)
splitAt n r | n <= 0        = (mempty  , r)
            | n >= length r = (r       , mempty)
            | otherwise     = (leftRope :> leftBS , rightBS :< rightRope)
  where
    (leftRope,centerBS :< rightRope) = F.split (> Length n) r
    (leftBS,rightBS) = BS.splitAt (n - length leftRope) centerBS

-- | @dropTail n@ drops @n@ characters from the end of a @Rope@
dropTail :: Int -> Rope -> Rope
dropTail n rs = fst $ splitAt (length rs - n) rs

-- | Get the character at given index. Unsafe, as bounds are not checked
charAt# :: Int -> Rope -> Char
charAt# n r = BS.index centerBS n'
 -- NOTE: n' is always a valid index for centerBS because of the contract provided by F.split
  where
    (leftRope,centerBS :< _) = F.split (> Length n) r
    n' = n - length leftRope
-- | Get the character at given index if it is within bounds
charAt :: Int -> Rope -> Maybe Char
charAt n r | n >= 0 && n < length r = Just $ charAt# n r
           | otherwise = Nothing

-- | Build up a Rope from a list of ByteStrings
fromList :: [ByteString] -> Rope
fromList bs = foldr (:<) mempty bs
-- | Flatten a Rope into a list of ByteStrings
toList :: Rope -> [ByteString]
toList = foldr (\a -> (getChunk a:)) []
toBS :: Rope -> ByteString
-- | Flatten a Rope into a single ByteString
toBS = BS.concat . toList
