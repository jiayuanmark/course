{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams word fn = let dict   = S.fromList . hlist . map NoCaseString . lines <$> readFile fn
                           cand   = map NoCaseString (permutations word)
                       in map ncString <$> filtering (\w -> S.member w <$> dict) cand

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
