{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Anagrams where

import Course.Core
import Course.Functor
import Course.List

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars ->
  FilePath ->
  IO (List Chars)
anagrams cs fp =
  (\s -> intersectBy equalIgnoringCase (lines s) (permutations cs)) <$> readFile fp

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars ->
  Chars ->
  Bool
equalIgnoringCase cs1 cs2 =
  and (uncurry (==) <$> zipWith (\c1 c2 -> (toLower c1, toLower c2)) cs1 cs2)
