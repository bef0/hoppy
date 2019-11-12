-- This file is part of Hoppy.
--
-- Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

-- | General routines.
--
-- Unlike "Foreign.Hoppy.Generator.Util", these are private to the package.
module Foreign.Hoppy.Generator.Common (
  filterMaybe,
  fromMaybeM,
  fromEitherM,
  maybeFail,
  whileJust_,
  for,
  butLast,
  listSubst,
  listSubst',
  doubleQuote,
  strInterpolate,
  zipWithM,
  -- * String utilities
  capitalize,
  lowerFirst,
  upperFirst,
  pluralize,
  -- * File utilities
  writeFileIfDifferent,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Char (toLower, toUpper)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

-- | @filterMaybe bad@ converts a @Just bad@ into a @Nothing@, returning all
-- other @Maybe@ values as is.
filterMaybe :: Eq a => a -> Maybe a -> Maybe a
filterMaybe bad (Just value) | value == bad = Nothing
filterMaybe _ mayb = mayb

-- | @fromMaybeM m x = maybe m return x@
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

-- | @fromEitherM f x = either f return x@
fromEitherM :: Monad m => (e -> m a) -> Either e a -> m a
fromEitherM = flip either return

-- | @maybeFail s x = maybe (fail s) x@
maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . error

-- | @whileJust_ gen act@ runs @act@ on values generated from @gen@ until @gen@
-- returns a @Nothing@.
whileJust_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ gen act = gen >>= \case
  Just x -> act x >> whileJust_ gen act
  Nothing -> return ()

-- | @for = flip map@
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Drops the last item from the list, if non-empty.
butLast :: [a] -> [a]
butLast [] = []
butLast xs = take (length xs - 1) xs

-- | @listSubst a b xs@ replaces all @x@ in @xs@ such that @x == a@ with @b@.
listSubst :: Eq a => a -> a -> [a] -> [a]
listSubst x x' = map $ \y -> if y == x then x' else y

-- | @listSubst' a bs xs@ replaces all @x@ in @xs@ such that @x == a@ with the
-- sequence of items @bs@.
listSubst' :: Eq a => a -> [a] -> [a] -> [a]
listSubst' x xs' = concatMap $ \y -> if y == x then xs' else [y]

-- | Renders a double-quoted string, enclosing the given characters in double
-- quotes and escaping double quotes and backslashes with backslashes.
doubleQuote :: String -> String
doubleQuote str = '"' : escape str ++ "\""
  where escape s =
          if any (\c -> c == '"' || c == '\\') s
          then listSubst' '"' "\\\"" $
               listSubst' '\\' "\\\\" s
          else s

-- | Takes a map of strings and a target string, and replaces references to keys
-- enclosed in braces in the target string with their values.  Returns a @Right@
-- with the replaced string on success, and when an unknown key is encountered
-- then a @Left@ with the unknown key.
strInterpolate :: Map String String -> String -> Either String String
strInterpolate values str = case L.findIndex ('{' ==) str of
  Nothing -> Right str
  Just openBraceIndex ->
    let (prefix, termAndSuffix) = splitAt openBraceIndex str
    in case L.findIndex ('}' ==) termAndSuffix of
         Nothing -> Right str
         Just closeBraceIndex ->
           let termLength = closeBraceIndex - 1
               term = take termLength $ tail termAndSuffix
               suffix = drop (closeBraceIndex + 1) termAndSuffix
           in case M.lookup term values of
                Nothing -> Left term
                Just value -> strInterpolate values $ prefix ++ value ++ suffix

-- | Zips two lists using a monadic function.
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ zipWith f xs ys

-- | Upper cases the first character of a string, and lower cases the rest of
-- it.  Does nothing to an empty string.
capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : map toLower cs

-- | Lower cases the first character of a string, if nonempty.
lowerFirst :: String -> String
lowerFirst "" = ""
lowerFirst (c:cs) = toLower c : cs

-- | Upper cases the first character of a string, if nonempty.
upperFirst :: String -> String
upperFirst "" = ""
upperFirst (c:cs) = toUpper c : cs

-- | Adds a noun onto a number in either singular or plural form, depending on
-- the number.
pluralize :: Int -> String -> String -> String
pluralize num singular plural =
  if num == 1
  then "1 " ++ singular
  else show num ++ " " ++ plural

-- | If the file specified does not exist or its contents does not match the
-- given string, then this writes the string to the file.
writeFileIfDifferent :: FilePath -> String -> IO ()
writeFileIfDifferent path newContents = do
  exists <- doesFileExist path
  -- We need to read the file strictly, otherwise lazy IO might try to write the
  -- file while it's still open and locked for reading.
  doWrite <- if exists
             then (newContents /=) <$> readStrictly
             else return True
  when doWrite $ writeFile path newContents
  where readStrictly = withFile path ReadMode $ \handle -> do
            contents <- hGetContents handle
            _ <- evaluate $ length contents
            return contents
