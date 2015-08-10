-- Preprocessor.hs
-- Copyright 2015 Remy E. Goldschmidt <taktoa@gmail.com>
-- This file is part of wai-middleware-preprocessor.
--    wai-middleware-preprocessor is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    wai-middleware-preprocessor is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY-- without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with wai-middleware-preprocessor. If not, see <http://www.gnu.org/licenses/>.

-- | Serve files subject to a preprocessing function. This library makes it
--   easy to integrate Javascript or CSS preprocessors into a WAI application
--   that will compile the relevant files at runtime
module Network.Wai.Middleware.Preprocessor
    ( -- * Middleware
      ppMiddleware, ppFileMiddleware
    , -- * Preprocessors
      Preprocessor, preprocessor
    , -- * Policies
      Policy, (<|>), (>->), policy, predicate
    , addBase, addSlash, contains, hasPrefix, hasSuffix, noDots, isNotAbsolute, only
    , -- * Utilities
      tryPolicy
    , -- * Miscellaneous
      maybeRead, replaceExt, extensions
    ) where

import           Control.Monad                 (when)
import           Control.Monad.Trans           (liftIO)
import           Data.Functor                  ((<$>))
import           Data.List.Split               (splitOn)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Network.Wai
import           Network.Wai.Middleware.Static
import           System.Directory              (doesFileExist)
-- | Newtype wrapper for 'String's that represent file extensions
newtype Extension = Extension { fromExt :: String -- ^ Extract extension
                              }

-- | Preprocessors are comprised of an input file extension ('String'),
-- an output file extension ('String'), and a transforming function
-- ('Text' -> 'Maybe' 'Text')
newtype Preprocessor = PP { runPP :: (Extension, Extension, Text -> Maybe Text)
                            -- ^ Extract a preprocessor
                          }

-- | Constructor for a preprocessor.
preprocessor :: String               -- ^ input extension
             -> String               -- ^ output extension
             -> (Text -> Maybe Text) -- ^ transforming function
             -> Preprocessor         -- ^ preprocessor
preprocessor ei eo f = PP (Extension ei, Extension eo, f)

-- | Run the preprocessor on a given file
runPreprocessor :: Preprocessor -> FilePath -> IO ()
runPreprocessor pp fp = do
  let (iext, oext, prepro) = runPP pp
  when (fromExt oext == last (extensions fp))
       (preprocess (replaceExt iext fp) fp prepro)

-- | Read in the input file, run through the preprocessor, and write it out
preprocess :: FilePath -> FilePath -> (Text -> Maybe Text) -> IO ()
preprocess ifp ofp pp = do
  input <- maybeRead ifp
  case input >>= pp of
   Just o  -> TIO.writeFile ofp o
   Nothing -> return ()

-- | Lazily read in a file as 'Text' if the file exists
maybeRead :: FilePath -> IO (Maybe Text)
maybeRead fp = do
  exists <- doesFileExist fp
  if exists then Just <$> TIO.readFile fp else return Nothing

-- | Replace the file extension of a 'FilePath' with a given extension
replaceExt :: Extension -> FilePath -> FilePath
replaceExt ext = replaceExt' "." (fromExt ext)

-- | Polymorphic version of replaceExt
replaceExt' :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceExt' sep ext fp = concatMap (++ sep) (init (extensions' sep fp)) ++ ext


-- | Run a preprocessor on incoming requests
--
-- For example, if your preprocessor has input extension ".fay" and output
-- extension ".js" (and just does the identity function)
--
-- > ppMiddleware (preprocessor ".fay" ".js" (Just))
--
-- and a request to "index.js" goes through the generated middleware
--
-- > GET "/index.js"
--
-- If "index.js" exists, it does nothing
--
-- If "index.js" does not exist, it does the following:
--
-- If "index.fay" exists, it will read it in, run the function on its content,
-- and it will write the output to "index.js"
--
-- Otherwise, it will do nothing
--
-- Finally, in all cases, it will pass the request along
ppMiddleware :: Preprocessor -> Middleware
ppMiddleware pp app req callback = do
  let fp = T.unpack $ T.intercalate "/" $ pathInfo req
  exists <- liftIO $ doesFileExist fp
  if exists
    then app req callback
    else liftIO (runPreprocessor pp fp) >> app req callback

-- | Preprocessor composed with static file server
--
-- This function is just the composition of staticPolicy from wai-middleware-static
-- and ppMiddleware. The policy creation functions from that library are also
-- exported from this library for ease of use.
ppFileMiddleware :: Policy -> Preprocessor -> Middleware
ppFileMiddleware pol pp = ppMiddleware pp . staticPolicy pol

-- | Split a 'FilePath' into period-delimited sections
extensions :: FilePath -> [String]
extensions = extensions' "."

-- | Polymorphic version of extensions
extensions' :: (Eq a) => [a] -> [a] -> [[a]]
extensions' = splitOn
