{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Tests.hs
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

module Network.Wai.Middleware.Preprocessor.Tests where

import           Distribution.TestSuite
import           Network.Wai.Middleware.Preprocessor
import           Test.QuickCheck

tests :: IO [Test]
tests = return []
