{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Data.Parse.MTree where

import GHC.TypeLits
import Data.Proxy
import Control.Monad.ST
import Data.STRef
import Data.List (intercalate)
import Data.Function (on)
import Control.Exception (assert)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Hashable
import Lens.Micro.Internal
import Data.HashTable.Class


import CSSR.Prelude
import CSSR.TypeAliases
import Data.Parse.Tree

data MPLeaf s = MPLeaf
  { obs_      :: Vector Event
  , count_    :: (ST s Integer)
  , children_ :: ST s (STRef s (HashMap Event (MPLeaf s)))
  }

-- initBranch :: Vector Event -> MPLeaf s
-- initBranch evts = MPLeaf evts (newSTRef 0) (newSTRef mempty)

freeze :: MPLeaf s -> PLeaf
freeze (MPLeaf o c' childs') = PLeaf (PLeafBody o c mempty) childs
  where
    c :: Integer
    c = liftST $ do
      c_ <- readSTRef c'
      return c_

    childs :: HashMap Event PLeaf
    childs = undefined





--   where
--     depth :: Int
--     depth = V.length events - 1
--
--     c :: Event
--     c = V.unsafeIndex events (depth + 1)
--
--     es :: Vector Event
--     es = V.take (depth + 1) events
--
--     mkBod :: Vector Event -> f MPLeaf
--     mkBod es' = MPLeaf es' (newSTRef 0) (newSTRef mempty)
--
--     childs :: f (HashMap Char MPLeaf)
--     childs = HM.singleton c <$> buildNew (depth + 1)

-- instance Eq MPLeaf where
--   a == b = (a `bodyEq` b) && (a `childrenEq` b)
--     where
--       bodyEq = (==) `on` (runST . _body)
--       childrenEq = (==) `on` children_

---------------------------------------------------------------------------------
-- We encounter the history say "110"
-- We go to the parse tree at the root
-- We take the 0 child of the root
-- We then take the 1 child of 0 (=10)
-- We then take the 1 child of 10 (=110)
--------------------------------------------------------------------------------


-- mkRoot & over (path (fromList "abc") . count) (+1)
--
-- path :: Vector Event -> MPLeaf s -> MPLeaf s
-- path events = go 0
--   where
--     go :: Int -> MPLeaf s -> MPLeaf s
--     go depth (MPLeaf body childs) =
--       assert (V.take depth events ==  obs_ body) $
--         if depth == V.length events - 1
--            then (MPLeaf <$> modifySTRef (+1) <*>:w
--                 )
--         then MPLeaf <$> fn body <*> pure childs
--         else MPLeaf <$> fn body <*> nextChilds
--
--       where
--         nextChilds :: f (HashMap Event MPLeaf)
--         nextChilds =
--           case HM.lookup c childs of
--             Just child -> HM.insert c <$> go (depth + 1) child <*> pure childs
--             Nothing -> HM.insert c <$> buildNew depth <*> pure childs
--           where
--             c :: Event
--             c = V.unsafeIndex events depth

--
--
-- type Event = Char
-- type Children = HashMap Event MPLeaf
-- type Parent = Maybe MPLeaf
-- type DataFileContents = Vector Event
--
-- current :: Vector Event -> Event
-- current = V.last
--
-- prior :: Vector Event -> Vector Event
-- prior = V.init
--
-- mkRoot :: MPLeaf
-- mkRoot = MPLeaf (MPLeafBody [] 0 mempty) mempty
--
-- mkLeaf :: Vector Event -> MPLeaf
-- mkLeaf obs = MPLeaf (MPLeafBody obs 0 mempty) mempty
--
-- -- FIXME: use pipes instead of loading the entire file into memory
-- buildTree :: Int -> DataFileContents -> MParseTree
-- buildTree n' (V.filter isValid -> chars) = MParseTree n' root
--   where
--     n :: Int
--     n = n' + 1
--
--     root :: MPLeaf
--     root = V.ifoldr ireducer mkRoot chars
--
--     ireducer :: Int -> Event -> MPLeaf -> MPLeaf
--     ireducer i _ tree = tree & over (path (sliceEvents i) . count) (+1)
--
--     sliceEvents :: Int -> Vector Event
--     sliceEvents i
--       | i + n < length chars = V.slice i                 n  chars
--       | otherwise            = V.slice i (length chars - i) chars
--
-- isValid :: Event -> Bool
-- isValid e = not $ HS.member e ['\r', '\n']


