{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Parse.Tree where

import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Lens.Micro.Internal

import CSSR.Prelude

-------------------------------------------------------------------------------
-- Parse Tree ADTs
-------------------------------------------------------------------------------

data ParseTree = ParseTree
  { _depth :: Int
  , _root :: PLeaf
  } deriving (Eq)

instance Show ParseTree where
  show (ParseTree d r) = "ParseTree{depth: " ++ show d ++ "}\n  root:" ++ show r

data PLeaf = PLeaf
  { _body :: PLeafBody
  , _children :: Children
  } deriving (Eq)

instance Show PLeaf where
  show = go 1 " "
    where
      indent :: Int -> String
      indent d = replicate (5 * d) ' '

      showLeaf :: Int -> Event -> PLeafBody -> String
      showLeaf d e b = "\n" ++ indent d ++ show e ++"->PLeaf{" ++ show b

      go :: Int -> Event -> PLeaf -> String
      go d e (PLeaf b cs)
        | length cs == 0 = showLeaf d e b ++ ", no children}"
        | otherwise = showLeaf d e b ++ "}\n"
                      ++ indent (d + 1) ++ "children:"
                      ++ printChilds d cs

      printChilds :: Int -> Children -> String
      printChilds d = intercalate "\n" . map (uncurry (go (d+1))) . HM.toList

data PLeafBody = PLeafBody
  { _obs       :: Vector Event
  , _count     :: Integer
  , _locations :: Locations
  } deriving (Eq)

instance Show PLeafBody where
  show (PLeafBody o c _) =
    "obs: " ++ show o ++ ", count: " ++ show c ++ ", ls: " ++ "<>"

type Children = HashMap Event PLeaf
type Parent = Maybe PLeaf

makeLenses ''PLeafBody
makeLenses ''PLeaf
makeLenses ''ParseTree

-------------------------------------------------------------------------------
-- Lenses for Parse Trees
-------------------------------------------------------------------------------
type instance Index PLeaf = Vector Event
type instance IxValue PLeaf = PLeaf

-- Example: set (ix (V.fromList "9") . body . count)  50000 mkRoot
--
instance Ixed PLeaf where
  ix :: Vector Event -> Traversal' PLeaf (IxValue PLeaf)
  ix histories = go 0
    where
      go dpth f p@(PLeaf bod childs)
        | V.length histories == dpth = f p
        | otherwise =
          case HM.lookup c childs of
            Nothing -> pure p
            Just child -> goAgain <$> go (dpth+1) f child
        where
          c :: Event
          c = histories V.! dpth

          goAgain :: PLeaf -> PLeaf
          goAgain child' = PLeaf bod (HM.insert c child' childs)

navigate :: ParseTree -> Vector Event -> Maybe PLeaf
navigate tree history = view root tree ^? ix history

-------------------------------------------------------------------------------
-- Unused: Pure creation of a Parse Tree. This is commented out in case we want
--         to use this in future benchmarking, or for science.
-------------------------------------------------------------------------------
-- Use: mkRoot & over (path (fromList "abc") . count) (+1)
-- ---
-- path :: forall f. Applicative f
--              => Vector Event
--              -> (PLeafBody -> f PLeafBody)
--              -> PLeaf
--              -> f PLeaf
-- path events fn = go 0
--   where
--     go :: Int -> PLeaf -> f PLeaf
--     go dpth (PLeaf body childs) =
--       assert (V.take dpth events ==  _obs body) $
--         if dpth == V.length events - 1
--         then PLeaf <$> fn body <*> pure childs
--         else PLeaf <$> fn body <*> nextChilds
--
--       where
--         nextChilds :: f (HashMap Event PLeaf)
--         nextChilds =
--           case HM.lookup c childs of
--             Just child -> HM.insert c <$> go (dpth + 1) child <*> pure childs
--             Nothing -> HM.insert c <$> buildNew dpth <*> pure childs
--           where
--             c :: Event
--             c = V.unsafeIndex events dpth
--
--
--         buildNew :: Int -> f PLeaf
--         buildNew d
--           | d == V.length events - 1 = PLeaf <$> mkBod events <*> pure mempty
--           | otherwise = PLeaf <$> mkBod es <*> childs_
--           where
--             c :: Event
--             c = V.unsafeIndex events (d + 1)
--
--             es :: Vector Event
--             es = V.take (d + 1) events
--
--             mkBod :: Vector Event -> f PLeafBody
--             mkBod es' = fn (PLeafBody es' 0 mempty)
--
--             childs_ :: f (HashMap Char PLeaf)
--             childs_ = HM.singleton c <$> buildNew (d + 1)
--
-- FIXME: use pipes instead of loading the entire file into memory
-- buildTree :: Int -> DataFileContents -> ParseTree
-- buildTree n' (V.filter isValid -> cs) = ParseTree n' rt
--   where
--     n :: Int
--     n = n' + 1
--
--     rt :: PLeaf
--     rt = V.ifoldr ireducer mkRoot cs
--
--     ireducer :: Int -> Event -> PLeaf -> PLeaf
--     ireducer i _ tree = tree & over (path (sliceEvents i) . count) (+1)
--
--     sliceEvents :: Int -> Vector Event
--     sliceEvents i
--       | i + n < length cs = V.slice i                 n  cs
--       | otherwise            = V.slice i (length cs - i) cs
--
-- mkRoot :: PLeaf
-- mkRoot = PLeaf (PLeafBody [] 0 mempty) mempty
--
-- mkLeaf :: Vector Event -> PLeaf
-- mkLeaf o = PLeaf (PLeafBody o 0 mempty) mempty
--
--
-------------------------------------------------------------------------------
-- FIXME: Looping Trees
-------------------------------------------------------------------------------

-- data LoopingTree = LoopingTree
--   { root :: LLeaf
--   , terminals:: Set LLeaf
--   , edges :: Set LLeaf
--   } deriving (Show, Eq)
--
-- data LLeaf = LLeaf
--   { obs       :: Vector Event
--   , histories :: Set PLeaf
--   , children  :: HashMap Char LLeaf
--   , distribution :: Vector Float
--   , isTerminal :: Bool
--   , isEdge :: Bool
--   , isLoop :: Bool
--   }

-- make a note: steal mitchell's brain
-- study all of the monoids as a lens supplement
-- do in-depth dives for each of the lens modules


