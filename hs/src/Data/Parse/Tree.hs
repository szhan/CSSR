{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Parse.Tree where

import Data.List
import Control.Exception (assert)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Lens.Micro.Internal

import CSSR.Prelude
import CSSR.TypeAliases

data ParseTree = ParseTree
  { depth :: Int
  , root :: PLeaf
  } deriving (Show, Eq)

data PLeaf = PLeaf
  { _body :: PLeafBody
  , _children :: Children
  } deriving (Eq)

instance Show PLeaf where
  show (PLeaf b cs) = "PLeaf {" ++ show b ++ ", children: [" ++ childs ++  "]}"
    where
      childs :: String
      childs = intercalate "\r" ((("\n\t, "++) . show) <$> (HM.toList cs))

data PLeafBody = PLeafBody
  { _obs       :: Vector Event
  , _count     :: Integer
  , _locations :: Locations
  } deriving (Eq)

instance Show PLeafBody where
  show (PLeafBody o c ls) =
    "obs: " ++ show o ++ ", count: " ++ show c ++ ", ls: " ++ show c

type instance Index PLeaf = Vector Event
type instance IxValue PLeaf = PLeaf

-- set (ix (V.fromList "9") . body . count)  50000 mkRoot
--
instance Ixed PLeaf where
  ix :: Vector Event -> Traversal' PLeaf (IxValue PLeaf)
  ix histories = go 0
    where
      go dpth f p@(PLeaf body childs)
        | V.length histories == dpth = f p
        | otherwise =
          case HM.lookup c childs of
            Nothing -> pure p
            Just child -> goAgain <$> go (dpth+1) f child
        where
          c :: Event
          c = histories V.! dpth

          goAgain :: PLeaf -> PLeaf
          goAgain child' = PLeaf body (HM.insert c child' childs)

navigate :: ParseTree -> Vector Event -> Maybe PLeaf
navigate tree history = (root tree) ^? ix history

-- mkRoot & over (path (fromList "abc") . count) (+1)
--
path :: forall f. Applicative f
             => Vector Event
             -> (PLeafBody -> f PLeafBody)
             -> PLeaf
             -> f PLeaf
path events fn = go 0
  where
    go :: Int -> PLeaf -> f PLeaf
    go dpth (PLeaf body childs) =
      assert (V.take dpth events ==  _obs body) $
        if dpth == V.length events - 1
        then PLeaf <$> fn body <*> pure childs
        else PLeaf <$> fn body <*> nextChilds

      where
        nextChilds :: f (HashMap Event PLeaf)
        nextChilds =
          case HM.lookup c childs of
            Just child -> HM.insert c <$> go (dpth + 1) child <*> pure childs
            Nothing -> HM.insert c <$> buildNew dpth <*> pure childs
          where
            c :: Event
            c = V.unsafeIndex events dpth


        buildNew :: Int -> f PLeaf
        buildNew d
          | d == V.length events - 1 = PLeaf <$> mkBod events <*> pure mempty
          | otherwise = PLeaf <$> mkBod es <*> childs_
          where
            c :: Event
            c = V.unsafeIndex events (d + 1)

            es :: Vector Event
            es = V.take (d + 1) events

            mkBod :: Vector Event -> f PLeafBody
            mkBod es' = fn (PLeafBody es' 0 mempty)

            childs_ :: f (HashMap Char PLeaf)
            childs_ = HM.singleton c <$> buildNew (d + 1)


type Children = HashMap Event PLeaf
type Parent = Maybe PLeaf

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
-- study all of the monoids
-- do in-depth dives for each of the lens modules

-- type Alphabet = [Event]
--
makeLenses ''PLeafBody
makeLenses ''PLeaf

current :: Vector Event -> Event
current = V.last

prior :: Vector Event -> Vector Event
prior = V.init

mkRoot :: PLeaf
mkRoot = PLeaf (PLeafBody [] 0 mempty) mempty

mkLeaf :: Vector Event -> PLeaf
mkLeaf o = PLeaf (PLeafBody o 0 mempty) mempty

-- FIXME: use pipes instead of loading the entire file into memory
buildTree :: Int -> DataFileContents -> ParseTree
buildTree n' (V.filter isValid -> cs) = ParseTree n' rt
  where
    n :: Int
    n = n' + 1

    rt :: PLeaf
    rt = V.ifoldr ireducer mkRoot cs

    ireducer :: Int -> Event -> PLeaf -> PLeaf
    ireducer i _ tree = tree & over (path (sliceEvents i) . count) (+1)

    sliceEvents :: Int -> Vector Event
    sliceEvents i
      | i + n < length cs = V.slice i                 n  cs
      | otherwise            = V.slice i (length cs - i) cs

isValid :: Event -> Bool
isValid e = not $ HS.member e ['\r', '\n']


