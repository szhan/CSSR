module Data.Hist.TreeSpec where

import qualified Data.Vector as V

import CSSR.Prelude.Test
import Data.Hist.Tree
import qualified Data.Parse.MTree as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "navigate" $ do
    findsJust ""
    findsJust "c"
    findsNothing "a"
    findsNothing "b"
    findsJust "cc"
    findsNothing "ac"
    findsJust "bc"

  describe "convert" $ do
    it "removes the last children from a Parse Tree" $
      all (isNothing . navigate tree . V.fromList) ["abc", "bcc"]
    it "keeps the children of last depth" $
      all (isJust . navigate tree . V.fromList) ["bc", "cc"]

  where
    findsJust :: [Event] -> Spec
    findsJust path = do
      let node = findNode path
      it ("finds node " ++ show path) $ isJust node
      it ("node " ++ show path ++ "'s path matches") $
        maybe False ((V.fromList path ==) . view (body . obs)) node

    findsNothing :: [Event] -> Spec
    findsNothing path =
      it ("fails to find node " ++ show path) $
        isNothing . navigate tree . V.fromList $ path

    findNode :: [Event] -> Maybe HLeaf
    findNode path = navigate tree . V.fromList $ path

    tree :: HistTree
    tree = uncurry convert $ M.buildTree 2 (V.fromList "abcc")


