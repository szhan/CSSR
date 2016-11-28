module Data.Parse.MTreeSpec where

import Data.Parse.MTree
import Data.Parse.Tree
import qualified Data.Vector as V

import CSSR.Prelude.Test

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "addPath"   addPathSpec
  describe "buildTree" buildTreeSpec


addPathSpec :: Spec
addPathSpec =
  describe "when we encounter the history 110" $ do
    it "keeps the root node" $ view (body . obs) rt == V.empty
    it "bumps the root node count" $ view (body . count) rt == 1

    childCheck "root" rt '0' (V.fromList "0") 1

    let _0 = findLeaf rt '0'
    childCheck "'0'" _0 '1' (V.fromList "10") 1

    let _10 = findLeaf _0 '1'
    childCheck "'10'" _10 '1' (V.fromList "110") 1

  where
    rt :: PLeaf
    rt = runST $ freeze =<< do
      rt' <- mkMRoot
      addPath (V.fromList "110") rt'
      return rt'

    childCheck :: String -> PLeaf -> Event -> Vector Event -> Integer -> Spec
    childCheck name lf nxt full c =
      describe ("the "++ name ++ " node") $ do
        let child = view children lf ^? ix nxt
        let full' = V.toList full
        it ("has a "++ show nxt ++" child") $ isJust child
        it ("has a "++ show nxt ++" child with a count of " ++ show c) $
          maybe False ((== c) . view (body . count)) child
        it (show nxt ++" child has observation "++ show full') $
          maybe False ((== full) . view (body . obs)) child

    findLeaf :: PLeaf -> Event -> PLeaf
    findLeaf lf o = fromMaybe emptyLeaf (view children lf ^? ix o)
      where
        emptyLeaf :: PLeaf
        emptyLeaf = PLeaf (PLeafBody mempty 0 mempty) mempty


buildTreeSpec :: Spec
buildTreeSpec =
    describe "when we build the tree \"abcc\" with depth 2" $ do
      let tree = buildTree 2 (V.fromList "abcc")
      it "makes   c the child of the root node" $ True
      it "makes  bc the child of  c" $ True
      it "makes abc the child of bc" $ True
      it "makes  cc the child of  c" $ True
      it "makes bcc the child of cc" $ True


