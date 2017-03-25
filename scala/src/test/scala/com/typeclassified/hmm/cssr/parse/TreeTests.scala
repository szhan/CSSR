package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.shared.ProbablisticAsserts
import com.typeclassified.hmm.cssr.trees.{ParseLeaf, ParseTree}
import org.scalatest.{WordSpec, BeforeAndAfter, Matchers}

import scala.collection.mutable.ListBuffer

class TreeTests extends WordSpec with Matchers with ProbablisticAsserts with LeafAsserts with BeforeAndAfter {
  var tree:ParseTree = null
  val abc = "abc"
  val abcabc = "abcabc"
  val abcbabcbb = "abcbabcbb"
  val testMap:Map[String, Array[List[List[String]]]] = Map(
    abc -> Array(
      asEvents("a", "b", "c"),
      asEvents("ab", "bc"),
      asEvents("abc")
    ),
    abcabc -> Array(
      asEvents("a", "b", "c"),
      asEvents("ab", "bc", "ca"),
      asEvents("abc", "bca", "cab")
    ),
    abcbabcbb -> Array(
      asEvents("a", "b", "c"),
      asEvents("ab", "bc", "cb", "ba", "bb"),
      asEvents("abc", "bcb", "cba", "bab", "cbb")
    )
  )

  before {
    AlphabetHolder.alphabet = Alphabet(abc.mkString(""), "")
    tree = ParseTree(AlphabetHolder.alphabet)
  }

  "loadData" should {

    "generating a single node for an single char array" in {
      var children:ListBuffer[ParseLeaf] = null
      var leaf:ParseLeaf = null
      tree = ParseTree.loadData(tree, "a", "", 4)

      children = tree.root.children
      children should have size 1

      leaf = children.head
      assertLeafProperties(leaf, asEvents("a"))

      children = children.head.children
      children should have size 0

      assertLeafProperties(leaf, asEvents("a"))
    }

    "generating a full tree, given a short string" in {
      tree = ParseTree.loadData(tree, abc, "", 4)
      var inspect:ListBuffer[ParseLeaf] = tree.root.children
      assertChildrenByExactBatch(inspect, testMap(abc)(0))

      inspect = inspect.flatMap(_.children)
      assertChildrenByExactBatch(inspect, testMap(abc)(1))

      inspect = inspect.flatMap(_.children)
      assertChildrenByExactBatch(inspect, testMap(abc)(2))

      inspect = inspect.flatMap(_.children)
      inspect should have size 0
    }

    "generating multiple root branches if they do not exist" in {
      pending
//      ParseTree.loadData(tree, "bc")
//      ParseTree.loadData(tree, "aa")

      val children = tree.root.children
      children should have size 2

      val leaf1 = children(0)
      assertLeafProperties(leaf1, asEvents("c"))
      leaf1.children should have size 1
      val leaf1Child = leaf1.children.head
      assertLeafProperties(leaf1Child, asEvents("bc"))


      val leaf2 = children(1)
      assertLeafProperties(leaf2, asEvents("a"))
      leaf2.children should have size 1
      val leaf2Child = leaf2.children.head
      assertLeafProperties(leaf2Child, asEvents("aa"))

//      ParseTree.loadHistory(tree, "ba")

      children should have size 2
      leaf1.children should have size 1
      leaf2.children should have size 2
      leaf2Child.children should have size 0
      leaf1Child.children should have size 0

      val leaf2NewChild = leaf2.children.filter(_ != leaf2Child).head
      assertLeafProperties(leaf2NewChild, asEvents("ba"))
    }

    "updating the distribution of the path to the terminal leaf, if the path does not exist" in {
      pending
//      ParseTree.loadHistory(tree, "cb") // loads "cb", updates
      val root = tree.root
      var leafInQuestion = root
      assertProbabalisticDetails(leafInQuestion, Array(0,1,0))

      val rootB = leafInQuestion.children.head
      leafInQuestion = rootB
      assertProbabalisticDetails(leafInQuestion, Array(0,0,1))

      val rootBC = leafInQuestion.children.head
      leafInQuestion = rootBC
      assertProbabalisticDetails(leafInQuestion, Array(0,0,0))
    }

    "updating the distribution of a leaf for it's next-step" should {
      "update the path to the terminal leaf" in {
        pending
//      ParseTree.loadHistory(tree, "cb")
        val root = tree.root
        val rootB = root.children.head
        val rootBC = rootB.children.head

        assertProbabalisticDetails(root, Array(0, 1, 0))

        assertProbabalisticDetails(rootB, Array(0, 0, 1))

        assertProbabalisticDetails(rootBC, Array(0, 0, 0))
      }

      "ignore path if already exists" in {
        pending
//      ParseTree.loadHistory(tree, "cb")
        val root = tree.root
        val rootB = root.children.head
        val rootBC = rootB.children.head

//      ParseTree.loadHistory(tree, "ab")
        assertProbabalisticDetails(root, Array(0, 1, 0))
        assertProbabalisticDetails(rootB, Array(1, 0, 1))
        assertProbabalisticDetails(rootB.children.head, Array(0, 0, 0))

//        ParseTree.loadHistory(tree, "a")
        assertProbabalisticDetails(root, Array(1, 1, 0))

        assertProbabalisticDetails(root.children.last, Array(0, 0, 0))
      }
    }
  }

  "loadData" when {
    "loading sequence 'abc' at lMax of 3 (windows of: abc, ab, bc, a, b, c)" should {

      "load all complete moving windows" in {
        pending
        tree = ParseTree.loadData(tree, abc, "", 3)

        var children:ListBuffer[ParseLeaf] = tree.root.children
        assertChildrenByExactBatch(children, testMap(abc)(0))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abc)(1))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abc)(2))

        children = children.flatMap(_.children)
        children should have size 0
      }
    }
    "loading sequence 'abcabc' at lMax of 3 (windows of:  abc, bca, cab, ab, bc, ca, a, b, c)" should {
      "load all combinations" in {
        pending
        tree = ParseTree.loadData(tree, abcabc, "", 3)

        var children:ListBuffer[ParseLeaf] = tree.root.children
        assertChildrenByExactBatch(children, testMap(abcabc)(0))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcabc)(1))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcabc)(2))

      }

      "include lMax+1 probabilities but not lMax+1 children" in {
        pending
        tree = ParseTree.loadData(tree, abcabc, "", 3)
        var children:ListBuffer[ParseLeaf] = tree.root.children // l1
        children = children.flatMap(_.children) //l2
        children = children.flatMap(_.children) //l3
        children = children.flatMap(_.children) //l4
        children should have size 0
      }
    }

    "loading sequence 'abcbabcbb' at lMax of 3 (windows of: abc, bcb, cba, bab, cbb, ab, bc, cb, ba, bb, a, b, c)" should {
      "load all combinations" in {
        pending
        tree = ParseTree.loadData(tree, abcbabcbb, "", 3)

        var children:ListBuffer[ParseLeaf] = tree.root.children
        assertChildrenByExactBatch(children, testMap(abcbabcbb)(0))

        assertChildrenByExactBatch(children.filter(_.observation == "b").head.children, asEvents("ab", "cb", "bb"))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcbabcbb)(1))

        assertChildrenByExactBatch(children.filter(_.observation == "b").flatMap(_.children), asEvents("abc", "cba", "cbb"))

        children = children.flatMap(_.children)
        assertChildrenByExactBatch(children, testMap(abcbabcbb)(2))

      }
      "finished, it should also include lMax+1 probabilities but not lMax+1 children" in {
        pending
        tree = ParseTree.loadData(tree, abcbabcbb, "", 3)
        var children:ListBuffer[ParseLeaf] = tree.root.children // l1
        children = children.flatMap(_.children) //l2
        children = children.flatMap(_.children) //l3
        children = children.flatMap(_.children) //l4
        children should have size 0
      }
    }

    "not add newlines" in {
      pending
      tree = ParseTree.loadData(tree, (abcabc+"\r\n"), "", 3)
      assertChildrenByExactBatch(tree.root.children, testMap(abcabc)(0))
    }
  }

  "navigateHistory" should {
    "be able to return the correct leaf" in {
      pending
      tree = ParseTree.loadData(tree, abc, "", 3)
      val maybeABC = tree.navigateHistory(asEvents("abc"))
      maybeABC should not be empty
      assertLeafProperties(maybeABC.get, asEvents("abc"))

      val notPresentA = tree.navigateHistory(asEvents("abca"))
      notPresentA shouldBe empty

      tree = ParseTree.loadData(tree, abcbabcbb, "", 3)

      val maybe_BB = tree.navigateHistory(asEvents("bb"))
      maybe_BB should not be empty
      assertLeafProperties(maybe_BB.get, asEvents("bb"))

      val maybeCBB = tree.navigateHistory(asEvents("cbb"))
      maybeCBB should not be empty
      assertLeafProperties(maybeCBB.get, asEvents("cbb"))

      maybe_BB.get.children.map(_.observed) should contain (maybeCBB.get.observed)

      tree = ParseTree.loadData(tree, "aabbabcab", "", 3)

      val maybe_AB = tree.navigateHistory(asEvents("ab"))
      maybe_AB should not be empty
      assertLeafProperties(maybe_AB.get, asEvents("ab"))

      val maybeAAB = tree.navigateHistory(asEvents("aab"))
      val maybeBAB = tree.navigateHistory(asEvents("bab"))
      val maybeCAB = tree.navigateHistory(asEvents("cab"))

      val maybeChildren = List(maybeAAB, maybeBAB, maybeCAB)

      maybeChildren foreach(_ should not be empty)

      maybe_AB.get.children.map(_.observed) should contain theSameElementsAs maybeChildren.map(_.get.observed)
    }
  }
  "getDepth" should {
    "be able to return the correct leaf" in {
      pending
      tree = ParseTree.loadData(tree, abcabc, "", 3)

      tree.getDepth(0).map(_.observed) should contain only ""

      tree.getDepth(1).map(_.observed) should contain theSameElementsAs testMap(abcabc)(0)

      tree.getDepth(2).map(_.observed) should contain theSameElementsAs testMap(abcabc)(1)

      tree.getDepth(3).map(_.observed) should contain theSameElementsAs testMap(abcabc)(2)

      tree.getDepth(4) should have size 0
    }
  }
}
