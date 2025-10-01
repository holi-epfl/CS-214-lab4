package cs214

import munit.Assertions.*
import huffman.CodeTree
import CodeTree.*

def weightedSum[T](tree: CodeTree[T], depth: Int = 0): Int =
  tree match
    case Leaf(symbol, weight) => weight * depth
    case Fork(left, right, symbols, weight) =>
      weightedSum(left, depth + 1) + weightedSum(right, depth + 1)

def symbolDistribution[T](tree: CodeTree[T]): Set[(T, Int)] =
  tree match
    case Leaf(symbol, weight) => Set((symbol, weight))
    case Fork(left, right, symbols, weight) =>
      symbolDistribution(left) ++ symbolDistribution(right)

def symbolsOf[T](tree: CodeTree[T]): Set[T] =
  tree match
    case Leaf(symbol, weight)               => Set(symbol)
    case Fork(left, right, symbols, weight) => symbols.toSet

def weightOf[T](tree: CodeTree[T]): Int =
  tree match
    case Leaf(symbol, weight)               => weight
    case Fork(left, right, symbols, weight) => weight

def assertCodeTree[T](tree: CodeTree[T]): Unit =
  tree match
    case Leaf(symbol, weight) => ()
    case Fork(left, right, symbols, weight) =>
      assertCodeTree(left)
      assertCodeTree(right)
      assert(symbolsOf(left) ++ symbolsOf(right) == symbols.toSet, f"symbols is incorrect at subtree $tree")
      assert(weightOf(left) + weightOf(right) == weight, f"weight is incorrect at subtree $tree")

def assertSameWeightedSum[T](obtainedTree: CodeTree[T], expectedTree: CodeTree[T]): Unit =
  assertEquals(weightedSum(obtainedTree), weightedSum(expectedTree), "obtained tree is not optimal")

def assertSameSymbolDistribution[T](obtainedTree: CodeTree[T], expectedTree: CodeTree[T]): Unit =
  assertEquals(symbolsOf(obtainedTree), symbolsOf(expectedTree), "obtained tree does not have the correct symbols")
  assertEquals(
    symbolDistribution(obtainedTree),
    symbolDistribution(expectedTree),
    "obtained tree does not have the correct frequencies"
  )

def assertEquivalentCodeTree[T](obtainedTree: CodeTree[T], expectedTree: CodeTree[T]): Unit =
  assertCodeTree(obtainedTree)
  assertCodeTree(expectedTree)
  assertSameSymbolDistribution(obtainedTree, expectedTree)
  assertSameWeightedSum(obtainedTree, expectedTree)
