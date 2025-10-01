package huffman

trait HuffmanImpl[T] extends HuffmanBase[T]:
  import CodeTree.*

  // Part 1: Basics

  def weight(tree: CodeTree[T]): Int =
    ???

  def symbols(tree: CodeTree[T]): List[T] =
    ???

  def makeCodeTree(left: CodeTree[T], right: CodeTree[T]): CodeTree[T] =
    Fork(left, right, symbols(left) ++ symbols(right), weight(left) + weight(right))

  // Part 2: Constructing Huffman trees

  def symbolFreqs(symbols: List[T]): List[(T, Int)] =
    ???

  def makeOrderedLeafList(freqs: List[(T, Int)]): List[Leaf[T]] =
    ???

  def isSingleton(trees: List[CodeTree[T]]): Boolean =
    ???

  def combine(trees: List[CodeTree[T]]): List[CodeTree[T]] =
    ???

  def until(
      isDone: List[CodeTree[T]] => Boolean,
      merge: List[CodeTree[T]] => List[CodeTree[T]]
  )(trees: List[CodeTree[T]]): List[CodeTree[T]] =
    ???

  def createCodeTree(symbols: List[T]): CodeTree[T] =
    ???

  // Part 3: Decoding
  // Reminder: type Bit = Int

  def decodeOne(tree: CodeTree[T], bits: List[Bit]): Option[(T, List[Bit])] =
    ???

  def decode(tree: CodeTree[T], bits: List[Bit]): List[T] =
    ???

  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree[T])(text: List[T]): List[Bit] =
    ???

  // Part 4b: Encoding using code table

  // Reminder: type CodeTable = List[(T, List[Bit])]

  def codeBits(table: CodeTable)(symbol: T): List[Bit] =
    ???

  def convert(tree: CodeTree[T]): CodeTable =
    ???

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    ???

  def quickEncode(tree: CodeTree[T])(text: List[T]): List[Bit] =
    ???
