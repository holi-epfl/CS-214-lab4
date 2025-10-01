package huffman

import scala.collection.immutable.Stream.Cons
import java.security.InvalidKeyException

trait HuffmanImpl[T] extends HuffmanBase[T]:
  import CodeTree.*

  // Part 1: Basics

  def weight(tree: CodeTree[T]): Int =
    tree match
      case Leaf(t, w) => w
      case Fork(l, r, ts, w) => w

  def symbols(tree: CodeTree[T]): List[T] =
    tree match
      case Leaf(symbol, weight) => symbol :: Nil
      case Fork(left, right, symbols, weight) => symbols
    
  def makeCodeTree(left: CodeTree[T], right: CodeTree[T]): CodeTree[T] =
    Fork(left, right, symbols(left) ++ symbols(right), weight(left) + weight(right))

  // Part 2: Constructing Huffman trees

  def symbolFreqs(symbols: List[T]): List[(T, Int)] =
    symbols match
      case Nil => Nil
      case head :: Nil => (head, 1) :: Nil
      case head :: tail => 
        val rightResult = symbolFreqs(tail)
        def addSymbolToFreq(symbol: T)(freq: List[(T, Int)]): List[(T, Int)] = 
          freq match
            case Nil => (symbol, 1) :: Nil
            case (ss, nn) :: next => 
              if ss == symbol then (ss, nn + 1) :: next
              else (ss, nn) :: addSymbolToFreq(symbol)(next)
        addSymbolToFreq(head)(rightResult)

  def makeOrderedLeafList(freqs: List[(T, Int)]): List[Leaf[T]] =
    def findSmallest(freqs: List[(T, Int)]): (T, Int) = //assume not empty!!!
      freqs match
        case Nil => throw InvalidKeyException()
        case (s, n) :: Nil => (s, n)
        case (s, n) :: next => 
          val (ss, nn) = findSmallest(next)
          if n < nn then (s, n) else (ss, nn)

    def removeSmallest(freqs: List[(T, Int)], smallest: (T, Int)): List[(T, Int)] =
      freqs match
        case head :: next => 
          if smallest == head then next
          else head :: removeSmallest(next, smallest)
        case Nil => Nil
    
    def putSmallestFirst(freqs: List[(T, Int)]): List[Leaf[T]] = 
      if freqs == Nil then Nil
      else 
        val (smallestT, smallestCount) = findSmallest(freqs)
        val removed = removeSmallest(freqs, (smallestT, smallestCount))
        Leaf(smallestT, smallestCount) :: putSmallestFirst(removed)

    putSmallestFirst(freqs)
      
  def isSingleton(trees: List[CodeTree[T]]): Boolean =
    trees match
      case head :: Nil => true
      case _ => false
    

  def combine(trees: List[CodeTree[T]]): List[CodeTree[T]] =
    def insert(trees: List[CodeTree[T]], tree: CodeTree[T]): List[CodeTree[T]] =
      trees match
        case Nil => tree :: Nil
        case head :: next => 
          if weight(tree) < weight(head) then 
            tree :: (head :: next)
          else 
            head :: insert(next, tree)
    
    trees match
      case Nil => Nil
      case head :: Nil => head :: Nil
      case first :: (second :: rest) =>
        val merged = makeCodeTree(first, second)
        insert(rest, merged)

  def until(
      isDone: List[CodeTree[T]] => Boolean,
      merge: List[CodeTree[T]] => List[CodeTree[T]]
  )(trees: List[CodeTree[T]]): List[CodeTree[T]] =
    if isDone(trees) then trees
    else 
      val treesMerged = merge(trees)
      until(isDone, merge)(treesMerged)

  def createCodeTree(symbols: List[T]): CodeTree[T] =
    val symbolFreq = symbolFreqs(symbols)
    val orderedLeaves = makeOrderedLeafList(symbolFreq)
    val treesDone = until(isSingleton, combine)(orderedLeaves)
    treesDone match
      case head :: next => head
      case Nil => throw InvalidKeyException()
    

  // Part 3: Decoding
  // Reminder: type Bit = Int

  def decodeOne(tree: CodeTree[T], bits: List[Bit]): Option[(T, List[Bit])] =
    tree match
      case Leaf(symbol, weight) => Some(symbol, bits)
      case Fork(left, right, symbols, weight) =>
        bits match
          case Nil => None
          case head :: next =>
            head match
              case 0 => decodeOne(left, next)
              case 1 => decodeOne(right, next)

  def decode(tree: CodeTree[T], bits: List[Bit]): List[T] =
    def decode_tail(bits_tail: List[Bit], pre: List[T]): List[T] =
      val thisDecoded = decodeOne(tree, bits_tail)
      thisDecoded match
        case Some(thisSymbol, remainingBits) => decode_tail(remainingBits, thisSymbol :: pre)
        case None => pre.reverse
    decode_tail(bits, Nil)
      

  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree[T])(text: List[T]): List[Bit] =
    def flatMap(text: List[T])(f: T => List[Bit]): List[Bit] = 
      text match
        case Nil => Nil
        case head :: next => 
          f(head) ++ flatMap(next)(f)

    def encodeOne(tree: CodeTree[T])(s: T): List[Bit] = 
      tree match
        case Leaf(symbol, weight) => Nil
        case Fork(left, right, symbols, weight) =>
          val leftResult = 
            left match
              case Leaf(symbolL, weightL) => 
                if symbolL == s then 0 :: Nil else Nil
              case Fork(_, _, _, _) =>
                val resultL = encodeOne(left)(s)
                resultL match
                  case Nil => Nil
                  case head :: next => 0 :: resultL
          val rightResult =
            right match
              case Leaf(symbolR, weightR) => 
                if symbolR == s then 1 :: Nil else Nil
              case Fork(_, _, _, _) =>
                val resultR = encodeOne(right)(s)
                resultR match
                  case Nil => Nil
                  case head :: next => 0 :: resultR
          if !(leftResult == Nil) then leftResult else rightResult

    flatMap(text)(encodeOne(tree))

  // Part 4b: Encoding using code table

  // Reminder: type CodeTable = List[(T, List[Bit])]

  def codeBits(table: CodeTable)(symbol: T): List[Bit] =
    table match
      case Nil => Nil
      case (k, v) :: next => if k == symbol then v else codeBits(next)(symbol)

  def convert(tree: CodeTree[T]): CodeTable =
    tree match
      case Leaf(symbol, weight) => (symbol, Nil) :: Nil
      case Fork(left, right, symbols, weight) =>
        mergeCodeTables(convert(left), convert(right))

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    def addPrefix(prefix: Bit, codeTable: CodeTable): List[(T, List[Bit])] =
      codeTable match
        case Nil => Nil 
        case (k, v) :: next => (k, prefix :: v) :: addPrefix(prefix, next)
    
    if a == Nil then b else addPrefix(0, a) ++ addPrefix(1, b)
 
  def quickEncode(tree: CodeTree[T])(text: List[T]): List[Bit] =
    val table = convert(tree)
    def quickFind(table: CodeTable)(text: List[T]): List[Bit] =
      text match
        case head :: next => codeBits(table)(head) ++ quickFind(table)(next)
        case Nil => Nil
    quickFind(table)(text)
    
