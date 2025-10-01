package huffman

/** A huffman code is represented by a binary tree.
  *
  * Every `Leaf` node of the tree represents one symbol that the tree can
  * encode. The weight of a `Leaf` is the frequency of appearance of the symbol.
  *
  * The branches of the huffman tree, the `Fork` nodes, represent a set
  * containing all the symbols present in the leaves below it. The weight of a
  * `Fork` node is the sum of the weights of these leaves.
  */
enum CodeTree[T]:
  case Leaf(symbol: T, weight: Int)
  case Fork(left: CodeTree[T], right: CodeTree[T], symbols: List[T], weight: Int)

type Bit = Int

trait HuffmanBase[T]:
  import CodeTree.*

  /** Return the weight of code tree `tree` (i.e., the weight of the root node).
    */
  def weight(tree: CodeTree[T]): Int

  /** Return the list of symbols represented in the code tree `tree`. */
  def symbols(tree: CodeTree[T]): List[T]

  /** Given two code trees `left` and `right`, generate a new code tree in which
    * the left child of root is `left`, and the right child of root is `right`.
    */
  def makeCodeTree(left: CodeTree[T], right: CodeTree[T]): CodeTree[T]

  /** This function computes for each unique symbol in the list `symbols` the
    * number of times it occurs.
    *
    * @param symbols
    *   The text to encode.
    * @return
    *   a list of unique symbols along with their frequencies. The order of the
    *   resulting list is not important.
    * @example
    *   the invocation `symbolFreqs(List('a', 'b', 'a'))` should return
    *   `List(('a', 2), ('b', 1))` (the order does not matter).
    */
  def symbolFreqs(symbols: List[T]): List[(T, Int)]

  /** Returns a list of `Leaf[T]` nodes for a given frequency table `freqs`.
    *
    * @param freqs
    *   A list of unique symbols along with their frequencies.
    * @return
    *   The returned list of leaves should be ordered by ascending weights (i.e.
    *   the head of the list should have the smallest weight), where the weight
    *   of a leaf is the frequency of the symbol.
    */
  def makeOrderedLeafList(freqs: List[(T, Int)]): List[Leaf[T]]

  /** Checks whether the list `trees` contains only one single code tree. */
  def isSingleton(trees: List[CodeTree[T]]): Boolean

  /** This function takes the first two elements of the list `trees` and
    * combines them into a single `Fork` node. This node is then added back into
    * the remaining elements of `trees` at a position such that the ordering by
    * weights is preserved.
    *
    * @param trees
    *   A list of code trees ordered by ascending weights.
    * @return
    *   If `trees` is a list of less than two elements, that list should be
    *   returned unchanged. Otherwise, return the list of trees after merging
    *   the first two elements.
    */
  def combine(trees: List[CodeTree[T]]): List[CodeTree[T]]

  /** This function will be called in the following way:
    *
    * until(isSingleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree[T]]`, `isSingleton` and `combine`
    * refer to the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the
    * list of code trees contains only one single tree, and then return that
    * singleton list.
    */
  def until(
      isDone: List[CodeTree[T]] => Boolean,
      merge: List[CodeTree[T]] => List[CodeTree[T]]
  )(trees: List[CodeTree[T]]): List[CodeTree[T]]

  /** This function creates a code tree for encoding the text `symbols`.
    *
    * @param symbols
    *   The text to encode.
    * @return
    *   A code tree built based on the symbol frequencies of `symbols`
    */
  def createCodeTree(symbols: List[T]): CodeTree[T]

  /** This function decodes one symbol from the bit sequence `bits` using the
    * code tree `tree`.
    *
    * @return
    *   If succeed, returns the resulting symbol and the remaining list of bits;
    *   otherwise, returns `None`.
    */
  def decodeOne(tree: CodeTree[T], bits: List[Bit]): Option[(T, List[Bit])]

  /** Decode the bit sequence `bits` using the code tree `tree` and returns the
    * resulting list of symbols.
    *
    * @param bits
    *   a bit sequence that was encoded using `tree`.
    */
  def decode(tree: CodeTree[T], bits: List[Bit]): List[T]

  /** This function encodes `text` using the code tree `tree` into a sequence of
    * bits.
    */
  def encode(tree: CodeTree[T])(text: List[T]): List[Bit]

  type CodeTable = List[(T, List[Bit])]

  /** This function returns the bit sequence that represents the symbol `symbol`
    * in the code table `table`.
    */
  def codeBits(table: CodeTable)(symbol: T): List[Bit]

  /** Given a code tree, create a code table which contains, for every symbol in
    * the code tree, the sequence of bits representing that symbol.
    */
  def convert(tree: CodeTree[T]): CodeTable

  /** This function takes two code tables and merges them into one. Depending on
    * how you use it in the `convert` method above, this merge method might also
    * do some transformations on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable

  /** This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a
    * code table and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree[T])(text: List[T]): List[Bit]
