package huffman

import cs214.*

class HuffmanCharTest extends munit.FunSuite:

  import HuffmanChar.*
  import CodeTree.*

  object TestTrees:
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

    val text0 = "encoreuntextetressecret".toList
    val bits0 = List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0,
      1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1,
      0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1)

    // tree1 is the code tree for text1.
    val text1 =
      "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source."
    val tree1 = Fork(
      Fork(
        Fork(
          Fork(
            Fork(Leaf('h', 8), Fork(Leaf('y', 5), Leaf('L', 5), List('y', 'L'), 10), List('h', 'y', 'L'), 18),
            Leaf('n', 19),
            List('h', 'y', 'L', 'n'),
            37
          ),
          Fork(Fork(Leaf('p', 10), Leaf('m', 11), List('p', 'm'), 21), Leaf('s', 21), List('p', 'm', 's'), 42),
          List('h', 'y', 'L', 'n', 'p', 'm', 's'),
          79
        ),
        Fork(
          Fork(
            Fork(
              Fork(
                Fork(Leaf('.', 3), Leaf('k', 3), List('.', 'k'), 6),
                Fork(
                  Fork(Leaf('V', 1), Fork(Leaf('-', 1), Leaf('S', 1), List('-', 'S'), 2), List('V', '-', 'S'), 3),
                  Leaf('I', 3),
                  List('V', '-', 'S', 'I'),
                  6
                ),
                List('.', 'k', 'V', '-', 'S', 'I'),
                12
              ),
              Leaf('u', 12),
              List('.', 'k', 'V', '-', 'S', 'I', 'u'),
              24
            ),
            Leaf('i', 24),
            List('.', 'k', 'V', '-', 'S', 'I', 'u', 'i'),
            48
          ),
          Fork(
            Fork(
              Leaf('d', 12),
              Fork(
                Fork(Leaf('0', 3), Fork(Leaf('v', 2), Leaf('w', 2), List('v', 'w'), 4), List('0', 'v', 'w'), 7),
                Leaf('f', 7),
                List('0', 'v', 'w', 'f'),
                14
              ),
              List('d', '0', 'v', 'w', 'f'),
              26
            ),
            Leaf('t', 26),
            List('d', '0', 'v', 'w', 'f', 't'),
            52
          ),
          List('.', 'k', 'V', '-', 'S', 'I', 'u', 'i', 'd', '0', 'v', 'w', 'f', 't'),
          100
        ),
        List('h', 'y', 'L', 'n', 'p', 'm', 's', '.', 'k', 'V', '-', 'S', 'I', 'u', 'i', 'd', '0', 'v', 'w', 'f', 't'),
        179
      ),
      Fork(
        Fork(
          Fork(Leaf('a', 26), Leaf('r', 27), List('a', 'r'), 53),
          Fork(
            Fork(
              Leaf('c', 14),
              Fork(Leaf('g', 7), Fork(Leaf('C', 4), Leaf('b', 4), List('C', 'b'), 8), List('g', 'C', 'b'), 15),
              List('c', 'g', 'C', 'b'),
              29
            ),
            Fork(
              Leaf('l', 15),
              Fork(
                Fork(
                  Fork(
                    Fork(Leaf('5', 1), Leaf('B', 1), List('5', 'B'), 2),
                    Fork(Leaf('x', 1), Leaf('4', 1), List('x', '4'), 2),
                    List('5', 'B', 'x', '4'),
                    4
                  ),
                  Fork(
                    Fork(Leaf('M', 1), Leaf('H', 1), List('M', 'H'), 2),
                    Fork(Leaf('2', 1), Leaf('R', 1), List('2', 'R'), 2),
                    List('M', 'H', '2', 'R'),
                    4
                  ),
                  List('5', 'B', 'x', '4', 'M', 'H', '2', 'R'),
                  8
                ),
                Leaf(',', 8),
                List('5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','),
                16
              ),
              List('l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','),
              31
            ),
            List('c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','),
            60
          ),
          List('a', 'r', 'c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','),
          113
        ),
        Fork(Fork(Leaf('o', 33), Leaf('e', 34), List('o', 'e'), 67), Leaf(' ', 69), List('o', 'e', ' '), 136),
        List('a', 'r', 'c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ',', 'o', 'e', ' '),
        249
      ),
      List('h', 'y', 'L', 'n', 'p', 'm', 's', '.', 'k', 'V', '-', 'S', 'I', 'u', 'i', 'd', '0', 'v', 'w', 'f', 't', 'a',
        'r', 'c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ',', 'o', 'e', ' '),
      428
    )

    // tree2 is the code tree for text2.
    val text2 =
      "Huffman coding is a lossless (i.e., keeping all information) compression algorithm that can be used to compress lists of symbols. It’s widely used in data compression tasks such as file archiving. For example, huffman coding is used in Gzip. Usually, for a normal, uncompressed text (i.e., a string), each character is a symbol represented by the same number of bits (usually eight). For the text ABEACADABEA, using ACSII code for each character, the encoded text has 11 * 8 = 88 bits."
    val tree2 = Fork(
      Fork(
        Fork(
          Fork(
            Leaf('r', 20),
            Fork(
              Leaf('f', 10),
              Fork(
                Fork(
                  Fork(Leaf('=', 1), Fork(Leaf('S', 1), Leaf('*', 1), List('S', '*'), 2), List('=', 'S', '*'), 3),
                  Leaf('(', 3),
                  List('=', 'S', '*', '('),
                  6
                ),
                Leaf('y', 6),
                List('=', 'S', '*', '(', 'y'),
                12
              ),
              List('f', '=', 'S', '*', '(', 'y'),
              22
            ),
            List('r', 'f', '=', 'S', '*', '(', 'y'),
            42
          ),
          Fork(Fork(Leaf('u', 12), Leaf('d', 12), List('u', 'd'), 24), Leaf('o', 24), List('u', 'd', 'o'), 48),
          List('r', 'f', '=', 'S', '*', '(', 'y', 'u', 'd', 'o'),
          90
        ),
        Fork(
          Fork(Leaf('t', 25), Leaf('i', 26), List('t', 'i'), 51),
          Fork(
            Fork(
              Fork(
                Leaf('A', 6),
                Fork(Leaf('8', 3), Fork(Leaf('C', 2), Leaf('1', 2), List('C', '1'), 4), List('8', 'C', '1'), 7),
                List('A', '8', 'C', '1'),
                13
              ),
              Leaf('m', 14),
              List('A', '8', 'C', '1', 'm'),
              27
            ),
            Leaf('a', 28),
            List('A', '8', 'C', '1', 'm', 'a'),
            55
          ),
          List('t', 'i', 'A', '8', 'C', '1', 'm', 'a'),
          106
        ),
        List('r', 'f', '=', 'S', '*', '(', 'y', 'u', 'd', 'o', 't', 'i', 'A', '8', 'C', '1', 'm', 'a'),
        196
      ),
      Fork(
        Fork(
          Fork(
            Fork(
              Leaf('h', 14),
              Fork(
                Leaf('b', 7),
                Fork(
                  Fork(
                    Fork(Leaf('w', 1), Leaf('v', 1), List('w', 'v'), 2),
                    Fork(Leaf('H', 1), Leaf('’', 1), List('H', '’'), 2),
                    List('w', 'v', 'H', '’'),
                    4
                  ),
                  Fork(
                    Fork(Leaf('U', 1), Leaf('D', 1), List('U', 'D'), 2),
                    Fork(Leaf('G', 1), Leaf('z', 1), List('G', 'z'), 2),
                    List('U', 'D', 'G', 'z'),
                    4
                  ),
                  List('w', 'v', 'H', '’', 'U', 'D', 'G', 'z'),
                  8
                ),
                List('b', 'w', 'v', 'H', '’', 'U', 'D', 'G', 'z'),
                15
              ),
              List('h', 'b', 'w', 'v', 'H', '’', 'U', 'D', 'G', 'z'),
              29
            ),
            Fork(
              Fork(Leaf(',', 8), Leaf('p', 8), List(',', 'p'), 16),
              Fork(
                Fork(
                  Fork(Leaf('B', 2), Leaf('E', 2), List('B', 'E'), 4),
                  Fork(Leaf('k', 2), Leaf('F', 2), List('k', 'F'), 4),
                  List('B', 'E', 'k', 'F'),
                  8
                ),
                Leaf('g', 8),
                List('B', 'E', 'k', 'F', 'g'),
                16
              ),
              List(',', 'p', 'B', 'E', 'k', 'F', 'g'),
              32
            ),
            List('h', 'b', 'w', 'v', 'H', '’', 'U', 'D', 'G', 'z', ',', 'p', 'B', 'E', 'k', 'F', 'g'),
            61
          ),
          Fork(Fork(Leaf('l', 16), Leaf('c', 17), List('l', 'c'), 33), Leaf('s', 37), List('l', 'c', 's'), 70),
          List('h', 'b', 'w', 'v', 'H', '’', 'U', 'D', 'G', 'z', ',', 'p', 'B', 'E', 'k', 'F', 'g', 'l', 'c', 's'),
          131
        ),
        Fork(
          Fork(
            Leaf('e', 38),
            Fork(
              Fork(
                Leaf('.', 9),
                Fork(Leaf('x', 4), Fork(Leaf(')', 3), Leaf('I', 3), List(')', 'I'), 6), List('x', ')', 'I'), 10),
                List('.', 'x', ')', 'I'),
                19
              ),
              Leaf('n', 20),
              List('.', 'x', ')', 'I', 'n'),
              39
            ),
            List('e', '.', 'x', ')', 'I', 'n'),
            77
          ),
          Leaf(' ', 81),
          List('e', '.', 'x', ')', 'I', 'n', ' '),
          158
        ),
        List('h', 'b', 'w', 'v', 'H', '’', 'U', 'D', 'G', 'z', ',', 'p', 'B', 'E', 'k', 'F', 'g', 'l', 'c', 's', 'e',
          '.', 'x', ')', 'I', 'n', ' '),
        289
      ),
      List('r', 'f', '=', 'S', '*', '(', 'y', 'u', 'd', 'o', 't', 'i', 'A', '8', 'C', '1', 'm', 'a', 'h', 'b', 'w', 'v',
        'H', '’', 'U', 'D', 'G', 'z', ',', 'p', 'B', 'E', 'k', 'F', 'g', 'l', 'c', 's', 'e', '.', 'x', ')', 'I', 'n',
        ' '),
      485
    )

  import TestTrees.*

  test("weight: weight of a simple leaf (1pts)"):
    assertEquals(weight(Leaf('c', 29)), 29)
    assertEquals(weight(Leaf('a', 42)), 42)

  test("weight: weight of a larger tree (2pts)"):
    assertEquals(weight(t1), 5)
    assertEquals(weight(t2), 9)

  test("symbols: symbol of a simple leaf is the leaf symbol (1pts)"):
    assertEquals(symbols(Leaf('u', 2)), List('u'))
    assertEquals(symbols(Leaf('e', 2)), List('e'))

  test("symbols: symbols of a larger tree (2pts)"):
    assertEquals(symbols(t1), List('a', 'b'))
    assertEquals(symbols(t2), List('a', 'b', 'd'))

  test("symbolFreqs: an empty list of symbols has no frequency (1pts)"):
    assertEquals(symbolFreqs(List()), List())

  test("symbolFreqs: symbol frequencies of strings (4pts)"):
    val tests: List[(String, List[(Char, Int)])] = List(
      "hello" -> List(('l', 2), ('o', 1), ('h', 1), ('e', 1)),
      "abbrakkadabbrra" -> List(('a', 5), ('b', 4), ('r', 3), ('k', 2), ('d', 1))
    )
    for (s, expected) <- tests do
      val answer = symbolFreqs(s.toList)
      assertEquals(
        answer.sortBy(_._1),
        expected.sortBy(_._1),
        s"Error: For string \"$s\", the output $answer does not match the expected answer $expected (Reminder: Ordering is not important here)."
      )

  test("makeOrderedLeafList: return empty list if the input is empty (1pts)"):
    assertEquals(makeOrderedLeafList(List()), List())

  test("makeOrderedLeafList: return ordered leaf list for some frequency table (5pts)"):
    val tests: List[(List[(Char, Int)], List[Leaf[Char]])] = List(
      List(('t', 2), ('e', 1), ('x', 3)) -> List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)),
      List(('a', 5), ('b', 4), ('r', 3), ('k', 2), ('d', 1)) -> List(
        Leaf('d', 1),
        Leaf('k', 2),
        Leaf('r', 3),
        Leaf('b', 4),
        Leaf('a', 5)
      )
    )
    for (freqs, expected) <- tests do
      val answer = makeOrderedLeafList(freqs)
      assertEquals(
        answer,
        expected,
        s"Error: For frequency table $freqs, the output $answer does not match the expected answer $expected."
      )
      assert(
        answer.sortBy(_.weight) == answer,
        s"Error: For frequency table $freqs, the output $answer is not sorted by weight."
      )

  test("isSingleton: Nil is not a singleton (1pts)"):
    assert(!isSingleton(List()))

  test("isSingleton: a longer list with >= 2 elements is not a singleton (1pts)"):
    assert(!isSingleton(List(Leaf('e', 1), Leaf('t', 2))))

  test("isSingleton: a list with one tree is a singleton (1pts)"):
    assert(isSingleton(List(TestTrees.t1)))

  test("combine: combine of an empty list should be empty (1pts)"):
    assertEquals(combine(Nil), Nil)

  test("combine: combining a singleton should remain unchanged (1pts)"):
    val l = List(Leaf('e', 1))
    assertEquals(combine(l), l)

  test("combine: combining some leaf lists (4pts)"):
    val tests: List[(List[Leaf[Char]], List[CodeTree[Char]])] = List(
      List[Leaf[Char]](Leaf('e', 1), Leaf('t', 2), Leaf('x', 4)) -> List(
        Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),
        Leaf('x', 4)
      ),
      List[Leaf[Char]](Leaf('d', 2), Leaf('k', 2), Leaf('r', 3), Leaf('b', 5)) -> List(
        Leaf('r', 3),
        Fork(Leaf('d', 2), Leaf('k', 2), List('d', 'k'), 4),
        Leaf('b', 5)
      )
    )
    for (trees, expected) <- tests do
      val answer = combine(trees)
      assertEquals(
        answer,
        expected,
        s"Error: For leaves list $trees, the output $answer does not match the expected answer $expected."
      )

  test("createCodeTree: create code tree for a simple text (5pts)"):
    val answer = createCodeTree("hello".toList)
    val expected = Fork(
      Leaf('l', 2),
      Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3),
      List('l', 'o', 'h', 'e'),
      5
    )
    assertEquivalentCodeTree(answer, expected)

  test("createCodeTree: create code tree for a longer text (5pts)"):
    val answer = createCodeTree("what do we know about a code tree?".toList)
    val expected = Fork(
      Fork(Leaf(' ', 7), Fork(Leaf('o', 4), Leaf('e', 4), List('o', 'e'), 8), List(' ', 'o', 'e'), 15),
      Fork(
        Fork(
          Fork(
            Fork(Leaf('n', 1), Leaf('b', 1), List('n', 'b'), 2),
            Fork(Leaf('h', 1), Leaf('k', 1), List('h', 'k'), 2),
            List('n', 'b', 'h', 'k'),
            4
          ),
          Fork(
            Fork(Leaf('r', 1), Leaf('?', 1), List('r', '?'), 2),
            Fork(Leaf('u', 1), Leaf('c', 1), List('u', 'c'), 2),
            List('r', '?', 'u', 'c'),
            4
          ),
          List('n', 'b', 'h', 'k', 'r', '?', 'u', 'c'),
          8
        ),
        Fork(
          Fork(Leaf('d', 2), Leaf('w', 3), List('d', 'w'), 5),
          Fork(Leaf('a', 3), Leaf('t', 3), List('a', 't'), 6),
          List('d', 'w', 'a', 't'),
          11
        ),
        List('n', 'b', 'h', 'k', 'r', '?', 'u', 'c', 'd', 'w', 'a', 't'),
        19
      ),
      List(' ', 'o', 'e', 'n', 'b', 'h', 'k', 'r', '?', 'u', 'c', 'd', 'w', 'a', 't'),
      34
    )
    assertEquivalentCodeTree(answer, expected)

  test("decode: decoding an empty text should return empty text (2pts)"):
    assertEquals(decode(Leaf('c', 10), Nil), Nil)
    assertEquals(decode(Fork(Leaf('c', 10), Leaf('d', 2), List('c', 'd'), 12), Nil), Nil)

  test("decode: decode the secret (4pts)"):
    assertEquals(decode(frenchCode, secret), "huffmanestcool".toList)
    assertEquals(decodedSecret, "huffmanestcool".toList)

  test("decode: decode some bits with frenchCode (4pts)"):
    assertEquals(decode(frenchCode, bits0), text0)

  test("encode: encode empty text with frenchCode should return empty bit sequence (1pts)"):
    assertEquals(encode(frenchCode)(List()), List())

  test("encode: encode some text with frenchCode (4pts)"):
    assertEquals(encode(frenchCode)(text0), bits0)

  test("combining decode and encode: decode and encode a very short text should be identity (4pts)"):
    assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)

  test("combining decode and encode: decode and encode a very short text with a larger tree should be identity (4pts)"):
    assertEquals(decode(tree1, encode(tree1)("abu".toList)), "abu".toList)
    assertEquals(decode(tree2, encode(tree2)("int".toList)), "int".toList)

  test("combining decode and encode: decode and encode some longer text should be identity (5pts)"):
    val syms1 = "literature from 45 BC, making it over 2000 years old.".toList
    assertEquals(decode(tree1, encode(tree1)(syms1)), syms1)
    val syms2 = "It’s widely used in compression tasks such as file archiving.".toList
    assertEquals(decode(tree2, encode(tree2)(syms2)), syms2)

  test(
    "combining createCodeTree and encode: 'createCodeTree(someText)' gives an optimal encoding, the number of bits when encoding 'someText' is minimal (5pts)"
  ):
    val codeTreeForText1 = createCodeTree(text1.toList)
    assertEquals(encode(codeTreeForText1)(text1.toList).length, 1919)

  test("convert: code table for a small tree is created correctly (5pts)"):
    assertEquals(
      convert(t2).sortBy(_._1),
      List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)))
    )

  test("convert: code table for a larger tree is created correctly (5pts)"):
    val answer = convert(tree1)
    val expected = List(
      (' ', List(1, 1, 1)),
      (',', List(1, 0, 1, 1, 1, 1)),
      ('-', List(0, 1, 0, 0, 0, 1, 0, 1, 0)),
      ('.', List(0, 1, 0, 0, 0, 0, 0)),
      ('0', List(0, 1, 1, 0, 1, 0, 0)),
      ('2', List(1, 0, 1, 1, 1, 0, 1, 1, 0)),
      ('4', List(1, 0, 1, 1, 1, 0, 0, 1, 1)),
      ('5', List(1, 0, 1, 1, 1, 0, 0, 0, 0)),
      ('B', List(1, 0, 1, 1, 1, 0, 0, 0, 1)),
      ('C', List(1, 0, 1, 0, 1, 1, 0)),
      ('H', List(1, 0, 1, 1, 1, 0, 1, 0, 1)),
      ('I', List(0, 1, 0, 0, 0, 1, 1)),
      ('L', List(0, 0, 0, 0, 1, 1)),
      ('M', List(1, 0, 1, 1, 1, 0, 1, 0, 0)),
      ('R', List(1, 0, 1, 1, 1, 0, 1, 1, 1)),
      ('S', List(0, 1, 0, 0, 0, 1, 0, 1, 1)),
      ('V', List(0, 1, 0, 0, 0, 1, 0, 0)),
      ('a', List(1, 0, 0, 0)),
      ('b', List(1, 0, 1, 0, 1, 1, 1)),
      ('c', List(1, 0, 1, 0, 0)),
      ('d', List(0, 1, 1, 0, 0)),
      ('e', List(1, 1, 0, 1)),
      ('f', List(0, 1, 1, 0, 1, 1)),
      ('g', List(1, 0, 1, 0, 1, 0)),
      ('h', List(0, 0, 0, 0, 0)),
      ('i', List(0, 1, 0, 1)),
      ('k', List(0, 1, 0, 0, 0, 0, 1)),
      ('l', List(1, 0, 1, 1, 0)),
      ('m', List(0, 0, 1, 0, 1)),
      ('n', List(0, 0, 0, 1)),
      ('o', List(1, 1, 0, 0)),
      ('p', List(0, 0, 1, 0, 0)),
      ('r', List(1, 0, 0, 1)),
      ('s', List(0, 0, 1, 1)),
      ('t', List(0, 1, 1, 1)),
      ('u', List(0, 1, 0, 0, 1)),
      ('v', List(0, 1, 1, 0, 1, 0, 1, 0)),
      ('w', List(0, 1, 1, 0, 1, 0, 1, 1)),
      ('x', List(1, 0, 1, 1, 1, 0, 0, 1, 0)),
      ('y', List(0, 0, 0, 0, 1, 0))
    )
    assertEquals(answer.sortBy(_._1), expected.sortBy(_._1))

  test("quickEncode: quick encode some text with frenchCode (3pts)"):
    assertEquals(quickEncode(frenchCode)(text0), bits0)

  test("quickEncode: quick encode longer text return the correct bit sequence (3pts)"):
    val res: List[Bit] = List(1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1,
      1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0,
      1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1,
      1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1,
      1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0,
      1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1)
    assertEquals(quickEncode(tree1)("looked up one of the more obscure Latin words".toList), res)

  test("combining decode and quickEncode: decode and quick encode is identity (4pts)"):
    assertEquals(
      decode(
        tree1,
        quickEncode(tree1)("ture from 45 BC, making it over 2000 years old. Richard Mc".toList)
      ),
      "ture from 45 BC, making it over 2000 years old. Richard Mc".toList
    )

  test(
    "createCodeTree: createCodeTree is implemented in terms of symbolFreqs, makeOrderedLeafList, isSingleton, combine and until (5pts)"
  ):
    class OK extends Throwable

    def throwsOK(method: String, instrumented: HuffmanChar): Unit =
      try
        instrumented.createCodeTree("hello".toList)
        fail(s"createCodeTree should be implemented in terms of $method")
      catch case _: OK => ()

    throwsOK(
      "symbolFreqs",
      new HuffmanChar:
        override def symbolFreqs(chars: List[Char]): List[(Char, Bit)] = throw OK()
    )

    throwsOK(
      "makeOrderedLeafList",
      new HuffmanChar:
        override def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf[Char]] = throw OK()
    )

    throwsOK(
      "isSingleton",
      new HuffmanChar:
        override def isSingleton(trees: List[CodeTree[Char]]): Boolean = throw OK()
    )

    throwsOK(
      "combine",
      new HuffmanChar:
        override def combine(trees: List[CodeTree[Char]]): List[CodeTree[Char]] = throw OK()
    )

    throwsOK(
      "until",
      new HuffmanChar:
        override def until(
            done: List[CodeTree[Char]] => Boolean,
            merge: List[CodeTree[Char]] => List[CodeTree[Char]]
        )(trees: List[CodeTree[Char]]): List[CodeTree[Char]] = throw OK()
    )
