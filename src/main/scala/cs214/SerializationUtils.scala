package cs214

import huffman.CodeTree
import upickle.default.*
import java.nio.file.{Files, Path, Paths}
import java.nio.file.StandardOpenOption.APPEND

object SerializationUtils:
  import CodeTree.*

  sealed trait SerialCodeTree derives ReadWriter
  case class SerialLeaf(char: Char, weight: Int) extends SerialCodeTree
  case class SerialFork(left: SerialCodeTree, right: SerialCodeTree, chars: List[Char], weight: Int)
      extends SerialCodeTree

  def intToByteArray(v: Int): Array[Byte] = Array(
    (v >>> 24).toByte,
    (v >>> 16).toByte,
    (v >>> 8).toByte,
    v.toByte
  )

  def convertToSerial(tree: CodeTree[Char]): SerialCodeTree = tree match
    case Leaf(c, w)               => SerialLeaf(c, w)
    case Fork(left, right, cs, w) => SerialFork(convertToSerial(left), convertToSerial(right), cs, w)

  def convertFromSerial(tree: SerialCodeTree): CodeTree[Char] = tree match
    case SerialLeaf(c, w)               => Leaf(c, w)
    case SerialFork(left, right, cs, w) => Fork(convertFromSerial(left), convertFromSerial(right), cs, w)

  def serialize(tree: CodeTree[Char], path: Path): Int =
    val json = write(convertToSerial(tree)).getBytes
    Files.write(path, intToByteArray(json.size))
    Files.write(path, json, APPEND)
    json.size

  def deserialize(bytes: Array[Byte]): CodeTree[Char] =
    val json = new String(bytes)
    convertFromSerial(read[SerialCodeTree](json))
