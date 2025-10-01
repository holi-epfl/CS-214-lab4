package cs214

import SerializationUtils.*
import huffman.{Bit, CodeTree, HuffmanChar}
import java.nio.file.{Files, Path, Paths}
import java.nio.file.StandardOpenOption.APPEND
import java.nio.ByteBuffer

object Main:

  def bitsToBytes(bits: List[Int]): List[Byte] =
    bits.grouped(8).toList.map { group =>
      val byte = group.foldLeft(0) { (acc, bit) => (acc << 1) | bit }.toByte
      if group.size < 8 then
        (byte << (8 - group.size)).toByte
      else byte
    }

  def extractPath(filePath: String): (Path, String) =
    val path = Paths.get(filePath)
    val filenameNoExt = path.getFileName.toString.split("\\.").head
    (path, filenameNoExt)

  def encodeFile(filePath: String): Unit =
    val path = Paths.get(filePath)
    val filenameNoExt = path.getFileName.toString.split("\\.").head
    val hufPath = Paths.get(path.getParent.toString, s"$filenameNoExt.huf")
    val textBytes = Files.readAllBytes(path)
    val text = new String(textBytes).toList
    val codeTree = HuffmanChar.createCodeTree(text)
    val n = serialize(codeTree, hufPath)
    val encodedBits = HuffmanChar.quickEncode(codeTree)(text)
    val encodedBytes = bitsToBytes(encodedBits)
    Files.write(hufPath, intToByteArray(encodedBits.size), APPEND)
    Files.write(hufPath, intToByteArray(encodedBytes.size), APPEND)
    Files.write(hufPath, encodedBytes.toArray, APPEND)
    val compressRatio = textBytes.size.toDouble / (12 + n + encodedBytes.size)
    println(s"Compression Finished.")
    println(s"Compression Ratio (Original Size / Compressed Size): ${(compressRatio * 100).round / 100.toDouble}")

  def decodeFile(filePath: String): Unit =
    def getCodeTree(buffer: ByteBuffer): CodeTree[Char] =
      val n = buffer.getInt()
      val bytes = new Array[Byte](n)
      buffer.get(bytes)
      deserialize(bytes)
    val path = Paths.get(filePath)
    val filenameNoExt = path.getFileName.toString.split("\\.").head
    val buffer = ByteBuffer.wrap(Files.readAllBytes(path))
    val codeTree = getCodeTree(buffer)
    val nBit = buffer.getInt()
    val nByte = buffer.getInt()
    val encodedBytes = new Array[Byte](nByte)
    buffer.get(encodedBytes)
    val encodedBits: List[Bit] =
      encodedBytes.flatMap { byte => (7 to 0 by -1).map { i => (byte >> i) & 1 } }.toList.take(nBit)
    val decodedBytes = HuffmanChar.decode(codeTree, encodedBits).mkString.getBytes
    Files.write(Paths.get(path.getParent.toString, s"$filenameNoExt-decoded.txt"), decodedBytes.toArray)

  /** Command-line interface for the `HuffmanChar` program.
    *
    * Note: this function is provided to you and you do not need to understand
    * how it works yet.
    *
    * @param args
    *   the command-line arguments.
    */
  @main def huffman(args: String*) =
    val usage = "Usage: encode path OR decode path"
    if args.length != 2 then throw new IllegalArgumentException(usage)
    val command = args(0)
    val path = args(1)

    command match
      case "encode" => encodeFile(path)
      case "decode" => decodeFile(path)
      case _        => throw new IllegalArgumentException(usage)
