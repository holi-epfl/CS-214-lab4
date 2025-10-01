package cs214

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

class MainTest extends munit.FunSuite:
  val thisFilePath = Paths.get(PathMacro.sourcePath)
  val assetsPath = thisFilePath.getParent.getParent.getParent.getParent.getParent.resolve("assets")
  val originPath = assetsPath.resolve("alice_in_wonderland.txt").toString
  val encodedPath = assetsPath.resolve("alice_in_wonderland.huf").toString
  val decodedPath = assetsPath.resolve("alice_in_wonderland-decoded.txt").toString

  def readFileContent(path: String): String =
    new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8)

  test("The Last Test: Encode and Decode alice_in_wonderland.txt (5pts)"):
    Main.huffman("encode", originPath.toString)
    Main.huffman("decode", encodedPath.toString)

    assertNoDiff(readFileContent(originPath), readFileContent(decodedPath))
