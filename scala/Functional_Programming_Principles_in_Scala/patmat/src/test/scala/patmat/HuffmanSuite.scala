package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {


  trait TestTrees {
    val dictionary1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val dictionary2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(dictionary1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(dictionary2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val c = combine(leaflist)
    assert(c === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }


  test("decode french dictionary") {
    new TestTrees {
      assert(decodedSecret.mkString === "huffmanestcool")
    }
  }

  test("encode") {
    new TestTrees {
      val l = encode(dictionary1)("ab".toList)
      assert(l.size === 2)
      assert(l === List(0, 1))
    }
  }

  test("convertCodeTreeTOCodeTable") {
    new TestTrees {
      val l: CodeTable = convert(dictionary2)
      assert(l === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))

    }
  }

  test("decode and encode with CodeTable") {
    new TestTrees {
      val text = "abdbbbdbbbbaabd"

      assert(decode(dictionary2, quickEncode(dictionary2)(text.toList)) === text.toList)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val text = "abdbbbdbbbbaabd"
      assert(decode(dictionary2, encode(dictionary2)(text.toList)) === text.toList)
    }
  }

  test("times 1") {
    val l = times(List('a', 'b', 'a'))
    assert(l.size == 2)
    assert(l(0)._1 == 'b' && l(0)._2 == 1)
    assert(l(1)._1 == 'a' && l(1)._2 == 2)
  }

  test("times 2") {
    val l = times(List('a', 'a', 'a'))
    assert(l.size == 1)
    assert(l(0)._1 == 'a' && l(0)._2 == 3)
  }

  test("times 3") {
    val l = times(List('a', 'b', 'c'))
    assert(l.size == 3)
    assert(l(0)._1 == 'c' && l(0)._2 == 1)
    assert(l(1)._1 == 'b' && l(1)._2 == 1)
    assert(l(2)._1 == 'a' && l(2)._2 == 1)
  }

  test("until") {
    val trees = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val l = until(singleton, combine)(trees)
    assert(l.size === 1)
  }
  def textToCompress = "I socialisti sono decimati ma non scompaiono come si temeva. Nel nuovo emiciclo ce ne saranno 46, ma il segretario Jean-Christophe Cambadelis non ha atteso neppure le prime proiezioni per dimettersi. Marine Le Pen - nonostante il Front National non conquisti neppure i 15 deputati necessari per formare un gruppo parlamentare"

  test("full test slow") {
    new TestTrees {
      val l = createCodeTree(textToCompress.toList)
      val t0 = System.nanoTime()
      val enc = encode(l)(textToCompress.toList)
      val t1 = System.nanoTime()
      val compressionRate = (enc.size / 8.0) / textToCompress.size
      println(s"SLOW:\ttime: ${t1 - t0} clear text size: ${textToCompress.size} compress text: ${(enc.size / 8.0)} compression rate: ${1.0 - compressionRate}")
      assert(decode(l, enc) === textToCompress.toList)
    }
  }

  test("full test quick") {
    new TestTrees {
      val l = createCodeTree(textToCompress.toList)
      val t0 = System.nanoTime()
      val enc = quickEncode(l)(textToCompress.toList)
      val t1 = System.nanoTime()
      val compressionRate = (enc.size / 8.0) / textToCompress.size
      println(s"QUICK:\ttime: ${t1 - t0} clear text size: ${textToCompress.size} compress text: ${(enc.size / 8.0)} compression rate: ${1.0 - compressionRate}")

      assert(decode(l, enc) === textToCompress.toList)
    }
  }

}
