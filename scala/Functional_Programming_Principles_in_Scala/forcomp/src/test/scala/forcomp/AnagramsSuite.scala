package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r2") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val x=subtract(lard, r)
    assert(x === lad)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    val x=subtract(lard, r)
    assert(x === lad)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abbac") {

    val abbac = List(('a', 2), ('b', 2), ('c', 2))
    val abbaccomb = List(
      List(),

      List(('a', 1)),
      List(('a', 2)),

      List(('b', 1)),
      List(('b', 2)),

      List(('c', 1)),
      List(('c', 2)),

      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 1), ('c', 1)),
      List(('a', 1), ('c', 2)),

      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 2)),
      List(('a', 2), ('c', 1)),
      List(('a', 2), ('c', 2)),

      List(('b', 1), ('c', 1)),
      List(('b', 1), ('c', 2)),
      List(('b', 2), ('c', 1)),
      List(('b', 2), ('c', 2)),


      List(('a', 2), ('b', 2), ('c', 2)),
      List(('a', 2), ('b', 2), ('c', 1)),
      List(('a', 2), ('b', 1), ('c', 2)),
      List(('a', 2), ('b', 1), ('c', 1)),

      List(('a', 1), ('b', 2), ('c', 2)),
      List(('a', 1), ('b', 2), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 2)),
      List(('a', 1), ('b', 1), ('c', 1))

    )
    val x = combinations(abbac)

    assert(x.toSet === abbaccomb.toSet)
    //    val b = allSingle(List(('a', 3), ('b', 2), ('c', 3)))
    //    b.foreach(println)
//    val x = combinations(abba)
//    val x0 = combinations2(List(List(('a', 3), ('a', 2), ('a', 1)), List(('b', 3), ('b', 2), ('b', 1)), List(('c', 3), ('c', 2), ('c', 1))))
//    val x1 = combinations2(List(List(('a', 2), ('a', 1)), List(('b', 2), ('b', 1)), List(('c', 2), ('c', 1))))
//    val x2 = combinations2(List(List(('a', 2), ('a', 1)), List(('b', 2))))
//    val x3 = combinations2(List(List(('a', 2), ('a', 1)), List(('c', 2), ('c', 1))))
//    val x4 = combinations2(List(List(('b', 2), ('b', 1)), List(('c', 2), ('c', 1))))
//    val x5 = combinations2(List(List(('a', 2), ('a', 1))))
//    val x6 = combinations2(List(List(('b', 2), ('a', 1))))
//    val x7 = combinations2(List(List(('c', 2), ('a', 1))))
//    val x8 = combinations2(List(List(('a', 2))))
//    val x9 = combinations2(List(List(('b', 2))))
//    val x10 = combinations2(List(List(('c', 2))))
//    x0.foreach(println)
//    x1.foreach(println)
//    x2.foreach(println)
//    x3.foreach(println)
//    x4.foreach(println)
//    x5.foreach(println)
//    x6.foreach(println)
//    x7.foreach(println)
//    x8.foreach(println)
//    x9.foreach(println)
//    x10.foreach(println)
//    assert((x0 ::: x1 ::: x2 ::: x3 ::: x4 ::: x5 ::: x6 ::: x7 ::: x8 ::: x9 ::: x10).toSet === abbacomb.toSet)

  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))

    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    val x = combinations(abba)

    assert(x.toSet === abbacomb.toSet)

  }


  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
