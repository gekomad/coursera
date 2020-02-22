package forcomp

import forcomp.Anagrams.Occurrences

import scala.collection.immutable


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = {
    val l = w.groupBy(a => a.toLower).toList
    val k = l map (r => (r._1, r._2.size))
    k.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")(_ + _))


  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = (dictionary.groupBy(a => wordOccurrences(a.toLowerCase))).withDefault(Map((Nil, Nil)))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).get


  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    *
    *
    */


  def combinations2(occurrences1: List[Occurrences]): List[Occurrences] = {

    occurrences1 match {
      case x :: y :: Nil =>
        x.map(a => y.map(b => a :: List(b))).flatten
      case x :: y :: z =>
        combinations2(y :: z).map(a => x.map(b => b :: a)) flatten
      case _ => occurrences1
    }
  }

  def incrComb(occurrences1: List[Occurrences]): List[Occurrences] = {


    val k = for {
      n <- 2 to occurrences1.length
      ll <- occurrences1 combinations n
      b <- combinations2(ll)
    } yield b
    val o = k.toList
    o
  }

  def allSingle(comb1: List[(Char, Int)]): List[Occurrences] = {

    def reduceSingle(h: (Char, Int)): List[(Char, Int)] = h match {
      case (c, i) if (i == 0) => Nil
      case (c, i) =>
        h :: reduceSingle((c, i - 1))
    }

    comb1 match {
      case Nil => Nil
      case a :: b =>
        List(reduceSingle(a)) ::: allSingle(b)
    }

  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def allCombinations(occurrences: Occurrences): List[List[(Char, Int)]] = {
      val l: Seq[List[(Char, Int)]] = for {
        n <- 1 to occurrences.size
        c <- occurrences.combinations(n)
      } yield c
      l.toList
    }

    val comb = allCombinations(occurrences)

    //    myprintln("comb:")
    //    comb.toSet.toList.foreach(myprintln)

    def decreaseList(comb: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
      val o = comb match {
        case Nil => Nil
        case a :: b =>
          allSingle(a) ::: decreaseList(b)
      }
      o
    }

    val o = decreaseList(comb).toSet.toList
    //    myprintln("\ndecrease initial list:")
    //    o.foreach(myprintln)
    val oo = incrComb(o)

    //    myprintln("\noo:")
    //    oo.foreach(myprintln)
    val ooo = o.flatten.map(List(_))
    val tot = (List(Nil) ::: ooo ::: oo ::: comb).toSet.toList
    //    myprintln("\ntot:")
    //    tot.foreach(myprintln)
    tot
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    * val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    * val r = List(('r', 1))
    */
  def check(tot: Map[Char, Int], parz: Occurrences): Boolean = {

    parz match {
      case Nil =>
        false
      case a :: b => if (tot.contains(a._1)) true else check(tot, b)

    }
  }

  def subtract(tot: Occurrences, parz: Occurrences): Occurrences = {
    val xx: Map[Char, Int] = tot.toMap
    if (!check(xx, parz)) tot else {

      parz match {
        case Nil => xx.toList

        case a :: b if (xx.contains(a._1)) =>
          val x = (xx updated(a._1, xx(a._1) - a._2)).filter(z => z._2 != 0)
          subtract(x.toList, b)

        case a :: b =>
          a :: subtract(xx.toList, b)
      }
    }
  }


  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def myprintln(a: String*) = {
    //println(a)
  }

  def sentenceAnagrams(sentence: Sentence): List[List[Word]] = {

    //    def findAnag(acc: List[List[(Char, Int)]], dictionary: List[List[(Char, Int)]], theSentence: List[(Char, Int)]): List[List[(Char, Int)]] = {
    //
    //      dictionary match {
    //        case Nil => acc
    //        case a :: b =>
    //          val newSentence = subtract(theSentence, a)
    //          //          myprintln(theSentence + " - " + a + " = " + newSentence)
    //          if (newSentence == List() || newSentence == theSentence) Nil else {
    //            assert(newSentence != theSentence)
    //            if (newSentence == Nil) {
    //              myprintln(a :: acc)
    //              a :: acc
    //            }
    //            else findAnag(a :: acc, dictionary, newSentence)
    //          }
    //      }
    //    }

    if (sentence == List()) List(Nil) else {
      val theSentence = sentenceOccurrences(sentence)
      //      val theSentence = theSentence1.toSet.toList
      myprintln("theSentence: " + sentence)
//      theSentence.foreach(myprintln)
      val allSubSet: List[Occurrences] = combinations(theSentence)

      myprintln("\nallSubSet")
//      allSubSet.foreach(myprintln)
      //    myprintln("\npermutations")

      //    val perm = allSubSet.permutations
      //    perm.foreach(myprintln)
      myprintln("------")
      myprintln("------")
      //      val o2 = findAnag(Nil, allSubSet.filter(a => a != List()), theSentence)
      //    for {
      //      n <- 1 to allSubSet.length
      //      k <- allSubSet combinations n
      //    } yield findAnag(Nil, allSubSet, theSentence)

      //    val o = o2.filter(a => a.length != 0)
      //    val oo: Seq[Option[List[Word]]] = o.map(a => dictionaryByOccurrences.get(a)).filter(d => d != None)
      val oo = allSubSet.map(a => dictionaryByOccurrences.get(a)) //.filter(d => d ==theSentence)

      //    oo.foreach(myprintln)
      oo.flatten
    }
  }


}
