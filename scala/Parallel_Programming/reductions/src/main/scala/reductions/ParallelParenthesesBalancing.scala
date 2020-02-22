package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.collection.immutable.ListMap

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  //  val hash = collection.mutable.Map[(Int, Int), (String, (Int, Int))]()


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {

    @tailrec
    def go(acc: Int, from: Int): Boolean =
      if (acc < 0) false else if (chars.length == from) acc == 0
      else if (chars(from) == '(') go(acc = acc + 1, from + 1) else if (chars(from) == ')') go(acc = acc - 1, from + 1) else
        go(acc, from + 1)


    go(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    *
    * def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    * def traverse(from: Int, until: Int, _???_: Int, _???_: Int): ???
    * *
    * def reduce(from: Int, until: Int): ??? = ???
    * *
    * reduce(0, chars.length) == ???
    * }
    */

  @tailrec
  def traverse1(chars: Array[Char], from: Int, to: Int, acc1: Int, acc2: Int): (Int, Int) =
    if (from >= to) (acc1, acc2)
    else if (chars(from) == '(') traverse1(chars, from + 1, to, acc1 + 1, acc2) else if (chars(from) == ')') {
      val acc11 = acc1 - 1
      val x = if (acc11 < 0) acc2 - 1 else acc2
      traverse1(chars, from + 1, to, acc11, x)
    } else
      traverse1(chars, from + 1, to, acc1, acc2)

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    //parallel reduction
    def reduce(from: Int, to: Int): (Int, Int) = { //    (n parentesi in excess, n parentesi aperte awaited a sinistra)
      if ((to - from) <= threshold) {
        val o = traverse(from, to, 0, 0)
        //        val x = chars.toList.slice(from, to).mkString
        //        hash += ((from, to) -> ("\"" + x + "\"", o))
        o
      } else {
        val m = from + ((to - from) / 2)
        val ((excessSx, _), (excessDx, awaitedDx)) = parallel(
          reduce(from, m), reduce(m, to)
        )
        val (excessTot, awaitedTot) = ((excessSx + excessDx), (excessSx + awaitedDx))
        //        val x = chars.toList.slice(from, to).mkString
        //        hash += ((from, to) -> ("\"" + x + "\"", (excessTot, awaitedSxTot)))
        (excessTot, awaitedTot)
      }
    }


    def traverse(from: Int, to: Int, acc1: Int, acc2: Int): (Int, Int) =
      traverse1(chars, from, to, acc1, acc2)

    val o: (Int, Int) = reduce(0, chars.length)
    //    chars.foreach(print)
    //    println()
    //    val hashOrd = hash.toSeq.sortBy(x => x._1._2 - x._1._1)
    //    hashOrd.foreach {
    //      x =>
    //        println(x._1 + " " + x._2._1 + " excess: " + x._2._2._1 + " awaitedSx: " + x._2._2._2)
    //    }
    //    hash.clear()
    val res = o._1 == o._2 && o._1 == 0
    //    println("res: " + res)
    res
  }
}
