package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

import scalashop.VerticalBoxBlur.blur

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    for {
      y <- (from until end)
      x <- (0 until src.width)
    } yield dst(x, y) = boxBlurKernel(src, x, y, radius)

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val chunk = src.height / numTasks
    if (chunk == 0) blur(src, dst, 0, src.height, radius) else {
      val h = 0 until src.height
      val r = {
        for {
          start <- h by chunk
        } yield (start, start + chunk)
      }.reverse.toList

      val range = ((r.head._1, src.height) :: r.tail).reverse

      val threads: Seq[ForkJoinTask[Unit]] = for {
        r <- range
      } yield task {
        blur(src, dst, r._1, r._2, radius)
      }

      threads.map(x => x.join())
    }
  }
}
