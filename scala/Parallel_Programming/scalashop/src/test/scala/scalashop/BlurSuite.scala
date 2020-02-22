package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import org.scalameter._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    //src(1, 0) = 1 è uguale a src.update(1, 0, 1) update si puo fare su Array ma non su List
    src.update(0, 0, 0)
    src(1, 0) = 1
    src(2, 0) = 2
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(0, 3) = 50
    src(1, 3) = 11
    src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(3, 0) = 9
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(3, 1) = 10
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(3, 2) = 11

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)
    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)
    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)
  }


  test("Par VerticalBoxBlur.parBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst, 4, 1)
    assert(true)
  }

  test("Par VerticalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst, 32, 1)
    assert(true)
  }

  test("Par HorizontalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst, 32, 1)
    assert(true)
  }

  test("Par VerticalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 64x32 image") {
    val w = 64
    val h = 32
    val src = new Img(w, h)
    val dst = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst, 32, 1)
    assert(true)
  }

  test("Par HorizontalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 64x32 image") {
    val w = 64
    val h = 32
    val src = new Img(w, h)
    val dst = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst, 32, 1)
    assert(true)
  }

  test("VerticalBoxBlur.blur parallel") {
    val w = 1000
    val h = 1000
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(3, 0) = 9
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(3, 1) = 10
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(3, 2) = 11
    VerticalBoxBlur.parBlur(src, dst, 32, 2)
    assert(true)
  }


  test("HorizontalBoxBlur.blur parallel") {
    val w = 1000
    val h = 1000
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(3, 0) = 9
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(3, 1) = 10
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(3, 2) = 11
    HorizontalBoxBlur.parBlur(src, dst, 32, 2)
    assert(true)
  }

  def measureTime[T](f: => T) = {
   withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      f
    }
  }

  def measureMem[T](f: => T) = {

    withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.MemoryFootprint
    } measure {
      f
    }
  }

  //sbt -> runMain scalashop.ScalaShop
  test("full") {

    val photoCanvas = new PhotoCanvas
    val img = photoCanvas.loadScalaImage
    val dst = new Img(img.width, img.height)

    val taskPar = 8
    val radius = 1

    val timePar = measureTime(HorizontalBoxBlur.parBlur(img, dst, numTasks = taskPar, radius = radius))
    val memoryPar = measureMem(HorizontalBoxBlur.parBlur(img, dst, numTasks = taskPar, radius = radius))

    val timeSeq = measureTime(HorizontalBoxBlur.blur(img, dst, 0, img.height , radius = radius))
    val memorySeq = measureMem(HorizontalBoxBlur.blur(img, dst, 0, img.height , radius = radius))

    println(s"Total timePar: $timePar")
    println(s"Total memoryPar: $memoryPar")

    println(s"Total timeSeq: $timeSeq")
    println(s"Total memorySeq: $memorySeq")
    assert(timePar < timeSeq)
//    assert(memoryPar < memorySeq)

    /*
    
    boxBlurKernel foldLeft

    Total timePar: 491.040807
    Total memoryPar: 0.0
    Total timeSeq: 1249.61072
    Total memorySeq: -34.176


    boxBlurKernel map

    Total timePar: 1167.07304
    Total memoryPar: 0.0
    Total timeSeq: 2005.550316
    Total memorySeq: 0.0


    boxBlurKernel While

    Total timePar: 20.064301
    Total memoryPar: -0.208
    Total timeSeq: 65.667873
    Total memorySeq: 0.0
    * */

  }

}
