
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def boxBlurKernel____map(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    require(radius >= 0)

    val x0 = clamp(x - radius, 0, src.width - 1)
    val y0 = clamp(y - radius, 0, src.height - 1)

    val x1 = clamp(x + radius, 0, src.width - 1)
    val y1 = clamp(y + radius, 0, src.height - 1)

    val o2 = for {
      xx <- x0 to x1
      yy <- y0 to y1
    } yield src(xx, yy)

    val rr = o2.map(red(_)).sum / o2.length
    val gg = o2.map(green(_)).sum / o2.length
    val bb = o2.map(blue(_)).sum / o2.length
    val aa = o2.map(alpha(_)).sum / o2.length

    val rgb = rgba(rr, gg, bb, aa)
    rgb

  }

  class ArrayMapInPlace[A](private val a: Array[A]) {

    def map(f: A => A) = {
      var c = 0
      while (c < a.length) {
        a(c) = f(a(c))
        c += 1
      }
      this
    }

  }

  def fff(): Unit = {

    val o = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val oo = o.scan(100)(_ + _)

    o.map()

    val o1 = List(1, 2, 3, 4, 5)
    val o2 = List(6, 7, 8, 9)
    val oo1 = o1.scan(100)(_ + _)
    val oo2 = o2.scan(0)(_ + _)


    val a = new ArrayMapInPlace(Array(1, 2, 3, 4, 5))

    val b = a.map(_ + 1)
    val p1 = for {
      rr <- a
    } yield rr
    val p = a.map(_ + 1)
    println(p)
  }

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    require(radius >= 0)
    fff
    val x0 = clamp(x - radius, 0, src.width - 1)
    val y0 = clamp(y - radius, 0, src.height - 1)

    val x1 = clamp(x + radius, 0, src.width - 1)
    val y1 = clamp(y + radius, 0, src.height - 1)

    var (rr, gg, bb, aa) = (0, 0, 0, 0)
    val o2 = for {
      xx <- x0 to x1
      yy <- y0 to y1
    } yield (rr += red(src(xx, yy)), gg += green(src(xx, yy)), bb += blue(src(xx, yy)), aa += alpha(src(xx, yy)))
    //val o2 = 0.to(100).flatMap(((xx) => 200.to(300).map(((yy) => scala.Tuple4(rr.$plus$eq(xx.$plus(yy)), gg.$plus$eq(xx.$plus(yy)), bb.$plus$eq(xx.$plus(yy)), aa.$plus$eq(xx.$plus(yy)))))))


    val rgb = rgba(rr / o2.length, gg / o2.length, bb / o2.length, aa / o2.length)
    rgb

  }

  def boxBlurKerndel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    require(radius >= 0)

    val x0 = clamp(x - radius, 0, src.width - 1)
    val y0 = clamp(y - radius, 0, src.height - 1)

    val x1 = clamp(x + radius, 0, src.width - 1)
    val y1 = clamp(y + radius, 0, src.height - 1)

    val o2 = for {
      xx <- x0 to x1
      yy <- y0 to y1
    } yield (red(src(xx, yy)), green(src(xx, yy)), blue(src(xx, yy)), alpha(src(xx, yy)))

    val (rr, gg, bb, aa) = o2.foldLeft((0, 0, 0, 0)) { (a, b) =>
      (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4)
    }

    val rgb = rgba(rr / o2.length, gg / o2.length, bb / o2.length, aa / o2.length)
    rgb

  }


  def boxBlurKernel4(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // imperative code behaves much better
    val xMin = clamp(x - radius, 0, src.width - 1)
    val xMax = clamp(x + radius, 0, src.width - 1)

    val yMin = clamp(y - radius, 0, src.height - 1)
    val yMax = clamp(y + radius, 0, src.height - 1)

    var xr = xMin
    var r, g, b, a, count = 0

    while (xr <= xMax) {
      var yr = yMin
      while (yr <= yMax) {
        val pix = src(xr, yr)

        r = r + red(pix)
        g = g + green(pix)
        b = b + blue(pix)
        a = a + alpha(pix)

        yr = yr + 1
        count = count + 1
      }
      xr = xr + 1
    }

    rgba(r / count, g / count, b / count, a / count)
  }


}
