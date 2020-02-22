package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    //Δ = b² - 4ac
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    Signal(if (delta() < 0.0) Set.empty[Double] else
      Set((-b() + Math.sqrt(delta())) / (2.0 * a()), (-b() - Math.sqrt(delta())) / (2.0 * a()))
    )
  }
}
