package calculator

object Polynomial {

  /**
   * Δ = b² - 4ac
   * @param a
   * @param b
   * @param c
   * @return
   */
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal {
      Math.pow(b.apply(), 2) - (4 * a.apply() * c.apply())
    }


  /**
   * (-b ± √Δ) / 2a
   * @param a
   * @param b
   * @param c
   * @param delta
   * @return
   */
  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {


    Signal {

      if(a()==0){
        Set(Double.NaN)
      }else if (delta() >= 0) {
        val sqrDelta: Double = Math.sqrt(delta())

        val res1 = (-b() + sqrDelta) / (2 * a())
        val res2 = (-b() - sqrDelta) / (2 * a())

        Set(res1.toDouble, res2.toDouble)

      } else {
        Set()
      }

    }

  }
}
