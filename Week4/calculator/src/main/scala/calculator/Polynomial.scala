package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    Signal {
      pow(b(), 2) - 4 * a() * c()
    }

  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {
      if (delta() < 0) Set[Double]()
      else {
        val n = (-b() + sqrt(delta())) / (2 * a())
        val p = (-b() - sqrt(delta())) / (2 * a())
        Set(n, p)
      }
    }

  }
}
