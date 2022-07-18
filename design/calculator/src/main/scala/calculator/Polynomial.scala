package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      Math.pow(b(), 2) - 4*a()*c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      if delta() < 0 then Set() 
      else if delta() == 0 then Set( -b()/(2*a()) )
      else 
        val deltaSqrt = Math.sqrt(delta())
        Set(
          (-b()+deltaSqrt)/(2*a()),
          (-b()-deltaSqrt)/(2*a())
        )
    }
