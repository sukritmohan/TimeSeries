package dataUtils

import scala.collection.mutable.Buffer

/**
 * User: sukrit
 * Date: 5/7/13
 */
class DoubleExponential {

  var S_t: Option[Double] = None
  var B_t: Option[Double] = None

  var curve: Option[List[Double]] = None

  private def doubleExponentialSmoothing(x: List[Double], alpha:Double, beta: Double) : List[Double] = {

    val St: Buffer[Double] = Buffer[Double]()
    val Bt: Buffer[Double] = Buffer[Double]()

    St += x(0)
    St += x(0)
    Bt += 0.toDouble
    Bt += x(1) - x(0)

    for(i <- 2 until x.length) {
      St += (alpha * x(i)) + (1.0 - alpha)*(St(i-1) + Bt(i-1))
      Bt += beta*(St(i) - St(i-1)) + (1-beta)*(Bt(i-1))
    }

    S_t = Some(St.last)
    B_t = Some(Bt.last)

    curve = Some(St.toList)

    curve.get
  }

  def generateCurve(x: List[Double], alpha: Double, beta: Double) : List[Double] = {
    doubleExponentialSmoothing (x, alpha, beta)
  }

  def getCurve() : List[Double] = {
    curve match {
      case Some(c) => c
      case None => throw new Exception("Curve hasn't been generated")
    }
  }

  def getForecast (dist: Int): Double = S_t match {
      case Some(v) =>
        v + dist*B_t.get
      case None => throw new Exception("Curve hasn't been generated")
  }

}
