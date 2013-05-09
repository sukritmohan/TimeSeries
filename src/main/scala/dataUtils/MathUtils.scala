package main.scala.dataUtils

/**
 * User: sukrit
 * Date: 5/6/13
 */
class MathUtils {

  /**
   * Mean square error
   *
   * @param data
   * @param prediction
   * @return
   */
  def calculateMSE(data: List[Double], prediction: List[Double]) : Double =
    ((data, prediction).zipped.map{
      (x,y) =>
        Math.pow((x - y), 2)
    }.sum) / data.length

  def derivative (data: List[(Double, Double)], points: Int) : List[(Double, Double)] = {
    data.sliding(points).toList.map {
      t=>
        (t(points/2)._1.toDouble, (t(points-1)._2 - t(0)._2).toDouble/(t(points-1)._1 - t(0)._1).toDouble)
    }
  }

}
