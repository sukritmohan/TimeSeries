package utils

import breeze.plot._

/**
 * User: sukrit
 * Date: 5/8/13
 */
class Plotter {

  val f = Figure()
  val m = scala.collection.mutable.Map[Int, breeze.plot.Plot]()
  var plotCount = 0

  def addPlot(data: List[(Double, Double)]) = {
    val p: breeze.plot.Plot = if(plotCount == 0)
        f.subplot(0)
      else
        f.subplot(plotCount+1, 1, plotCount)
    m += (plotCount + 1 -> p)
    plotCount += 1
    p += plot(data.map(_._1), data.map(_._2))
  }

  def appendPlot(plotNum : Int, data: List[(Double, Double)]) = {
    val p = m.get(plotNum).get
    p += plot(data.map(_._1), data.map(_._2))
  }

  def savePlot(outPath: String) = f.saveas(outPath)

}
