package main.scala.dataUtils

import breeze.plot._

/**
 * User: sukrit
 * Date: 5/8/13
 */
class Plotter {

  val f = Figure()
  val m = scala.collection.mutable.Map[Int, breeze.plot.Plot]()
  var plotCount = 0

  def addSubplot(data: List[(Double, Double)]) = {
    val p: breeze.plot.Plot = f.subplot(plotCount + 1, 1, plotCount)
    m += (plotCount + 1 -> p)
    plotCount += 1
    p += plot(data.map(_._1), data.map(_._2))
  }

  def addPlot(plotNum : Int, data: List[(Double, Double)]) = {
    val p = m.get(plotNum).get
    p += plot(data.map(_._1), data.map(_._2))
  }

  def savePlot(outPath: String) = f.saveas(outPath)

}
