package dataUtils

import utils._
import utils.RichFile.enrichFile
import java.io.File

/**
 * User: sukrit
 * Date: 5/9/13
 */
object DataPreparation {

  def prepareStockTimeSeries(ticker: String) : List[(Double, Double)]  = {
    val input = Constants.STOCK_PRICE_DIRECTORY + ticker + """.dat"""
    val file = new File(input)
    file.getLinesList.reverse.map {
      line =>
        val split = line.split(",")
        val date = Date.getEpochTime(split(0)).toDouble
        val value = split(2).toDouble
        (date, value)
    }
  }

  def prepareStockTimeSeries(ticker: String, start: Date, end: Date) : List[(Double, Double)] = {
    val startDate = start.getEpochTime().toDouble
    val stopDate = end.getEpochTime().toDouble

    prepareStockTimeSeries(ticker).filter{
      ts=>
        ts._1 >= startDate && ts._1 < stopDate
    }.sortWith(_._1 < _._1)
  }

}
