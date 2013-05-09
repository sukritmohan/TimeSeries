package dataUtils

import utils._
import utils.RichFile.enrichFile
import java.io.File

/**
 * User: sukrit
 * Date: 5/9/13
 */
object DataPreparation {

  def prepareStockTimeSeries(ticker: String) = {
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

}
