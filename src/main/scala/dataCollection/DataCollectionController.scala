package dataCollection

/**
 * User: sukrit
 * Date: 4/25/13
 */

import scala.io._
import utils._

object DataCollectionController {

  def main(args: Array[String]): Unit = {

    val tickerFile = Source.fromFile(Constants.PROJECT_DIRECTORY + """lib/cleanTicker.csv""")

    tickerFile.getLines().foreach(line=>{
      val split = line.split(",")
      val company = new Company(split(0), split(1), split(2), split(3), split(4), split(5))

      company updateStockData

      println("Updated data for " + split(0))
    })

  }
}
