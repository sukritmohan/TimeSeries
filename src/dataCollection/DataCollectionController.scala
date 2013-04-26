package dataCollection

/**
 * User: sukrit
 * Date: 4/25/13
 */

import scala.io._
import utils._

object DataCollectionController {

  def main(args: Array[String]): Unit = {
    /*
    val toSave = Constants.PROJECT_DIRECTORY + """lib/lines.png"""

    val xV = for(i <- 1 to 1000)
    			yield (i)

    val xVal = xV toArray
    val f = Figure()
    val p = f.subplot(0)
	//val x = linspace(0.0,1.0)
	p += plot(xVal, xVal :^ 2.0)
	p += plot(x, x :^ 3.0, '.')
	p.xlabel = "x axis"
	p.ylabel = "y axis"
	f.saveas(toSave)
	*/


    val tickerFile = Source.fromFile(Constants.PROJECT_DIRECTORY + """lib/cleanTicker.csv""")

    tickerFile.getLines().foreach(line=>{
      val split = line.split(",")
      val company = new Company(split(0), split(1), split(2), split(3), split(4), split(5))

      company updateStockData

      println("Updated data for " + split(0))
    })

  }
}
