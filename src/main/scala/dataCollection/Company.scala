package dataCollection

/**
 * User: sukrit
 * Date: 4/25/13
 */

import utils.RichFile.enrichFile
import utils._
import java.io._
import scala.io._

class Company (val ticker : String,
               private val companyName : String,
               private val sector : String,
               private val detailsPage : String,
               private val pressPage : String,
               private val stockPage : String) {

  val dataFileName = Constants.PROJECT_DIRECTORY + """../data/stockPrices/""" + ticker.toLowerCase + """.dat"""

  private val stockPageRegex = """(.*)d=(.*)(&amp;)e=(.*)&amp;f=(.*)&amp;(.*)a=(.*)&amp;b=(.*)&amp;c=(.*)&amp;(.*)""".r

  private val stockPageForToday=
    stockPage match {
      case stockPageRegex(head, d, filler, e, f, mid, a, b, c, tail) =>
        val today = Date.getToday
        head + "d=" + (today.month-1).toString + filler + "e=" + (today.date).toString + filler + "f=" + (today.year).toString + filler +
          mid + "a=" + a + filler + "b=" + b + filler + "c=" + c + filler + tail
    }

  def updateStockData()
  {
    /*
     * first check if stock data with this ticker exists.
     * if not, then generate the file, and get the stock page fresh till today.
     * if it does, then get the last entry date and get only from then till today.
     * update the stock data file for this ticker.
     */

    val datafile = new File(dataFileName)

    if(datafile exists)
    {
      //get last date
      val (y,m,d) = getLastDate(datafile)

      //we only need to retrieve from last date + 1.
      val retrieveDate = new Date(y,m,d) + 1

      val newStockPage =
        stockPageForToday match{
          case stockPageRegex(head, d, filler, e, f, mid, a, b, c, tail) =>
            head + "d=" + d + filler + "e=" + e + filler + "f=" + f + filler + mid +
              "a=" + (retrieveDate.month-1).toString + filler + "b=" + retrieveDate.date.toString + filler + "c=" + retrieveDate.year.toString + filler + tail
        }

      val page = processPage(newStockPage).mkString("\n")
      datafile.prependWithNewLine(page)
    }
    else
    {
      //generate new file
      val page = processPage(stockPageForToday).mkString("\n")
      datafile.text = page
    }
  }

  private def processPage (website : String) : List[String] = {
    val lines = Source.fromURL(website).getLines.toList

    for{
      line <- lines.tail
      parts = line.trim.split(',')
    }yield (parts(0) + "," +  parts(5) + "," +  parts(6))
  }

  private def getLastDate(file : File) = {
    val parts = file.firstLine.split(",")
    val date = parts(0).split("-")

    (date(0), date(1), date(2))
  }
}