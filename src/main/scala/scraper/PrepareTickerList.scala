package scraper

/**
 * User: sukrit
 * Date: 4/25/13
 */

import utils._
import scala.io.Source
import scala.collection.mutable.Map
import java.io._

object PrepareTickerList {

  def main(args: Array[String]): Unit = {
    getCleanTickerList
  }

  private def getCleanTickerList() : Unit = {
    val tickerFile = Source.fromFile(Constants.PROJECT_DIRECTORY + """lib/ticker.csv""")
    val toWrite = Constants.PROJECT_DIRECTORY + """lib/cleanTicker.csv"""

    val map = Map[String, String]()

    tickerFile.getLines().foreach(line=>{
      val parts = line.split(',')
      val ticker = parts(0) substring(1, parts(0).length-1) trim
      val isInvalid = ticker.exists(a => !(a.isLetterOrDigit || a.isWhitespace))
      if(!isInvalid){
        //prepare the line to store.
        val companyName = parts(1) substring(1, parts(1).length-1)
        val sector = parts(6) substring(1, parts(6).length-1)
        val companyDetailsPage = parts(8) substring(1, parts(8).length-1)
        val companyPressPage = "http://www.nasdaq.com/symbol/" + ticker.toLowerCase + "/press-releases"
        //#'s represent numbers. d is (month-1), e (date-1), f (year, eg. 2012)
        val stockHistoryPage = extractCSVWebsite(
          "http://finance.yahoo.com/q/hp?a=&b=&c=&d=3&e=15&f=2012&g=d&s=" + ticker + "%2C+&ql=1"
        );
        val storeLine = ticker + "," + companyName + "," + sector + "," + companyDetailsPage + "," +
          companyPressPage + "," + stockHistoryPage;
        println(storeLine)

        if(stockHistoryPage != "")
          map(ticker) = storeLine
      }

    })

    val writer = new PrintWriter(new File(toWrite))

    map.valuesIterator.foreach{
      line => writer write line
      writer write "\n"
    }
    writer.close()
  }

  private def getWebpage(url:String) : String =
  {
    val page = Source.fromURL(url).mkString
    return page
  }

  private def extractCSVWebsiteFromWebpage(page:String) : String =
  {
    val regEx = """http://ichart.finance.yahoo.com/table.csv(.*)img """.r

    val website = regEx.findFirstIn(page).mkString

    val stockChart = website match {
      case regEx(tail) => "http://ichart.finance.yahoo.com/table.csv" + tail.substring(0, tail.length-3)
      case _ => ""
    }

    //make stock chart generic
    makeStockChartWebsiteGeneric(stockChart)
  }

  private def makeStockChartWebsiteGeneric(url : String) : String =
  {
    val regEx2 = """(.*)d=(.*)&amp;e=(.*)&amp;f=(.*)&amp;g=(.*)""".r

    url match {
      case regEx2(head, d, e, f, tail) => (head + "d=#&amp;e=#&amp;f=#&amp;g=" + tail)
      case _ => ""
    }
  }

  private def extractCSVWebsite(url : String) : String =
  {
    val page : String = getWebpage(url)

    extractCSVWebsiteFromWebpage(page)
  }
}
