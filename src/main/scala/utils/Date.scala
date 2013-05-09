package utils

import java.text.SimpleDateFormat

/**
 * User: sukrit
 * Date: 4/25/13
 */

class Date (val year: Int, val month: Int, val date: Int){

  private val leapYear = if((year%100==0 && year%400==0) || (year%100!=0 && year%4==0))
    true
  else
    false

  private val isValid = checkValidity

  if(!isValid)
    throw new Exception ("Invalid format!")

  def this(y:String, m:String, d:String) = this(y.toInt, m.toInt, d.toInt)

  def + (n : Int): Date = {
    val max = getMaxDateForMonth
    val addedD = this.date + n

    if (addedD <= max)
      new Date(this.year, this.month, addedD)
    else
    {
      val newN = n - (max - this.date) - 1
      val newD = 1
      val newM = if(this.month+1>12) 1 else this.month+1
      val newY = if(newM == 1) this.year+1 else this.year

      new Date(newY, newM, newD) + newN
    }
  }

  private def checkValidity : Boolean = {
    val maxDate = getMaxDateForMonth

    if(year<=0)
      false
    else if (month<=0 || month>12)
      false
    else if (date<=0 || date>maxDate)
      false
    else
      true
  }

  private def getMaxDateForMonth : Int = {
    month match {
      case 1 => 31
      case 2 => if(leapYear)29
      else 28
      case 3 => 31
      case 4 => 30
      case 5 => 31
      case 6 => 30
      case 7 => 31
      case 8 => 31
      case 9 => 30
      case 10 => 31
      case 11 => 30
      case 12 => 31
    }
  }

  override def toString() : String = year.toString + "-" + month.toString + "-" + date.toString

  def getEpochTime() = Date.getEpochTime(this)
}

object Date {

  val df = new SimpleDateFormat("yyyy-MM-dd")

  def getToday() = {
    val today = new java.util.Date
    new Date(today.getYear() + 1900, today.getMonth() + 1, today.getDate())
  }

  def getEpochTime(d: Date) = df.parse(d.toString()).getTime
}