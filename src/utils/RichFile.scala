package utils

/**
 * User: sukrit
 * Date: 4/25/13
 */

import java.io._
import scala.io._

class RichFile( file: File ) {

  def text = {
    if (file.exists())
      Source.fromFile( file ).mkString
    else
      ""
  }

  def text_=( s: String ) {
    withBufferedWriter(new FileWriter(file))
    {
      writer => writer.write(s)
    }
  }

  def append (s: String) = {
    withBufferedWriter(new FileWriter(file,true))
    {
      writer => writer.write(s)
    }
  }

  def appendAfterNewLine ( s: String) = {
    withBufferedWriter(new FileWriter(file,true))
    {
      writer => writer.newLine ; writer.write(s)
    }
  }

  def prepend (s: String) = { text = s + text }

  def prependWithNewLine (s:String) = { text = s + "\n" + text }

  def firstLine : String = {
    val lines = getLinesList

    lines match{
      case List() => ""
      case _ => lines head
    }
  }

  def lastLine : String = {
    val lines = getLinesList

    lines match{
      case List() => ""
      case _ => lines last
    }
  }

  def getLinesList : List[String] = {
    if (file.exists()){
      Source.fromFile(file).getLines.toList
    }
    else
      List()
  }

  private def withBufferedWriter(fstream:FileWriter)(op : BufferedWriter => Unit)
  {
    val writer = new BufferedWriter(fstream)
    try{ op(writer) }
    finally { writer.close ; fstream.close()}
  }
}

object RichFile {

  implicit def enrichFile( file: File ) = new RichFile( file )

}
