package nlp

/**
 * User: sukrit
 * Date: 5/23/13
 */
import scala.collection.mutable.Buffer
import breeze.text.tokenize._
import breeze.text.segment._
import breeze.text.analyze._

class VarNGrams {

  case class NGTree(node: String, children: scala.collection.mutable.Set[NGTree], var occurrenceCount: Int) {
    def getChild(child: String) = {
      children.find(_.node == child)
    }

    def getChildCountForProbability(child: String) = {
      getChild(child) match {
        case Some(childNode) => childNode.getCount
        case None => 1
      }
    }

    def getOrCreateChild(child: String) = {
      getChild(child) match {
        case Some(childTree) => Some(childTree)
        case None => addChild(child)
      }
    }

    def incrementChild(child: String) = {
      val retrievedChild = getOrCreateChild(child).get
      retrievedChild.occurrenceCount += 1
      retrievedChild
    }

    def addChild(child: String) = {
      children += NGTree(child, scala.collection.mutable.Set[NGTree](), 0)
      getChild(child)
    }

    def incrementNode() = { this.occurrenceCount += 1 }

    def getCount = occurrenceCount
  }

  val root = NGTree("", scala.collection.mutable.Set[NGTree](), 0)

  def addDocToTree(doc: String, maxLengthToken: Int = 5) = {

    val sentencedTokens = getSentencedTokens(doc)

    val tokenWindows = sentencedTokens.flatMap {senTok =>

      val buf = if(senTok != null && !senTok.isEmpty)
          senTok.sliding(maxLengthToken).toBuffer
        else
          Buffer[List[String]]()

      if(!buf.isEmpty) {
        var last = buf.last
        while(!last.tail.isEmpty) {
          buf += last.tail
          last = last.tail
        }
      }

      buf
    }

    tokenWindows.foreach{
      window =>
        root.incrementNode
        var curr_node = root
        window.foreach{
          token =>
            curr_node = curr_node.incrementChild(token)
        }
    }
  }

  def generateGramTokens(doc: String, maxLengthToken: Int = 5) = {

    val sentencedTokens = getSentencedTokens(doc)

    sentencedTokens.map {
      sentenceTokens =>
        val gramsProb = generateGramsForSentence(sentenceTokens, scala.collection.mutable.Map[String, (Buffer[String], Double)]())
        val grams = gramsProb._1
        grams.toList
    }
  }

  private def generateGramsForSentence(tokens: List[String],
                                       map : scala.collection.mutable.Map[String, (Buffer[String], Double)]) :
    (Buffer[String], Double, scala.collection.mutable.Map[String, (Buffer[String], Double)])= {

    val thisGram = Buffer[String]()
    var maxProbability = 0.0
    val retGrams = Buffer[String]()
    val rootCount = root.getCount.toDouble

    var remainingTokens = tokens

    var currNodeOpt = if(remainingTokens.isEmpty) {
        maxProbability = 1.0
        None
      }
      else Some(root)

    while(currNodeOpt != None) {

      val next_word = remainingTokens.head
      thisGram += next_word
      remainingTokens = remainingTokens.tail

      val currNode = currNodeOpt.get

      val parentCount = currNode.getCount.toDouble

      val nextNodeOpt = currNode.getChild(next_word)

      val childCount = nextNodeOpt match {
        case Some(node) => node.getCount.toDouble
        case None => parentCount * parentCount / rootCount
      }

      val prob : Double = childCount/parentCount

      // Find out if we've already seen seen this string and have solved for best grams.
      val mapOpt = map.get(remainingTokens.mkString(" "))

      val (afterGrams: scala.collection.mutable.Buffer[String], afterProb: Double) = mapOpt match {
        case Some(tuple) =>
          tuple
        case None =>
          // do recursive call.
          val bestAfter = generateGramsForSentence(remainingTokens, map)
          val bestAfterGrams: Buffer[String] = bestAfter._1
          val bestAfterProb: Double = bestAfter._2

          map ++= bestAfter._3

          (bestAfterGrams, bestAfterProb)
      }

      val newProb = prob * afterProb

      if(newProb > maxProbability)
      {
        maxProbability = newProb
        retGrams.clear
        retGrams += thisGram.mkString(" ")
        retGrams ++= afterGrams
      }

      currNodeOpt = if(remainingTokens.isEmpty) None else nextNodeOpt

    }

    map(tokens.mkString(" ")) = (retGrams, maxProbability)

    (retGrams, maxProbability, map)
  }

  private def getSentencedTokens(str: String) = {

    val sentences = (new JavaSentenceSegmenter)(str).toList

    val tokenized = sentences.map(PTBTokenizer).map(_.toList)

    tokenized.map { for( word <- _ ) yield (new PorterStemmer)(word) }.map{
      sentenceTokens =>
        sentenceTokens.flatMap{
          token =>
            val newToken = token.filter(ch => ch.isLetter || ch.isDigit)
            //should also filter stop-words or something.
            if(newToken.isEmpty) None
            else Some(newToken)
        }
    }
  }

}
