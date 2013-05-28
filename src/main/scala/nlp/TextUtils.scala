package nlp

/**
 * User: sukrit@quantifind.com
 * Date: 5/14/13
 */
object TextUtils {

  /**
   *
   * The entity needs to exist in a document a minimum of <threshold> times for this entity to matter
   *
   * @param docEntityList
   * @param threshold
   * @return
   */
  def tfIdf (docEntityList: List[(String, List[String])], threshold: Int) = {

    //max term frequency for a term across all documents
    var maxTermFrequency = scala.collection.mutable.Map[String, Int]()
    //how many documents contain this term at least once
    var documentsForTerm = scala.collection.mutable.Map[String, Int]()

    val numDocs = docEntityList.size

    //populate the maxTermFrequency and documentsForTerm maps
    docEntityList.foreach {
      document =>
        val entities = document._2

        val tfs = entities.groupBy(v=>v).mapValues(_.size).filter(_._2 > threshold)

        maxTermFrequency = maxTermFrequency ++ tfs.map{ case (k,v) => k -> Math.max(v,maxTermFrequency.getOrElse(k,0)) }

        tfs.keys.foreach{
          key =>
            documentsForTerm(key) = documentsForTerm.getOrElse(key, 0) + 1
        }
    }

    docEntityList.map {
      document =>
        val docId = document._1
        val entities = document._2

        val tfs = entities.groupBy(v=>v).mapValues(_.size).filter(_._2 > threshold)
        val tf = tfs.map {
          t =>
            val term = t._1
            val freq = t._2
            // augmented frequency, to prevent a bias towards longer documents
            (term, 0.5 + (freq.toDouble*0.5/maxTermFrequency(term).toDouble))
        }

        val idf = tf.map {
          t =>
            val term = t._1
            val df = documentsForTerm(term).toDouble
            (term, Math.log(numDocs.toDouble / df))
        }

        val tfIDF = tf.map {
          t =>
            val term = t._1
            val tFreq = t._2
            val idFreq = idf(term)
            val tf_idf = tFreq * idFreq
            (term, tf_idf)
        }

        (docId, tfIDF.toList)
    }
  }

}

