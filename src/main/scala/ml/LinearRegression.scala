package ml

/**
 * User: sukrit
 * Date: 5/2/13
 */
object LinearRegression {


  /**
   * standard deviation implementation
   * @param list
   * @return
   */
  def stdDev(list: List[Double]): Double = list match {
    case Nil => 0.0
    case l =>
      val avg = list.sum / list.length
      math.sqrt((0.0 /: l) {
        (a,e) => a + math.pow(e - avg, 2.0)
      } / list.size)
  }


  /**
   * straight line fit through the data
   * @param data
   * @return y=mx+c
   */
  def simpleLRFit(data: List[(Double,Double)]) = {

    val sumX = data.map(_._1).sum
    val sumX2 = data.map(d => d._1 * d._1).sum

    val sumY = data.map(_._2).sum

    val xBar = sumX / data.length
    val yBar = sumY / data.length

    val x2Bar = sumX2 / data.length

    val sumXY = data.map (d => d._1 * d._2).sum

    val xyBar = sumXY / data.length

    val m = (xyBar - (xBar*yBar)) / (x2Bar - (xBar*xBar))

    val c = yBar - (m*xBar)

    (m, c)
  }
}


class LinearRegressionTraining (trainingSet: List[(List[Double], Double)], iterations: Int, learningRate: Double) {

  val x = trainingSet.map(_._1)
  val Y = trainingSet.map(_._2)

  var θ : List[Double] = null
  var iter = iterations
  var α = learningRate

  def linearFunc(input: List[Double]) = { (input,θ).zipped.map(_*_).sum }

  var hypFunc : List[Double] => Double = linearFunc

  def setNumberOfIterations (newIter: Int) { iter = newIter }
  def setHypothesisFunction(f: List[Double] => Double) { hypFunc = f }
  def setLearningRate (learningRate: Double) { α = learningRate }

  def computeWeights = new GradientDescent(hypFunc, α).batchGD(x, Y, trainingSet.map(_ => Math.random), iter)

  def getWeights = θ match {
    case null => computeWeights
    case w => w
  }
}


class LinearRegressionClassification (features: List[Double], weights: List[Double]) {
  def classify = (features, weights).zipped.map(_*_).sum
}

class LinearRegression {

  var weights: List[Double] = null

  def trainClassifier(trainer: LinearRegressionTraining) = {
    weights = trainer.computeWeights
    weights
  }

  def trainClassifier(trainingSet: List[(List[Double], Double)], iterations: Int, learningRate: Double) = {
    val trainer = new LinearRegressionTraining(trainingSet, iterations, learningRate)
    weights = trainer.computeWeights
    weights
  }

  def getWeights = weights

  def classify(features: List[Double]) = weights match {
    case null => None
    case w => Some(new LinearRegressionClassification(features, w).classify)
  }

}
