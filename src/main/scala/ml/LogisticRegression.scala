package ml

/**
 * User: sukrit@quantifind.com
 * Date: 5/3/13
 */
class LogisticRegressionTraining (trainingSet: List[(List[Double], Double)], iterations: Int, learningRate: Double) {

  val x = trainingSet.map(_._1)
  val Y = trainingSet.map(_._2)

  var θ : List[Double] = null
  var iter = iterations
  var α = learningRate

  def sigmoidFunc(input: List[Double]) = {
    val θTx = (input,θ).zipped.map(_*_).sum

    (1/(1 + Math.exp(-θTx)))
  }

  var hypFunc : List[Double] => Double = sigmoidFunc

  def setNumberOfIterations (newIter: Int) { iter = newIter }
  def setHypothesisFunction(f: List[Double] => Double) { hypFunc = f }
  def setLearningRate (learningRate: Double) { α = learningRate }

  def computeWeights = new GradientDescent(hypFunc, α).batchGD(x, Y, trainingSet.map(_ => Math.random), iter)

  def getWeights = θ match {
    case null => computeWeights
    case w => w
  }
}


class LogisticRegressionClassification (features: List[Double], weights: List[Double]) {
  def classify = Math.round((features, weights).zipped.map(_*_).sum).toInt
}

class LogisticRegression {

  var weights: List[Double] = null

  def trainClassifier(trainer: LogisticRegressionTraining) = {
    weights = trainer.computeWeights
    weights
  }

  def trainClassifier(trainingSet: List[(List[Double], Double)], iterations: Int, learningRate: Double) = {
    val trainer = new LogisticRegressionTraining(trainingSet, iterations, learningRate)
    weights = trainer.computeWeights
    weights
  }

  def getWeights = weights

  def classify(features: List[Double]) = weights match {
    case null => None
    case w => Some(new LogisticRegressionClassification(features, w).classify)
  }

}





