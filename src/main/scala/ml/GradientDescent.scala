package ml

/**
 * User: sukrit@quantifind.com
 * Date: 5/3/13
 */
class GradientDescent (hypothesis: (List[Double]) => Double, learningRate: Double) {

  /**
   * Batch gradient descent.
   * @param x
   * @param y
   * @param theta
   * @param ITERATIONS
   */
  def batchGD(x: List[List[Double]], y: List[Double], theta: List[Double], ITERATIONS: Int) = {

    var θ = theta
    val f = x(0).length //number of features
    val m = y.length  //number of training examples

    def getNewθ = {

      var newθ = List [Double] ()

      for (i <- (0 until f).reverse) {
        newθ = (θ(i) - learningRate * summedOverExamples(i))::newθ
      }

      def summedOverExamples (feature: Int) : Double = x.zip(y).map {
        t : (List[Double], Double) =>
          (hypothesis(t._1) - t._2) * t._1(feature)
      }.sum

      newθ

    }

    for (iter <- 0 to ITERATIONS) {
      //θ(i) = θ(i) - α (SUM[(hypothesis(xz) - yz) * xz(i))])
      θ = getNewθ
    }

    θ

  }

}
