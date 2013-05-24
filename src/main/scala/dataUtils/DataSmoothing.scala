package dataUtils

/**
 * User: sukrit
 * Date: 5/6/13
 */
object DataSmoothing {

  def linearSmoothing(list: List[Double], α: Int) = {
    list.take(α - 1) ::: (list.sliding(α).toList.map (l => l.sum/α))
  }

  def exponentialSmoothing(list: List[Double], α: Double) = {
    list.foldLeft(List[Double](list.head)) ((l,e) => (α*e + (1-α)*l.head) :: l).reverse.tail
  }

  def tripleExponentialSmoothing(list:List[Double], alpha: Double, beta: Double, gamma: Double, period: Int,
    forecast: Int): List[Double] =  HoltWintersSmoothing.forecast(list, alpha, beta, gamma, period, forecast)

}
