package main.scala.dataUtils

import scala.collection.mutable.Buffer

/**
 * User: sukrit
 * Date: 5/6/13
 *
 * Ported from: http://n-chandra.blogspot.com/2011/04/holt-winters-triple-exponential.html
 */
object HoltWintersSmoothing {

  /**
   * Calculates the initial values and returns the forecast for the future m periods.
   *
   * @param y - Time series data.
   * @param alpha - Exponential smoothing coefficients for level components.
   * @param beta - Exponential smoothing coefficients for trend components.
   * @param gamma - Exponential smoothing coefficients for seasonal components.
   * @param period - A complete season's data consists of L periods. And we need
   * to estimate the trend factor from one period to the next. To
   * accomplish this, it is advisable to use two complete seasons;
   * that is, 2L periods.
   * @param m - Number of extrapolated future data points.
   *
   * @param debug - Print debug values. Useful for testing.
   *
   */
  def forecast(y: List[Double], alpha: Double, beta: Double,
    gamma: Double, period: Int, m: Int, debug: Boolean = false): List[Double] = {

    validateArguments(y, alpha, beta, gamma, period, m)

    val seasons: Int = y.length/period

    val a0: Double = calculateInitialLevel(y)
    val b0: Double = calculateInitialTrend(y, period)
    val initialSeasonalIndices: List[Double] = calculateSeasonalIndices(y, period, seasons)

    /*
    if (debug) {
      println(String.format(
        "Total observations: %d, Seasons %d, Periods %d", y.length,
        seasons, period))
      println("Initial level value a0: " + a0)
      println("Initial trend value b0: " + b0)
      printArray("Seasonal Indices: ", initialSeasonalIndices)
    } */

    val forecast: List[Double] = calculateHoltWinters(y, a0, b0, alpha, beta, gamma,
      initialSeasonalIndices, period, m, debug);

    if (debug) {
      printArray("Forecast", forecast);
    }

    forecast
  }


  /**
   * Validate input.
   *
   * @param y
   * @param alpha
   * @param beta
   * @param gamma
   * @param m
   */
  private def validateArguments(y: List[Double], alpha: Double, beta: Double,
    gamma: Double, period: Int, m: Int) {
    if (y == null || y.isEmpty) {
      throw new IllegalArgumentException("Value of y should be not null");
    }

    if(m <= 0){
      throw new IllegalArgumentException("Value of m must be greater than 0.");
    }

    if(m > period){
      throw new IllegalArgumentException("Value of m must be <= period.");
    }

    if((alpha < 0.0) || (alpha > 1.0)){
      throw new IllegalArgumentException("Value of Alpha should satisfy 0.0 <= alpha <= 1.0");
    }

    if((beta < 0.0) || (beta > 1.0)){
      throw new IllegalArgumentException("Value of Beta should satisfy 0.0 <= beta <= 1.0");
    }

    if((gamma < 0.0) || (gamma > 1.0)){
      throw new IllegalArgumentException("Value of Gamma should satisfy 0.0 <= gamma <= 1.0");
    }
  }

  /**
   * This method realizes the Holt-Winters equations.
   *
   * @param y
   * @param a0
   * @param b0
   * @param alpha
   * @param beta
   * @param gamma
   * @param initialSeasonalIndices
   * @param period
   * @param m
   * @param debug
   * @return - Forecast for m periods.
   */
  private def calculateHoltWinters(y: List[Double], a0: Double, b0: Double, alpha: Double, beta: Double,
    gamma: Double, initialSeasonalIndices: List[Double], period: Int, m: Int, debug: Boolean = false): List[Double] =  {

    var St: Buffer[Double] = y.map(_ => -1.toDouble).toBuffer
    var Bt: Buffer[Double] = y.map(_ => -1.toDouble).toBuffer
    var It: Buffer[Double] = y.map(_ => -1.toDouble).toBuffer
    var Ft: Buffer[Double] = (y.map(_ => -1.toDouble) ::: y.take(m).map(_ => -1.toDouble)).toBuffer

    // Initialize base values
    St(1) = a0
    Bt(1) = b0

    for (i <- 0 until period) {
      It(i) = initialSeasonalIndices(i)
    }

    // Start calculations
    for (i <- 2 until y.length) {

      // Calculate overall smoothing
      if ((i - period) >= 0) {
        St(i) = alpha * y(i) / It(i - period) + (1.0 - alpha) * (St(i - 1) + Bt(i - 1))
      } else {
        St(i) = alpha * y(i) + (1.0 - alpha) * (St(i - 1) + Bt(i - 1))
      }

      // Calculate trend smoothing
      Bt(i) = beta * (St(i) - St(i - 1)) + (1 - beta) * Bt(i - 1)

      // Calculate seasonal smoothing
      if ((i - period) >= 0) {
        It(i) = gamma * y(i) / St(i) + (1.0 - gamma) * It(i - period)
      }

      // Calculate forecast
      if (((i + m) >= period)) {
        Ft(i + m) = (St(i) + (m * Bt(i))) * It(i - period + m)
      }

      /*
      if (debug) {
        println(String.format(
          "i = %d, y = %d, S = %f, Bt = %f, It = %f, F = %f", i,
          y(i), St(i), Bt(i), It(i), Ft(i)))
      } */
    }

    Ft.toList
  }

  /**
   * See: http://robjhyndman.com/researchtips/hw-initialization/ 1st period's
   * average can be taken. But y[0] works better.
   *
   * @return - Initial Level value i.e. St[1]
   */
  private def calculateInitialLevel(y: List[Double]): Double = y.head

  /**
   * See: http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc435.htm
   *
   * @return - Initial trend - Bt[1]
   */
  private def calculateInitialTrend(y: List[Double], period: Int): Double =  {
    val y0 = y.take(period)
    val y1 = y.slice(period, 2*period)

    (y0, y1).zipped.map((a,b) => b-a).sum / Math.pow(period, 2)
  }

  /**
   * See: http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc435.htm
   *
   * @return - Seasonal Indices.
   */
  private def calculateSeasonalIndices(y: List[Double], period: Int, seasons: Int) : List[Double] = {

    val seasonalAverage : List[Double] = y.grouped(period).toList.map { _.sum.toDouble / period.toDouble }

    var seasonalIndices = List[Double]()

    var averagedObservations: Buffer[Double] = y.map(_ => -1.toDouble).toBuffer

    for (i <- 0 until seasons;
         j <- 0 until period) {
      averagedObservations((i * period) + j) = y((i * period) + j) / seasonalAverage(i)
    }

    for (i <- (0 until period).reverse) {
      var sum: Double = 0
      for (j <- 0 until seasons) {
        sum += averagedObservations((j * period) + i)
      }
      seasonalIndices = sum/seasons :: seasonalIndices
    }


    seasonalIndices
  }

  /**
   * Utility method to print array values.
   *
   * @param description
   * @param data
   */
  private def printArray(description: String, data: List[Double]) {
    println(description)
    data.foreach(println)
  }

}
