package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
    measureParallelCountChange(ParallelCountChange.myThreshold(amount,coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var count: Int = 0
    def Iter(m:Int,cs: List[Int]): Unit = {
      if(m==0) count += 1
      else if(m > 0 && cs.nonEmpty) { Iter(m, cs.tail); Iter(m - cs.head, cs)}
    }
    if(money >= 0) Iter(money, coins)
    count
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
      var count: Int = 0
      if (money == 0) count = 1
      else if (money > 0 && coins.nonEmpty) {
        if (!threshold(money, coins)) {
          val (left,right) = parallel(parCountChange(money, coins.tail,threshold),parCountChange(money - coins.head, coins,threshold))
          count = left + right
        } else count = countChange(money, coins)
      }
      count
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = //1.42  1.20
  (money: Int, _: List[Int]) => money <= startingMoney * 2 / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = //1.68   1.23
    (_: Int, coins: List[Int]) => coins.length <= totalCoins * 2 / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = { //1.81 1.28
    val threshold: Int =  startingMoney * allCoins.length / 2
    (money: Int,coins: List[Int]) => money * coins.length <= threshold
  }

  def myThreshold(startingMoney: Int,allCoins: List[Int]): Threshold = { //1.67 1.1461
    val threshold: Int = startingMoney  / allCoins.min
    (money: Int,coins: List[Int]) =>  money / coins.head <= threshold  /  2
  }
}
