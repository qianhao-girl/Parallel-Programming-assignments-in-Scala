package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var bl: Boolean = true
    var left,right: Int = 0

    def iter(n: Int): Unit ={
      if (n < chars.length){
        if(chars(n) == '(') { left += 1; iter(n + 1) }
        else if(chars(n) == ')') { right += 1; if(right > left) bl = false else iter(n + 1) }
        else iter(n + 1)
      }else if(left!=right) bl = false
    }

    iter(0)
    bl
  }

  //There are more functional version of balance:
  def balance2(chars: Array[Char]): Boolean = {
    def helper(chs: Array[Char],acc: Int): Int = {
      if(acc < 0 || chs.isEmpty) acc
      else if(chs.head == '(') helper(chs.tail,acc + 1)
      else if(chs.head == ')') helper(chs.tail,acc - 1)
      else helper(chs.tail,acc)
    }
    helper(chars,0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int):(Int,Int) = {
      var (i,left,right) = (idx,0,0)
      while(i < until){
        if(chars(i) == '(') left += 1
        else if(chars(i) == ')') { if(left > right) left -= 1 else right += 1 }
        i += 1
      }
      (left,right)
    }

    def reduce(from: Int, until: Int):(Int,Int) = {
      if(until - from <= threshold) traverse(from,until)// 必须有‘=’，因为threshold==1时也要可行
      else {
        val mid = (from + until) / 2
        val (left,right) = parallel(reduce(from,mid),reduce(mid,until))
        if(from == 0 && left._1 < left._2) left //这一行也可以去掉(You can remove this line and 'else' in the next Line,it still works)
        else if(left._1 >= right._2 && right._2 >0) (left._1 - right._2 + right._1, left._2)
        else (left._1 + right._1, left._2 + right._2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
