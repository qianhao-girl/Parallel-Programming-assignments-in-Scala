package reductions

import org.scalameter._
import common._
import scala.Ordering


object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    var m: Float = 0
    var i: Int = 1
    output(0) = 0
    while(i < input.length){
      m = max(m,input(i) / i)
      output(i) = m
      i = i + 1
    }
  }

  //There is a functional version lineOfSight
  def lineOfSight2(input: Array[Float], output: Array[Float]): Unit ={
    input.zipWithIndex.foreach{
      case (_,0) => output(0) = 0
      case (xs,idx) => output(idx) = max(output(idx - 1), xs / idx)
    }
  }


  sealed abstract class Tree {
    def maxPrevious: Float
  }
  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }
  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree


  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {//if from = 0
    //val out: Array[Float] = Array.fill(until - from)(0)
    var mA: Float = 0
    if(from == 0) mA = input(from)

    var i: Int = from
    while(i < until){
      mA = max(mA, input(i) / i)
      i = i + 1
    }

    mA
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */

  /*
  * [Test Description] upsweep should correctly compute the tree on the indices 1
  * until 5 of a 5 element array for threshold 1
  *[Observed Error] test has been aborted
  */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if(end - from <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else{
      val mid = (from + end) / 2
      val (tL,tR) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Node(tL,tR)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {

    var mA: Float = startingAngle
    var i: Int = from

    if(i == 0) {mA = max(startingAngle,input(i));output(i) = mA; i = i + 1}
    while(i < until){
      mA = max(mA, input(i) / i)
      output(i) = mA
      i = i + 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match{
    case Leaf(from, end, _) => downsweepSequential(input, output, startingAngle, from, end)
    case Node(l , r) =>
      parallel(downsweep(input, output, startingAngle, l), downsweep(input, output,max(startingAngle, l.maxPrevious) , r))
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val tree: Tree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0f, tree)
  }
}
