package kmeans

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

//The k-means algorithm is very sensitive to the initial choice of means.
// There are three choice strategies:

//  1: Uniform Choice in the entire color space, not in Image
/*Uniform Choice is the simplest strategy. It chooses n colors uniformly in the entire color space,
 *regardless of the colors used in the image. If the image has a dominant color, the means created by this
 * strategy will likely be very far away from the clusters formed by this dominant color.
 * You can try setting the Uniform Choice strategy with 1, 10 and 30 steps.
 * You will notice the initial choice is quite bad, but the quality improves as the k-means algorithm is applied in more steps.
*/
// 2: Random Sampling
/*Random Sampling is another simple strategy, but with better results.
 *For the initial means, it randomly samples n colors from the image.
 * This yields good results if the image has few dominant colors, but it cannot handle subtle nuances in the image.
 * Again, if you try this strategy with 1, 10 and 30 k-means iteration steps, you will notice improvements
 * as the k-means algorithm is ran more.
 */
//Uniform Random
/*Uniform Random is the most complex strategy to pick means, but it also produces the best results.
 *It works by uniformly splitting the color space in sub-spaces. It then counts the number of pixels that
 *have colors belonging to that sub-space. Based on this number, it chooses a proportional number of means
 * in the sub-space, by randomly sampling from the pixels in that sub-space.
 * Therefore, if your image has dominant colors, this strategy will drop a proportional number of means
 * for each dominant color, thus allowing the k-means algorithm to capture fine nuances.
 */

class KMeans {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer] // points(rand.nextInt(points.length))
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    val meanToPointsMap = points.groupBy(point => findClosest(point,means))
    means.par.map(mean => mean -> meanToPointsMap.getOrElse(mean, GenSeq())).toMap // parSeq.toMap
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    //for(o <- oldMeans.par) oldMeans.updated(oldMeans.indexOf(o), findAverage(o,classified(o)))
    oldMeans.par.map(o => findAverage(o,classified(o)))
  }


  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    (oldMeans zip newMeans).forall{ case (o,n) => o.squareDistance(n) <= eta }
  }

// My bad version:
  def myConverged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    var res: Boolean = true
    for{
      i <- (0 until oldMeans.length).par
      if oldMeans(i).squareDistance(newMeans(i)) > eta
    } res = false
    res
  }

  //小心假的中心点的集合为空，在定义UPDATE要注意
  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val newMeans: GenSeq[Point] = update(classify(points,means),means)
    if (!converged(eta)(means,newMeans)) kMeans(points, newMeans, eta) else newMeans // your implementation need to be tail recursive
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
