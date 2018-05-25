package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      y <- from until end
      x <- 0 until src.width
    } dst.update(x, y, boxBlurKernel(src,x,y,radius))
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */

  //parBlur2 passes the BlurSuite.scala,but fails on  runMain scalashop.HorizontalBoxBlurRunner,the reason remains unknown
  def parBlur2(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val step: Int = src.height / numTasks max 1
    val lst1: List[Int] = (0 to src.height).map(_ * step).toList// a trick different from parBlur1
    lst1.zip(lst1.tail).map(pair => task(blur(src,dst,pair._1,pair._2,radius))).foreach(_.join())
  }// map().foreach() 可以，foreach().foreach() 不可以

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val s: Int = src.height / numTasks max 1
    val lst1: List[Int] =  (0 until src.height by s).toList
    val lst2: List[Int] = lst1.tail:::List(src.height)
    val fromToEnd: List[(Int,Int)] = lst1.zip(lst2)

    def tasks(): Unit = {
      for{
        (from,end) <- fromToEnd
      } yield task(blur(src,dst,from,end,radius))
    }.foreach(_.join())

    tasks()
  }

}
