package scalashop

import org.scalameter._
import common._

object RectangularBoxBlurRunner {

  val standardConfig = config (
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 30,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  )  withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width,height)
    val dst = new Img(width,height)
    val seqTime = standardConfig measure {
      RectangularBoxBlur.blur(src,dst,(0,0),width,height,radius)
    }

    val numTasks = 10
    val parTime = standardConfig measure {
      RectangularBoxBlur.parBlur(src,dst,numTasks,radius)
    }
  }


  object RectangularBoxBlur {
   def blur(src: Img,dst: Img,tl: (Int,Int),w: Int,h: Int,radius: Int): Unit = {

   }
  }
}
