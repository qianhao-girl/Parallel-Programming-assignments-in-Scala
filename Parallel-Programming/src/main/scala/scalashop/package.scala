
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val iMin: Int = clamp(y - radius, 0, src.height - 1)
    val iMax: Int = clamp(y + radius, 0, src.height - 1)
    val jMin: Int = clamp(x - radius, 0, src.width - 1)
    val jMax: Int = clamp(x + radius, 0, src.width - 1)
    val totalNum: Int = (jMax - jMin + 1) * (iMax - iMin + 1)

    var r,g,b,a: Int  = 0
    var i: Int = iMin

    while(i <= iMax) {
      var j: Int = jMin
      while(j <=  jMax){
        val pixel: RGBA = src(j,i)  // 错误：src(i,j) !!!!
        r = r + red(pixel); g = g + green(pixel); b = b + blue(pixel); a = a + alpha(pixel)
        j = j + 1
      }
      i = i + 1
    }
    rgba(r / totalNum, g / totalNum, b/ totalNum, a / totalNum)
  }

}
