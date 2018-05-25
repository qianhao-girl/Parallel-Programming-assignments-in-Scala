package barneshut
package conctrees

import scala.reflect.ClassTag
import org.scalameter._

// 在一个object ConcBuffer 内，始终保持只有一个chunk: Array[T] 不在conc内，一个chunk满了就将其放入conc: Conc[T] 内
class ConcBuffer[@specialized(Byte, Char, Int, Long, Float, Double) T: ClassTag](
  val k: Int, private var conc: Conc[T]
) extends Traversable[T] {
  require(k > 0)

  def this() = this(128, Conc.Empty)
  
  private var chunk: Array[T] = new Array(k)
  private var lastSize: Int = 0


  def foreach[U](f: T => U): Unit = {
    conc.foreach(f)

    var i = 0
    while (i < lastSize) {
      f(chunk(i))
      i += 1
    }
  }

  final def +=(elem: T): this.type = {
    if (lastSize >= k) expand()
    chunk(lastSize) = elem
    lastSize += 1
    this
  }

  private def pack() { //　这里用到了Chunk类，路径一：pack() -> expand() -> +=(elem: T);
                                            // 路径二：pack() -> result，要最终结果的时候要将未满的chunk加入conc
    conc = Conc.appendTop(conc, new Conc.Chunk(chunk, lastSize, k))
  }                             // 在最后result时要将未满的chunk加入conc中，lastSize 对应于Conc.Chunk 中的size,k　为 Array的大小


  private def expand() {
    pack()  // 将满的chunk放入conc内
    chunk = new Array(k)  // 将chunk变为一个新的Array[T]
    lastSize = 0
  }


  final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    val combinedConc = this.result <> that.result
    this.clear()
    that.clear()
    new ConcBuffer(k, combinedConc)
  }



  def clear() {
    conc = Conc.Empty
    chunk = new Array(k)
    lastSize = 0
  }

  def result: Conc[T] = {
    pack()
    conc
  }
}

object ConcBufferRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val size = 1000000

    def run(p: Int) {
      val taskSupport = new collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(p))
      val strings = (0 until size).map(_.toString)
      val time = standardConfig measure {
        val parallelized = strings.par
        parallelized.tasksupport = taskSupport
        parallelized.aggregate(new ConcBuffer[String])(_ += _, _ combine _).result
      }
      println(s"p = $p, time = $time ms")
    }

    run(1)
    run(2)
    run(4)
    run(8)
  }

}
