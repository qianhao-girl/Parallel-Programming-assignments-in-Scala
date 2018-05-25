package barneshut
package conctrees

import scala.annotation.tailrec

sealed trait Conc[@specialized(Int, Long, Float, Double) +T] {
  def level: Int
  def size: Int  //elements in subtree
  def left: Conc[T]
  def right: Conc[T]
  def normalized = this
}

object Conc {

  case class <>[+T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
  }

  sealed trait Leaf[T] extends Conc[T] {
    def left = sys.error("Leaves do not have children.")
    def right = sys.error("Leaves do not have children.")
  }

  case object Empty extends Leaf[Nothing] {
    def level = 0
    def size = 0
  }

  class Single[@specialized(Int, Long, Float, Double) T](val x: T) extends Leaf[T] {
    def level = 0
    def size = 1
    override def toString = s"Single($x)"
  }

  //k为Array的大小，size 为Array 内现存元素的数量
  class Chunk[@specialized(Int, Long, Float, Double) T](val array: Array[T], val size: Int, val k: Int)
  extends Leaf[T] {
    def level = 0
    override def toString = s"Chunk(${array.mkString("", ", ", "")}; $size; $k)"
  }

  case class Append[+T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
    override def normalized = {
      def wrap[T](xs: Conc[T], ys: Conc[T]): Conc[T] = (xs: @unchecked) match {
        case Append(ws, zs) => wrap(ws, zs <> ys)
        case xs => xs <> ys
      }
      wrap(left, right)
    }
  }

  def concatTop[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    if (xs == Empty) ys
    else if (ys == Empty) xs
    else concat(xs, ys)
  }

  private def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) new <>(xs, ys)
    else if (diff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right.left)
          val nr = nrr
          new <>(nl, nr)
        }
      }
    } else {
      if (ys.right.level >= ys.left.level) {
        val nl = concat(xs, ys.left)
        new <>(nl, ys.right)
      } else {
        val nll = concat(xs, ys.left.left)
        if (nll.level == ys.level - 3) {
          val nl = new <>(nll, ys.left.right)
          val nr = ys.right
          new <>(nl, nr)
        } else {
          val nl = nll
          val nr = new <>(ys.left.right, ys.right)
          new <>(nl, nr)
        }
      }
    }
  }

  def appendTop[T](xs: Conc[T], ys: Leaf[T]): Conc[T] = (xs: @unchecked) match {
    case xs: Append[T] => append(xs, ys) //2. Append(1,1) + 1 -> append(Append(1,1),Leaf(1))  //7. 1(0)1 + 1 -> append(Append((Leaf(1) <> Leaf(1)) <> (Leaf(1) <> Leaf(1)),Leaf(1)),Leaf(1))
    case _ <> _ => new Append(xs, ys)  //1. 1(0) + 1 -> Append(1,1) : 11 //6. 100 + 1 -> Append((Leaf(1) <> Leaf(1)) <> (Leaf(1) <> Leaf(1)),Leaf(1)): 1(0)1
    case Empty => ys //don't forget this !!!
    case xs: Leaf[T] => new <>(xs, ys)// 0. 1 + 1 -> 1(0) ???
  }
  @tailrec private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) new Append(xs, ys) //8. 0 = 0
    else {
      val zs = new <>(xs.right, ys) //3. zs =  Leaf(1) <> Leaf(1)  //9. zs = Leaf(1) <> Leaf(1)
      xs.left match {//4. xs.left:  Leaf(1) <> Leaf(1)  //10. ws: (Leaf(1) <> Leaf(1)) <> (Leaf(1) <> Leaf(1))
        case ws @ Append(_, _) => append(ws, zs)
        case ws if ws.level <= zs.level => ws <> zs //5. {Leaf(1) <> Leaf(1)} <> {Leaf(1) <> Leaf(1)}: 100 + 1
        case ws => new Append(ws, zs)//if ws.level > zs.level => Append(Leaf(1) <> Leaf(1),Leaf(1) <> Leaf(1)): error found  //12. 2 > 0 => Append((Leaf(1) <> Leaf(1)) <> (Leaf(1) <> Leaf(1)),Leaf(1) <> Leaf(1)): 11(0)
      }
    }
  }

  def traverse[@specialized(Int, Long, Float, Double) T, @specialized(Int, Long, Float, Double) U](xs: Conc[T], f: T => U): Unit = (xs: @unchecked) match {
    case left <> right =>
      traverse(left, f)
      traverse(right, f)
    case s: Single[T] =>
      f(s.x)
    case c: Chunk[T] =>
      val a = c.array
      val sz = c.size
      var i = 0
      while (i < sz) {
        f(a(i))
        i += 1
      }
    case Empty => //什么也不返回可以这么用
    case Append(left, right) =>
      traverse(left, f)
      traverse(right, f)
    case _ =>
      sys.error("All cases should have been covered: " + xs + ", " + xs.getClass)
  }

}
