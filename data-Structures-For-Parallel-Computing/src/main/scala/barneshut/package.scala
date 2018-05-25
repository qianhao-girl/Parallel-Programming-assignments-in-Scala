import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float
    //size is the length of the side of the cell
    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0f
    def total: Int = 0 // total bodies in this cell
    def insert(b: Body): Quad = {
     Leaf(centerX, centerY, size, Seq(b))
    }
  }

  case class Fork( // which divides a spatial cell into four quadrants
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + nw.size / 2
    val centerY: Float = nw.centerY + nw.size / 2
    val size: Float = nw.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = (nw.massX * nw.mass + ne.massX * ne.mass +  sw.massX * sw.mass + se.massX * se.mass)  / mass
    val massY: Float = (nw.massY * nw.mass + ne.massY * ne.mass +  sw.massY * sw.mass + se.massY * se.mass)  / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if(b.x <= centerX)  if(b.y <= centerY) Fork(nw.insert(b), ne, sw, se) else Fork(nw, ne, sw.insert(b), se)
      else if(b.y > centerY) Fork(nw, ne, sw, se.insert(b)) else Fork(nw, ne.insert(b), sw, se)
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val mass: Float = bodies.foldLeft(0f)((z,x) => z + x.mass)
    val massX: Float = bodies.foldLeft(0f)((z,b) => z + b.mass * b.x) / mass
    val massY: Float = bodies.foldLeft(0f)((z,b) => z + b.mass * b.y) / mass
    val total: Int = bodies.length
    def insert(b: Body): Quad = {
      if(size > minimumSize) {
        val nQ = Fork(Empty(centerX - size / 4, centerY - size / 4, size / 2), Empty(centerX + size / 4, centerY - size / 4, size / 2),
          Empty(centerX - size / 4, centerY + size / 4, size / 2), Empty(centerX + size / 4, centerY + size / 4, size / 2))
        (b+:bodies).foldLeft(nQ)((quad,body)=> quad.insert(body)) //错误 nQ.insert(b); bodies.foreach(b => nQ.insert(b))；nQ
    }
      else Leaf(centerX, centerY, size, bodies.++:(Seq(b)))

    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted（用弹射器发射，快速移动） from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot(弹弓), as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          //力的分解：
          val xn = (thatMassX - x) / dist //(xn,yn) -> 力的方向（单位向量）
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>   // no force
        case Leaf(_, _, _, bodies) => bodies.foreach(b => addForce(b.mass, b.x, b.y)) // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>  // When are we allowed to approximate a cluster of bodies with a single point? The heuristic that is used is that the size of the cell divided by the distance dist between the center of mass and the particle is less than some constant theta.
          if(quad.size / distance(x, y, quad.massX, quad.massY) < theta) addForce(quad.mass, quad.massX, quad.massY)
          else List(nw, ne, sw, se).par.foreach(q => traverse(q)) // otherwise, to traverse the quad with more small size
      } // see if node is far enough from the body, or recursion is needed

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    // Importantly, if the Body lies outside of the Boundaries, it should be considered to be located at the
    // closest point within the Boundaries for the purpose of finding which ConcBuffer should hold the body.????????
    def +=(b: Body): SectorMatrix = {
      var (x, y) = (b.x, b.y) // x,y = (b.x, b.y): x == y==(b.x, b.y)

      if(boundaries.minX > x || x > boundaries.maxX  || boundaries.minY > y  || y > boundaries.maxY){
        if(x < boundaries.minX) x = boundaries.minX else if(x > boundaries.maxX) x = boundaries.maxX // == math.max(boundaries.minX, math.min(x, boundaries.maxX))
        if(y < boundaries.minY) y = boundaries.minY else if(y > boundaries.maxY ) y = boundaries.maxY
      }

      val i = ((x - boundaries.minX ) / sectorSize).toInt
      val j = ((y - boundaries.minY) / sectorSize).toInt
      matrix(j * sectorPrecision + i) += b //这里用到了ConcBuffer的 +=’
      this
    }
    /*def +=(b: Body): SectorMatrix = {
      def getPos(p1: Float, p2: Float):Int = {
        ((p1 - p2) / sectorSize).toInt max 0 min sectorPrecision - 1
      }
      this(getPos(b.x,boundaries.minX),  getPos(b.y,boundaries.minY)) += b
      this
    }
    */



    def combine(that: SectorMatrix): SectorMatrix = {
      for(i <- matrix.indices) matrix.update(i, matrix(i).combine(that.matrix(i)))//这里用到了ConcBuffer
      //matrix.zip(that.matrix).foreach{case (fo,la) => fo.combine(la)}
      this
    }

   //The toQuad method on the SectorMatrix does this:
   //the sector matrix:  a quadtree can be constructed in parallel for each sector,those little quadtrees can then be linked together.
    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)  // matrix(y * sectorPrecision + x): ConcBuffer[Body]
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2  // 四分之一正方体的边长
          val nAchievedParallelism = achievedParallelism * 4 // 4、16 、64。。。
          val (nw, ne, sw, se) =                         //4
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis() //System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
