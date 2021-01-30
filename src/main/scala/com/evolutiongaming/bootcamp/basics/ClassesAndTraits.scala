package com.evolutiongaming.bootcamp.basics

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.
object ClassesAndTraits {
  sealed trait Shape extends Located with Bounded with Movable with Area

  sealed trait Area {
    def area: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    def distance(that: Point): Double =
      Math.sqrt(Math.pow(that.x - this.x, 2) + Math.pow(that.y - this.y, 2))

    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point =
      Point(x + dx, y + dy)

    override def area: Double = throw new UnsupportedOperationException
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override val x: Double = centerX
    override val y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius

    override def move(dx: Double, dy: Double): Circle =
      Circle(centerX + dx, centerY + dy, radius)

    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  case class Rectangle(x: Double, y: Double, width: Double, height: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height

    override def move(dx: Double, dy: Double): Rectangle =
      Rectangle(x + dx, y + dy, width + dx, height + dy)

    override def area: Double = (maxX - minX) * (maxY - minY)
  }

  case class Triangle(a: Point, b: Point, c: Point) extends Shape {
    override def x: Double = a.x
    override def y: Double = a.y

    override def minX: Double = List(a, b, c).map(_.x).min
    override def maxX: Double = List(a, b, c).map(_.x).max
    override def minY: Double = List(a, b, c).map(_.y).min
    override def maxY: Double = List(a, b, c).map(_.y).max

    override def move(dx: Double, dy: Double): Triangle =
      Triangle(Point(x + dx, y + dy), Point(x + dx, y + dy), Point(x + dx, y + dy))

    override def area: Double = {
      val ab = a.distance(b)
      val bc = b.distance(c)
      val ca = c.distance(a)
      val p = (ab + bc + ca) / 2
      Math.sqrt(p * (p - ab) * (p - bc) * (p - ca))
    }
  }

  case class Square(x: Double, y: Double, width: Double) extends Shape {
    private val height: Double = width

    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y - height

    override def move(dx: Double, dy: Double): Square =
      Square(x + dx, y + dy, width)

    override def area: Double = x * x
  }
}
