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
  // 2D Shapes Hierarchy

  sealed trait Shape extends Located with Bounded with Movable with Area

  sealed trait Area {
    def area: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Movable
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

  final case class Point(x: Double, y: Double) extends Located with Movable {
    def distance(that: Point): Double =
      Math.sqrt(Math.pow(that.x - this.x, 2) + Math.pow(that.y - this.y, 2))

    override def move(dx: Double, dy: Double): Point =
      Point(x + dx, y + dy)
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

  final case class Rectangle(x: Double, y: Double, width: Double, height: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height

    override def move(dx: Double, dy: Double): Rectangle =
      Rectangle(x + dx, y + dy, width + dx, height + dy)

    override def area: Double = (maxX - minX) * (maxY - minY)
  }

  final case class Triangle(a: Point, b: Point, c: Point) extends Shape {
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

  final case class Square(x: Double, y: Double, width: Double) extends Shape {
    private val height: Double = width

    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y - height

    override def move(dx: Double, dy: Double): Square =
      Square(x + dx, y + dy, width)

    override def area: Double = x * x
  }

  // 3D Shapes Hierarchy

  sealed trait Shape3D extends Located3D with Movable3D with SurfaceArea with Volume with Bounded3D

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait SurfaceArea {
    def surfaceArea: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Located3D with Movable3D  {
    override def move(dx: Double, dy: Double, dz: Double): Point3D =
      Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double)
    extends Shape3D {

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(x + dx, y + dy, z + dz, radius)

    override def volume: Double = (4 * Math.PI * Math.pow(radius, 3)) / 3

    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
  }

  final case class Cube(x: Double, y: Double, z: Double, sideLength: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x + sideLength
    override def minY: Double = y
    override def maxY: Double = y + sideLength
    override def minZ: Double = z
    override def maxZ: Double = z + sideLength

    override def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(x + dx, y + dy, z + dz, sideLength)

    override def volume: Double = Math.pow(sideLength, 3)

    override def surfaceArea: Double = 6 * Math.pow(sideLength, 2)
  }

  final case class Cuboid(
    x: Double, y: Double, z: Double,
    width: Double, height: Double, length: Double,
  ) extends Shape3D {

    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height
    override def minZ: Double = z
    override def maxZ: Double = z + length

    override def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(x + dx, y + dy, z + dz, width, height, length)

    override def volume: Double = length * width * height
    override def surfaceArea: Double = 2 * (length * width + length * height + width * height)
  }

  final case class Pyramid(p1: Point3D, p2: Point3D, p3: Point3D, p4: Point3D) extends Shape3D {
    override def x: Double = p1.x
    override def y: Double = p1.y
    override def z: Double = p1.z

    override def minX: Double = List(p1, p2, p3, p4).map(_.x).min
    override def maxX: Double = List(p1, p2, p3, p4).map(_.x).max
    override def minY: Double = List(p1, p2, p3, p4).map(_.y).min
    override def maxY: Double = List(p1, p2, p3, p4).map(_.y).max
    override def minZ: Double = List(p1, p2, p3, p4).map(_.z).min
    override def maxZ: Double = List(p1, p2, p3, p4).map(_.z).max

    override def move(dx: Double, dy: Double, dz: Double): Pyramid =
      Pyramid(
        Point3D(x + dx, y + dy, z + dz),
        Point3D(x + dx, y + dy, z + dz),
        Point3D(x + dx, y + dy, z + dz),
        Point3D(x + dx, y + dy, z + dz)
      )

    override def volume: Double = ???
    override def surfaceArea: Double = ???
  }
}
