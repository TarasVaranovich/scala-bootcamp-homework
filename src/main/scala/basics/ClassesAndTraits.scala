package basics

object ClassesAndTraits {

  sealed trait Shape[A] extends Located with Bounded with Movable[A]

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

  sealed trait Movable[A] {
    def move(dx: Double, dy: Double): A
  }

  /**
   * Trait represents possible type of surface area
   */
  sealed trait AreaPresented {
    def area: Double
  }

  /**
   * Trait represents shape angle position relative its center in radians
   * in cartesian coordinate system
   */
  sealed trait Rotatable[A] {
    def position: Double
    def rotate(angle: Double): A
  }

  final case class Point(x: Double, y: Double) extends Shape[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double)
    extends Shape[Circle] with AreaPresented {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Triangle(centerX: Double, centerY: Double,
                            sideA: Double, sideB: Double, sideC: Double,
                            position: Double)
    extends Shape[Triangle] with Rotatable[Triangle] with AreaPresented {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def move(dx: Double, dy: Double): Triangle = ???
    override def rotate(angle: Double): Triangle = ???
    override def area: Double = ???
  }

  final case class Square(centerX: Double, centerY: Double,
                          side: Double,
                          position: Double) extends Shape[Square] with Rotatable[Square] with AreaPresented {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def move(dx: Double, dy: Double): Square = ???
    override def rotate(angle: Double): Square = ???
    override def area: Double = Math.pow(side, 2)
  }

  sealed trait Shape3D[A] extends Located3D with Bounded3D with Movable3D[A]

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D[A] {
    def move(dx: Double, dy: Double, dz: Double): A
  }

  /**
   * Trait represents shape angle position relative its center in radians
   * in three dimensional space
   */
  sealed trait Rotatable3D[A] {
    def positionX: Double
    def positionY: Double
    def positionZ: Double
    def rotate(angleX: Double, angleY: Double, angleZ: Double): A
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D[Point3D] {
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Point3D = ???
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double,
                          radius: Double) extends Shape3D[Sphere] with AreaPresented {
    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Sphere = ???
    override def area: Double = ???
  }

  final case class Cube(centerX: Double, centerY: Double, centerZ: Double,
                        side: Double,
                        positionX: Double, positionY: Double, positionZ: Double)
    extends Shape3D[Cube] with Rotatable3D[Cube] with AreaPresented {
    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Cube = ???
    override def rotate(angleX: Double, angleY: Double, angleZ: Double): Cube = ???
    override def area: Double = ???
  }

  final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double,
                          sideA: Double, sideB: Double, sideC: Double,
                          positionX: Double, positionY: Double, positionZ: Double)
    extends Shape3D[Cuboid] with Rotatable3D[Cuboid] with AreaPresented {
    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = ???
    override def rotate(angleX: Double, angleY: Double, angleZ: Double): Cuboid = ???
    override def area: Double = ???
  }

  final case class Tetrahedron(centerX: Double, centerY: Double, centerZ: Double,
                               sideA: Double, sideB: Double, sideC: Double,
                               sideD: Double, sideE: Double, sideF: Double,
                               positionX: Double, positionY: Double, positionZ: Double)
    extends Shape3D[Tetrahedron] with Rotatable3D[Tetrahedron] with AreaPresented {
    override def z: Double = ???
    override def x: Double = ???
    override def y: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Tetrahedron = ???
    override def rotate(angleX: Double, angleY: Double, angleZ: Double): Tetrahedron = ???
    override def area: Double = ???
  }

}
