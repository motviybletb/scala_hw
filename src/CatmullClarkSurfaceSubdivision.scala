import scala.collection.mutable
import java.util.Objects

object CatmullClarkSurfaceSubdivision {

  def main(args: Array[String]): Unit = {
    var faces = List(
      new Face(List(new Point(-1.0,  1.0,  1.0),
        new Point(-1.0, -1.0,  1.0),
        new Point( 1.0, -1.0,  1.0),
        new Point( 1.0,  1.0,  1.0))),

      new Face(List(new Point( 1.0,  1.0,  1.0),
        new Point( 1.0, -1.0,  1.0),
        new Point( 1.0, -1.0, -1.0),
        new Point( 1.0,  1.0, -1.0))),

      new Face(List(new Point( 1.0,  1.0, -1.0),
        new Point( 1.0, -1.0, -1.0),
        new Point(-1.0, -1.0, -1.0),
        new Point(-1.0,  1.0, -1.0))),

      new Face(List(new Point(-1.0,  1.0, -1.0),
        new Point(-1.0,  1.0,  1.0),
        new Point( 1.0,  1.0,  1.0),
        new Point( 1.0,  1.0, -1.0))),

      new Face(List(new Point(-1.0,  1.0, -1.0),
        new Point(-1.0, -1.0, -1.0),
        new Point(-1.0, -1.0,  1.0),
        new Point(-1.0,  1.0,  1.0))),

      new Face(List(new Point(-1.0, -1.0, -1.0),
        new Point(-1.0, -1.0,  1.0),
        new Point( 1.0, -1.0,  1.0),
        new Point( 1.0, -1.0, -1.0)))
    )

    displaySurface(faces)
    val iterations = 1
    for (_ <- 0 until iterations) {
      faces = catmullClarkSurfaceSubdivision(faces)
    }
    displaySurface(faces)
  }

  private def catmullClarkSurfaceSubdivision(faces: List[Face]): List[Face] = {
    val edges = faces.flatMap(face => face.edges)
    for (edge <- edges) {
      val facePointsForEdge = faces.filter(face => face.contains(edge)).map(face => face.facePoint)
      if (facePointsForEdge.size == 2) {
        edge.holeEdge = false
        edge.edgePoint = centroid(List(edge.midEdge, centroid(facePointsForEdge)))
      } else {
        edge.holeEdge = true
        edge.edgePoint = edge.midEdge
      }
    }

    val nextVertices = this.nextVertices(edges, faces)

    val nextFaces = mutable.ListBuffer[Face]()
    for (face <- faces) {
      if (face.vertices.size >= 3) {
        val facePoint = face.facePoint
        for (i <- face.vertices.indices) {
          nextFaces += new Face(List(
            nextVertices(face.vertices(i)),
            face.edges(i).edgePoint,
            facePoint,
            face.edges(Math.floorMod(i - 1, face.vertices.size)).edgePoint
          ))
        }
      }
    }
    nextFaces.toList
  }

  private def nextVertices(edges: List[Edge], faces: List[Face]): Map[Point, Point] = {
    val nextVertices = mutable.HashMap[Point, Point]()
    val vertices = faces.flatMap(face => face.vertices).distinct

    for (vertex <- vertices) {
      val facesForVertex = faces.filter(face => face.contains(vertex))
      val edgesForVertex = edges.filter(edge => edge.contains(vertex)).distinct

      if (facesForVertex.size != edgesForVertex.size) {
        val midEdgeOfHoleEdges = mutable.ListBuffer[Point]()
        midEdgeOfHoleEdges ++= edgesForVertex.filter(edge => edge.holeEdge).map(edge => edge.midEdge)
        midEdgeOfHoleEdges += vertex
        nextVertices(vertex) = centroid(midEdgeOfHoleEdges.toList)
      } else {
        val faceCount = facesForVertex.size
        val multipleOne = (faceCount - 3).toDouble / faceCount
        val multipleTwo = 1.0 / faceCount
        val multipleThree = 2.0 / faceCount

        val nextVertexOne = vertex.multiply(multipleOne)
        val facePoints = facesForVertex.map(face => face.facePoint)
        val nextVertexTwo = centroid(facePoints).multiply(multipleTwo)
        val midEdges = edgesForVertex.map(edge => edge.midEdge)
        val nextVertexThree = centroid(midEdges).multiply(multipleThree)
        val nextVertexFour = nextVertexOne.add(nextVertexTwo)

        nextVertices(vertex) = nextVertexFour.add(nextVertexThree)
      }
    }
    nextVertices.toMap
  }

  private def centroid(points: List[Point]): Point = {
    points.foldLeft(Point.ZERO)((left, right) => left.add(right)).divide(points.size)
  }

  private def displaySurface(faces: List[Face]): Unit = {
    println("Surface {")
    faces.foreach(println)
    println("}" + System.lineSeparator())
  }

  final class Point(val x: Double, val y: Double, val z: Double) extends Ordered[Point] {

    override def compare(other: Point): Int = {
      if (x == other.x) {
        if (y == other.y) {
          java.lang.Double.compare(z, other.z)
        } else {
          java.lang.Double.compare(y, other.y)
        }
      } else {
        java.lang.Double.compare(x, other.x)
      }
    }

    override def equals(other: Any): Boolean = other match {
      case point: Point => x == point.x && y == point.y && z == point.z
      case _ => false
    }

    override def hashCode(): Int = {
      Objects.hash(Double.box(x), Double.box(y), Double.box(z))
    }

    def add(other: Point): Point = {
      new Point(x + other.x, y + other.y, z + other.z)
    }

    def multiply(factor: Double): Point = {
      new Point(x * factor, y * factor, z * factor)
    }

    def divide(factor: Double): Point = {
      multiply(1.0 / factor)
    }

    override def toString: String = {
      "(" + format(x) + ", " + format(y) + ", " + format(z) + ")"
    }

    private def format(value: Double): String = {
      if (value >= 0) String.format(" %.3f", Double.box(value)) else String.format("%.3f", Double.box(value))
    }
  }

  private object Point {
    val ZERO = new Point(0.0, 0.0, 0.0)
  }

  private final class Edge(aBegin: Point, aEnd: Point) {

    val (begin, end) = if (aBegin.compareTo(aEnd) <= 0) (aBegin, aEnd) else (aEnd, aBegin)
    val midEdge: Point = centroid(List(begin, end))

    var holeEdge: Boolean = _
    var edgePoint: Point = _

    override def equals(other: Any): Boolean = other match {
      case edge: Edge => begin.equals(edge.begin) && end.equals(edge.end)
      case _ => false
    }

    override def hashCode(): Int = {
      Objects.hash(begin, end)
    }

    def contains(point: Point): Boolean = {
      point.equals(begin) || point.equals(end)
    }
  }

  private final class Face(aVertices: List[Point]) {

    val vertices: List[Point] = mutable.ListBuffer(aVertices: _*).toList
    val facePoint: Point = centroid(vertices)

    val edges: List[Edge] = {
      val edgesList = mutable.ListBuffer[Edge]()
      for (i <- 0 until vertices.size - 1) {
        edgesList += new Edge(vertices(i), vertices(i + 1))
      }
      edgesList += new Edge(vertices.last, vertices.head)
      edgesList.toList
    }

    def contains(vertex: Point): Boolean = {
      vertices.contains(vertex)
    }

    def contains(edge: Edge): Boolean = {
      contains(edge.begin) && contains(edge.end)
    }

    override def toString: String = {
      "Face: " + vertices.map(point => point.toString).mkString("; ")
    }
  }
}

