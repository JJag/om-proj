package util

/**
  * @author JJag
  */
object VectorUtils {

  def mean(s: Seq[Vector[Float]]): Vector[Float] = {
    s.transpose.map(_.sum).toVector.map(_ / s.size)
  }

  def distance(v1: Vector[Float], v2: Vector[Float]) = {
    (v1 zip v2).
      map { case (x1, x2) => (x1 - x2) * (x1 - x2) }.
      sum
  }
}
