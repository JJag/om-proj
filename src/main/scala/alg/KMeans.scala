package alg

import util.VectorUtils

/**
  * @author JJag
  */
case class KMeans(k: Int, maxErr: Float) {
  private val random = scala.util.Random

  def apply(s: List[Vector[Float]]): IndexedSeq[Vector[Float]] = {
    var centroids: IndexedSeq[Vector[Float]] = IndexedSeq.fill(k)(initRandomly(-10, 10, s.head.length))
    centroids = s.zipWithIndex.flatMap { case (v, i) => if (i % (s.size / k) == 0) List(v) else Nil }.toIndexedSeq

    var mse = Float.MaxValue
    var step = 0
    while (step < 100) {
      step += 1
      val withCentroid = s.map(v => {
        val closestCentroid = centroids.minBy(c => VectorUtils.distance(v, c))
        (v, closestCentroid)
      })

      val errors: List[Float] = withCentroid.map { case (v, c) => VectorUtils.distance(v, c) }
      mse = errors.sum / errors.size

      val clusters: Map[Vector[Float], List[Vector[Float]]] = withCentroid.groupBy(_._2).mapValues(_.map(_._1))
      centroids = clusters.map { case (c, v) => VectorUtils.mean(v) }.toIndexedSeq
    }
    centroids
  }

  private def initRandomly(from: Float, to: Float, dim: Int): Vector[Float] =
    Vector.fill(dim)(from + (to * random.nextFloat()))
}
