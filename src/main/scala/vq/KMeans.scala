package vq

import util.VectorUtils._

class KMeans(K: Int, maxErr: Float) extends VQAlg{
  private val random = scala.util.Random

  override def quantize(s: Seq[Vector[Float]]): IndexedSeq[Vector[Float]] = {
    var centroids: IndexedSeq[Vector[Float]] = IndexedSeq.fill(K)(initRandomly(-10, 10, s.head.length))
    centroids = s.zipWithIndex.flatMap { case (v, i) => if (i % (s.size / K) == 0) List(v) else Nil }.toIndexedSeq

    var mse = Float.MaxValue
    var step = 0
    while (step < 100) {
      step += 1
      val withCentroid = s.map(v => {
        val closestCentroid = centroids.minBy(c => distance(v, c))
        (v, closestCentroid)
      })

      val errors: Seq[Float] = withCentroid.map { case (v, c) => distance(v, c) }
      mse = errors.sum / errors.size

      val clusters: Map[Vector[Float], Seq[Vector[Float]]] = withCentroid.groupBy(_._2).mapValues(_.map(_._1))
      centroids = clusters.map { case (c, v) => mean(v) }.toIndexedSeq
    }
    centroids
  }

  private def initRandomly(from: Float, to: Float, dim: Int): Vector[Float] =
    Vector.fill(dim)(from + (to * random.nextFloat()))
}
