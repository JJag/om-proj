package alg

import util.VectorUtils._

object Lbg {

  type Vec = Vector[Float]

  def apply(K: Int, s: Seq[Vector[Float]]): List[Vector[Float]] = {
    val initCodeword = mean(s)
    var codebook: List[Vec] = List(initCodeword)
    var k = 1
    while (k < K) {
      codebook = codebook.flatMap(splitVector(_, 0.01f))
      k = k * 2
      var clustersByCentroids: Map[Vec, Seq[Vec]] =
        s.groupBy(v => codebook.minBy(c => distance(v, c)))
      var currentDistortion = distortion(clustersByCentroids)
      var lastDistortion = currentDistortion
      do {
        lastDistortion = currentDistortion
        val updatedClusters = clustersByCentroids.values.map(mean)
        codebook = updatedClusters.toList
        clustersByCentroids = s.groupBy(v => codebook.minBy(c => distance(v, c)))
        currentDistortion = distortion(clustersByCentroids)
      } while ((lastDistortion - currentDistortion) / lastDistortion > 0.2 / k)
    }
    println("zrobiono")
    codebook
  }

  private def distortion(clustersByCentroids: Map[Vec, Seq[Vec]]): Float = {
    clustersByCentroids.map {
      case (centr, cluster) => cluster.map(v => distance(centr, v)).sum
    }.sum
  }

  private def splitVector(v: Vec, eps: Float): List[Vec] =
    List(
      v.map(_ * (1 + eps)),
      v.map(_ * (1 - eps))
    )
}