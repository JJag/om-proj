package classifier

import util.VectorUtils._

class CodebookClassifier(codebookMap: Map[String, Seq[Vector[Float]]]) {
  def classify(x: Vector[Float]): String = {
    val distortionByClass = codebookMap.mapValues(_.map(distance(_, x)).min)
    distortionByClass.minBy(_._2)._1
  }
}
