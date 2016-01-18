package util

/**
  * @author JJag
  */
object CollectionUtils {

  implicit class WithZipWith[T](coll: Traversable[T]) {
    def zipWith[U](f: T => U) = coll.map(elem => (elem, f(elem)))
  }

  implicit class WithMostCommonElem[T](coll: Seq[T]) {
    def mostCommonElem: T =
      coll.groupBy(x => x).mapValues(_.length).maxBy(_._2)._1
  }

}
