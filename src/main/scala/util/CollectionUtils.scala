package util

/**
  * @author JJag
  */
object CollectionUtils {
  implicit class WithZipWith[T](coll: Traversable[T]) {
    def zipWith[U](f: T => U) = coll.map(elem => (elem, f(elem)))
  }

}
