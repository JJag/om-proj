package vq

trait VQAlg {
  def quantize(s: Seq[Vector[Float]]): Seq[Vector[Float]]
}
