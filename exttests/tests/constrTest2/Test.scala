class Y[F](x: Int) extends Object
class Test[T](k: Int, m: Int) extends Y[T](k) {
  def this(k: Int) = { this(k, k); 5 }
}