class A[S]
trait Y[U]
trait Test[T] extends A[T] with Y[Int] {
  type X = T
}
