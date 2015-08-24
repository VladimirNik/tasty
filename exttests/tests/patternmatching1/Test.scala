class Test {
  private[this] val x: List[Int] = null
  x match {
    case b: List[Int] if x.size > 0  => true
    case q @ Nil if x.size == 0 => { println("test"); true }
    case _ => false
  }
}
