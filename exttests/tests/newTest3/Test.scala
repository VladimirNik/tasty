class Test[U](x: U) extends Object { self =>
  private[this] val a = new Test("ff")
  class Y extends Object
  private[this] val b = new Test(new Y)
}