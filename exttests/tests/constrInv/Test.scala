class Test extends Object {
  class W extends Object
  class Y[T] extends Object
  class F extends Object {
    private[this] val u = new Y[W]()
  }
}

