class Test extends Object {
  object Test1 extends Object {
    class Test2 extends Object {
      private[this] val y1: A1.type = A1
      private[this] val y2 = A1.A2
      object A1 extends Object {
        object A2 extends Object {
          object A3 extends Object {
            def j(x: A1.type) = 7
            def z(u: A2.type) = u
          }
        }
      }
    }
  }
}