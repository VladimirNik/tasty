object Test extends Object { self1 =>
  class Test2 {
    object Test3 {
      object A extends Object { self2 =>
        private[this] val a = self1
      }
    }
  }
}