object Test extends Object { self1 =>
  def a = 8
  object A extends Object { self2 =>
    def a = 7
    self1.a
    self2.a
  }
}