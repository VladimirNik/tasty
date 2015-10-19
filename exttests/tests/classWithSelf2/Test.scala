class Y extends Object { def u = 9 }
class Test extends Object { self: Y =>
  def b = 9
  self.b
  self.u
}
