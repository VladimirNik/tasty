class Y(x: Int) extends Object { self =>
}
class Test(y: Int) extends Y(y) { self =>
  class Test2(y: Int) extends Test(y) { self =>  
    private[this] val b = y
    private[this] val c = Test.this.y
  }  
}