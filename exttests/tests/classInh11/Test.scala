class Y2
trait Y3
class Test extends Object { self: Y2 with Y3 =>
  class A(x: Object) extends Object { self: Y3 =>
     object T1 extends Object { 
       class Y4 extends Object { self: Y3 =>
         class T3 extends Object { self: Y4 with Y3 =>
           class T5(x: Object) extends T2(x) { self => }
         }
       }
     }
     class T2(x: Object) extends Object { self => }
  }
  class X(x: Object) extends A(x) with Y3 { self => }
  trait U1 extends Object { self => }
  object U2 extends Object { self => }
}