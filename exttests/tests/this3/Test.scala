class Test {
  class X {
     def a(p: Test) = p
     def b(p: X) = p
     a(Test.this)
     b(X.this)
  }
}