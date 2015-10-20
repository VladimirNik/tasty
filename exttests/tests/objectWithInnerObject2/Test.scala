class Test extends Object {
  object Test2 extends Object {
    object Test extends Object {
      def a(x: Test.type, y: Test2.type, z: Test) = 5
    }
  }
}