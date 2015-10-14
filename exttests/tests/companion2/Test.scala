class Test extends Object {
  class X private () extends Object
  object X extends Object { def getInstance = new X() }
  X.getInstance
}