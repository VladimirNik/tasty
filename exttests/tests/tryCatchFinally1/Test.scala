class Test extends Object {                 
  def a = try { null; null.toString } catch { case _ => println("1"); println("Hello")}
  def b = try { null; println("42") } finally { println("2"); println("443") }
}