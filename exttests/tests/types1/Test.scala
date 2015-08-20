class K
class X[Y]
class G[A, B, C]
class Test extends X[String] {
  private[this] val test1: X[K] = null
  private[this] val test2: G[K, String, Int] = null
  private[this] val test3: G[Int, List[String], K] = null
}
