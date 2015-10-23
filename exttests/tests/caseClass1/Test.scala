case class Test() {
  //due to incorrect exceptions processing in FromTasty (throw new Exception() problem)
  override def productElement(g: Int): Any = null
}