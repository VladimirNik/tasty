class Test {
	def a = this
	def b(c: Test) = c
	b(a)
}