# tasty
One of the concepts introduced by scala.meta is AST persistence – the ability to persist trees at compile time after the typechecking and load them at runtime. TASTY is a typed AST serialization format.

One of the main targets of TASTY is to be the interchange format between languages implementing it. In general case trees serialized in one language should be deserializable in other languages supporting TASTY and vice versa.

The reference implementation of TASTY is a version for the Dotty programming language. Our goal is to implement TASTY support for scalac in a way that trees pickled from scalac could be unpickled by dotc and used for the compilation and analysis in the same way as native Dotty trees.

License for Dotty code: [text](LICENSE-Dotty.md)
