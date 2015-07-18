package scala.tasty.internal

import scala.tools.nsc.Global
import scala.tasty.internal.dotc.ast.TTrees
import scala.tasty.internal.dotc.core._
import scala.tasty.internal.dotc.core.tasty._

class API(val global: Global) extends TTrees
                              with TAnnotations
                              with TConstants
                              with TContexts
                              with TDenotations
                              with TSymbols
                              with TSymDenotations
                              with TTypeApplications
                              with TTypes
                              
class PicklerAPI(override val global: Global) extends API(global)
                                              with PositionPicklers
                                              with TastyPicklers
                                              with TastyPrinters
                                              with TreeBuffers
                                              with TreePicklers