package org.jetbrains.plugins.scala.lang.psi.controlFlow.impl.base

import org.jetbrains.plugins.scala.dfa.{DfConcreteBoolean, DfConcreteInt, DfValue}
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScBooleanLiteral, ScIntLiteral, ScLiteral, ScNullLiteral, ScStringLiteral}
import org.jetbrains.plugins.scala.lang.psi.controlFlow.CfgBuilder

trait ScLiteralCfgBuildingImpl { this: ScLiteral =>

  override def buildActualExpressionControlFlow(withResult: Boolean)(implicit builder: CfgBuilder): Unit = {
    this match {
      case ScNullLiteral() => builder.pushNull()
      case ScBooleanLiteral(value) => builder.push(DfValue.boolean(value))
      case ScIntLiteral(value) => builder.push(DfValue.int(value))
      case ScStringLiteral(value) => builder.push(DfValue.string(value))
      case _ => ???
    }

    if (!withResult) {
      builder.pop()
    }
  }
}
