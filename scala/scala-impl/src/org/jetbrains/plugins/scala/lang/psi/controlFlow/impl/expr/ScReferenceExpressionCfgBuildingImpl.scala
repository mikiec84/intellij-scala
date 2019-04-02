package org.jetbrains.plugins.scala.lang.psi.controlFlow.impl.expr

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.controlFlow.CfgBuilder

trait ScReferenceExpressionCfgBuildingImpl { this: ScReferenceExpression =>
  protected override def buildActualExpressionControlFlow(withResult: Boolean)(implicit builder: CfgBuilder): Unit = {

    bind() match {
      case Some(result) =>
        // todo: this is totally incomplete! (this needs to respect the correct context as well as qualified references)
        builder
          .pushCtx()
          .read(result.element)
      case None =>
        builder.pushAny()
    }


    if (!withResult) {
      builder.pop()
    }
  }
}
