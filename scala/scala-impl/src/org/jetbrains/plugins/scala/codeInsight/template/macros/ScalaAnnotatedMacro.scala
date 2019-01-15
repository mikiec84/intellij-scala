package org.jetbrains.plugins.scala
package codeInsight
package template
package macros

import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.codeInsight.template._
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.AnnotatedMembersSearch
import com.intellij.psi.{PsiClass, PsiMember}
import com.intellij.util.{EmptyQuery, Query}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager

import scala.collection.JavaConverters._

/**
 * @author Roman.Shein
 * @since 21.09.2015.
 */
class ScalaAnnotatedMacro extends ScalaMacro("macro.annotated") {

  protected def getAnnotatedMembers(params: Array[Expression], context: ExpressionContext): Query[PsiMember] = {
    (params, context) match {
      case (null, _) => EmptyQuery.getEmptyQuery[PsiMember]
      case (_, null) => EmptyQuery.getEmptyQuery[PsiMember]
      case _ if params.length > 0 => //TODO should params.length always equal 1?
        val project = context.getProject
        val scope = GlobalSearchScope.allScope(project)
        Option(params.head.calculateResult(context)).flatMap(res => ScalaPsiManager.instance(project).
                getCachedClass(scope, res.toString)).map(AnnotatedMembersSearch.search(_, scope)).
                getOrElse(EmptyQuery.getEmptyQuery[PsiMember])
    }
  }

  override def calculateResult(params: Array[Expression], context: ExpressionContext): Result = {
    Option(getAnnotatedMembers(params, context).findFirst()).map(member => new TextResult(member match {
      case psiClass: PsiClass => psiClass.getQualifiedName
      case _ => member.getName
    })).orNull
  }

  override def calculateQuickResult(params: Array[Expression], context: ExpressionContext): Result = calculateResult(params, context)

  override def calculateLookupItems(params: Array[Expression], context: ExpressionContext): Array[LookupElement] = {
    val secondParamName = if (params.length > 1) params(1).calculateResult(context).toString else null
    val isShortName = secondParamName != null && !secondParamName.toBoolean
    val project = context.getProject
    val outerClass: Option[PsiClass] = Option(secondParamName).flatMap { secondParamName =>
      ScalaPsiManager.instance(project).getCachedClass(GlobalSearchScope.allScope(project), secondParamName)
    }
    getAnnotatedMembers(params, context).findAll()
      .asScala
      .filter(outerClass.isDefined && outerClass.contains(_))
      .map {
        case psiClass: PsiClass if !isShortName => psiClass.getQualifiedName
        case notClass => notClass.getName
      }
      .toSet[String]
      .map(LookupElementBuilder.create)
      .toArray
  }

  override protected def message(nameKey: String): String = ScalaMacro.message(nameKey)
}