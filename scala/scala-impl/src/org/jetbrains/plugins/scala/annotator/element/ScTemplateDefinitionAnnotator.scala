package org.jetbrains.plugins.scala.annotator.element

import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.psi.{PsiMethod, PsiModifier}
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.annotator.AnnotatorUtils.ErrorAnnotationMessage
import org.jetbrains.plugins.scala.annotator.quickfix.{ImplementMethodsQuickFix, ModifierQuickFix}
import org.jetbrains.plugins.scala.annotator.template._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaModifier
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotationsHolder
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScNewTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScDeclaration, ScFunctionDefinition, ScTypeAliasDeclaration}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScModifierListOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.TypeDefinitionMembers
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, ValueClassType}
import org.jetbrains.plugins.scala.overrideImplement.{ScalaOIUtil, ScalaTypedMember}

object ScTemplateDefinitionAnnotator extends ElementAnnotator[ScTemplateDefinition] {
  override def annotate(element: ScTemplateDefinition, holder: AnnotationHolder, typeAware: Boolean): Unit = {
    annotateFinalClassInheritance(element, holder)
    annotateMultipleInheritance(element, holder)
    annotateNeedsToBeTrait(element, holder)
    annotateUndefinedMember(element, holder)
    annotateSealedclassInheritance(element, holder)
    annotateNeedsToBeAbstract(element, holder, typeAware)
    annotateNeedsToBeMixin(element, holder)

    if (typeAware) {
      annotateIllegalInheritance(element, holder)
      annotateObjectCreationImpossible(element, holder)
    }
  }

  // TODO package private
  def annotateFinalClassInheritance(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = {
    val newInstance = element.isInstanceOf[ScNewTemplateDefinition]
    val hasBody = element.extendsBlock.templateBody.isDefined

    if (newInstance && !hasBody) return

    superRefs(element).collect {
      case (range, clazz) if clazz.hasFinalModifier =>
        (range, ScalaBundle.message("illegal.inheritance.from.final.kind", kindOf(clazz, toLowerCase = true), clazz.name))
      case (range, clazz) if ValueClassType.extendsAnyVal(clazz) =>
        (range, ScalaBundle.message("illegal.inheritance.from.value.class", clazz.name))
    }.foreach {
      case (range, message) =>
        holder.createErrorAnnotation(range, message)
    }
  }

  def annotateIllegalInheritance(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = {
    element.selfTypeElement.flatMap(_.`type`().toOption).
      orElse(element.`type`().toOption)
      .foreach { ownType =>

        collectSuperRefs(element) {
          _.extractClassType
        }.foreach {
          case (range, (clazz: ScTemplateDefinition, substitutor)) =>
            clazz.selfType.filterNot { selfType =>
              ownType.conforms(substitutor(selfType))
            }.foreach { selfType =>
              holder.createErrorAnnotation(range, ScalaBundle.message("illegal.inheritance.self.type", ownType.presentableText, selfType.presentableText))
            }
          case _ =>
        }
      }
  }

  // TODO package private
  def annotateObjectCreationImpossible(element: ScTemplateDefinition, holder: AnnotationHolder) {
    val isNew = element.isInstanceOf[ScNewTemplateDefinition]
    val isObject = element.isInstanceOf[ScObject]

    if (!isNew && !isObject) return

    val refs = superRefs(element)

    val hasAbstract = refs.exists {
      case (_, clazz) => isAbstract(clazz)
    }

    if (hasAbstract) {
      refs match {
        case (defaultRange, _) :: _ =>
          val undefined = for {
            member <- ScalaOIUtil.getMembersToImplement(element)
            if member.isInstanceOf[ScalaTypedMember] // See SCL-2887
          } yield {
            try {
              (member.getText, member.getParentNodeDelegate.getText)
            } catch {
              case iae: IllegalArgumentException =>
                throw new RuntimeException("member: " + member.getText, iae)
            }
          }

          if (undefined.nonEmpty) {
            val range = element match {
              case _: ScNewTemplateDefinition => defaultRange
              case scalaObject: ScObject => scalaObject.nameId.getTextRange
            }

            val annotation = holder.createErrorAnnotation(range, objectCreationImpossibleMessage(undefined.toSeq: _*))
            annotation.registerFix(new ImplementMethodsQuickFix(element))
          }
        case _ =>
      }
    }
  }

  def annotateMultipleInheritance(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = {
    superRefs(element).groupBy(_._2).flatMap {
      case (clazz, entries) if isMixable(clazz) && entries.size > 1 => entries.map {
        case (range, _) => (range, ScalaBundle.message("illegal.inheritance.multiple", kindOf(clazz), clazz.name))
      }
      case _ => Seq.empty
    }.foreach {
      case (range, message) =>
        holder.createErrorAnnotation(range, message)
    }
  }

  def annotateNeedsToBeTrait(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = superRefs(element) match {
    case _ :: tail =>
      tail.collect {
        case (range, clazz) if !isMixable(clazz) =>
          (range, ScalaBundle.message("illegal.mixin", kindOf(clazz), clazz.name))
      }.foreach {
        case (range, message) =>
          holder.createErrorAnnotation(range, message)
      }
    case _ =>
  }

  // TODO package private
  def annotateUndefinedMember(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = {
    val isNew = element.isInstanceOf[ScNewTemplateDefinition]
    val isObject = element.isInstanceOf[ScObject]

    if (!isNew && !isObject) return

    element.physicalExtendsBlock.members.foreach {
      case _: ScTypeAliasDeclaration => // abstract type declarations are allowed
      case declaration: ScDeclaration =>
        val isNative = declaration match {
          case a: ScAnnotationsHolder => a.hasAnnotation("scala.native")
          case _ => false
        }
        if (!isNative) holder.createErrorAnnotation(declaration, ScalaBundle.message("illegal.undefined.member"))
      case _ =>
    }
  }

  // TODO test
  private def annotateSealedclassInheritance(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = element.getContainingFile match {
    case file: ScalaFile if !file.isCompiled =>
      val references = element match {
        case templateDefinition: ScNewTemplateDefinition if templateDefinition.extendsBlock.templateBody.isEmpty => Nil
        case _ => superRefs(element)
      }
      val fileNavigationElement = file.getNavigationElement

      references.collect {
        case (range, definition@ErrorAnnotationMessage(message))
          if definition.getContainingFile.getNavigationElement != fileNavigationElement =>
          (range, message)
      }.foreach {
        case (range, message) =>
          holder.createErrorAnnotation(range, message)
      }
    case _ =>
  }

  // TODO package private
  def annotateNeedsToBeAbstract(element: ScTemplateDefinition, holder: AnnotationHolder, typeAware: Boolean): Unit = element match {
    case _: ScNewTemplateDefinition | _: ScObject =>
    case _ if !typeAware || isAbstract(element) =>
    case _ =>
      ScalaOIUtil.getMembersToImplement(element, withOwn = true).collectFirst {
        case member: ScalaTypedMember /* SCL-2887 */ =>
          ScalaBundle.message(
            "member.implementation.required",
            kindOf(element),
            element.name,
            member.getText,
            member.getParentNodeDelegate.getText)
      }.foreach { message =>
        val nameId = element.nameId
        val fixes = {
          val maybeModifierFix = element match {
            case owner: ScModifierListOwner => Some(new ModifierQuickFix.Add(owner, nameId, ScalaModifier.Abstract))
            case _ => None
          }

          val maybeImplementFix = if (ScalaOIUtil.getMembersToImplement(element).nonEmpty) Some(new ImplementMethodsQuickFix(element))
          else None

          maybeModifierFix ++ maybeImplementFix

        }
        val annotation = holder.createErrorAnnotation(nameId, message)
        fixes.foreach(annotation.registerFix)
      }
  }

  def annotateNeedsToBeMixin(element: ScTemplateDefinition, holder: AnnotationHolder): Unit = {
    if (element.isInstanceOf[ScTrait]) return

    val nodes = TypeDefinitionMembers.getSignatures(element).allNodesIterator

    def isOverrideAndAbstract(definition: ScFunctionDefinition) =
      definition.hasModifierPropertyScala(PsiModifier.ABSTRACT) &&
        definition.hasModifierPropertyScala("override")

    for (node <- nodes) {
      node.info match {
        case PhysicalMethodSignature(function: ScFunctionDefinition, _) if isOverrideAndAbstract(function) =>
          val flag = node.supers.map(_.info.namedElement).forall {
            case f: ScFunctionDefinition => isOverrideAndAbstract(f)
            case _: ScBindingPattern => true
            case m: PsiMethod => m.hasModifierProperty(PsiModifier.ABSTRACT)
            case _ => true
          }

          for {
            place <- element match {
              case _ if !flag => None
              case typeDefinition: ScTypeDefinition => Some(typeDefinition.nameId)
              case templateDefinition: ScNewTemplateDefinition =>
                templateDefinition.extendsBlock.templateParents
                  .flatMap(_.typeElements.headOption)
              case _ => None
            }

            message = ScalaBundle.message(
              "mixin.required",
              kindOf(element),
              element.name,
              function.name,
              function.containingClass.name
            )
          } holder.createErrorAnnotation(place, message)
        case _ => //todo: vals?
      }
    }
  }

  def objectCreationImpossibleMessage(members: (String, String)*): String =
    s"Object creation impossible, since " + members.map {
      case (first, second) => s" member $first in $second is not defined"
    }.mkString("; ")
}
