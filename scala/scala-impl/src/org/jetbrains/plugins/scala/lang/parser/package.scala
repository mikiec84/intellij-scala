package org.jetbrains.plugins.scala
package lang

import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType

package object parser {

  implicit class PsiBuilderExt[B <: PsiBuilder](private val repr: B) extends AnyVal {

    def build(elementType: IElementType)
             (parse: B => Boolean): Boolean = {
      val marker = repr.mark()
      val result = parse(repr)

      if (result) marker.done(elementType)
      else marker.rollbackTo()

      result
    }

    def predict(parse: B => Boolean): Boolean = {
      val marker = repr.mark()
      repr.advanceLexer()

      val result = parse(repr)
      marker.rollbackTo()

      result
    }

    def checkedAdvanceLexer(): Unit = if (!repr.eof) {
      repr.advanceLexer()
    }

    def lookAhead(expected: IElementType, elementTypes: IElementType*): Boolean = repr.getTokenType match {
      case `expected` =>
        elementTypes.toList match {
          case Nil => true
          case list => predict(matchTokenTypes(_, list))
        }
      case _ => false
    }

    def lookBack(expected: IElementType): Boolean = {
      val (newSteps, _) = skipWhiteSpacesAndComments(1)
      val (_, actual) = skipWhiteSpacesAndComments(newSteps + 1)
      expected == actual
    }

    @annotation.tailrec
    final def skipWhiteSpacesAndComments(steps: Int,
                                         accumulator: IElementType = null): (Int, IElementType) =
      repr.getCurrentOffset match {
        case offset if steps < offset =>
          repr.rawLookup(-steps) match {
            case whiteSpace if TokenSets.WHITESPACE_OR_COMMENT_SET.contains(whiteSpace) => skipWhiteSpacesAndComments(steps + 1, whiteSpace)
            case result => (steps, result)
          }
        case _ => (steps, accumulator)
      }

    def invalidVarId: Boolean = !(repr.getTokenText match {
      case "" | "`" => false
      case text => text.head.isUpper || (text.head == '`' && text.last == '`')
    })
  }

  implicit class ScalaPsiBuilderExt(private val repr: parser.parsing.builder.ScalaPsiBuilder) extends AnyVal {

    def consumeTrailingComma(expectedBrace: IElementType): Boolean = {
      val result = repr.isTrailingComma &&
        repr.predict {
          expectedBrace == _.getTokenType && findPreviousNewLine.isDefined
        }

      if (result) {
        repr.advanceLexer()
      }
      result
    }

    def findPreviousNewLine: Option[String] = {
      val (steps, _) = repr.skipWhiteSpacesAndComments(1)

      val originalSubText = repr.getOriginalText.subSequence(
        repr.rawTokenTypeStart(1 - steps),
        repr.rawTokenTypeStart(0)
      ).toString
      if (originalSubText.contains('\n')) Some(originalSubText)
      else None
    }
  }

  @annotation.tailrec
  private[this] def matchTokenTypes(builder: PsiBuilder, list: List[IElementType]): Boolean = list match {
    case Nil => true
    case head :: tail if head == builder.getTokenType && !builder.eof =>
      builder.advanceLexer()
      matchTokenTypes(builder, tail)
    case _ => false
  }

}
