package lab2

import lab1.{Identifier, Lexeme, Literal}
import ParseErrorMsg._

sealed trait Procedure {
  def name: String
  def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]]
}

case object Op_If extends Procedure {
  val name = "operator_if"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == if_token =>
        for {
          after_comp <- Comp(tail)
          result <- analyze(after_comp)
        } yield result
      case _ => Left(ParseErrorMsg(if_token, lexemes.map(_.value).mkString, name))
    }
  }

  private def analyze(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == then_token => Exp(tail).flatMap {
        value => value.headOption match {
          case Some(symbol) if symbol.value == state_end_token => Right(value.drop(1))
          case _ => Left(ParseErrorMsg(state_end_token, value.map(_.value).mkString, name))
        }
      }
      case _ => Left(ParseErrorMsg(then_token, lexemes.map(_.value).mkString, name))
    }
  }
}

case object Exp extends Procedure {
  val name = "expression"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Func(lexemes) match {
      case Right(value) => value match {
        case head :: tail if head.value == point_token => Inner_Val(tail)
        case _ => Right(value)
      }
      case Left(a) => checkForLiteral(lexemes)
    }
  }

  def checkForLiteral(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Literal) => Right(lexemes.tail)
      case _ => checkForVal(lexemes)
    }
  }

  def checkForVal(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Inner_Val(lexemes).flatMap(value => checkForExp(value))
  }

  def checkForExp(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == minus_token => Exp(tail)
      case _ => Right(lexemes)
    }
  }
}

case object Func extends Procedure {
  val name = "function"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Inner_Val(lexemes).flatMap {
      case head :: tail if head.value == left_brace_token => analyze(tail)
      case any => Left(ParseErrorMsg(left_brace_token, any.map(_.value).mkString, name))
    }
  }

  def analyzeEmbeddedFunc(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Identifier) if lexemes.tail.nonEmpty => lexemes.tail match {
        case head :: tail if head.value == left_brace_token => analyze(tail)
        case any => Left(ParseErrorMsg(left_brace_token, any.map(_.value).mkString, name))
      }
      case _ => Left(ParseErrorMsg(id_token, lexemes.map(_.value).mkString, name))
    }
  }


  def analyze(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Param(lexemes).flatMap {
      case head :: tail if head.value == right_brace_token => Right(tail)
      case any => Left(ParseErrorMsg(right_brace_token, any.map(_.value).mkString, name))
    }
  }
}

case object Param extends Procedure {
  val name = "parameters"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Exp(lexemes).flatMap {
      case head :: tail if head.value == comma_token => Param(tail)
      case any => Right(any)
    }
  }
}

case object Val extends Procedure {
  val name = "value"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Identifier) => checkForUpBrace(lexemes.tail)
      case _ => Left(ParseErrorMsg(id_token, lexemes.map(_.value).mkString, name))
    }
  }

  def checkForUpBrace(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
      lexemes match {
        case head :: tail if head.value == up_brace_token => Right(tail)
        case any => Right(any)
      }
  }
}

case object Inner_Val extends Procedure {
  val name = "inner_value"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Val(lexemes).flatMap {
      case head :: tail if head.value == point_token => Inner_Val(tail)
      case any => Right(any)
    }
  }
}

case object Comp extends Procedure {
  val name = "operation_comparison"

  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Exp(lexemes).flatMap {
      case head :: tail if head.value == comp_token => Exp(tail)
      case _ => Left(ParseErrorMsg(comp_token, lexemes.map(_.value).mkString, name))
    }
  }
}