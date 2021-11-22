package lab2

import lab1.{Identifier, Lexeme, Literal}

sealed trait Procedure {
  def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]]
}

case object Op_If extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == "if" =>
        for {
          after_comp <- Comp(tail)
          result <- analyze(after_comp)
        } yield result
      case _ => Left(new ErrorInIfOperator(lexemes.map(_.value).mkString))
    }
  }

  private def analyze(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == "then" => Exp(tail).flatMap {
        value => value.headOption match {
          case Some(symbol) if symbol.value == ";" => Right(value.drop(1))
          case _ => Left(new ErrorInIfOperator(lexemes.map(_.value).mkString))
        }
      }
      case _ => Left(new ErrorInIfOperator(lexemes.map(_.value).mkString))
    }
  }
}

case object Exp extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Func(lexemes) match {
      case Right(value) => value match {
        case head :: tail if head.value == "." => Val(tail)
        case _ => Right(value)
      }
      case Left(_) => checkForVal(lexemes)
    }
  }

  def checkForVal(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Val(lexemes).flatMap(value => checkForExp(value))
  }

  def checkForExp(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == "-" => Exp(tail)
      case _ => Right(lexemes)
    }
  }
}

case object Func extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Identifier) if lexemes.tail.nonEmpty => lexemes.tail match {
        case head :: tail if head.value == "(" => analyze(tail)
        case head :: tail if head.value == "." => analyzeEmbeddedFunc(tail)
        case any => Left(new ErrorInFuncOperator(any.map(_.value).mkString))
      }
      case _ => Left(new ErrorInFuncOperator(lexemes.map(_.value).mkString))
    }
  }

  def analyzeEmbeddedFunc(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Identifier) if lexemes.tail.nonEmpty => lexemes.tail match {
        case head :: tail if head.value == "(" => analyze(tail)
        case any => Left(new ErrorInFuncOperator(any.map(_.value).mkString))
      }
      case _ => Left(new ErrorInFuncOperator(lexemes.map(_.value).mkString))
    }
  }


  def analyze(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Param(lexemes).flatMap {
      case head :: tail if head.value == ")" => Right(tail)
      case any => Left(new ErrorInFuncOperator(any.map(_.value).mkString))
    }
  }
}

case object Param extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Exp(lexemes).flatMap {
      case head :: tail if head.value == "," => Param(tail)
      case any => Right(any)
    }
  }
}

case object Val extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Literal) => Right(lexemes.tail)
      case _ => checkInnerVal(lexemes)
    }
  }

  def checkInnerVal(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
      Inner_Val(lexemes).flatMap {
        case head :: tail if head.value == "." => Val(tail)
        case _ => Right(lexemes.tail)
      }
  }
}

case object Inner_Val extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Identifier) if lexemes.tail.nonEmpty => lexemes.tail match {
        case head :: tail if head.value == "^" => Right(tail)
        case _ => Right(lexemes.tail)
      }
      case _ => Left(new ErrorInInnerValueOperator(lexemes.map(_.value).mkString))
    }
  }
}

case object Comp extends Procedure {
  override def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Exp(lexemes).flatMap {
      case head :: tail if head.value == "<=" => Exp(tail)
      case _ => Left(new ErrorInCompOperator(lexemes.map(_.value).mkString))
    }
  }
}