package lab2

import lab1.{Identifier, Lexeme, Literal}
import ParseErrorMsg._
import lab2.TetradStorage.{Result, Tetrad}

sealed trait Procedure {
  def name: String
  def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]]
  def getOperationName(lexemes: List[Lexeme], acum: String = "", cache: TetradStorage): String =
    if(lexemes.headOption.map(_.value).getOrElse("") == point_token)
      getOperationName(lexemes.drop(2), acum + "." + lexemes.drop(1).headOption.map(_.value).getOrElse(""), cache)
    else {
      if(lexemes.headOption.map(_.value).getOrElse("") == up_brace_token)
        getOperationName(lexemes.drop(1), acum + lexemes.headOption.map(_.value).getOrElse(""), cache)
      else
        if(lexemes.headOption.map(_.value).getOrElse("") == left_brace_token)
          cache.getLast.result.name + afterFunc(lexemes.drop(1), cache)
        else
          acum
    }

  def afterFunc(lexeme: List[Lexeme], cache: TetradStorage): String =
    if(lexeme.headOption.map(_.value).contains(right_brace_token))
      getOperationName(lexeme.drop(1), cache = cache)
    else {
      if(lexeme.isEmpty)
        ""
      else afterFunc(lexeme.drop(1), cache)
    }

  def minusTetrads(lexemes: List[Lexeme], cache: TetradStorage, acc: String = ""): String = {
    var inner_tetrad = Tetrad.empty.copy(
      operation = minus_token,
      operand1 = acc + getOperationName(lexemes.drop(1), lexemes.headOption.map(_.value).getOrElse(""), cache))
    if (lexemes.drop(1).headOption.map(_.value).getOrElse("") == minus_token){
      val result = minusTetrads(lexemes.drop(2), cache)
      val tempNum = cache.numberOfTemporaryResults
      inner_tetrad = inner_tetrad.copy(operand2 = result, result = Result(tempNum, ""))
      if(!cache.ifExist(inner_tetrad)) {
        cache.modifyTemp
        cache.put(inner_tetrad)
      }
      s"T$tempNum"
    }
    else
      if (lexemes.drop(1).headOption.map(_.value).getOrElse("") == point_token) {
        minusTetrads(lexemes.drop(2), cache, acc + lexemes.headOption.map(_.value).getOrElse("") + ".")
      }
      else
        acc + getOperationName(lexemes.drop(1), lexemes.headOption.map(_.value).getOrElse(""), cache)
  }
}

case object Op_If extends Procedure {
  val name = "operator_if"
  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
    var inner_tetrad = Tetrad.empty.copy(operation = "BP")

    lexemes match {
      case head :: tail if head.value == if_token =>
        for {
          after_comp <- Comp[F](tail, cache)
          lastResult = cache.getLast.result.name
          modified = inner_tetrad.copy(operand2 = lastResult)
          _          = cache.put(modified)
          result <- analyze(after_comp, cache)
          _ = cache.modifyOperand1(modified, cache.size.toString)
        } yield result
      case _ => Left(ParseErrorMsg(if_token, lexemes.map(_.value).mkString, name))
    }
  }

  private def analyze(lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes match {
      case head :: tail if head.value == then_token => Exp(tail, cache).flatMap {
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

  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {

    var inner_tetrad = Tetrad.empty

    def checkForLiteral(lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
      lexemes.headOption match {
        case Some(_: Literal) =>
          inner_tetrad = inner_tetrad.copy(operand1 = getOperationName(lexemes.drop(1), lexemes.headOption.map(_.value).getOrElse(""), cache))
          checkForExp(lexemes.tail, cache)
        case _ => checkForVal(lexemes, cache)
      }
    }

    def checkForVal(lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
      Inner_Val(lexemes, cache).flatMap{ value =>
        inner_tetrad = inner_tetrad.copy(operand1 = getOperationName(lexemes.drop(1), lexemes.headOption.map(_.value).getOrElse(""), cache))
        checkForExp(value, cache)
      }
    }

    def checkForExp(lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
      lexemes match {
        case head :: tail if head.value == minus_token =>
          inner_tetrad = inner_tetrad.copy(
            operation = minus_token,
            operand2 = minusTetrads(tail, cache),
            result = Result(cache.numberOfTemporaryResults, ""))
          if(!cache.ifExist(inner_tetrad)) {
            cache.modifyTemp
            cache.put(inner_tetrad)
          }
          Exp(tail, cache)
        case _ => Right(lexemes)
      }
    }

    Func(lexemes, cache) match {
      case Right(value) => value match {
        case head :: tail if head.value == point_token => Inner_Val(tail, cache)
        case _ => Right(value)
      }
      case Left(a) => checkForLiteral(lexemes, cache)
    }

  }

}

case object Func extends Procedure {
  val name = "function"

  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {

    var inner_tetrad_cache: Tetrad = Tetrad.empty

    def getOperationNameInParam(lexemes: List[Lexeme], acum: String = "", cache: TetradStorage): String =
      if(lexemes.headOption.map(_.value).getOrElse("") == point_token)
        getOperationNameInParam(lexemes.drop(2), acum + "." + lexemes.drop(1).headOption.map(_.value).getOrElse(""), cache)
      else {
        if(lexemes.headOption.map(_.value).getOrElse("") == up_brace_token)
          getOperationNameInParam(lexemes.drop(1), acum + lexemes.headOption.map(_.value).getOrElse(""), cache)
        else
          if(lexemes.headOption.map(_.value).getOrElse("") == left_brace_token || lexemes.headOption.map(_.value).getOrElse("") == minus_token)
            cache.getLast.result.name
          else
            acum
      }

    def getFunctionName(lexemes: List[Lexeme], acum: String = "", cache: TetradStorage): String =
      if(lexemes.headOption.map(_.value).getOrElse("") == point_token)
        getFunctionName(lexemes.drop(2), acum + "." + lexemes.drop(1).headOption.map(_.value).getOrElse(""), cache)
      else {
        if(lexemes.headOption.map(_.value).getOrElse("") == up_brace_token)
          getFunctionName(lexemes.drop(1), acum + lexemes.headOption.map(_.value).getOrElse(""), cache)
        else acum
      }

    def analyze(lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
      Param(lexemes, cache).flatMap {
        case head :: tail if head.value == right_brace_token =>
          cache.put(inner_tetrad_cache.copy(operand1 = getOperationNameInParam(lexemes.drop(1), lexemes.headOption.map(_.value).getOrElse(""), cache), operand2 = secondOperand(lexemes.drop(1)), result = Result(s"T${cache.numberOfTemporaryResults}", Nil)))
          cache.modifyTemp
          Right(tail)
        case any => Left(ParseErrorMsg(right_brace_token, any.map(_.value).mkString, name))
      }
    }

    def secondOperand(lexemes: List[Lexeme]): String =
      if(lexemes.headOption.map(_.value).getOrElse("") != right_brace_token) {
        if(lexemes.headOption.map(_.value).getOrElse("") == comma_token)
          getOperationNameInParam(lexemes.drop(2), lexemes.drop(1).headOption.map(_.value).getOrElse(""), cache)
        else
          lexemes.headOption.map(_.value).getOrElse("")
      }
      else ""

    Inner_Val(lexemes, cache).flatMap {
      case head :: tail if head.value == left_brace_token =>
        inner_tetrad_cache = inner_tetrad_cache.copy(operation = getFunctionName(lexemes.drop(1), lexemes.headOption.map(_.value).getOrElse(""), cache))
        analyze(tail, cache)
      case any => Left(ParseErrorMsg(left_brace_token, any.map(_.value).mkString, name))
    }
  }
}

case object Param extends Procedure {
  val name = "parameters"

  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
    Exp(lexemes, cache).flatMap {
      case head :: tail if head.value == comma_token => Param(tail, cache)
      case any => Right(any)
    }
  }
}

case object Val extends Procedure {
  val name = "value"

  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
    lexemes.headOption match {
      case Some(_: Identifier) => checkForUpBrace(lexemes.tail, cache)
      case _ => Left(ParseErrorMsg(id_token, lexemes.map(_.value).mkString, name))
    }
  }

  def checkForUpBrace(lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
      lexemes match {
        case head :: tail if head.value == up_brace_token => Right(tail)
        case any => Right(any)
      }
  }
}

case object Inner_Val extends Procedure {
  val name = "inner_value"

  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
    Val(lexemes, cache).flatMap {
      case head :: tail if head.value == point_token => Inner_Val(tail, cache)
      case any => Right(any)
    }
  }
}

case object Comp extends Procedure {
  val name = "operation_comparison"

  override def apply[F[_]](lexemes: List[Lexeme], cache: TetradStorage): Either[ParseErrorMsg, List[Lexeme]] = {
    var inner_tetrad = Tetrad.empty
    Exp(lexemes, cache).flatMap {
      case head :: tail if head.value == comp_token =>
        inner_tetrad = inner_tetrad.copy(
          operation = "-",
          operand1 = minusTetrads(lexemes, cache),
          operand2 = minusTetrads(tail, cache),
          result = Result(cache.numberOfTemporaryResults, ""))
        if(inner_tetrad.operand1 == s"T${cache.numberOfTemporaryResults}")
          inner_tetrad = inner_tetrad.copy(operand1 = s"T${cache.numberOfTemporaryResults-1}")
        cache.modifyTemp
        cache.put(inner_tetrad)
        Exp(tail, cache)
      case _ => Left(ParseErrorMsg(comp_token, lexemes.map(_.value).mkString, name))
    }
  }
}