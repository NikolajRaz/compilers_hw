package lab1

object Scanner {
  def analyze(string: String): List[Either[ErrorMsg, Lexeme]] = fillLexemeChart(string.toList)

  private def fillLexemeChart(string: List[Char], accumulatedLexeme: List[Char] = List.empty): List[Either[ErrorMsg, Lexeme]] = {
    string match {
      case head :: tail => head match {
        case letter if letter.isLetter                           =>
          fillLexemeChart(tail, accumulatedLexeme ++ List(letter))
        case literal if literal.isDigit                          =>
          List(Right(Literal(literal.toString))) ++ fillLexemeChart(tail, List.empty)
        case delimiter if Grammar.delimiters.contains(delimiter) =>
          checkDelimiter(accumulatedLexeme, delimiter, tail)
        case Grammar.smaller                                     =>
          checkTwoPositionedDelimiter(tail)
        case Grammar.bigger                                      =>
          checkTwoPositionedDelimiter(accumulatedLexeme, tail)
        case Grammar.equal                                       =>
          checkTwoPositionedDelimiter(accumulatedLexeme, tail)
        case _                                                   =>
          List(Left(new WrongSymbolError))
      }
      case _ =>
        accumulatedLexeme.headOption match {
          case Some(letter) if letter.isLetter                           =>
            List(Right(checkKeyWord(accumulatedLexeme.mkString)))
          case Some(delimiter) if Grammar.delimiters.contains(delimiter) =>
            List(Right(Delimiter(delimiter.toString)))
          case Some(_)                                                   =>
            List(Left(new WrongSymbolError))
          case None                                                      =>
            List.empty
        }
    }
  }

  private def checkDelimiter(ac: List[Char], delimiter: Char, tail: List[Char]): List[Either[ErrorMsg, Lexeme]] =
    ac.lastOption match {
      case Some(letter) if letter.isLetter =>
        List(Right(checkKeyWord(ac.mkString))) ++
          List(Right(Delimiter(delimiter.toString))) ++
          fillLexemeChart(tail)
      case Some(Grammar.smaller) => List(Left(new TwoPositionedDelimiterError))
      case _ =>  List(Right(Delimiter(delimiter.toString))) ++ fillLexemeChart(tail)
    }

  private def checkKeyWord(identifier: String): Lexeme =
    if(Grammar.keywords.contains(identifier)) KeyWord(identifier)
    else Identifier(identifier)

  private def checkTwoPositionedDelimiter(tail: List[Char]): List[Either[ErrorMsg, Lexeme]] =
    tail.headOption match {
      case Some(Grammar.bigger) => List(Right(TwoPositionedDelimiter(Grammar.smaller + Grammar.bigger.toString))) ++ fillLexemeChart(tail, List(Grammar.smaller))
      case Some(Grammar.equal) => List(Right(TwoPositionedDelimiter(Grammar.smaller + Grammar.equal.toString))) ++ fillLexemeChart(tail, List(Grammar.smaller))
      case _ => List(Left(new TwoPositionedDelimiterError))
    }

  private def checkTwoPositionedDelimiter(ac: List[Char], tail: List[Char]): List[Either[ErrorMsg, Lexeme]] =
    ac.headOption match {
      case Some(Grammar.smaller) => fillLexemeChart(tail)
      case _ => List(Left(new TwoPositionedDelimiterError))
    }
}
