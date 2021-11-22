package lab2

import lab1.Lexeme

object Analyzer {
  def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    Op_If(lexemes)
  }
}
