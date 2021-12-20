package lab2

import lab1.Lexeme
import lab2.TetradStorage.{Result, Tetrad}

object Analyzer {
  def apply(lexemes: List[Lexeme]): Either[ParseErrorMsg, List[Lexeme]] = {
    val cache = TetradStorage.apply()
    cache.put(Tetrad("BLOCK", "", "", Result.empty))
    val res = Op_If(lexemes, cache)
    cache.put(Tetrad("BLCKEND", "", "", Result.empty))
    cache.get.foreach(println(_))
    res
  }
}
