package lab1

trait Lexeme {
  def name: String
  def value: String = ""
}

class Identifier extends Lexeme {
  def name = "Identifier"
}
object Identifier {
  def apply(v: String): Identifier = {
    new Identifier {
      override def value: String = v
    }
  }
}

class KeyWord extends Lexeme {
  def name = "KeyWord"
}
object KeyWord {
  def apply(v: String): KeyWord = {
    new KeyWord {
      override def value: String = v
    }
  }
}

class Delimiter extends Lexeme {
  def name = "Delimiter"
}
object Delimiter {
  def apply(v: String): Delimiter = {
    new Delimiter {
      val delim: String = if(v == " ") "SPACE" else v
      override def value: String = delim
    }
  }
}

class TwoPositionedDelimiter extends Lexeme {
  def name = "TwoPositionedDelimiter"
}
object TwoPositionedDelimiter {
  def apply(v: String): TwoPositionedDelimiter = new TwoPositionedDelimiter {
    override def value: String = v
  }
}

class Literal extends Lexeme {
  def name = "literal"
}

object Literal {
  def apply(v: String): Literal = {
    new Literal {
      override def value: String = v
    }
  }
}
