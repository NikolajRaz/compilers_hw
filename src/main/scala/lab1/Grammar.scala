package lab1

object Grammar {
  val delimiters: List[Char] = List(';', ':', ' ', '(', ')', '.', ',', '^','-')
  val smaller: Char = '<'
  val bigger: Char = '>'
  val equal: Char = '='
  val delimiters2: List[String] = List(smaller + bigger.toString, smaller + equal.toString)
  val keywords: List[String] = List("Boolean", "Word", "End", "then", "nil", "if", "of", "case", "inherited", "Begin", "procedure")
}
