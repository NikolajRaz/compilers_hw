package lab2

sealed abstract class ParseErrorMsg(place: String) {
  def value: String
}

class ErrorInIfOperator(place: String) extends ParseErrorMsg(place) {
  val value = s"Error during parsing if_operator on $place"
}

class ErrorInExpOperator(place: String) extends ParseErrorMsg(place) {
  val value = s"Error during parsing expression on $place"
}

class ErrorInFuncOperator(place: String) extends ParseErrorMsg(place) {
  val value = s"Error during parsing function on $place"
}

class ErrorInInnerValueOperator(place: String) extends ParseErrorMsg(place) {
  val value = s"Error during parsing inner value on $place"
}

class ErrorInValueOperator(place: String) extends ParseErrorMsg(place) {
  val value = s"Error during parsing value on $place"
}

class ErrorInCompOperator(place: String) extends ParseErrorMsg(place) {
  val value = s"Error during parsing comparison operation on $place"
}