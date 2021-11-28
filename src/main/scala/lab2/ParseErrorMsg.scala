package lab2

sealed trait ParseErrorMsg {
  def value: String
}

object ParseErrorMsg {

  val if_token = "if"
  val then_token = "then"
  val comp_token = "<="
  val id_token = "id"
  val literal_token = "literal"
  val left_brace_token = "("
  val right_brace_token = ")"
  val state_end_token = ";"
  val point_token = "."
  val comma_token = ","
  val up_brace_token = "^"
  val minus_token = "-"

  def apply(nonParsedToken: String, place: String, name: String): ParseErrorMsg = new ParseErrorMsg {
    override def value: String = s"Error during parsing '$nonParsedToken' on '$place' while parsing $name"
  }
}