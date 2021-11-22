package lab1

trait ErrorMsg {
  def value: String
}

class WrongSymbolError extends ErrorMsg {
  val value = "Wrong symbol"
}

class TwoPositionedDelimiterError extends ErrorMsg {
  val value =  "Wrong symbol in two positioned delimiter"
}

class ErrorDuringTheScan extends ErrorMsg {
  val value = "Happens error during the text scanning"
}
