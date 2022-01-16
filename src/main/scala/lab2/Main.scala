package lab2

import cats.data.EitherT
import lab1.Scanner

import scala.io.Source

object Main extends App {
  val input = "input2.txt"
  val bufferedSource = Source.fromFile(input)
  val scanner = EitherT(bufferedSource.getLines().toList.flatMap { line =>
    try {
      val result = Scanner.analyze(line)
      EitherT(result).leftMap(error => println(s"${error.value} in '$line'"))
      result
    } catch {
      case e: Exception =>
        println(s"Error: ${e.getMessage}")
        List.empty
    }
  })

  val result = scanner.collectRight.filterNot(lexeme => lexeme.value == "SPACE")

  println{
    Analyzer(result) match {
      case Right(_) => ""
      case Left(error) => error.value
    }
  }
}
