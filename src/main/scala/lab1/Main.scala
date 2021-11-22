package lab1

import cats.data.EitherT

import scala.io.Source

object Main extends App {
  val input = "input.txt"
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

  if(scanner.isLeft.contains(true)){}
  else {
    val result = scanner.collectRight
    //Последовательный вывод по мере того как лексемы встречаются в тексте
    //result.foreach(v => println(s"${v.name} | ${v.value}"))

    //Вывод сгруппированных по типу и значению лексем
    val ids = result.collect { case x: Identifier => x }
      .groupBy(_.value)
      .map(_._2.head)
    ids.foreach(v => println(s"${v.name} | ${v.value}"))
    val keywords = result.collect { case x: KeyWord => x }
      .groupBy(_.value)
      .map(_._2.head)
    keywords.foreach(v => println(s"${v.name} | ${v.value}"))
    val delimiters = result.collect { case x: Delimiter => x }
      .groupBy(_.value)
      .map(_._2.head)
    delimiters.foreach(v => println(s"${v.name} | ${v.value}"))
    val delimiters2 = result.collect { case x: TwoPositionedDelimiter => x }
      .groupBy(_.value)
      .map(_._2.head)
    delimiters2.foreach(v => println(s"${v.name} | ${v.value}"))
    val literals = result.collect { case x: Literal => x }
      .groupBy(_.value)
      .map(_._2.head)
    literals.foreach(v => println(s"${v.name} | ${v.value}"))

  }
}
