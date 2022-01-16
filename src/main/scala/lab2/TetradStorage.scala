package lab2

import lab2.TetradStorage.Tetrad

sealed trait TetradStorage {
  def put(tetrad: Tetrad): Unit
  def get: List[Tetrad]
  def getLast: Tetrad
  def numberOfTemporaryResults: Int
  def modifyTemp: Unit
  def ifExist(tetrad: Tetrad): Boolean
  def removeLast: Unit
  def modifyOperand1(tetrad: Tetrad, operand1: String): Unit
  def size: Int
}

object TetradStorage {
  final case class Result(name: String, value: Any) {
    override def toString: String = s"$name"
  }

  object Result {
    def empty: Result = Result("", Nil)
    def apply(name: Int, value: Any): Result = Result(s"T$name", value)
  }

  final case class Tetrad(operation: String, operand1: String, operand2: String, result: Result) {
    override def toString: String = s"$operation, $operand1, $operand2, $result"
  }

  object Tetrad {
    def empty: Tetrad = Tetrad("", "", "", Result.empty)
  }

  def apply(): TetradStorage = new TetradStorage {

    var tetrads: List[Tetrad] = List.empty
    var temps: Int = 1

    override def put(tetrad: Tetrad): Unit = {
      tetrads = tetrads ++ List(tetrad)
    }

    override def get: List[Tetrad] = tetrads

    override def getLast: Tetrad = tetrads.lastOption.getOrElse(Tetrad.empty)

    override def numberOfTemporaryResults: Int = temps

    override def modifyTemp: Unit = {
      temps = temps + 1
    }

    override def ifExist(tetrad: Tetrad): Boolean =
      tetrads.exists(curr => curr.operation == tetrad.operation && curr.operand1 == tetrad.operand1 && curr.operand2 == tetrad.operand2) ||
        tetrads.exists(curr => curr.operation == tetrad.operation && curr.operand1 == tetrad.operand1) ||
        tetrads.exists(curr => curr.operation == tetrad.operation && curr.operand2 == tetrad.operand2)

    override def removeLast: Unit = tetrads = tetrads.dropRight(1)

    override def modifyOperand1(tetrad: Tetrad, operand1: String): Unit = tetrads = tetrads.map {
      curr =>
        if(curr == tetrad)
          curr.copy(operand1 = operand1)
        else curr
    }
    ()

    override def size: Int = tetrads.size
  }
}
