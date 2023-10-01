package tetris.logic

/*case class rotateTetromino(tetromino: Tetromino)
{
  class rotateIcell(tetromino: Tetromino)
  {
    val tetrominoBody: Vector[Vector[Point]] = tetromino.body
    val rotatedTetromino: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => rotateLeft(point, 'I')))
    //rotateLeft(tetromino, 0)
  }

  class rotateOCell(tetromino: Tetromino) {
    val tetrominoBody: Vector[Vector[Point]] = tetromino.body
    val rotatedTetromino: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => rotateLeft(point, 'O')))
  }

  class rotateOtherCell(tetromino: Tetromino)
  {
    val tetrominoBody: Vector[Vector[Point]] = tetromino.body
    val rotatedTetromino: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => rotateLeft(point, ' ')))
  }

  def rotateLeft(point: Point, typeOfTetromino : Char) : Point =
  {
    //val tetrominoBody = tetromino.body
    //val rotatedTetromino = tetrominoBody.map(row => row.map(point => point.x = point.y, point.y = 0 - point.x))
    typeOfTetromino match
    {
      case 'I' =>
        val newX = point.y
        val newY = 0 - point.x + 1
        Point(newX, newY)

      case ' ' =>
        val newX = point.y
        val newY = 0 - point.x
        Point(newX, newY)

      case 'O' =>
        Point(point.x, point.y)
    }
  }
}
*/

abstract class abstractTetromino {
  def rotateLeft(point: Point, typeOfTetromino: Char): Point = {
    //val tetrominoBody = tetromino.body
    //val rotatedTetromino = tetrominoBody.map(row => row.map(point => point.x = point.y, point.y = 0 - point.x))
    typeOfTetromino match {
      case 'I' =>
        val newX = point.y
        val newY = 0 - point.x + 1
        Point(newX, newY)

      case ' ' =>
        val newX = point.y
        val newY = 0 - point.x
        Point(newX, newY)

      case 'O' =>
        Point(point.x, point.y)
    }
  }

  def rotateRight(point: Point, typeOfTetromino: Char): Point = {
    typeOfTetromino match {
      case 'I' =>
        val newX = 0 - point.y + 1
        val newY = point.x
        Point(newX, newY)

      case ' ' =>
        val newX = 0 - point.y
        val newY = point.x
        Point(newX, newY)

      case 'O' =>
        Point(point.x, point.y)
    }
  }
}

class rotateIcell(tetromino: Tetromino) extends abstractTetromino {
  val tetrominoBody: Vector[Vector[Point]] = tetromino.body
  val rotatedLeft: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => super.rotateLeft(point, 'I')))
  val rotatedRight: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => super.rotateRight(point, 'I')))
}

class rotateOtherCell(tetromino: Tetromino) extends abstractTetromino {
  val tetrominoBody: Vector[Vector[Point]] = tetromino.body
  val rotatedLeft: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => super.rotateLeft(point, ' ')))
  val rotatedRight: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => super.rotateRight(point, ' ')))
}

class rotateOCell(tetromino: Tetromino) extends abstractTetromino {
  val tetrominoBody: Vector[Vector[Point]] = tetromino.body
  val rotatedLeft: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => super.rotateLeft(point, 'O')))
  val rotatedRight: Vector[Vector[Point]] = tetrominoBody.map(row => row.map(point => super.rotateRight(point, 'O')))
}



