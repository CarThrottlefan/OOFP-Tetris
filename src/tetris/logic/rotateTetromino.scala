package tetris.logic

abstract class abstractTetromino(tetromino: Tetromino)
{
  def rotateLeft(typeOfTetromino: Char): (Vector[Point], Vector[Point]) =
  {
    typeOfTetromino match
    {
      case 'I' =>
        val rotatedRelative = tetromino.relativeTetromino.map(point => Point(point.y, -point.x + 1))
        val rotatedTetromino = rotatedRelative.map(point => Point(tetromino.anchor.x + point.x, tetromino.anchor.y + point.y))
        return (rotatedTetromino, rotatedRelative)

      case ' ' =>
        val rotatedRelative = tetromino.relativeTetromino.map(point => Point(point.y, -point.x))
        val rotatedTetromino = rotatedRelative.map(point => Point(tetromino.anchor.x + point.x, tetromino.anchor.y + point.y))
        return (rotatedTetromino, rotatedRelative)

      case 'O' =>
        return (tetromino.body, tetromino.relativeTetromino)
    }
  }

  def rotateRight(typeOfTetromino: Char): (Vector[Point], Vector[Point]) =
  {
    typeOfTetromino match
    {
      case 'I' =>
        val rotatedRelative = tetromino.relativeTetromino.map(point => Point(-point.y + 1, point.x))
        val rotatedTetromino = rotatedRelative.map(point => Point(tetromino.anchor.x + point.x, tetromino.anchor.y + point.y))
        return (rotatedTetromino, rotatedRelative)

      case ' ' =>
        val rotatedRelative = tetromino.relativeTetromino.map(point => Point(-point.y, point.x))
        val rotatedTetromino = rotatedRelative.map(point => Point(tetromino.anchor.x + point.x, tetromino.anchor.y + point.y))
        return (rotatedTetromino, rotatedRelative)

      case 'O' =>
        return (tetromino.body, tetromino.relativeTetromino)
    }
  }
}

class rotateICell(tetromino: Tetromino) extends abstractTetromino(tetromino: Tetromino)
{
  val (rotatedLeftBody: Vector[Point], rotatedLeftRelative: Vector[Point]) = rotateLeft('I')
  val (rotatedRightBody: Vector[Point], rotatedRightRelative: Vector[Point]) = rotateRight('I')
}

class rotateOtherCell(tetromino: Tetromino) extends abstractTetromino(tetromino: Tetromino)
{
  val (rotatedLeftBody: Vector[Point], rotatedLeftRelative: Vector[Point]) = rotateLeft(' ')
  val (rotatedRightBody: Vector[Point], rotatedRightRelative: Vector[Point]) = rotateRight(' ')
}

class rotateOCell(tetromino: Tetromino) extends abstractTetromino(tetromino: Tetromino)
{
  val (rotatedLeftBody: Vector[Point], rotatedLeftRelative: Vector[Point]) = rotateLeft('O')
  val (rotatedRightBody: Vector[Point], rotatedRightRelative: Vector[Point]) = rotateRight('O')
}
