package tetris.logic

class gameState(currBoard : Seq[Seq[CellType]])
{
  var tetromino : Tetromino = Tetromino()
  var board : Seq[Seq[CellType]] = currBoard
  var gameOver : Boolean = false
  def clearLine(board: Seq[Seq[CellType]], rowIndex : Int) : Seq[Seq[CellType]] =
  {
    var newBoard : Seq[Seq[CellType]] = board

    val newEmptyRow: Seq[CellType] =
    {
      val length = board.head.length
      var row: Seq[CellType] = Seq()

      for (i <- 0 until length)
      {
        row = row :+ Empty
      }
      row
    }

    for(i <- rowIndex to 1 by -1)
    {
      newBoard = newBoard.updated(i, board(i - 1))
      val x : Int = 1
      val u : Int = x + 1
    }
    newBoard = newBoard.updated(0, newEmptyRow)

    return newBoard
  }
}