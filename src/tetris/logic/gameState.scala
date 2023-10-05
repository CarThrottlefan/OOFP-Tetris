package tetris.logic

class gameState(currTetromino : Tetromino, currBoard : Seq[Seq[CellType]])
{
  var tetromino : Tetromino = currTetromino
  var board : Seq[Seq[CellType]] = currBoard
  //var nonEmptyPoints : Map[Point, CellType] = tetromino.body
  /*def clearFullLine(): Seq[Seq[CellType]] = {
  }*/ //for each row check if in that row all cell types are !empty

}