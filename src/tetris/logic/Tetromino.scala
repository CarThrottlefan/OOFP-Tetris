package tetris.logic
case class Tetromino (){
  var randIndex : Int = 0
  var anchorX : Int = 0
  var anchorY: Int = 1
  var anchor : Point = Point(0,0)
  var body : Vector[Vector[Point]] = Vector[Vector[Point]] ()
  var cellType : CellType =  ICell

 def getTetrominoShape: Vector[Vector[Point]]  =
 {
   anchor = Point(anchorX,anchorY)
   var newTetromino : Vector[Vector[Point]] = Vector[Vector[Point]] ()
   randIndex match
   {
     //I
     case 0 => //TODO: Replace the lines below with a function
       val firstRow = Vector[Point]()
       val secondRow = Vector[Point](Point(anchorX - 1, anchorY), anchor,
         Point(anchorX + 1, anchorY), Point(anchorX + 2, anchorY))
       val tetromino = Vector[Vector[Point]] () :+ firstRow :+ secondRow
       newTetromino = tetromino

     //J
     case 1 =>
       val firstRow = Vector[Point](Point(anchorX - 1, anchorY - 1))
       val secondRow = Vector[Point](Point(anchorX - 1, anchorY), anchor,
         Point(anchorX + 1, anchorY))
       val tetromino = Vector[Vector[Point]]() :+ firstRow :+ secondRow
       newTetromino = tetromino
       cellType = JCell

     //L
     case 2 =>
       val firstRow = Vector[Point](Point(anchorX + 1, anchorY - 1))
       val secondRow = Vector[Point](Point(anchorX - 1, anchorY), anchor,
         Point(anchorX + 1, anchorY))
       val tetromino = Vector[Vector[Point]]() :+ firstRow :+ secondRow
       newTetromino = tetromino
       cellType = LCell

     //O
     case 3 =>
       val firstRow = Vector[Point](Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY - 1))
       val secondRow = Vector[Point](anchor, Point(anchorX + 1, anchorY))
       val tetromino = Vector[Vector[Point]]() :+ firstRow :+ secondRow
       newTetromino = tetromino
       cellType = OCell

     //S
     case 4 =>
       val firstRow = Vector[Point](Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY - 1))
       val secondRow = Vector[Point](Point(anchorX - 1, anchorY), anchor)
       val tetromino = Vector[Vector[Point]]() :+ firstRow :+ secondRow
       newTetromino = tetromino
       cellType = SCell

     //T
     case 5 =>
       val firstRow = Vector[Point](Point(anchorX, anchorY - 1))
       val secondRow = Vector[Point](Point(anchorX - 1, anchorY), anchor,
         Point(anchorX + 1, anchorY))
       val tetromino = Vector[Vector[Point]]() :+ firstRow :+ secondRow
       newTetromino = tetromino
       cellType = TCell

     //Z
     case 6 =>
       val firstRow = Vector[Point](Point(anchorX - 1, anchorY - 1), Point(anchorX, anchorY - 1))
       val secondRow = Vector[Point](anchor, Point(anchorX + 1, anchorY))
       val tetromino = Vector[Vector[Point]]() :+ firstRow :+ secondRow
       newTetromino = tetromino
       cellType = ZCell
   }
   newTetromino
 }

  def rotateTetromino(tetromino: Tetromino) : Tetromino =
  {
    tetromino.cellType match
    {
      case ICell =>
        //call class for this type of cell (newTetromino)
        //tetromino = newTetromino

      case JCell =>

      case LCell =>

      case OCell =>

      case SCell =>

      case TCell =>

      case ZCell =>

    }
    tetromino
  }
}