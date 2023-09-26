package tetris.logic
case class Tetromino (val randIndex: Int){
  var anchorX : Int = 0
  var anchorY: Int = 0

  val anchor : Point = Point(anchorX,anchorY)

 def getTetrominoShape(randIndex : Int): Vector[Vector[Point]]  =
 {
   var newTetromino : Vector[Vector[Point]] = Vector[Vector[Point]] ()
   randIndex match
   {
     case 0 =>
       val firstRow = Vector[Point]()
       val secondRow = Vector[Point](Point(anchorX - 1, anchorY - 1), anchor,
         Point(anchorX + 1, anchorY + 1), Point(anchorX + 2, anchorY + 2))
       val tetromino = Vector[Vector[Point]] () :+ firstRow :+ secondRow
       newTetromino = tetromino
   }
   newTetromino
 }
}