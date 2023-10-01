package tetris.logic
case class Tetromino (){
  var randIndex : Int = 0
  var anchor : Point = Point(0,0)
  var body : Vector[Point] = Vector[Point] ()
  var cellType : CellType =  ICell
  var relativeTetromino : Vector[Point] = Vector()

 def getTetrominoShape: Vector[Point]  =
 {
   val anchorX = anchor.x
   val anchorY = anchor.y
   //anchor = Point(anchorX,anchorY)
   var newTetromino : Vector[Point] = Vector()
   //var relativeTetromino : Vector[Point] = Vector()
   randIndex match
   {
     //I
     /*case 0 => //TODO: Replace the lines below with a function
       newTetromino = Vector(Point(anchorX - 1, anchorY), anchor, Point(anchorX + 1, anchorY), Point(anchorX + 2, anchorY))*/
     case 0 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(1, 0), Point(2, 0))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))

     //J
     case 1 =>
       /*newTetromino = Vector(
         Vector(Point(anchorX - 1, anchorY - 1), Point(anchorX - 1, anchorY), anchor, Point(anchorX + 1, anchorY))
       )*/
          relativeTetromino = Vector(Point(-1,-1),Point(-1,0), Point(0,0), Point(1,0))
          newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
          cellType = JCell

     //L
     case 2 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(1, 0), Point(1, -1))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = LCell

     case 3 =>
       relativeTetromino = Vector(Point(0, 0), Point(1, 0), Point(1, -1), Point(0, -1))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = OCell

     case 4 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(0, -1), Point(1, -1))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = SCell

     case 5 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(0, -1), Point(1, 0))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = TCell

     case 6 =>
       relativeTetromino = Vector(Point(-1, -1), Point(0, -1), Point(0, 0), Point(1, 0))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = ZCell
     /*case 2 =>
       newTetromino = Vector(
         Vector(Point(anchorX + 1, anchorY - 1), Point(anchorX - 1, anchorY), anchor, Point(anchorX + 1, anchorY))
       )
       cellType = LCell

     //O
     case 3 =>
       newTetromino = Vector(Vector(Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY - 1), Point(anchorX, anchorY), Point(anchorX + 1, anchorY)))
       cellType = OCell

     //S
     case 4 =>
       newTetromino = Vector(Vector(Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY - 1), anchor, Point(anchorX - 1, anchorY)))
       cellType = SCell

     //T
     case 5 =>
       newTetromino = Vector(Vector(Point(anchorX, anchorY - 1), Point(anchorX - 1, anchorY), anchor, Point(anchorX + 1, anchorY)))
       cellType = TCell

     //Z
     case 6 =>
       newTetromino = Vector(Vector(Point(anchorX - 1, anchorY - 1), Point(anchorX, anchorY - 1), anchor, Point(anchorX + 1, anchorY)))
       cellType = ZCell*/
   }
   newTetromino
 }
}