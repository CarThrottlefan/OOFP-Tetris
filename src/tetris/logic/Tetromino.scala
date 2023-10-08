package tetris.logic
case class Tetromino (){
  var randIndex : Int = 0
  var anchor : Point = Point(0,0)
  var body : Vector[Point] = Vector[Point] ()
  var cellType : CellType =  ICell
  var relativeTetromino : Vector[Point] = Vector()

 def getTetrominoShape: Vector[Point] =
 {
   randIndex match
   {
     //I
     case 0 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(1, 0), Point(2, 0))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       return newTetromino

     //J
     case 1 =>
       relativeTetromino = Vector(Point(-1,-1),Point(-1,0), Point(0,0), Point(1,0))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = JCell
       return newTetromino

     //L
     case 2 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(1, 0), Point(1, -1))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = LCell
       return newTetromino

     //O
     case 3 =>
       relativeTetromino = Vector(Point(0, 0), Point(1, 0), Point(1, -1), Point(0, -1))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = OCell
       return newTetromino

     //S
     case 4 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(0, -1), Point(1, -1))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = SCell
       return newTetromino

     //T
     case 5 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(0, -1), Point(1, 0))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = TCell
       return newTetromino

     //Z
     case 6 =>
       relativeTetromino = Vector(Point(-1, -1), Point(0, -1), Point(0, 0), Point(1, 0))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = ZCell
       return newTetromino

     case 7 =>
       relativeTetromino = Vector(Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0))
       val newTetromino:Vector[Point] = relativeTetromino.map(point => Point(point.x, point.y))
       cellType = Empty
       return newTetromino
   }
 }
}