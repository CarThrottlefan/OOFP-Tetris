package tetris.logic
case class Tetromino (){
  var randIndex : Int = 0
  var anchor : Point = Point(0,0)
  var body : Vector[Point] = Vector[Point] ()
  var cellType : CellType =  ICell
  var relativeTetromino : Vector[Point] = Vector()

 def getTetrominoShape: Vector[Point]  =
 {
   var newTetromino : Vector[Point] = Vector()
   randIndex match
   {
     //I //TODO replace below with a function?
     case 0 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(1, 0), Point(2, 0))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))

     //J
     case 1 =>
          relativeTetromino = Vector(Point(-1,-1),Point(-1,0), Point(0,0), Point(1,0))
          newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
          cellType = JCell

     //L
     case 2 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(1, 0), Point(1, -1))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = LCell

     //O
     case 3 =>
       relativeTetromino = Vector(Point(0, 0), Point(1, 0), Point(1, -1), Point(0, -1))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = OCell

     SCell
     case 4 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(0, -1), Point(1, -1))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = SCell

     //T
     case 5 =>
       relativeTetromino = Vector(Point(-1, 0), Point(0, 0), Point(0, -1), Point(1, 0))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = TCell

     //Z
     case 6 =>
       relativeTetromino = Vector(Point(-1, -1), Point(0, -1), Point(0, 0), Point(1, 0))
       newTetromino = relativeTetromino.map(point => Point(point.x + anchor.x, point.y + anchor.y))
       cellType = ZCell
   }
   newTetromino
 }
}