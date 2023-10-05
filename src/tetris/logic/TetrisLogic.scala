package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]])
{

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))


  val initTetromino : Tetromino = spawnTetromino()// initializes the game
  initTetromino.body = initTetromino.getTetrominoShape

  def spawnTetromino() : Tetromino =
  {
    val randomNum : Int = randomGen.randomInt(7) // calls the random generator to generate
                                                        // a random tetromino
    val newTetromino = Tetromino()
    newTetromino.randIndex = randomNum
    if(gridDims.width % 2 == 0)
      {
        val anchorX : Int = gridDims.width / 2 - 1
        newTetromino.anchor = Point(anchorX, 1)
      }
    else
      {
        val anchorX : Int = gridDims.width / 2
        newTetromino.anchor = Point(anchorX, 1)
      }
    newTetromino
  }
  var currGameState = new gameState(initTetromino, initialBoard)
  //currGameState.board = initialBoard

  def inBounds(newXDelta: Int, newYDelta: Int) : (Boolean) =
  {
    if((newXDelta < 0 || newXDelta >= gridDims.width) && newYDelta >= gridDims.height)
      {
        return false
      }
    if(newXDelta < 0 || newXDelta >= gridDims.width)
      {
        return false
      }
    if(newYDelta >= gridDims.height)
      {

        return false
      }
    return true
  }

  /*def isLineFull(currState: gameState) : Unit =
  {
    val currBoard = currState.currBoard
    //val lineFull : Boolean = currBoard.forall(row => row.forall()) //TODO I need to find a way to check if every point on the line is != Empty
  }*/

  def moveTetromino(tetromino: Tetromino, xDelta: Int, yDelta: Int) : Unit =
  {
    tetromino.anchor = Point(tetromino.anchor.x + xDelta, tetromino.anchor.y + yDelta)
    tetromino.body = tetromino.body.map(point => Point(point.x + xDelta, point.y + yDelta))
  }

  def rotateLeft(): Unit =
  {
    val tetrominoType = currGameState.tetromino.cellType

    tetrominoType match
    {
      case ICell =>
        val rotateI = new rotateICell(currGameState.tetromino)
        currGameState.tetromino.body = rotateI.rotatedLeftBody
        currGameState.tetromino.relativeTetromino = rotateI.rotatedLeftRelative

      case JCell | LCell | SCell | TCell | ZCell =>
        val rotateOtherCell = new rotateOtherCell(currGameState.tetromino)
        currGameState.tetromino.body = rotateOtherCell.rotatedLeftBody
        currGameState.tetromino.relativeTetromino = rotateOtherCell.rotatedLeftRelative

      case OCell =>
        val rotateOCell = new rotateOCell(currGameState.tetromino)
        currGameState.tetromino.body = rotateOCell.rotatedLeftBody
        currGameState.tetromino.relativeTetromino = rotateOCell.rotatedLeftRelative
    }
  }

  // TODO implement me
  def rotateRight(): Unit =
  {
    val tetrominoType = currGameState.tetromino.cellType

    tetrominoType match
    {
      case ICell =>
        val rotateI = new rotateICell(currGameState.tetromino)
        currGameState.tetromino.body = rotateI.rotatedRightBody
        currGameState.tetromino.relativeTetromino = rotateI.rotatedRightRelative

      case JCell | LCell | SCell | TCell | ZCell =>
        val rotateOtherCell = new rotateOtherCell(currGameState.tetromino)
        currGameState.tetromino.body = rotateOtherCell.rotatedRightBody
        currGameState.tetromino.relativeTetromino = rotateOtherCell.rotatedRightRelative

      case OCell =>
        val rotateOCell = new rotateOCell(currGameState.tetromino)
        currGameState.tetromino.body = rotateOCell.rotatedRightBody
        currGameState.tetromino.relativeTetromino = rotateOCell.rotatedRightRelative
    }
  }

  def moveLeft(): Unit =
  {
    if(currGameState.tetromino.body(0).x == 0)
    {
      if (currGameState.tetromino.body.forall(point => currGameState.board(point.y)(0) != Empty))
        handleNewTetromino()
    }

    else
    {
      if(currGameState.tetromino.body.forall(point => inBounds(point.x - 1, point.y) && (currGameState.board(point.y)(point.x - 1) == Empty)))
        {
          moveTetromino(currGameState.tetromino, xDelta = -1, yDelta = 0)
        }

      else if (currGameState.tetromino.body.forall(point => currGameState.board(point.y)(point.x - 1) != Empty))
      {
        handleNewTetromino()
      }
    }


  }

  def moveRight(): Unit =
  {
    if (currGameState.tetromino.body.forall(point => inBounds(point.x + 1, point.y) && (currGameState.board(point.y)(point.x + 1) == Empty)))
      {
        moveTetromino(currGameState.tetromino, xDelta = 1, yDelta = 0)
      }
    else if(currGameState.tetromino.body.forall(point => currGameState.board(point.y)(point.x + 1) != Empty))
      {
        handleNewTetromino()
      }
  }

  def moveDown(): Unit = {
    if (currGameState.tetromino.body.forall(point => inBounds(point.x, point.y + 1) && (currGameState.board(point.y + 1)(point.x) == Empty)))
      {
        moveTetromino(currGameState.tetromino, xDelta = 0, yDelta = 1)
      }
    else
    {
      handleNewTetromino()
    }
  }

  def handleNewTetromino() : Unit = {

    // i loop through the tetromino, and I use this
    //val updatedSeq: Seq[Seq[Int]] = existingSeq.updated(rowIndex, existingSeq(rowIndex).updated(colIndex, newValue))
    // with each x of a point replaces rowIndex, colIndex is the y and newValue is CellType
    var newBoard : Seq[Seq[CellType]] = currGameState.board
    currGameState.tetromino.body.foreach { point =>
      newBoard = currGameState.board.updated(point.y, currGameState.board(point.y).updated(point.x, currGameState.tetromino.cellType))
      currGameState.board = newBoard
    }
    //currGameState.board = newBoard
    val newTetromino = spawnTetromino()
    currGameState.tetromino = newTetromino
    currGameState.tetromino.body = newTetromino.getTetrominoShape
  }



  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean = false

  def getCellType(p : Point): CellType =
  {
    if(currGameState.board(p.y)(p.x) != Empty)
      {
        return currGameState.board(p.y)(p.x)
      }
    if (currGameState.tetromino.body.contains(p))
      {
        return currGameState.tetromino.cellType
      }

    return Empty
  }
}

object TetrisLogic
{

  val FramesPerSecond: Int = 2 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] =
  {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}