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
                  val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  var tetromino : Tetromino = spawnTetromino()// initializes the game
  tetromino.body = tetromino.getTetrominoShape

  def spawnTetromino() : Tetromino = {
    val randomNum : Int = randomGen.randomInt(7) // calls the random generator to generate
                                                        // a random tetromino
    val newTetromino = Tetromino()
    newTetromino.randIndex = randomNum //TODO change back to randomNum
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

  def moveTetromino(tetromino: Tetromino, xDelta: Int, yDelta: Int) : Unit = {
    tetromino.anchor = Point(tetromino.anchor.x + xDelta, tetromino.anchor.y + yDelta)
    tetromino.body = tetromino.body.map(point => Point(point.x + xDelta, point.y + yDelta))
  }

  def rotateLeft(): Unit = {
    val tetrominoType = tetromino.cellType

    tetrominoType match
    {
      case ICell =>
        val rotateI = new rotateICell(tetromino)
        tetromino.body = rotateI.rotatedLeftBody
        tetromino.relativeTetromino = rotateI.rotatedLeftRelative

      case JCell | LCell | SCell | TCell | ZCell =>
        val rotateOtherCell = new rotateOtherCell(tetromino)
        tetromino.body = rotateOtherCell.rotatedLeftBody
        tetromino.relativeTetromino = rotateOtherCell.rotatedLeftRelative

      case OCell =>
        val rotateOCell = new rotateOCell(tetromino)
        tetromino.body = rotateOCell.rotatedLeftBody
        tetromino.relativeTetromino = rotateOCell.rotatedLeftRelative
    }
  }

  // TODO implement me
  def rotateRight(): Unit =
  {
    val tetrominoType = tetromino.cellType

    tetrominoType match
    {
      case ICell =>
        val rotateI = new rotateICell(tetromino)
        tetromino.body = rotateI.rotatedRightBody
        tetromino.relativeTetromino = rotateI.rotatedRightRelative

      case JCell | LCell | SCell | TCell | ZCell =>
        val rotateOtherCell = new rotateOtherCell(tetromino)
        tetromino.body = rotateOtherCell.rotatedRightBody
        tetromino.relativeTetromino = rotateOtherCell.rotatedRightRelative

      case OCell =>
        val rotateOCell = new rotateOCell(tetromino)
        tetromino.body = rotateOCell.rotatedRightBody
        tetromino.relativeTetromino = rotateOCell.rotatedRightRelative
    }
  }

  def moveLeft(): Unit =
  {
    moveTetromino(tetromino, xDelta = -1, yDelta = 0)
  }

  def moveRight(): Unit =
  {
    moveTetromino(tetromino, xDelta = 1, yDelta = 0)
  }

  def moveDown(): Unit = {
    moveTetromino(tetromino, xDelta = 0, yDelta = 1)
  }

  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean = false

  def getCellType(p : Point): CellType = {
    if (tetromino.body.contains(p))
      {
        return tetromino.cellType
      }
    else
      {
        return Empty
      }
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 2 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
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