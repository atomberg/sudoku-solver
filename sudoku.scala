import scala.io.Source
import qboard.QBoard

class Sudoku(size: Int, stream: Iterator[Int]){

  val N = size * size
  var qboard = new QBoard(size, stream)
  var steps = 0

  /* Backtrack wrapper */
  def solve (): Boolean = qboard.solved() match {
    case true => true
    case false => {
      var soln = backtrack(qboard)
      if (soln == null) false
      else {qboard = soln; true}
    }
  }

  /* Main solver routine */
  def backtrack (board: QBoard): QBoard = {
    steps += 1
    println(steps + ": " + board.unknowns)
    val spot = board.nextSpot()
    if (spot == null)
      return null
    for (v <- board.possibilities(spot)) {
      var copy = board.clone()
      if (copy.fill(spot, v)) {
        if (copy.solved())
          return copy
        copy = backtrack(copy)
        if (copy != null)
          return copy
      }
    }
    return null
  }

  def print () = qboard.print()
}

object Solver extends App {

  /* Parse command line arguments and determine the source of the puzzle */
  val src = args match {
    case Array() => Source.stdin
    case _ => Source.fromFile(args(0))
  }

  /* Parse the stream of characters from src and construct the Sudoku */
  val puzzle = src.foldLeft(List[Int]())(parseStream).reverse match {
    case size :: t if (size > 0 && size < 9 && t.length >= size * size)
      => new Sudoku (size, t.toIterator)
    case size :: t if (size > 0 && size < 9)
      => throw new IllegalArgumentException(
          "Puzzle size does not correspond to file contents.")
    case size :: t
      => throw new IllegalArgumentException("Size must be between 1 and 8.")
    case _ => throw new IllegalArgumentException("Empty file.")
  }
  puzzle.solve()
  puzzle.print()

  /* Stream parser function that filters out all characters except 0-9 and x. */
  def parseStream (list: List[Int], char: Char): List[Int] =
    (char - '0') match {
      case 72 | 0 => 0 :: list
      case i if (i > 0 && i < 10) => i :: list
      case _ => list
  }
}
