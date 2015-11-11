import scala.io.Source
import sudoku.Sudoku

object Solver {

  def main (args: Array[String]) {
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
  }

  /* Stream parser function that filters out all characters except 0-9 and x. */
  def parseStream (list: List[Int], char: Char): List[Int] =
    (char - '0') match {
      case 72 | 0 => 0 :: list
      case i if (i > 0 && i < 10) => i :: list
      case _ => list
  }
}
