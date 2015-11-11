package sudoku
import qboard.QBoard

class Sudoku(size: Int, stream: Iterator[Int]){

  val N = size * size
  var qboard = new QBoard(size, stream)

  def solve (): Boolean = qboard.solved() match {
    case true => true
    case false => {
      var sol = backtrack(qboard)
      if (sol == null) false
      else {qboard = sol; true}
    }
  }

  def backtrack (board: QBoard): QBoard = {
    val spot = board.nextSpot()
    if (spot == null)
      return null
    for (v <- board.possibilities(spot._1, spot._2)) {
      var copy = board.clone()
      if (copy.fill(spot._1, spot._2, v)) {
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
