import akka.actor.{ Actor, ActorSystem, Props }
import scala.io.Source
import qboard.QBoard

/* Define message classes for the actors */
case class SolvePuzzle (puzzle: Vector[Int])
case class CheckPossibility (puzzle: Vector[Int], spot: (Int, Int), v: Long)

/* Define the actor classe */
class SolverActor (size: Int) extends Actor {
  def receive = {
    case SolvePuzzle (puzzle) => {
      val board = new QBoard(size, puzzle.toIterator)
      val spot = board.nextSpot()
      if (spot == null) context.stop(self)
      for (v <- board.possibilities(spot))
        context.actorOf(Props(classOf[SolverActor], size)).!(
          CheckPossibility(puzzle, spot, v))
    }
    case CheckPossibility (puzzle, spot, v) => {
      val board = new QBoard(size, puzzle.toIterator)
      if (board.fill(spot, v)) {
        if (board.solved()) {
          board.print()
          context.system.shutdown()
        } else {
          val ns = board.nextSpot()
          if (ns == null) context.stop(self)
          for (v <- board.possibilities(ns))
              context.actorOf(Props(classOf[SolverActor], size)).!(
                CheckPossibility(board.toImmutableVector(), ns, v))
        }
      }
      //context.stop(self)
    }
    case _ => {
      println("Error: message not recognized")
      context.stop(self)
    }
  }
}

object PSolver extends App {

  /* Parse command line arguments and determine the source of the puzzle */
  val src = args match {
    case Array() => Source.stdin
    case _ => Source.fromFile(args(0))
  }

  /* Parse the stream of characters from src and construct the Sudoku */
  val puzzle = src.foldLeft(List[Int]())(parseStream).reverse match {
    case size :: t if (size > 0 && size < 9 && t.length >= size * size)
      => new QBoard (size, t.toIterator)
    case size :: t if (size > 0 && size < 9)
      => throw new IllegalArgumentException(
          "Puzzle size does not correspond to file contents.")
    case size :: t
      => throw new IllegalArgumentException("Size must be between 1 and 8.")
    case _ => throw new IllegalArgumentException("Empty file.")
  }

  if (puzzle.solved())
    puzzle.print()
  else { /* Create a MasterSolver actor and pass all responsability to it */
    val system = ActorSystem("SudokuSystem")
    val master = system.actorOf(Props(classOf[SolverActor], puzzle.size))
    master ! SolvePuzzle(puzzle.toImmutableVector())
  }

  /* Stream parser function that filters out all characters except 0-9 and x. */
  def parseStream (list: List[Int], char: Char): List[Int] =
    (char - '0') match {
      case 72 | 0 => 0 :: list
      case i if (i > 0 && i < 10) => i :: list
      case _ => list
  }
}
