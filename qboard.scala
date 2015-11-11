package qboard
import java.lang.Long

class QBoard (size: Int) {

  /* Constants */
  val N = size * size
  val ONE = 0x1L
  val NONE = 0x0L
  val ALL = (ONE << N) - 1

  /* Matrix representation of the possibilities cube using bits */
  val puzzle = Array.fill[Array[Long]](N)(Array.fill[Long](N)(0))
  /* Matrix representation of current state of the sudoku board using ints */
  val solution = Array.ofDim[Int](N, N)

  /* Keep track of all numbers already used in each row/col/block */
  val rows = Array.fill[Long](N)(NONE)
  val cols = Array.fill[Long](N)(NONE)
  val blocks = Array.fill[Long](N)(NONE)

  /* Auxiliary constructor used to initialize a new puzzle */
  def this (size: Int, stream: Iterator[Int]) = {
    /* Create an empty qboard using size first */
    this(size)

    /* Fill in the matrices and arrays using the stream provided */
    for (i <- 0 until this.N; j <- 0 until this.N) {
        val x = stream.next()
        this.solution(i)(j) = x
        this.puzzle(i)(j) = x match {
          case 0 => this.ALL
          case x => {
            val longx = toBin(x)
            this.rows(i) |= longx
            this.cols(j) |= longx
            this.blocks(size * (i / size) + (j / size)) |= longx
            longx
          }
        }
    }
    this.update()
  }

  override def clone(): QBoard = {
    val Copy = new QBoard(size);
    Array.copy(rows, 0, Copy.rows, 0, N)
    Array.copy(cols, 0, Copy.cols, 0, N)
    Array.copy(blocks, 0, Copy.blocks, 0, N)
    for (i <- 0 until N) {
        Array.copy(puzzle(i), 0, Copy.puzzle(i), 0, N)
      	Array.copy(solution(i), 0, Copy.solution(i), 0, N)
    }
    return Copy
  }

  def update(): Boolean = {
    for (r <- 0 until N; c <- 0 until N) {
      if (solution(r)(c) == 0) {
        val b = size * (r / size) + (c / size)
        puzzle(r)(c) = (~rows(r)) & (~cols(c)) & (~blocks(b)) & ALL
        if (puzzle(r)(c) == NONE ||
            (Long.bitCount(puzzle(r)(c)) == 1 && !fill(r,c, puzzle(r)(c))))
          return false
      }
    }
    return true
  }

  def fill(r: Int, c: Int, v: Long): Boolean = {
    val b = size * (r / size) + (c / size)
    solution(r)(c) = toInt(v)
    puzzle(r)(c) = v
    rows(r) |= v
    cols(c) |= v
    blocks(b) |= v
    for (i <- 0 until N) {
      if (solution(r)(i) == 0) {
        puzzle(r)(i) &= (~rows(r))
        if (puzzle(r)(i) == NONE ||
            (Long.bitCount(puzzle(r)(i)) == 1 && !fill(r,i, puzzle(r)(i))))
          return false
      }
      if (solution(i)(c) == 0) {
        puzzle(i)(c) &= (~cols(c))
        if (puzzle(i)(c) == NONE ||
            (Long.bitCount(puzzle(i)(c)) == 1 && !fill(i, c, puzzle(i)(c)) ))
          return false
      }
      val br = (b / size) * size + i / size
      val bc = (b % size) * size + b % size
      if (solution(br)(bc) == 0 ) {
        puzzle(br)(bc) &= (~blocks(b));
        if (puzzle(br)(bc) == NONE ||
          (Long.bitCount(puzzle(br)(bc)) == 1 && !fill(br, bc, puzzle(br)(bc))))
        return false
      }
    }
    return true
  }

  def nextSpot(): (Int, Int) = {
    for (r <- 0 until N; c <- 0 until N)
      if (solution(r)(c) == 0) return (r, c)
    return null
  }

  def possibilities (r: Int, c: Int): List[Long] = {
    var L = List[Long]()
    var x = puzzle(r)(c)
    for (_ <- 0 until N; if x != 0) {
      L = Long.highestOneBit(x) :: L
      x &= ~L.head
    }
    return L
  }

  def solved(): Boolean = {
 	  var ok_so_far = true
	  for (i <- 0 until N; if ok_so_far) {
 		   ok_so_far &= (rows(i) == ALL) & (cols(i) == ALL) & (blocks(i) == ALL)
 	  }
   	return ok_so_far
  }

  def toBin(m: Int): Long = ONE << (m - 1)
  def toInt(s: Long): Int = Long.numberOfTrailingZeros(s) + 1

  def print () = {
    val digits = N match {
      case N if N < 10 => 1
      case _ => 2
    }
    val sep = "-" * ((digits + 1) * N + 2 * size - 3)
    for (r <- 0 until N){
      var line = ""
      for (c <- 0 until N){
        line = line + (c match {
          case c if (c > 0 && c % size == 0)
            => "| " + fixedWidthInt(solution(r)(c), digits) + " "
          case _ => fixedWidthInt(solution(r)(c), digits) + " "
        })
      }
      r match {
        case r if (r > 0 && r % size == 0) => println(sep); println(line)
        case _ => println(line)
      }
    }
  }

  def fixedWidthInt(x: Int, d: Int): String = d match {
    case 1 => x.toString
    case 2 if (x > 9 && x < 100) => x.toString
    case 2 => " " + x.toString
    case _ => x.toString
  }
}
