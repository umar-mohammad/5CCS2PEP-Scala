// Preliminary Part about finding Knight's tours
//===============================================

object CW9a extends App{
//final_tests_change
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end are of any help.



type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
	if (path.contains(x)) false
	else if (x._1 >= dim || x._2 >= dim || x._1 < 0 || x._2 < 0) false
	else true 
}

// val pos1 = (4,4)
// val pos2 = (3,3)
// val pos3 = (5,5)
// val path = List(pos1)

// println(is_legal(5, path, pos1))
// println(is_legal(5, path, pos2))
// println(is_legal(5, path, pos3))

//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
	generate_all_knight_moves(x)
	.filter(is_legal(dim, path, _))
}

/*generate all next positons after knight move, produces list ordered clockwise*/
def generate_all_knight_moves(x: Pos) : List[Pos] = {
	List((1, 2), (2,1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2))
	.map(tr => ((tr._1 + x._1), (tr._2 + x._2)))
}

// print(legal_moves(8, List(), (2,2)))


//some testcases

// assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
// assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(3) Complete the two recursive functions below. 
//    They exhaustively search for knight's tours starting from the 
//    given path. The first function counts all possible tours, 
//    and the second collects all tours in a list of paths.

// var count = 0;
// def count_tours(dim: Int, path: Path) : /*Int*/ Unit = {
// 	if (path.size == (dim*dim)) {
// 		count+=1
// 		println(count)
// 	}
// 	generate_all_knight_moves(path.last).foreach((x => {
// 		if (is_legal(dim, path, x)) {
// 			count_tours(dim, (path:+x))
// 		}
// 	}))
// }

def count_tours(dim: Int, path: Path) : Int = {
	// if the path length is equal to the square of the dimension all the 
	// squares have been visited to return 1 for valid tour

	// if legal_moves does not generate any possible positions and the path length is
	// not the square of dimension, this is not a tour return 0

	// if lega_moves generates possible positions then call count_tours on that 
	// sum whatever the method returns
	if(path.length == dim*dim) 1
	else legal_moves(dim, path, path.last).map(x => count_tours(dim, path:+x)).sum
}

// def print_for_every_pos(dim: Int) : Unit = {
// 	val list = for(i <- 0 to dim-1; j<- 0 to dim-1) yield (i,j) 
// 	println(list.map(x => count_tours(dim, List(x))).sum)
// }

// println(print_for_every_pos(5))
def enum_tours(dim: Int, path: Path) : List[Path] = {
	if(path.length == dim*dim) List(path)
	else legal_moves(dim, path, path.last).map(x => enum_tours(dim, path:+x)).flatten
}

//(4) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = ???


// testcases
//
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) Implement a function that uses the first-function from (5) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.

def first_tour(dim: Int, path: Path) : Option[Path] = ???
 


/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//     time_needed(count_tours(dim, List((0, 0))))
// in order to print out the time that is needed for 
// running count_tours


// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}


*/

}
