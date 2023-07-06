// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b extends App {
//final_tests_change
import scala.annotation.tailrec
// !!! Copy any function you need from file knight1.scala !!!
//
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
	if (path.contains(x)) false
	else if (x._1 >= dim || x._2 >= dim || x._1 < 0 || x._2 < 0) false
	else true 
}

def generate_all_knight_moves(x: Pos) : List[Pos] = {
	List((1, 2), (2,1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2))
	.map(tr => ((tr._1 + x._1), (tr._2 + x._2)))
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
	generate_all_knight_moves(x)
	.filter(is_legal(dim, path, _))
}

// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    (for (move <- legal_moves(dim, path, x)) yield 
    (move, legal_moves(dim, path:+ move, move).length))
    .sortBy(_._2)
    .map(x => x._1)
}
//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 

def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    closed_tour_finder(dim, List(path))
}

// (8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.
def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    open_tour_finder(dim, List(path))
}

@tailrec
def closed_tour_finder(dim: Int, path_accumulator: List[Path]) : Option[Path] = {
    if (path_accumulator.isEmpty) None
    else {
        val last_path = path_accumulator.head // O(1)
        if (last_path.length == dim*dim && generate_all_knight_moves(last_path.last).contains(last_path.head)) Some(last_path)
        else {
            val new_paths = for (possible_move <- ordered_moves(dim, last_path, last_path.head)) yield possible_move+:last_path
            closed_tour_finder(dim, new_paths ++ path_accumulator.tail)
        }
    }
}

@tailrec
def open_tour_finder(dim: Int, path_accumulator: List[Path]) : Option[Path] = {
    if (path_accumulator.isEmpty) None
    else {
        val last_path = path_accumulator.head // O(1)
        if (last_path.length == dim*dim) Some(last_path)
        else {
            val new_paths = for (possible_move <- ordered_moves(dim, last_path, last_path.head)) yield possible_move+:last_path
            open_tour_finder(dim, new_paths ++ path_accumulator.tail)
        }
    }
}

// def time_needed[T](code: => T) : T = {
//   val start = System.nanoTime()
//   val result = code
//   val end = System.nanoTime()
//   println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
//   result
// }

}
