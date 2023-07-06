// Finding a single tour on a "mega" board
//=========================================

object CW9c extends App {
//final_tests_change
import scala.annotation.tailrec
// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala   

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

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    (for (move <- legal_moves(dim, path, x)) yield 
    (move, legal_moves(dim, path:+ move, move).length))
    .sortBy(_._2)
    .map(x => x._1)
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

// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    open_tour_finder(dim, List(path))
}
// println(tour_on_mega_board(70, List((0,0))))

}
