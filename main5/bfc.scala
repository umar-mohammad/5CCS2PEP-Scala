// Core Part about a "Compiler" for the Brainf*** language
//======================================================


object CW10b extends App {
//final_tests_change
import scala.annotation.tailrec

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

def load_bff(name: String) : String = {
    try {
        Source.fromFile(name).mkString
    } catch {
        case e : java.io.FileNotFoundException => ""
    }
}

def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    mem + (mp -> v)
}

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
    if (pc < prog.length) {
        level match {
            case 0 => prog.charAt(pc) match {
                case '[' => jumpRight(prog, pc+1, level+1)
                case ']' => pc+1
                case _ => jumpRight(prog, pc+1, level)
            }
            case _ => prog.charAt(pc) match {
                case '[' => jumpRight(prog, pc+1, level+1)
                case ']' => jumpRight(prog, pc+1, level-1) 
                case _ => jumpRight(prog, pc+1, level)
            }
        }
    } else {
        pc
    }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
    if (pc >= 0) {
        level match {
            case 0 => prog.charAt(pc) match {
                case '[' => pc+1
                case ']' => jumpLeft(prog, pc-1, level+1)
                case _ => jumpLeft(prog, pc-1, level)
            }
            case _ => prog.charAt(pc) match {
                case '[' => jumpLeft(prog, pc-1, level-1)
                case ']' => jumpLeft(prog, pc-1, level+1) 
                case _ => jumpLeft(prog, pc-1, level)
            }
        }
    } else {
        pc
    }
}

// TASKS
//=======

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information about
//     pc positions where '[' or a ']' are stored. The information
//     is to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 
//     in order to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should immediately look up the jump address in the jtable.
 
def jtable_aux(pg: String, pc: Int, map: Map[Int, Int] = Map()) : Map[Int,Int]= {
	if (pc < pg.length) {
		pg.charAt(pc) match {
		case '[' => jtable_aux(pg, pc+1, map + (pc -> jumpRight(pg, pc+1, 0)))
		case ']' => jtable_aux(pg, pc+1, map + (pc -> jumpLeft(pg, pc-1, 0)))
		case _ => jtable_aux(pg, pc+1, map)
		}
	}
	else {
		map
	}

}
def jtable(pg: String) : Map[Int, Int] = {
	jtable_aux(pg, 0)
}

// println(jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"""))


// testcase
//
// println(jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"""))
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
	if (pc >= 0 && pc < pg.length) {
        pg.charAt(pc) match {
            case '>' => compute2(pg, tb, pc+1, mp+1, mem)
            case '<' => compute2(pg, tb, pc+1, mp-1, mem)
            case '+' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
            case '-' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
            case '.' => {
                print(sread(mem,mp).toChar) 
                compute2(pg, tb, pc+1, mp, mem)
            }
            case '[' => {
                if (sread(mem,mp)== 0) {
                    compute2(pg, tb, tb(pc), mp, mem)
                } else {
                    compute2(pg, tb, pc+1, mp, mem)
                }
            }
            case ']' => {
                if (sread(mem,mp) != 0) {
                    compute2(pg, tb, tb(pc), mp, mem)
                } else {
                    compute2(pg, tb, pc+1, mp, mem)
                }
            }
            case '*' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp) * sread(mem, mp-1)))
            case '@' => compute2(pg, tb, pc+1, mp, write(mem, sread(mem,mp), sread(mem, mp-1)))
            case '#' => {
                print(sread(mem,mp).toInt)
                compute2(pg, tb, pc+1, mp, mem)
            }
            case _ => compute2(pg, tb, pc+1, mp, mem)
        }
    }
    else {
        mem
    }
}

def run2(pg: String, m: Mem = Map()) = {
	compute2(pg, jtable(pg), 0, 0, m)
}

// run2("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")

// testcases
// run2(load_bff("benchmark.bf"))
// println(time_needed(1, run2(load_bff("benchmark.bf"))))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-.,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = {
    """\[-\]""".r.replaceAllIn("""[^<>+-.\[\]@#*]""".r.replaceAllIn(s, ""), "0")
}

// println(optimise(load_bff("mandelbrot.bf")).length)  // => 11205

// println(optimise("**2  [-]"))

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
	if (pc >= 0 && pc < pg.length) {
        pg.charAt(pc) match {
            case '>' => compute3(pg, tb, pc+1, mp+1, mem)
            case '<' => compute3(pg, tb, pc+1, mp-1, mem)
            case '+' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
            case '-' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
            case '.' => {
                print(sread(mem,mp).toChar) 
                compute3(pg, tb, pc+1, mp, mem)
            }
            case '[' => {
                if (sread(mem,mp)== 0) {
                    compute3(pg, tb, tb(pc), mp, mem)
                } else {
                    compute3(pg, tb, pc+1, mp, mem)
                }
            }
            case ']' => {
                if (sread(mem,mp) != 0) {
                    compute3(pg, tb, tb(pc), mp, mem)
                } else {
                    compute3(pg, tb, pc+1, mp, mem)
                }
            }
            case '*' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp) * sread(mem, mp-1)))
            case '@' => compute3(pg, tb, pc+1, mp, write(mem, sread(mem,mp), sread(mem, mp-1)))
            case '#' => {
                print(sread(mem,mp).toInt)
                compute3(pg, tb, pc+1, mp, mem)
            }
            case '0' => compute3(pg, tb, pc+1, mp, write(mem, mp, 0))
            case _ => compute3(pg, tb, pc+1, mp, mem)
        }
    }
    else {
        mem
    }
}

def run3(pg: String, m: Mem = Map()) = {
    compute3(pg, jtable(pg), 0, 0, m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.


// >++[<+++++++++++++>‐]<[[
// 0->1
// 1->3
// 4->5
// 5->17
// 18->19
// 19->20
// 21->22

// starting at the pointer go right until you find a symbol different than the one you want
// return the new pointer position and the updated result string
@tailrec
def goRight(original_string: String, pointer: Int, counter: Int, symbol: Char, result_string: String) : (Int, String) = {
    if (pointer < original_string.length) {
        if (counter == 26) {
            (pointer, result_string + symbol + (counter + 64).toChar) 
        } else {
            original_string.charAt(pointer) match {
                case `symbol` => {
                    goRight(original_string, pointer+1, counter+1, symbol, result_string)
                }
                case _ => (pointer, result_string + symbol + (counter + 64).toChar) 
            }
        }
    } else {
        (pointer, result_string + symbol + (counter + 64).toChar) 
    }
}

// println(goRight(">>>>>><", 0, 0, '>', ""))
// ">>>>>><"
//  0123456
// >A+B[<A+M>A‐A]<A[[
// >++[<+++++++++++++>‐]<[[
// println(combineAux(">++[<+++++++++++++>-]<[[", 0, ""))

@tailrec
def combineAux(original_string: String, pointer: Int = 0, result_string: String = "") : String = {
    if (pointer < original_string.length) {
        original_string.charAt(pointer) match {
            case '+' => {
                val (updated_pointer, updated_string) = goRight(original_string, pointer+1, 1, '+', result_string)
                combineAux(original_string, updated_pointer, updated_string)
            }
            case '-' => {
                val (updated_pointer, updated_string) = goRight(original_string, pointer+1, 1, '-', result_string)
                combineAux(original_string, updated_pointer, updated_string)
            }
            case '>' => {
                val (updated_pointer, updated_string) = goRight(original_string, pointer+1, 1, '>', result_string)
                combineAux(original_string, updated_pointer, updated_string)
            }
            case '<' => {
                val (updated_pointer, updated_string) = goRight(original_string, pointer+1, 1, '<', result_string)
                combineAux(original_string, updated_pointer, updated_string)
            }
            case char  => combineAux(original_string, pointer+1, result_string+char)
        }
    } else {
        result_string
    }
}

def combine(s: String) : String = {
    combineAux(s)
}

// testcase
// println(combine(load_bff("benchmark.bf")))

// println(combine(">>>>>>>>>>>>>>>>>>>>>>>>>>"))
// println(combine(">>>>>>>>>>>>>>>>>>>>>>>>>>>"))
// println(combine(">>>>>>>>>>>>>>>>>>>>>>>>>>>>"))

// val str = ">Z"

def getValue(pg: String, pc: Int) : Int = {
    if (pc < pg.length) pg.charAt(pc).toInt-64 else 1
}

// println(getIncrement(str,1))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
	if (pc >= 0 && pc < pg.length) {
        pg.charAt(pc) match {
            case '>' => compute4(pg, tb, pc+2, mp+getValue(pg, pc+1), mem)
            case '<' => compute4(pg, tb, pc+2, mp-getValue(pg, pc+1), mem)
            case '+' => compute4(pg, tb, pc+2, mp, write(mem, mp, sread(mem, mp)+getValue(pg, pc+1)))
            case '-' => compute4(pg, tb, pc+2, mp, write(mem, mp, sread(mem, mp)-getValue(pg, pc+1)))
            case '.' => {
                print(sread(mem,mp).toChar) 
                compute4(pg, tb, pc+1, mp, mem)
            }
            case '[' => {
                if (sread(mem,mp)== 0) {
                    compute4(pg, tb, tb(pc), mp, mem)
                } else {
                    compute4(pg, tb, pc+1, mp, mem)
                }
            }
            case ']' => {
                if (sread(mem,mp) != 0) {
                    compute4(pg, tb, tb(pc), mp, mem)
                } else {
                    compute4(pg, tb, pc+1, mp, mem)
                }
            }
            case '*' => compute4(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp) * sread(mem, mp-1)))
            case '@' => compute4(pg, tb, pc+1, mp, write(mem, sread(mem,mp), sread(mem, mp-1)))
            case '#' => {
                print(sread(mem,mp).toInt)
                compute4(pg, tb, pc+1, mp, mem)
            }
            case '0' => compute4(pg, tb, pc+1, mp, write(mem, mp, 0))
            case _ => compute4(pg, tb, pc+1, mp, mem)
        }
    }
    else {
        mem
    }
}


// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = {
    val combined = combine(optimise(pg))
    compute4(combined, jtable(combined), 0, 0, m)
}


// testcases
// println(combine(optimise(load_bff("benchmark.bf")))) // => """>A+B[<A+M>A-A]<A[[....."""
// run4("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.""")
// val b = combine(a)
// println(a)
// println(b)
// run3(a)
// run4(b)

// testcases (they should now run much faster)
// println(time_needed(1, run4(load_bff("benchmark.bf"))))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))
// println("old: " + time_needed(1, run2(load_bff("benchmark.bf"))), "new: " + time_needed(1, run4(load_bff("benchmark.bf"))))
// println(run2(load_bff("sierpinski.bf")) == run4(load_bff("sierpinski.bf")))
// println(time_needed(10, run4("""+++>+@+@+@+@+@""")))
// println(time_needed(10, run2("""+++>+@+@+@+@+@""")))
// println(time_needed(100, run4("""+++>+@+@+@+@+@"""))/time_needed(100, run2("""+++>+@+@+@+@+@""")))



}
