// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW8b {
//final_tests_change
// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

private def is_op(op: String) : Boolean = {
	ops.contains(op)
}

private def is_number(sym: String) : Boolean = {
	sym.charAt(0).isDigit
}

private def is_left_parenthesis(sym: String) : Boolean = {
	sym == "("
}

private def is_right_parenthesis(sym: String) : Boolean = {
	sym == ")"
}

private def add_to_queue_until_left_bracket(st: Toks = Nil, out: Toks = Nil) : (Toks, Toks) = {
	if (st.last == "(") (st.init, out)
	else {
		add_to_queue_until_left_bracket(st.init, out++List(st.last))
	}
}

private def add_to_queue_until_stack_is_empty(st: Toks = Nil, out: Toks = Nil) : Toks = {
	if (st.isEmpty) out
	else {
		add_to_queue_until_stack_is_empty(st.init, out++List(st.last))
	}
}

private def prec(op1: String, op2: String) : Boolean = {
  assoc(op1) match {
    case RA => precs(op1) > precs(op2)
    case LA => precs(op1) >= precs(op2)
  }
}

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
		if (toks.isEmpty) {
			add_to_queue_until_stack_is_empty(st, out)
		}
		else if (is_number(toks.head)) {
			syard(toks.tail, st, out++List(toks.head)) 
		}
		else if (is_op(toks.head)) {
			if (st.isEmpty) syard(toks.tail, st++List(toks.head), out)
			else {
				if (is_left_parenthesis(st.last)) {
					syard(toks.tail, st++List(toks.head), out)
				}
				else if (prec(st.last, toks.head)) syard(toks.tail, st.init++List(toks.head), out++List(st.last)) 
				else {
					syard(toks.tail, st++List(toks.head), out)
				}
			}
		}
		else if (is_left_parenthesis(toks.head)) {
			syard(toks.tail, st++List(toks.head), out)
		}
		else if (is_right_parenthesis(toks.head)) {
			val (stack, queue) = add_to_queue_until_left_bracket(st, out)
			syard(toks.tail, stack, queue)
		}
		else {
			out
		}
}


// test cases
// println(syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3")))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Int] = Nil) : Int = {
	if (toks.isEmpty) {
		st.last
	}
	else if (is_number(toks.head)) {
		compute(toks.tail, st++List(toks.head.toInt))
	}
	else if (is_op(toks.head)){
		val n1 = st.last
		val stk_after_pop = st.init
		val n2 = stk_after_pop.last
		val stk_after_second_pop = stk_after_pop.init

		toks.head match {
          case "+" => compute(toks.tail, stk_after_second_pop++List(n2.toInt + n1.toInt))
          case "*" => compute(toks.tail, stk_after_second_pop++List(n2.toInt * n1.toInt))
		      case "-" => compute(toks.tail, stk_after_second_pop++List(n2.toInt - n1.toInt))
		      case "/" => {
            try {
              compute(toks.tail, stk_after_second_pop++List(n2.toInt / n1.toInt))
            } catch {
              case e: Exception => 0
            }
			    }
          case "^" => compute(toks.tail, stk_after_second_pop++List(Math.pow(n2.toInt, n1.toInt).toInt))
    }
  }
	else {
	st.last
	}
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// println(compute(syard(split("4 ^ 3 ^ 2"))))      // 262144
// println(compute(syard(split("4 ^ ( 3 ^ 2 )"))))  // 262144
// println(compute(syard(split("( 4 ^ 3 ) ^ 2"))))  // 4096
// println(compute(syard(split("( 3 + 1 ) ^ 2 ^ 3"))))   // 65536

}
