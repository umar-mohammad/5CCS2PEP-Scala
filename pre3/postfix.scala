// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {
//final_tests_change
// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)


// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

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

private def prec(op1: String, op2: String) : Boolean = {
	precs(op1) >= precs(op2)
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


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

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
// println(syard(split("3 + 4 * ( 2 - 1 )")))  // 3 4 2 1 - * +
// println(syard(split("10 + 12 * 33")) )      // 10 12 33 * +
// println(syard(split("( 5 + 7 ) * 2"))  )    // 5 7 + 2 *
// println(syard(split("5 + 7 / 2"))      )    // 5 7 2 / +
// println(syard(split("5 * 7 / 2"))      )    // 5 7 * 2 /
// println(syard(split("9 + 24 / ( 7 - 3 )")) )// 9 24 7 3 - / +

// println(syard(split("3 + 4 + 5")))   // 3 4 + 5 +
// println(syard(split("( ( 3 + 4 ) + 5 )"))  )  // 3 4 + 5 +
// println(syard(split("( 3 + ( 4 + 5 ) )"))  )  // 3 4 5 + +
// println(syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )"))) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

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
        }
	}
	else {
		st.last
	}
}


// test cases
// println(compute(syard(split("3 + 4 * ( 2 - 1 )"))))  // 7 // 3 4 2 1 - * +
// println(compute(syard(split("10 + 12 * 33"))))       // 406
// println(compute(syard(split("( 5 + 7 ) * 2"))))      // 24
// println(compute(syard(split("5 + 7 / 2"))))          // 8
// println(compute(syard(split("5 * 7 / 2"))))          // 17
// println(compute(syard(split("9 + 24 / ( 7 - 3 )")))) // 15

}


