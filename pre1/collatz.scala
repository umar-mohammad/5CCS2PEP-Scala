// Preliminary Part about the 3n+1 conjecture
//============================================

object CW6a {
//final_tests_change
def collatz(n: Long) : Long = {
    if (n == 1) 0
    else if(n % 2 == 0) collatz(n / 2) + 1
    else collatz((3 * n) + 1) + 1
}

def collatz_max(bnd: Long) : (Long, Long) = {
    val result = for(n <- (1 to bnd.toInt)) 
        yield(collatz(n), n.toLong)
    result.max
}

def is_pow_of_two(n: Long) : Boolean = {
    if (n == 0) false else if ((n & (n-1)) == 0) true else false
}

def is_hard(n: Long) : Boolean = {
    is_pow_of_two(3*n+1)
}

def last_odd(n: Long) : Long = {
    if(n % 2 == 0) last_odd(n/2)
    else if(is_hard(n)) n
    else last_odd((3 * n) + 1)
}
}

