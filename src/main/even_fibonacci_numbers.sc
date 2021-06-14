/*
Returns true if n is an even number else false
*/
def iseven(x: Int): Boolean = {
  if (x%2 == 0) true else false
}

/*
  Returns the sum of even numbers in fibonacci series within the range n
  n: number of terms in fibonacci series
 */
def fibonacci(n: Int): Int = {
  def next_term(x: Int, previous_one: Int, previous_two: Int, total: Int): Int = {
    if (x > 0) {
      val next = previous_one + previous_two
      if (iseven(next))
        return next_term(x-1, next, previous_one, total+next)
      else
        return next_term(x-1, next, previous_one, total)
    }
    return total
  }

  if (n == 1)
    return 0
  else if (n == 2)
    return 2
  else {
    val total = 2
    return next_term(n-2, 2, 1, 2)}
}

// test cases
println(fibonacci(10)) //1, 2, 3, 5, 8, 13, 21, 34, 55, 89
println(fibonacci(1))
println(fibonacci(2))
println(fibonacci(20))