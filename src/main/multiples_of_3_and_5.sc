 /*
 Returns true if n is a multiple of x else false
 */
 def ismultiple(n: Int, x: Int): Boolean = {
   if (n%x == 0) true else false
 }

 /*
  Returns the sum of natural numbers which are multiples of x and y within the range n
  x: multiple 1
  y: multiple 2
  n: all natural numbers lower than n are checked
 */
 def sum_multiples(x: Int, y: Int, n: Int): Int = {
  def helper(m: Int, total: Int): Int = {
   if (m < n){

    if (ismultiple(m, x) && ismultiple(m, y)) {
      return helper(m + math.min(x, y), total+m)
     }

    else if (ismultiple(m, x) || ismultiple(m, y)){
     return helper(m + 1, total+m)
     }
    else
     return helper(m+1, total)
    }
   else
     return total
   }
  return helper(math.min(x, y), 0)
 }

 // test cases
 println(sum_multiples(3, 5, 10))
 println(sum_multiples(2, 10, 10))
 println(sum_multiples(9, 3, 30))
 println(sum_multiples(10, 5, 10))