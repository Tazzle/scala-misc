
def fib(n: Int): Int = {
  val initialCount = 1
  val seed1 = 0
  val seed2 = 0

  def go(n: Int, count: Int, prev: Int, curr: Int): Int = {
    val result = prev + curr
    println(result)
    if(count == 1) go(n, count+1, prev, curr+1)
    else if(count == 2) go(n, count+1, prev, curr)
    else if(count == n) result
    else go(n, count+1, curr, result)        
    }   
  go(n, initialCount, seed1, seed2)
 }

fib(5)
