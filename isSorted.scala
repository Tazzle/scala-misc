def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Int = {
    if(n >= as.length) -1
    else if(ordered(as(n), as(n+1))) n
    else loop(n + 1)
  }
  loop(0)
}