val exampleList = List(1, 2, 3, 4)

//exercise 3.2
def tail[A](l: List[A]): List[A] = l match {
  case Nil       => Nil
  case ::(a, as) => as
}
tail(exampleList)

//exercise 3.3
def setHead[A](l: List[A], replacement: A): List[A] = l match {
  case Nil       => List(replacement)
  case ::(a, as) => ::(replacement, as)
}
setHead(Nil, 10)

//exercise 3.4
@annotation.tailrec
private def drop[A](l: List[A], n: Int): List[A] = l match {
  case Nil => Nil
  case ::(a, as) =>
  	if(n >= l.length) Nil
    if (n > 1) drop(as, n-1)
  	else as
}
drop(exampleList, 2)

//exercise 3.5
def matchesTwo(n: Int): Boolean = {
  if (n == 2) true else false
}
​
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case ::(a, as) =>
    if (f(a)) dropWhile(as, f)
    else l
}
dropWhile(List(2,2,2,4,5,6), matchesTwo)

//exercise 3.6
def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case ::(a, as) =>
  	if(as.length > 1)::(a, init(::(as.head, as.tail)))
    else List(a)
}
init(List(3,6,78,23,12,4556,66))

