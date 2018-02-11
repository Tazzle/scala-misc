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

//exercise 3.7
def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case ::(x, xs) =>
    if (x == z) z
    else f(x, foldRight(xs, z)(f))
}

val listOfDoubles = List(1.0, 2.0, 3.0, 0.0, 5.0, 6.0, 7.0, 8.0, 9.0)
foldRight(listOfDoubles, 0.0)(_*_)

//exercise 3.9
def length[A](as: List[A]): Int = foldRight(as, Nil:List[A])(::(_,_)).length


//exercise 3.10
@annotation.tailrec
private def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil       => z
  case ::(x, xs) => foldLeft(xs, f(z, x))(f)
}
foldLeft(List(1,2,3), 0.0)(_+_)

//exercise 3.11
def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
sum(List(1,2,3))
def product(ns: List[Double]) = foldLeft(ns, 1.0)(_*_)
product(List(1.0, 2.0, 3.0))
def length[A](as: List[A]): Int = foldLeft(as, Nil: List[A])((x,y) => ::(y, x)).length  
length(List(1,2,3,4,5,6))