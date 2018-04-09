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
  	if(as.length > 1)::(a, init(as))
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

//exercise 3.9 compute the length of a list using foldRight
def length[A](as: List[A]): Int = foldRight(as, Nil:List[A])(::(_,_)).length


//exercise 3.10 write a general list-recursion function, foldLeft, that is tail-recursive
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

//exercise 3.12 using ::: (not introduced in book at this stage)
def reverse[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case ::(a, as) => reverse(as) ::: a :: Nil
}
reverse(List(1, 2, 3))

//exercise 3.12 - used hint for this - not 100% fruit of own brainwave
//pre-hint assumed was possible using only Cons and recursion ***
def reverse[A](l: List[A]): List[A] = {
  @annotation.tailrec
  def go(l: List[A], temp: List[A]): List[A] = l match {
    case Nil       => temp
    case ::(a, as) => go(as, ::(a, temp))
  }
  go(l, Nil)
}
reverse(List(1, 2, 3, 4, 5, 6))

//exercise 3.12 - write reverse using a *fold*
def reverse[A](l: List[A], temp: List[A]):List[A] = foldLeft(l, temp)((x,y) => ::(y,x))

//exercise 3.13 writing foldLeft in terms of foldRight (get stack overflow)
private def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case ::(x, xs) => foldRight(as, z)((b,a) => f(a,b))
}
foldLeft(List(1,2,3), 0.0)(_+_)

//exercise 3.13 writing foldRight in terms of foldLeft
def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case ::(x, xs) => foldLeft(as.reverse, z)((b,a)=> f(a,b))
}
foldRight(List(1,2,3), 0.0)(_+_)

//exercise 3.14 `append` item at end of list
def append[A](l: List[A], value: A): List[A] = foldRight(l, List(value))((x,y) => ::(x,y))                        
append(List(1,2,3), 4)

//exercise 3.15 - Write a function that concatenates a list of lists into a single list (using functions we have already defined)
def flattenLists[A](as: List[List[A]]): List[A] = {
  def go(l: List[List[A]], temp: List[A]): List[A] = l match {
    case Nil => temp
    case ::(a, as) =>
    go(as, foldLeft(a, temp)((b, a) => append(b, a)))
  }
  go(as, Nil)
}
flattenLists(List(List(1,2,3), List(2,3,4)))

//3.16 write a function that transforms a list of integers by adding 1 to each element. 
//unsure whether can use append or reverse
def transformList(l: List[Int], temp: List[Int]): List[Int] = {
  def go(l:List[Int], temp: List[Int]): List[Int] = l match {  
  case Nil => temp
  case ::(a,as) => 
  go(as, ::(a+1, temp))
}
  go(l, temp)
}
transformList(List(1,2,3), Nil)

//3.16 write a function that transforms a list of integers by adding 1 to each element. 
//after looking at hint  - use a foldRight
def transformList(l: List[Int], temp: List[Int]): List[Int] = {
  foldRight(l, temp)((a,b) => ::(a+1,b))  
}
transformList(List(1,2,3), Nil)

//3.17 write a function that turns each value in a List[Double] into a String
//using foldRight
def transformDoubleToString(l:List[Double], temp:List[String]): List[String] = 
foldRight(l, temp)((a,b) => ::(a.toString, b))
transformDoubleToString(List(1.0,2.0,3.44), Nil)

//3.17 write a function that turns each value in a List[Double] into a String
//using foldLeft
def transformDoubleToString(l: List[Double], temp: List[String]): List[String] = 
foldLeft(l, temp)((b,a) => append(b, a.toString))
transformDoubleToString(List(1.0,2.0,3.44), Nil)

//Exercise 3.18
//Write a function map that generalizes modifying each element in a list 
//while maintaining the structure of the list. 
def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => ::(f(a), b))
map(List(1, 2, 3))((x) => x.toString)

//Exercise 3.19
//Write a function filter that removes elements from a list 
//unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].
def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
  case Nil => as
  case ::(x, xs) => foldRight(as, Nil: List[A])((a,b) => if (f(a)) ::(a, b) else b)  	
}
filter(List(1, 2, 3, 4))((x) => if (x % 2 == 0) true else false)

//Exercise 3.20
//Write a function flatMap that works like map
//except that the function given will return a list instead of a single result, 
//and that list should be inserted into the final resulting list
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flattenLists(map(as)(f))
flatMap(List(1,2,3))(i => List(i,i))

//exercse 3.21
//Use flatMap to implement filter.
def filter[A](as: List[A])(f: A => Boolean): List[A] = as match{
  case Nil => as
  case ::(x, xs) => flatMap(as)(a => if(f(a)) ::(a, Nil) else Nil)
}
filter(List(1, 2, 3, 4))((x) => if (x % 2 == 0) true else false)

//exercise 3.22
//Write a function that accepts two lists and 
//constructs a new list by adding corresponding elements.
//***** using Scastie. implicit conversion not available. using scala.Int
def zipIntegerLists[Int](as1: List[scala.Int], as2: List[scala.Int]): List[scala.Int] = {
  @annotation.tailrec
  def go(l1: List[scala.Int], l2: List[scala.Int], result: List[scala.Int]): List[scala.Int] = l1 match {
    case Nil => result
    case ::(x, xs) => 
    go(l1.tail, l2.tail, ::(sum(List(l1.head, l2.head)), result))
  }
  reverse(go(as1, as2, Nil: List[scala.Int]))
}
zipIntegerLists(List(1,2,3), List(4,5,6))

//exercse 3.23
//Generalize the function you just wrote so that 
//it’s not specific to integers or addition.
//Name your generalized function zipWith.
//***vague description, assuming it has to return tuples, 
//like Scala's `zipWithIndex`***
def zipWith[A](as1: List[A], as2: List[A]): List[(A,A)] = {
  @annotation.tailrec
  def go(l1: List[A], l2: List[A], result: List[(A,A)]): List[(A,A)] = l1 match {
    case Nil => result
    case ::(x,xs) => go(l1.tail, l2.tail, ::((l1.head, l2.head), result))
  }  
  reverse(go(as1, as2, Nil: List[(A,A)]))
}
zipWith(List(1,2,"test"), List("test", 2, 1))

