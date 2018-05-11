//Chapter 4: Handling Errors without Exceptions

//exercise 4.:1 Implement all functions on Option

//exercise 4.1
//implement functions on Option. You should be able to implement
//all the functions besides `map` or `getOrElse` without resorting
//to pattern matching.
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None        => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None        => default
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(x => f(x)).getOrElse(None)
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this == None) ob
    else this
  }
  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(x => if (f(x)) Some(x) else None)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

Some(16).map(x => x.toDouble)
None.getOrElse(2)
Some(1).flatMap(x => Some(x.toDouble))
None.orElse(Some(2))
Some(1).filter(x => x > 1)