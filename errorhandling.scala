//Chapter 4: Handling Errors without Exceptions

//exercise 4.:1 Implement all functionso on Option

// 1) map
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

val integerOption = Some(1)
integerOption.map(x => x.toDouble)

// 2) flatMap 3) getOrElse
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None        => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }
    def flatMap[B](f: A => Option[B]): Option[B] = {
      this.map(x => f(x)).getOrElse(None)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

val integerOption = Some(16)
None.getOrElse(2)
integerOption.flatMap(x => Some(x.toDouble))