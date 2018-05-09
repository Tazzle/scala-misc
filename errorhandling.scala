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