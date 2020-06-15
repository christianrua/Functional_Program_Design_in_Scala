package week1Package

object monad extends App {

  trait M[T]{
    def flatMap[U](f: T => M[U]): M[U]
  }

  def unit[T](x: T):M[T]

  abstract class Option[+T]{
    def flatMap[U](f: T => Option[U]): Option[U] = this match {
      case Some(x) => f(x)
      case None => None
    }
  }

  abstract class Try[+T]{
    def faltMap[U](f: T => Try[U]): Try[U] = this match {
      case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex)}
      case fail: Failure => fail
    }

    def map[U](f: T => U): Try[U] = this match {
      case Success(x) => Try(f(x))
      case fail: Failure => fail
    }
  }
  case class Success[T](x: T) extends Try[T]
  case class Failure(ex: Exception) extends Try[Nothing]

  object Try {
    def apply[T](expr: => T): Try[T]=
      try Success(expr)
      catch {
        case NonFatal(ex) => Failure(ex)
      }
  }

}
