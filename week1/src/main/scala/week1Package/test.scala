package week1Package
import week1Package.randomTree._

object test extends App{

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  def single[T](x: T)= new Generator[T] {
    def generate = x
  }

  val booleans = integers.map(_ >= 0)

  def pairs[T,U](t: Generator[T], u: Generator[U]): Generator[(T,U)] = for {
    x <- t
    y <- u
  } yield (x,y)

  def emptyList = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  def lists: Generator[List[Int]] = for {
    cutoff <- booleans
    list <- if(cutoff) emptyList else nonEmptyLists
  } yield list

  def test[T](r: Generator[T], noTimes: Int = 100) (test: T => Boolean): Unit = {
    for(_ <- 0 until noTimes){
      val value = r.generate
      assert(test(value), "Test failed for: "+value)
    }
    println("Tests passed "+noTimes+" times")
  }

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }

}
