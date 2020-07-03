package week2
import week2.Pouring

object test extends App{
  val problem = new Pouring(Vector(4,9))
  println("--------------------------")
  println(problem.moves)
  println("--------------------------")
  println(problem.pathSets.take(3).toList)
  println("--------------------------")
  println(problem.solution(6))
}
