package week2

object lazyEvaluation extends App{

    def expr = {
      val x ={print("x");1}
      lazy  val y = {print("y");2}
      def z = {print("z");3}
      println(z+y+x+z+y+x)
    }
  expr
}
