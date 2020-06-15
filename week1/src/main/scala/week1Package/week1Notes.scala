package week1Package

object week1Notes extends App {

  abstract class JSON
  case class JSeq (elems: List[JSON]) extends JSON
  case class JObj (bindings: Map[String, JSON]) extends JSON
  case class JNum (num: Double) extends  JSON
  case class JStr (str: String) extends JSON
  case class JBool (b: Boolean) extends JSON
  case object JNull extends JSON

  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21 2nd Street"),
      "state" -> JStr("NY"),
      "postalCode" -> JNum(10021)
      )
    ),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"), "number" -> JStr("212 555-1234")
      )),
      JObj(Map(
        "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
      ))
    ))

  ))

  def show(json:JSON): String = json match {
    case JSeq(elems) =>
      "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
    val assocs = bindings map {
      case (key, value) => "\"" + key + "\":" + show(value)
    }
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => '\"' + str + '\"'
    case JBool(b) => b.toString
    case JNull => "null"
  }

  println(show(data))

  val f: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
  }
  println("value of f is: ")
  println(f.isDefinedAt((List(1,2,3))))

  val g: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest =>
      rest match {
        case Nil => "two"
      }
  }

  println("value of g is: ")
  println(g.isDefinedAt(List(1,2,3)))


  case class Book(title: String, authors: List[String])

  val books = List(
    Book(
      title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald","Sussman Gerald J.")
    ),
    Book(
      title = "Introduction to Functional Programing",
      authors = List("Bird, Richard", "Wlader, Phil")),
    Book(
      title = "Effective Java",
      authors = List("Bloch, Joshua")
    ),
    Book(
      title = "Effective Java 2",
      authors = List("Bloch, Joshua")
    ),
    Book(
      title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")
    ),
  )

  val titlesByBird = for(b <- books; a <- b.authors if a startsWith "Bird,") yield b.title
  println(titlesByBird)

  val uniqueAuthor = {
    for{
      b1 <- books
      b2 <- books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1
  }.distinct

  println(uniqueAuthor)

}
