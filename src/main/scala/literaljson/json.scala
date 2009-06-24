package literaljson

object Json {
  sealed abstract class JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JNumber(num: Double) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JObject(obj: List[(String, JValue)]) extends JValue
  case class JArray(arr: List[JValue]) extends JValue

  // FIXME move
  sealed abstract class Doc {
    def <>(other: Doc) = Cons(this, other)
  }
  case object Empty extends Doc
  case object Line extends Doc
  case class Text(value: String) extends Doc
  case class Cons(d1: Doc, d2: Doc) extends Doc

  def render(value: JValue): Doc = value match {
    case JBool(true)  => text("true")
    case JBool(false) => text("false")
    case JNumber(n)   => text(n.toString)
    case JNull        => text("null")
    case JString(s)   => text("\"" + s + "\"")
    case JArray(arr)  => text("[") <> series(arr.map(render(_))) <> text("]")
    case JObject(obj) => text("{") <> series(obj.map(f => text("\"" + f._1 + "\":") <> render(f._2))) <> text("}")
  }

  private def fold(docs: List[Doc]) = docs.foldLeft[Doc](Empty)(_ <> _)
  private def series(docs: List[Doc]) = fold(punctuate(text(","), docs))
  private def punctuate(p: Doc, docs: List[Doc]): List[Doc] = docs match {
    case Nil => Nil
    case List(d) => List(d)
    case d :: ds => (d <> p) :: punctuate(p, ds)
  }

  private def text(s: String) = if (s == "") Empty else Text(s)
}

object PrettyPrinter {
  import Json._

  def compact(d: Doc) = {
    def layout(doc: Doc): String = doc match {
      case Empty        => ""
      case Text(s)      => s 
      case Line         => "\n"
      case Cons(d1, d2) => layout(d1) + layout(d2)
    }
    layout(d)
  }
}
