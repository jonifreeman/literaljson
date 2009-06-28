package literaljson

object JsonAST {
  sealed abstract class JValue
  case object JNothing extends JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JDouble(num: Double) extends JValue
  case class JInt(num: BigInt) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JObject(obj: List[(String, JValue)]) extends JValue
  case class JArray(arr: List[JValue]) extends JValue

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
    case JDouble(n)   => text(n.toString)
    case JInt(n)      => text(n.toString)
    case JNull        => text("null")
    case JNothing     => error("can't render 'nothing'")
    case JString(s)   => text("\"" + s + "\"")
    case JArray(arr)  => text("[") <> series(trimArr(arr).map(render(_))) <> text("]")
    case JObject(obj) => text("{") <> series(trimObj(obj).map(f => text("\"" + f._1 + "\":") <> render(f._2))) <> text("}")
  }

  private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
  private def trimObj(xs: List[(String, JValue)]) = xs.filter(_._2 != JNothing)
  private def fold(docs: List[Doc]) = docs.foldLeft[Doc](Empty)(_ <> _)
  private def series(docs: List[Doc]) = fold(punctuate(text(","), docs))
  private def punctuate(p: Doc, docs: List[Doc]): List[Doc] = docs match {
    case Nil => Nil
    case List(d) => List(d)
    case d :: ds => (d <> p) :: punctuate(p, ds)
  }

  private def text(s: String) = if (s == "") Empty else Text(s)
}

object JsonDSL extends Printer {
  import JsonAST._

  implicit def int2jvalue(x: Int) = JInt(x)
  implicit def long2jvalue(x: Long) = JInt(x)
  implicit def bigint2jvalue(x: BigInt) = JInt(x)
  implicit def double2jvalue(x: Double) = JDouble(x)
  implicit def bigdecimal2jvalue(x: BigDecimal) = JDouble(x.doubleValue)
  implicit def boolean2jvalue(x: Boolean) = JBool(x)
  implicit def string2jvalue(x: String) = JString(x)
  implicit def seq2jvalue[A <% JValue](s: Seq[A]) = JArray(s.toList.map { a => val v: JValue = a; v })
  implicit def option2jvalue[A <% JValue](opt: Option[A]): JValue = opt match {
    case Some(x) => x
    case None => JNothing
  }

  implicit def pair2jvalue[A <% JValue](t: (String, A)) = JObject(List((t._1 -> t._2)))
  implicit def list2jvalue(l: List[(String, JValue)]) = JObject(l)
  implicit def jobject2assoc(o: JObject) = new JsonListAssoc(o.obj)
  implicit def pair2Assoc[A <% JValue](t: (String, A)) = new JsonAssoc(t)

  class JsonAssoc[A <% JValue](left: (String, A)) {
    def ~[B <% JValue](right: (String, B)) = {
      val l: JValue = left._2
      val r: JValue = right._2
      JObject((left._1, l) :: (right._1, r) :: Nil)
    }

    def ~(right: JObject) = {
      val l: JValue = left._2
      JObject((left._1, l) :: right.obj)
    }
  }

  class JsonListAssoc(left: List[(String, JValue)]) {
    def ~(right: (String, JValue)) = JObject(left ::: List(right))
    def ~(right: JObject) = JObject(left ::: right.obj)
  }
}

trait Printer {
  import JsonAST._

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
