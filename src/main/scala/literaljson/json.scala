package literaljson

object JsonAST {
  import scala.text.Document
  import scala.text.Document._

  sealed abstract class JValue
  case object JNothing extends JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JDouble(num: Double) extends JValue
  case class JInt(num: BigInt) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JObject(obj: List[(String, JValue)]) extends JValue
  case class JArray(arr: List[JValue]) extends JValue

  def render(value: JValue): Document = {
    def render(value: JValue, indentation: Int): Document = value match {
      case JBool(true)  => text("true")
      case JBool(false) => text("false")
      case JDouble(n)   => text(n.toString)
      case JInt(n)      => text(n.toString)
      case JNull        => text("null")
      case JNothing     => error("can't render 'nothing'")
      case JString(s)   => text("\"" + s + "\"")
      case JArray(arr)  => text("[") :: series(trimArr(arr).map(render(_, indentation))) :: text("]")
      case JObject(obj) => 
        val nested = fields(trimObj(obj).map(f => text("\"" + f._1 + "\":") :: render(f._2, indentation + 2)))
        text("{") :: break :: nest(indentation, nested) :: break :: text("}")
    }

    render(value, 0)
  }

  private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
  private def trimObj(xs: List[(String, JValue)]) = xs.filter(_._2 != JNothing)
  private def fold(docs: List[Document]) = docs.foldLeft[Document](empty)(_ :: _)
  private def series(docs: List[Document]) = fold(punctuate(text(","), docs))
  private def fields(docs: List[Document]) = fold(punctuate(text(",") :: break, docs))
  private def punctuate(p: Document, docs: List[Document]): List[Document] = docs match {
    case Nil => Nil
    case List(d) => List(d)
    case d :: ds => (d :: p) :: punctuate(p, ds)
  }
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
  import scala.text._

  def compact(d: Document) = {
    def layout(doc: Document): String = doc match {
      case DocText(s)      => s 
      case DocCons(d1, d2) => layout(d1) + layout(d2)
      case DocBreak        => ""
      case DocNest(_, d)   => layout(d)
      case DocGroup(d)     => layout(d)
      case DocNil          => ""
    }
    layout(d)
  }

  def pretty(d: Document) = {
    val s = new java.io.StringWriter
    d.format(80, s)
    s.toString
  }
}
