package literaljson

object JsonAST {
  import scala.text.Document
  import scala.text.Document._

  sealed abstract class JValue {
    def \\(nameToFind: String) = {
      def find(json: JValue): List[JField] = json match {
        case JObject(l) => l.foldLeft(List[JField]())((a, e) => find(e) ::: a)
        case JArray(l) => l.foldLeft(List[JField]())((a, e) => find(e) ::: a)
        case field @ JField(name, value) if name == nameToFind => field :: Nil
        case JField(_, value) => find(value)
        case _ => Nil
      }
      find(this)
    }
  }

  case object JNothing extends JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JDouble(num: Double) extends JValue
  case class JInt(num: BigInt) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JField(name: String, value: JValue) extends JValue
  case class JObject(obj: List[JField]) extends JValue
  case class JArray(arr: List[JValue]) extends JValue

  def render(value: JValue): Document = value match {
    case JBool(true)  => text("true")
    case JBool(false) => text("false")
    case JDouble(n)   => text(n.toString)
    case JInt(n)      => text(n.toString)
    case JNull        => text("null")
    case JNothing     => error("can't render 'nothing'")
    case JString(s)   => text("\"" + quote(s) + "\"")
    case JArray(arr)  => text("[") :: series(trimArr(arr).map(render(_))) :: text("]")
    case JField(n, v) => text("\"" + n + "\":") :: render(v)
    case JObject(obj) => 
      val nested = break :: fields(trimObj(obj).map(f => text("\"" + f.name + "\":") :: render(f.value)))
      text("{") :: nest(2, nested) :: break :: text("}")
  }

  private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
  private def trimObj(xs: List[JField]) = xs.filter(_.value != JNothing)
  private def fold(docs: List[Document]) = docs.foldLeft[Document](empty)(_ :: _)
  private def series(docs: List[Document]) = fold(punctuate(text(","), docs))
  private def fields(docs: List[Document]) = fold(punctuate(text(",") :: break, docs))
  private def punctuate(p: Document, docs: List[Document]): List[Document] = docs match {
    case Nil => Nil
    case List(d) => List(d)
    case d :: ds => (d :: p) :: punctuate(p, ds)
  }

  private def quote(s: String) = (s.map { 
    _ match {
      case '\r' => "\\r"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case c if ((c >= '\u0000' && c < '\u001f') || (c >= '\u0080' && c < '\u00a0') || (c >= '\u2000' && c < '\u2100')) => "\\u%04x".format(c.asInstanceOf[Int])
      case c => c
    }}).mkString
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

  implicit def pair2jvalue[A <% JValue](t: (String, A)) = JObject(List(JField(t._1, t._2)))
  implicit def list2jvalue(l: List[JField]) = JObject(l)
  implicit def jobject2assoc(o: JObject) = new JsonListAssoc(o.obj)
  implicit def pair2Assoc[A <% JValue](t: (String, A)) = new JsonAssoc(t)

  class JsonAssoc[A <% JValue](left: (String, A)) {
    def ~[B <% JValue](right: (String, B)) = {
      val l: JValue = left._2
      val r: JValue = right._2
      JObject(JField(left._1, l) :: JField(right._1, r) :: Nil)
    }

    def ~(right: JObject) = {
      val l: JValue = left._2
      JObject(JField(left._1, l) :: right.obj)
    }
  }

  class JsonListAssoc(left: List[JField]) {
    def ~(right: (String, JValue)) = JObject(left ::: List(JField(right._1, right._2)))
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
