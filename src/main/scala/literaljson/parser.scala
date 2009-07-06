package literaljson

object JsonParser {
  import JsonAST._

  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token

  trait MValue {
    def toJValue: JValue
  }

  case class MField(name: String, var value: MValue) extends MValue {
    def toJValue = JField(name, value.toJValue)
  }

  case class MString(value: String) extends MValue {
    def toJValue = JString(value)
  }

  case class MObject() extends MValue {
    var fields = List[MField]()

    def +=(f: MField) = fields = f :: fields

    def toJValue = JObject(fields.map(_.toJValue))
  }
  
  def parse(s: String): Option[JValue] = {
    val p = new Parser(s)
    val vals = new ValStack
    var token: Token = null
    var roots = List[JValue]()
    do {
      token = p.nextToken
      token match {
        case OpenObj => 
          vals.push(MObject())
        case FieldStart(name) => 
          vals.push(MField(name, null))
        case StringVal(x) => 
          val field = vals.pop[MField]
          field.value = MString(x)
          vals.peek[MObject] += field
        case CloseObj =>
          val obj = vals.pop[MValue]
          vals.peekOption match {
            case Some(f: MField) => 
              f.value = obj
              val field = vals.pop[MField]
              vals.peek[MObject] += field
            case Some(o: MObject) => o += obj.asInstanceOf[MField]
            case None => roots = obj.toJValue :: roots
          }
        case End =>
      }
    } while (token != End)

    Some(roots.head) // FIXME
  }

  private class ValStack {
    import scala.collection.mutable.Stack
    val stack = new Stack[MValue]()

    def pop[A <: MValue] = stack.pop match {
      case x: A => x
      case x => error("unexpected " + x)
    }

    def push(v: MValue) = stack.push(v)

    def peek[A <: MValue] = stack.top match {
      case x: A => x
      case x => error("unexpected " + x)
    }

    def peekOption = if (stack isEmpty) None else Some(stack.top)
  }

  private class Parser(private var rest: String) {
    var fieldNameMode = true

    def nextToken: Token = {
      var i = 0      
      try {
        while (true) {
          var c = rest.charAt(i)
          if (c == '{') {
            rest = rest.substring(i + 1)
            fieldNameMode = true
            return OpenObj
          } else if (c == '}') {
            rest = rest.substring(i + 1)
            return CloseObj
          } else if (c == '"') {
            val end = rest.indexOf("\"", i + 1)
            val value = rest.substring(i + 1, end)
            rest = rest.substring(end + 1)
            if (fieldNameMode) return FieldStart(value)
            else {
              fieldNameMode = true
              return StringVal(value)
            }
          } else if (c == ':') {
            fieldNameMode = false
            i = i + 1
          }
          else if (c == ' ' || c == ',') i = i + 1
          else error("unknown token " + c)
        }
        error("parse error " + rest)
      } catch {
        case e: StringIndexOutOfBoundsException => End
      }
    }
  }
}
