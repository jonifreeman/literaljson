package literaljson

object JsonParser {
  import JsonAST._

  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token
  case object OpenArr extends Token
  case object CloseArr extends Token

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

  case class MArray() extends MValue {
    var values = List[MValue]()

    def +=(v: MValue) = values = v :: values

    def toJValue = JArray(values.map(_.toJValue))
  }
  
  def parse(s: String): Option[JValue] = {
    val p = new Parser(s)
    val vals = new ValStack
    var token: Token = null
    var roots = List[JValue]()

    def closeBlock(v: MValue) {
      vals.peekOption match {
        case Some(f: MField) => 
          f.value = v
          val field = vals.pop[MField]
          vals.peek[MObject] += field
        case Some(o: MObject) => o += v.asInstanceOf[MField]
        case Some(a: MArray) => a += v
        case None => roots = v.toJValue :: roots
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj => 
          vals.push(MObject())
        case FieldStart(name) => 
          vals.push(MField(name, null))
        case StringVal(x) => 
          vals.peek[MValue] match {
            case f: MField =>
              vals.pop[MField]
              f.value = MString(x)
              vals.peek[MObject] += f
            case a: MArray =>
              a += MString(x)
          }
        case CloseObj =>
          closeBlock(vals.pop[MValue])          
        case OpenArr => 
          vals.push(MArray())
        case CloseArr =>
          val arr = vals.pop[MArray]
          closeBlock(arr)
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
    import scala.collection.mutable.Stack

    val blocks = new Stack[BlockMode]()
    var fieldNameMode = true

    def nextToken: Token = {
      var i = 0      
      try {
        while (true) {
          var c = rest.charAt(i)
          if (c == '{') {
            blocks.push(OBJECT)
            rest = rest.substring(i + 1)
            fieldNameMode = true
            return OpenObj
          } else if (c == '}') {
            blocks.pop
            rest = rest.substring(i + 1)
            return CloseObj
          } else if (c == '"') {
            val end = rest.indexOf("\"", i + 1)
            val value = rest.substring(i + 1, end)
            rest = rest.substring(end + 1)
            if (fieldNameMode && blocks.top == OBJECT) return FieldStart(value)
            else {
              fieldNameMode = true
              return StringVal(value)
            }
          } else if (c == ':') {
            fieldNameMode = false
            i = i + 1
          } else if (c == '[') {
            blocks.push(ARRAY)
            rest = rest.substring(i + 1)
            return OpenArr
          } else if (c == ']') {
            blocks.pop
            rest = rest.substring(i + 1)
            return CloseArr
          }
          else if (c == ' ' || c == '\n' || c == ',') i = i + 1
          else error("unknown token " + c)
        }
        error("parse error " + rest)
      } catch {
        case e: StringIndexOutOfBoundsException => End
      }
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }
}
