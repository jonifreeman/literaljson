package literaljson

/** Fast imperative parser.
 */
object JsonParser {
  import JsonAST._

  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token
  case class IntVal(value: BigInt) extends Token
  case class DoubleVal(value: Double) extends Token
  case class BoolVal(value: Boolean) extends Token
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

  case class MInt(value: BigInt) extends MValue {
    def toJValue = JInt(value)
  }

  case class MDouble(value: Double) extends MValue {
    def toJValue = JDouble(value)
  }

  case class MBool(value: Boolean) extends MValue {
    def toJValue = JBool(value)
  }

  trait MBlock[A <: MValue] {
    protected var elems = List[A]()
    def +=(f: A) = elems = f :: elems
  }

  case class MObject() extends MValue with MBlock[MField] {
    def toJValue = JObject(elems.map(_.toJValue).reverse)
  }

  case class MArray() extends MValue with MBlock[MValue] {
    def toJValue = JArray(elems.map(_.toJValue).reverse)
  }
  
  def parse(s: String): Option[JValue] = {
    val p = new Parser(new StringBuilder(s))
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

    def newValue(v: MValue) {
      vals.peek[MValue] match {
        case f: MField =>
          vals.pop[MField]
          f.value = v
          vals.peek[MObject] += f
        case a: MArray =>
          a += v
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(MObject())
        case FieldStart(name) => vals.push(MField(name, null))
        case StringVal(x)     => newValue(MString(x))
        case IntVal(x)        => newValue(MInt(x))
        case DoubleVal(x)     => newValue(MDouble(x))
        case BoolVal(x)       => newValue(MBool(x))
        case CloseObj         => closeBlock(vals.pop[MValue])          
        case OpenArr          => vals.push(MArray())
        case CloseArr         => closeBlock(vals.pop[MArray])
        case End              =>
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

  private class Parser(rest: StringBuilder) {
    import scala.collection.mutable.Stack

    val blocks = new Stack[BlockMode]()
    var fieldNameMode = true
    var cur = 0

    def nextToken: Token = {
      def indexOfLastDigit(s: StringBuilder, index: Int): Int = {
        var i = index
        while (true) {
          val c = s.charAt(i)
          if (!(Character.isDigit(c) || c == '.')) return i-1
          i = i+1
        }
        error("expected Number")
      }

      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseString(startIndex: Int): String = {
        var i = startIndex
        while (true) {
          if (rest.charAt(i) == '\\') {
            if (rest.charAt(i+1) == '"') rest.deleteCharAt(i)
          } else if (rest.charAt(i) == '"') {
            return rest.substring(startIndex, i)
          }
          i = i+1
        }
        error("can't happen")
      }

      try {
        while (true) {
          rest.charAt(cur) match {
            case '{' =>
              blocks.push(OBJECT)
              cur = cur+1
              fieldNameMode = true
              return OpenObj
            case '}' =>
              blocks.pop
              cur = cur+1
              return CloseObj
            case '"' =>
              val value = parseString(cur+1)
              cur = cur+value.length+2
              if (fieldNameMode && blocks.top == OBJECT) return FieldStart(value)
              else {
                fieldNameMode = true
                return StringVal(value)
              }
            case c if Character.isDigit(c) =>
              val end = indexOfLastDigit(rest, cur)
              val value = rest.substring(cur, end+1)
              cur = end+1
              fieldNameMode = true
              if (value.contains('.')) return DoubleVal(value.toDouble) else return IntVal(BigInt(value))
            case 't' =>
              if (rest.charAt(cur+1) == 'r' && rest.charAt(cur+2) == 'u' && rest.charAt(cur+3) == 'e' && isDelimiter(rest.charAt(cur+4))) {
                cur = cur+4
                return BoolVal(true)
              }
              error("expected boolean")
            case 'f' =>
              if (rest.charAt(cur+1) == 'a' && rest.charAt(cur+2) == 'l' && rest.charAt(cur+3) == 's' && rest.charAt(cur+4) == 'e' && isDelimiter(rest.charAt(cur+5))) {
                cur = cur+5
                return BoolVal(false)
              }
              error("expected boolean")
            case ':' =>
              fieldNameMode = false
              cur = cur+1
            case '[' =>
              blocks.push(ARRAY)
              cur = cur+1
              return OpenArr
            case ']' =>
              blocks.pop
              cur = cur+1
              return CloseArr
            case c if isDelimiter(c) => cur = cur+1
            case c => error("unknown token " + c)
          }
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
