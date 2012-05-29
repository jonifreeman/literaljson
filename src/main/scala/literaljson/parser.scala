package literaljson

/** Fast imperative parser.
 */
object JsonParser {
  import JsonAST._

  case class ParseError(message: String)

  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token
  case class IntVal(value: BigInt) extends Token
  case class DoubleVal(value: Double) extends Token
  case class BoolVal(value: Boolean) extends Token
  case object NullVal extends Token
  case object OpenArr extends Token
  case object CloseArr extends Token

  trait MValue {
    def toJValue: JValue
  }

  case class MField(name: String, var value: MValue) extends MValue {
    def toJValue = JField(name, value.toJValue)
  }

  case object MNull extends MValue {
    def toJValue = JNull
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

  def parse(s: String): Either[ParseError, JValue] =
    try {
      Right(parse0(s))
    } catch {
      case e: Exception => Left(ParseError(e.getMessage))
    }

  private def parse0(s: String): JValue = {
    val p = new Parser(s)
    val vals = new ValStack
    var token: Token = null
    var root: Option[JValue] = None

    def closeBlock(v: MValue) {
      vals.peekOption match {
        case Some(f: MField) =>
          f.value = v
          val field = vals.pop[MField]
          vals.peek[MObject] += field
        case Some(o: MObject) => o += v.asInstanceOf[MField]
        case Some(a: MArray) => a += v
        case None => root = Some(v.toJValue)
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
        case NullVal          => newValue(MNull)
        case CloseObj         => closeBlock(vals.pop[MValue])
        case OpenArr          => vals.push(MArray())
        case CloseArr         => closeBlock(vals.pop[MArray])
        case End              =>
      }
    } while (token != End)

    root.get
  }

  private class ValStack {
    import java.util.LinkedList
    val stack = new LinkedList[MValue]()

    def pop[A <: MValue] = stack.poll match {
      case x: A => x
      case x => error("unexpected " + x)
    }

    def push(v: MValue) = stack.addFirst(v)

    def peek[A <: MValue] = stack.peek match {
      case x: A => x
      case x => error("unexpected " + x)
    }

    def peekOption = if (stack isEmpty) None else Some(stack.peek)
  }

  private class Parser(buf: String) {
    import java.util.LinkedList

    val blocks = new LinkedList[BlockMode]()
    var fieldNameMode = true
    var cur = 0 // Pointer which points current parsing location

    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseFieldName: String = {
        cur = cur+1
        val start = cur
        while (true) {
          val c = buf.charAt(cur)
          if (c == '"') {
            cur = cur+1
            return buf.substring(start, cur-1)
          }
          cur = cur+1
        }
        error("can't happen")
      }

      def parseString: String = {
        cur = cur+1
        val s = new StringBuilder
        while (true) {
          var c = buf.charAt(cur)
          if (c == '"') {
            cur = cur+1
            return s.toString
          }

          c = if (c == '\\') {
            cur = cur+1
            buf.charAt(cur) match {
              case '"' => '"'
              case 'n' => '\n'
              case 't' => '\t'
              case 'r' => '\r'
              case _ => '\\'
            }
          } else c
          s.append(c)
          cur = cur+1
        }
        error("can't happen")
      }

      def parseValue = {
        var i = cur+1
        var wasInt = true
        var doubleVal = false
        while (wasInt) {
          val c = buf.charAt(i)
          if (c == '.') {
            doubleVal = true
            i = i+1
          } else if (!(Character.isDigit(c) || c == '.')) {
            wasInt = false
          } else {
            i = i+1
          }
        }
        val value = buf.substring(cur, i)
        cur = i
        if (doubleVal) DoubleVal(value.toDouble) else IntVal(BigInt(value))
      }

      val len = buf.length
      while (cur < len) {
        buf.charAt(cur) match {
          case '{' =>
            blocks.addFirst(OBJECT)
            cur = cur+1
            fieldNameMode = true
            return OpenObj
          case '}' =>
            blocks.poll
            cur = cur+1
            return CloseObj
          case '"' =>
            if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseFieldName)
            else {
              fieldNameMode = true
              return StringVal(parseString)
            }
          case 't' =>
            fieldNameMode = true
            if (buf.charAt(cur+1) == 'r' && buf.charAt(cur+2) == 'u' && buf.charAt(cur+3) == 'e' && isDelimiter(buf.charAt(cur+4))) {
              cur = cur+4
              return BoolVal(true)
            }
            error("expected boolean")
          case 'f' =>
            fieldNameMode = true
            if (buf.charAt(cur+1) == 'a' && buf.charAt(cur+2) == 'l' && buf.charAt(cur+3) == 's' && buf.charAt(cur+4) == 'e' && isDelimiter(buf.charAt(cur+5))) {
              cur = cur+5
              return BoolVal(false)
            }
            error("expected boolean")
          case 'n' =>
            fieldNameMode = true
            if (buf.charAt(cur+1) == 'u' && buf.charAt(cur+2) == 'l' && buf.charAt(cur+3) == 'l' && isDelimiter(buf.charAt(cur+4))) {
              cur = cur+4
              return NullVal
            }
            error("expected null")
          case ':' =>
            fieldNameMode = false
            cur = cur+1
          case '[' =>
            blocks.addFirst(ARRAY)
            cur = cur+1
            return OpenArr
          case ']' =>
            fieldNameMode = true
            blocks.poll
            cur = cur+1
            return CloseArr
          case c if Character.isDigit(c) || c == '-' =>
            fieldNameMode = true
            return parseValue
          case c if isDelimiter(c) => cur = cur+1
          case c => error("unknown token " + c)
        }
      }
      End
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }
}
