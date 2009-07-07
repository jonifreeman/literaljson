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

  private class Parser(private var rest: String) {
    import scala.collection.mutable.Stack

    val blocks = new Stack[BlockMode]()
    var fieldNameMode = true

    def nextToken: Token = {
      def indexOfLastDigit(s: String, index: Int): Int = {
        var i = index
        while (true) {
          val c = s.charAt(i)
          if (!(Character.isDigit(c) || c == '.')) return i - 1
          i = i + 1
        }
        error("expected Number")
      }

      var i = 0      
      try {
        while (true) {
          rest.charAt(i) match {
            case '{' =>
              blocks.push(OBJECT)
              rest = rest.substring(i + 1)
              fieldNameMode = true
              return OpenObj
            case '}' =>
              blocks.pop
              rest = rest.substring(i + 1)
              return CloseObj
            case '"' =>
              val end = rest.indexOf("\"", i + 1)
              val value = rest.substring(i + 1, end)
              rest = rest.substring(end + 1)
              if (fieldNameMode && blocks.top == OBJECT) return FieldStart(value)
              else {
                fieldNameMode = true
                return StringVal(value)
              }
            case c if Character.isDigit(c) =>
              val end = indexOfLastDigit(rest, i)
              val value = rest.substring(i, end + 1)
              rest = rest.substring(end + 1)
              fieldNameMode = true
              if (value.contains('.')) return DoubleVal(value.toDouble) else return IntVal(BigInt(value))
            case ':' =>
              fieldNameMode = false
              i = i + 1
            case '[' =>
              blocks.push(ARRAY)
              rest = rest.substring(i + 1)
              return OpenArr
            case ']' =>
              blocks.pop
              rest = rest.substring(i + 1)
              return CloseArr
            case ' ' | '\n' | ',' => i = i + 1
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
