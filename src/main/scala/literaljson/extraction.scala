package literaljson 

import scala.reflect.Manifest
import JsonAST._

/** Function to extract values from JSON AST using case classes.
 *  Note, this is experimental and not fully implemented yet.
 *
 *  See: example.scala "Extraction example", "Partial extraction example"
 */
object Extraction {
  /** Intermediate format which describes the mapping.
   *  This ADT is constructed (and then memoized) from given case class using reflection.
   *
   *  Example mapping.
   *
   *  package xx 
   *  case class Person(name: String, address: Address, children: List[Child])
   *  case class Address(street: String, city: String)
   *  case class Child(name: String, age: BigInt)
   *
   *  will produce following Mapping:
   *
   *  Constructor(None, "xx.Person", List(
   *    Value("name"),
   *    Constructor(Some("address"), "xx.Address", List(Value("street"), Value("city"))),
   *    ListConstructor("children", "xx.Child", List(Value("name"), Value("age")))))
   */
  sealed abstract class Mapping
  case class Value(path: String) extends Mapping
  case class Constructor(path: Option[String], classname: String, args: List[Mapping]) extends Mapping
  case class ListConstructor(path: String, classname: String, args: List[Mapping]) extends Mapping

  // FIXME; should the return type be Either[MappingError, A] ?
  def extract[A](json: JValue)(implicit mf: Manifest[A]) = {
    val mapping = memoize(mf.erasure)

    def newInstance(classname: String, args: List[Any]) = {
      val clazz = Class.forName(classname)
      val argTypes = args.map {
        case x: List[_] => classOf[List[_]]
        case x => x.asInstanceOf[AnyRef].getClass
      }
      clazz.getConstructor(argTypes.toArray: _*).newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
    }

    def build(root: JValue, mapping: Mapping, argStack: List[Any]): List[Any] = mapping match {
      case Value(path) => fieldValue(root, path).values :: argStack
      case Constructor(path, classname, args) => 
        val newRoot = path match {
          case Some(p) => root \ p
          case None => root
        }
        newInstance(classname, args.flatMap(build(newRoot, _, argStack))) :: Nil
      case ListConstructor(path, classname, args) => 
        val arr = fieldValue(root, path).asInstanceOf[JArray]
        arr.arr.map(elem => newInstance(classname, args.flatMap(build(elem, _, argStack)))) :: argStack
    }

    def fieldValue(json: JValue, path: String) = (json \ path).asInstanceOf[JField].value

    build(json, mapping, Nil).head
  }

  // FIXME memoize
  private def memoize(clazz: Class[_]) = {
    def makeMapping(path: Option[String], clazz: Class[_], isList: Boolean): Mapping = isList match {
      case false => Constructor(path, clazz.getName, constructorArgs(clazz))
      case true  => ListConstructor(path.get, clazz.getName, constructorArgs(clazz))
    }

    // FIXME add rest of the primitives
    def constructorArgs(clazz: Class[_]) = clazz.getDeclaredFields.map { x =>
      if (x.getType == classOf[String]) Value(x.getName)
      else if (x.getType == classOf[BigInt]) Value(x.getName)
      else if (x.getType == classOf[List[_]]) makeMapping(Some(x.getName), Util.parametrizedType(x), true)
      else makeMapping(Some(x.getName), x.getType, false)
    }.toList.reverse // FIXME Java6 returns these in reverse order, verify that and check other vms
    
    makeMapping(None, clazz, false)
  }
}
