package literaljson 

import scala.reflect.Manifest
import JsonAST._

object Extraction {
  /** Intermediate format which describes the mapping.
   *  This ADT is constructed (and then memoized) from given case class using reflection.
   */
  sealed abstract class Mapping
  case class Value(path: List[String]) extends Mapping
  case class Constructor(classname: String, args: List[Mapping]) extends Mapping
//  case class ListConstructor(constructors: List[Constructor]) extends Mapping

  // FIXME can this be JValue?
  def extract[A](obj: JObject)(implicit mf: Manifest[A]) = {
    val mapping = memoize(mf.erasure)
    println(mapping)

    def newInstance(classname: String, args: List[Any]) = {
      val clazz = Class.forName(classname)
      val argTypes = args.map {
        case x: List[_] => classOf[List[_]]
        case x => x.asInstanceOf[AnyRef].getClass
      }
      clazz.getConstructor(argTypes.toArray: _*).newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
    }

    def build(mapping: Mapping, argStack: List[Any]): List[Any] = mapping match {
      case Value(path) => (path.foldLeft(obj)(_ \\ _)).values.values.next :: argStack // FIXME must use \
      case Constructor(classname, args) => newInstance(classname, args.flatMap(build(_, argStack))) :: Nil
//      case ListConstructor(classname, args) => List(newInstance(classname, args.flatMap(build(_, argStack))) :: Nil)
    }

    build(mapping, Nil).head
  }

  // FIXME memoize
  def memoize(clazz: Class[_]) = {
    def makeMapping(clazz: Class[_], path: List[String]): Mapping = 
      Constructor(clazz.getName, clazz.getDeclaredFields.map { x =>
        if (x.getType == classOf[String]) Value(path + x.getName) // FIXME + is deprecated
        else makeMapping(x.getType, path + x.getName) // FIXME + is deprecated
      }.toList.reverse)
    
    makeMapping(clazz, Nil)
  }
}

case class Person(name: String, address: Address/*, children: List[Child]*/)
case class Address(street: String, city: String)
case class Child(name: String, age: BigInt)
//  case class Child(name: String, age: Int)

