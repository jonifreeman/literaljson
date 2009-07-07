package literaljson

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import JsonAST._

trait JValueGen {
  def genJValue: Gen[JValue] = frequency((5, genSimple), (1, lzy(genArray)), (1, lzy(genObject)))
  def genSimple: Gen[JValue] = oneOf(
    value(JNull), 
    arbitrary[Int].map(JInt(_)),
    arbitrary[Double].map(JDouble(_)),
    arbitrary[Boolean].map(JBool(_)),
    arbitrary[String].map(JString(_)))

  def genArray: Gen[JValue] = for (l <- genList) yield JArray(l)
  def genObject: Gen[JValue] = for (l <- genFieldList) yield JObject(l)

  def genList = Gen.containerOfN[List, JValue](listSize, genJValue)
  def genFieldList = Gen.containerOfN[List, JField](listSize, genField)
  def genField = for (name <- identifier; value <- genJValue) yield JField(name, value)
  def listSize = choose(0, 5).sample.get
}
