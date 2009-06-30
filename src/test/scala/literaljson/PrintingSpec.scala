package literaljson

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

object PrintingSpec extends Properties("Printing") {
  import JsonAST._
  import JsonDSL._
  import scala.text.Document

  specify("rendering does not change semantics", (json: Document) => parse(pretty(json)) == parse(compact(json)))

  private def parse(json: String) = scala.util.parsing.json.JSON.parse(json)

  implicit def arbDoc: Arbitrary[Document] = Arbitrary(genJValue.map(render(_)))
  
  def genJValue: Gen[JValue] = frequency((5, genSimple), (1, lzy(genArray)), (1, lzy(genObject)))
  def genSimple: Gen[JValue] = oneOf(
    value(JNull), 
    arbitrary[Int].map(JInt(_)),
    arbitrary[Double].map(JDouble(_)),
    arbitrary[Boolean].map(JBool(_)),
    arbitrary[String].map(JString(_)))

  def genArray: Gen[JValue] = for (l <- genList) yield JArray(l)
  def genObject: Gen[JValue] = for (l <- genTupleList) yield JObject(l)

  def genList = Gen.containerOfN[List, JValue](listSize, genJValue)
  def genTupleList = Gen.containerOfN[List, (String, JValue)](listSize, genField)
  def genField = for (name <- identifier; value <- genJValue) yield (name, value)
  def listSize = choose(0, 5).sample.get
}
