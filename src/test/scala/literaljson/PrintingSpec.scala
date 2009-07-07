package literaljson

import org.scalacheck._

object PrintingSpec extends Properties("Printing") with JValueGen {
  import JsonAST._
  import JsonDSL._
  import scala.text.Document

  specify("rendering does not change semantics", (json: Document) => parse(pretty(json)) == parse(compact(json)))

  private def parse(json: String) = scala.util.parsing.json.JSON.parse(json)

  implicit def arbDoc: Arbitrary[Document] = Arbitrary(genJValue.map(render(_)))
}
