package literaljson

import org.scalacheck._

object ParserSpec extends Properties("Parser") with JValueGen {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  specify("any valid json can be parsed", (json: JValue) => parse(pretty(render(json))).isRight)

  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genObject)
}
