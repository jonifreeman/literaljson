package literaljson

import org.scalatest._

class ExampleSuite extends FunSuite {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  test("Lotto example") {
    val lottoAST = parse(lotto)
    val renderedLotto = compact(render(lottoAST))
    assert(lottoAST == parse(renderedLotto))
  }

  test("Person example") {
    val personAST = parse(person)
    val renderedPerson = pretty(render(personAST))
    assert(personAST == parse(renderedPerson))
    assert(render(personAST) == render(personDSL))

    assert(compact(render(personAST \\ "name")) == """{"name":"Joe","name":"Marilyn"}""")
    assert(compact(render(personAST \ "name")) == """{"name":"Joe"}""")
  }

  test("Quoted example") {
    val quotedAST = parse(quoted)
    expect(compact(render(quotedAST))) ("""{"foo":"\n\t\r"}""")
  }

  val lotto = """
{
  "lotto":{
    "lotto-id":5,
    "winning-numbers":[2,45,34,23,7,5,3],
    "winners":[ {
      "winner-id":23,
      "numbers":[2,45,34,23,3, 5]
    },{
      "winner-id" : 54 ,
      "numbers":[ 52,3, 12,11,18,22 ]
    }]
  }
}
"""

  val person = """
{ 
  "person": {
    "name": "Joe",
    "age": 35,
    "spouse": {
      "person": {
        "name": "Marilyn"
        "age": 33
      }
    }
  }
}
"""

  val personDSL = 
    ( "person" ->
     ("name" -> "Joe") ~
     ("age" -> 35) ~
     ("spouse" -> 
      ("person" -> 
       ("name" -> "Marilyn") ~
       ("age" -> 33)
     )
    )
   )

  val quoted = "{ \"foo\" : \"\n\t\r\" }"
}

