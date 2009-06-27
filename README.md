LiteralJson contains formatting utilities for JSON.

DSL rules
---------

* Primitive types map to JSON primitives.
* Any seq produces JSON array.

      scala> val json = List(1, 2, 3)

      scala> compact(JsonAST.render(json))

      res0: String = [1,2,3]

* Tuple2[String, A] produces field.

      scala> val json = ("name", "joe")

      scala> compact(JsonAST.render(json))

      res1: String = {"name":"joe"}

* ~ operator produces object by combining fields.

      scala> val json = ("name", "joe") ~ ("age", 35)

      scala> compact(JsonAST.render(json))

      res2: String = {"name":"joe","age":35}

Example
-------

    object JsonExample extends Application {
      import literaljson.JsonAST
      import literaljson.JsonDSL._

      case class Winner(id: Long, numbers: List[Int])
      case class Lotto(id: Long, winningNumbers: List[Int], winners: List[Winner])

      val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
      val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5, 3), winners)

      val json = 
        ("lotto" ->
          ("lotto-id" -> lotto.id) ~
          ("winning-numbers" -> lotto.winningNumbers) ~
          ("winners" ->
            lotto.winners.map { w =>
              (("winner-id" -> w.id) ~
               ("numbers" -> w.numbers))}))

      println(compact(JsonAST.render(json)))
    }

    scala> JsonExample
    {"lotto":{"lotto-id":5,"winning-numbers":[2,45,34,23,7,5,3],"winners":[{"winner-id":23,"numbers":[2,45,34,23,3,5]},{"winner-id":54,"numbers":[52,3,12,11,18,22]}]}}

Example produces following pretty printed JSON:

    { 
      "lotto": {
        "lotto-id": 5,
        "winning-numbers": [2, 45, 34, 23, 7, 5, 3] 
        "winners": [
          {
            "winner-id": 23,
            "card": [2, 45, 34, 23, 3, 5] 
          },
          {
            "winner-id": 54,
            "card": [52, 3, 12, 11, 18, 22] 
          } 
        ]
      }
    }

TODO + ideas
------------

* Support for Options.
* String escaping.
* Pretty printing.
* Add parser which parses to AST.

Kudos
-----

* The original idea for DSL syntax was taken from a Lift mailing list ([by Marius](http://markmail.org/message/lniven2hn22vhupu)).

* The idea for AST and rendering was taken from [Real World Haskell book](http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html).
