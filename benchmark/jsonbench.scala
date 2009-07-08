/**
 * Following libs are required to compile and run the benchmark:
 * - jackson-core-asl-1.0.0.jar
 * - jackson-mapper-asl-1.0.0.jar
 * - literaljson-0.1.jar
 */
object jsonbench {
  import scala.util.parsing.json.JSON
  import org.codehaus.jackson._
  import org.codehaus.jackson.map._
  import literaljson.JsonParser

  def main(args: Array[String]) = {
    benchmark("Scala std") { JSON.parse(json) }
    val mapper = new ObjectMapper
    benchmark("Jackson") { mapper.readValue(json, classOf[java.util.HashMap[_, _]]) }
    benchmark("Literaljson") { JsonParser.parse(json) }
  }

  def benchmark(name: String)(f: => Any) = {
    println("warmup")
    (1 to 1000).foreach(i => f)
    println("warmup done")
    val t = time {
      (1 to 50000).foreach(i => f)
    }
    println(name + "\t" + t + "ms")
  }

  def time(f: => Any): Long = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis - start
  }

  val json = """
{
  "glossary": {
    "title": "example glossary",
    "GlossDiv": {
      "title": "S",
      "GlossList": {
        "GlossEntry": {
          "ID": "SGML",
	  "SortAs": "SGML",
	  "GlossTerm": "Standard Generalized Markup Language",
	  "Acronym": "SGML",
	  "Abbrev": "ISO 8879:1986",
	  "GlossDef": {
            "para": "A meta-markup language, used to create markup languages such as DocBook.",
	    "GlossSeeAlso": ["GML", "XML"]
          },
	  "GlossSee": "markup"
        }
      }
    }
  }
}
"""
}
