Benchmarking standard Scala JSON parser, Jackson parser and Literaljson parser
------------------------------------------------------------------------------

Benchmark measures how long it takes to parse 50 000 times the first JSON document
from http://www.json.org/example.html. 

Facts:

* Ubuntu 8.10
* Lenovo T60p
* Scala 2.7.4
* java version "1.6.0_10"
  Java(TM) SE Runtime Environment (build 1.6.0_10-b33)
  Java HotSpot(TM) Server VM (build 11.0-b15, mixed mode)
* Exec: scala jsonbench

Parsing 50 000 json documents:

    Scala std	  175215 ms
    Jackson       896 ms
    Literaljson	  867 ms

Jackson and Literaljson finished parsing in about same time, 200 times faster than standard Scala parser.

So is Literaljson as fast as Jackson? Not quite. Jackson wins if the number of parsed 
documents is increased to 500 000.

Parsing 500 000 json documents:

    Jackson       4304 ms
    Literaljson   5803 ms

