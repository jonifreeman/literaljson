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

    Scala std	  167127 ms
    Jackson       370 ms
    Literaljson	  549 ms

Summary:

* Jackson was fastest.
* Literaljson was about 300 times faster than standard Scala parser.

