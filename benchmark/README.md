Benchmarking standard Scala JSON parser, Jackson parser and Literaljson parser
------------------------------------------------------------------------------

Benchmark measures how long it takes to parse 50 000 times the first JSON document
from http://www.json.org/example.html. 

Facts:

* Ubuntu 8.10
* Lenovo T60p
* Scala 2.7.4
* Java HotSpot(TM) Server VM (build 11.0-b15, mixed mode)


    Scala std	  175215ms
    Jackson       896ms
    Literaljson	  867ms

Jackson and Literaljson finished parsing in about same time, 200 times faster than standard Scala parser.

So is Literaljson as fast as Jackson? Not (yet :). Jackson wins if the number of parsed 
documents are increased to 500 000.

    Jackson       4930ms
    Literaljson   8711ms

