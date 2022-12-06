package com.day6

import scala.io.Source

val inputFile = "src/main/resources/day6/input.txt"
val markerSize = 14; // 4;

def solution(): Unit =
  val count = Source.fromFile(inputFile)
    .sliding(markerSize)
    .map(str => str.toSet)
    .takeWhile(set => set.size < markerSize)
    .count(_ => true) + markerSize
  println(count)
