package com.day4

import scala.io.Source

case class Range(val low: Int, val high: Int)

def solution(): Unit = 
  var runningTotal = Source.fromFile("C:/Users/slink/projects/adventofcode22/src/main/resources/day4/input.txt")
    .getLines()
    .map(parseLine)
    .count(doRangesPartiallyOverlap)
  println(runningTotal)

def parseLine(line: String): (Range, Range) = 
  return line.split(",") match {
    case Array(rangeStr1, rangeStr2) => (createRangeFromString(rangeStr1), createRangeFromString(rangeStr2))
  }

def createRangeFromString(rangeStr: String): Range =
  rangeStr.split("-") match {
    case Array(low, high) => new Range(low.toInt, high.toInt)
  }

def doRangesFullyOverlap(ranges: (Range, Range)): Boolean = 
  (ranges(0).high >= ranges(1).high && ranges(0).low <= ranges(1).low) ||
  (ranges(1).high >= ranges(0).high && ranges(1).low <= ranges(0).low)
  
def doRangesPartiallyOverlap(ranges: (Range, Range)): Boolean = 
  (ranges(0).high >= ranges(1).low && ranges(0).high <= ranges(1).high) ||
  (ranges(0).low <= ranges(1).high && ranges(0).low >= ranges(1).low) ||
  doRangesFullyOverlap(ranges)