package com.day3

import scala.io.Source

def solution(): Unit = 
  var runningTotal = 0
  Source.fromFile("C:/Users/slink/projects/adventofcode22/src/main/resources/day3/input.txt")
    .getLines()
    .grouped(3)
    .foreach { group =>
      runningTotal += getPriority(findCommonItem(group))
    }
    println(runningTotal)

def findCommonItem(comp1: String, comp2: String): Char =
  var charset = Set[Int]()
  for (c <- comp1.charStepper.iterator) {
    charset = charset + c
  }

  for (c <- comp2.charStepper.iterator) {
    if charset.contains(c) then return c.toChar
  }

  throw new Exception("No common items found in rucksack")

def findCommonItem(group: Seq[String]): Char = group
  .reduce { (commonItems: String, sack: String) => commonItems.filter(item => sack.contains(item)) }
  .charAt(0)

def getPriority(item: Char): Int = if item.isUpper then return item.intValue - 38 else return item.intValue - 96