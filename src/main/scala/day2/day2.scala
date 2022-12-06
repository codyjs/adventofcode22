package com.day2

import scala.io.Source

enum Move(val beats: Int, val weakTo: Int, val score: Int):
  case Rock extends Move(2, 1, 1)
  case Paper extends Move(0, 2, 2)
  case Scissors extends Move(1, 0, 3)

val moveCycle = List(Move.Rock, Move.Paper, Move.Scissors)

val moveScoreMap = Map(
  "X" -> 1,
  "Y" -> 2,
  "Z" -> 3
)

val outcomeMap = Map(
  "A X" -> 3, // Rock Rock
  "A Y" -> 6, // Rock Paper
  "A Z" -> 0, // Rock Scissors
  "B X" -> 0, // Paper Rock
  "B Y" -> 3, // Paper Paper
  "B Z" -> 6, // Paper Scissors
  "C X" -> 6, // Scissors Rock
  "C Y" -> 0, // Scissors Paper
  "C Z" -> 3  // Scissors Scissors
)

def solution(): Unit = 
  var runningTotal = 0
  Source.fromFile("C:/Users/slink/projects/adventofcode22/src/main/resources/day2/input.txt")
    .getLines()
    .foreach { line =>
      runningTotal += getScore(line)
    }
  println(runningTotal)

def getMoveScore(move: String): Int = moveScoreMap.get(move).get

def getOutcomeScore(line: String): Int = outcomeMap.get(line).get

def getScore(line: String): Int =
  val tokens = line.split(" ")
  val opponentMove = tokens(0) match
    case "A" => Move.Rock
    case "B" => Move.Paper
    case "C" => Move.Scissors
    case _ => throw new Exception("Unknown move " + tokens(0))
  return if tokens(1) == "X" then
    // lose
    0 + moveCycle(opponentMove.beats).score
  else if tokens(1) == "Y" then
    // draw
    3 + opponentMove.score
  else if tokens(1) == "Z" then
    // win
    6 + moveCycle(opponentMove.weakTo).score
  else throw new Exception("Unknown outcome " + tokens(1))
  