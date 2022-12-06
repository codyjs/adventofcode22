package com.day5

import scala.io.Source

case class Instruction(val moveCount: Int, val fromId: Int, val toId: Int)

def solution(): Unit = 
  val inputIterator = Source.fromFile("C:/Users/slink/projects/adventofcode22/src/main/resources/day5/input.txt").getLines()
  val stacks = getStacksFromInput(inputIterator)
  val procedure = getInstructionsFromInput(inputIterator)
  val newStacks = executeProcedurePart2(stacks, procedure)
  println(getTopCrates(newStacks))
  
def getStacksFromInput(input: Iterator[String]): Vector[List[Char]] = input
  .takeWhile(line => line.startsWith("["))
  .map(extractCrates)
  .flatMap(identity)
  .grouped(9)
  .foldLeft(Vector(List(), List(), List(), List(), List(), List(), List(), List(), List()))(accumulateCrates)

def extractCrates(line: String): Iterator[Option[Char]] = line
  .grouped(4)
  .map(crate => crate.charAt(1) match {
    case c if c.isSpaceChar => None
    case c if c.isLetter => Option(c)
  })

def accumulateCrates(accumulator: Vector[List[Char]], crateRow: Seq[Option[Char]]) = accumulator
  .zip(crateRow)
  .map((stack: List[Char], maybeCrate: Option[Char]) => maybeCrate.map(crate => stack :+ crate).getOrElse(stack))

def getInstructionsFromInput(input: Iterator[String]): Iterator[Instruction] = input
  .filter(line => line.startsWith("move"))
  .map(line => line.split(" "))
  .map(tokens => new Instruction(tokens(1).toInt, tokens(3).toInt - 1, tokens(5).toInt - 1))

def executeProcedurePart1(stacks: Vector[List[Char]], procedure: Iterator[Instruction]) = procedure
  .foldLeft(stacks)(crateMover9000)

def crateMover9000(stacks: Vector[List[Char]], instruction: Instruction): Vector[List[Char]] = 
  var fromStack = stacks(instruction.fromId)
  val popped = fromStack.head
  fromStack = fromStack.tail
  var toStack = stacks(instruction.toId)
  toStack = popped :: toStack
  val newStacks = stacks
    .updated(instruction.fromId, fromStack)
    .updated(instruction.toId, toStack)
  return if (instruction.moveCount > 1) then
    crateMover9000(newStacks, new Instruction(instruction.moveCount - 1, instruction.fromId, instruction.toId))
  else
    newStacks

def executeProcedurePart2(stacks: Vector[List[Char]], procedure: Iterator[Instruction]) = procedure
  .foldLeft(stacks)(crateMover9001)

def crateMover9001(stacks: Vector[List[Char]], instruction: Instruction): Vector[List[Char]] = 
  var fromStack = stacks(instruction.fromId)
  val (newFromStack, poppedList) = popFromStack(instruction.moveCount, fromStack)
  fromStack = newFromStack
  var toStack = stacks(instruction.toId)
  toStack = poppedList ::: toStack
  val newStacks = stacks
    .updated(instruction.fromId, fromStack)
    .updated(instruction.toId, toStack)
  return newStacks

def popFromStack(x: Int, stack: List[Char], accumulator: List[Char] = List()): (List[Char], List[Char]) =
  if x == 1 then return (stack.tail, accumulator :+ stack.head)
  else popFromStack(x - 1, stack.tail, accumulator :+ stack.head)

def getTopCrates(stacks: Vector[List[Char]]): String = stacks
  .map(stack => stack.head)
  .foldLeft("")((str: String, c: Char) => str :+ c)