package com.day9

import scala.io.Source

enum Direction:
    case Up, Down, Left, Right

case class Move(val dir: Direction, val num: Int)
extension (m: Move)
    def decrement: Move = Move(m.dir, m.num - 1)

type Coord = (Int, Int)
extension (c: Coord)
    def x: Int = c._1
    def y: Int = c._2
    def moveInDirection(dir: Direction): Coord = dir match
        case Direction.Up    => (x, y + 1)
        case Direction.Down  => (x, y - 1)
        case Direction.Left  => (x - 1, y)
        case Direction.Right => (x + 1, y)
    

val inputFile = "src/main/resources/day9/input.txt"

def solution(): Unit =
    val moves = Source.fromFile(inputFile).getLines().map(str =>
        val tokens = str.split(" ")
        tokens(0) match
            case "U" => Move(Direction.Up, tokens(1).toInt)
            case "D" => Move(Direction.Down, tokens(1).toInt)
            case "L" => Move(Direction.Left, tokens(1).toInt)
            case "R" => Move(Direction.Right, tokens(1).toInt)
            case x => throw Exception(s"Unknown direction $x")
        
    ).toList

    val parseDoneTime = System.currentTimeMillis()

    val part1Rope = List((0, 0), (0, 0))
    val part1visitedCoords = performMoves(moves, part1Rope).toSet

    val part1DoneTime = System.currentTimeMillis()
    println(s"Part1: ${part1visitedCoords.size}, finished in ${part1DoneTime - parseDoneTime} ms")

    val part2Rope = (0 until 10).map(_ => (0, 0)).toList
    val part2visitedCoords = performMoves(moves, part2Rope).toSet

    val part2DoneTime = System.currentTimeMillis()

def performMoves(moves: List[Move], rope: List[Coord]): List[Coord] =
    moves match
        case move :: next =>
            val newHead = rope.head.moveInDirection(move.dir) 
            val newRope = newHead :: moveRope(rope.tail, newHead)
            val newMoves = if move.num <= 1 then next else move.decrement :: next
            rope.last :: performMoves(newMoves, newRope)
        case Nil => List(rope.last)

def moveRope(rope: List[Coord], leader: Coord): List[Coord] = rope match
    case head :: next =>
        val newHead = followLeader(head, leader)
        newHead :: moveRope(next, newHead)
    case Nil => List()

def followLeader(follower: Coord, leader: Coord): Coord =
    val coordDifference = (leader.x - follower.x, leader.y - follower.y)
    if Math.abs(coordDifference.x) <= 1 && Math.abs(coordDifference.y) <= 1 then follower
    else if coordDifference.x == 0 then
        follower.moveInDirection(if coordDifference.y > 0 then Direction.Up else Direction.Down)
    else if coordDifference.y == 0 then
        follower.moveInDirection(if coordDifference.x > 0 then Direction.Right else Direction.Left)
    else
        (follower.x + (if coordDifference.x > 0 then 1 else -1), follower.y + (if coordDifference.y > 0 then 1 else -1))
