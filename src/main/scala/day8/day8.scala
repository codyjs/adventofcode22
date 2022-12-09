package com.day8

import scala.io.Source
import scala.collection.IndexedSeqView.Slice

val inputFile = "src/main/resources/day8/input.txt"

enum Direction:
    case Left, Right, Top, Bottom

def solution(): Unit = 
    val treeMatrix = Source.fromFile(inputFile).getLines().map(str => str.toArray).toArray
    val visibleCount = (0 until treeMatrix.size).foldLeft(0)((runningTotal, y) =>
        (0 until treeMatrix.size).foldLeft(runningTotal)((runningTotal, x) =>
            val result = isVisible(treeMatrix, x, y)
            runningTotal + (if result then 1 else 0)
        )
    )
    val maxScenicScore = (0 until treeMatrix.size).foldLeft(0)((runningMax, y) =>
        (0 until treeMatrix.size).foldLeft(runningMax)((runningMax, x) =>
            Math.max(runningMax, getScenicScore(treeMatrix, x, y))
        )
    )
    println(s"Part1: $visibleCount")
    println(s"Part2: $maxScenicScore")

def isVisible(matrix: Array[Array[Char]], x: Int, y: Int): Boolean = 
    // if the tree is on an edge, it is visible:
    if x == 0 || x == (matrix.size - 1) || y == 0 || y == (matrix.size - 1) then return true

    var visibleFrom: Option[Direction] = None
    var result = false

    if isVisibleFrom(matrix, x, y, Direction.Left) then
        visibleFrom = Some(Direction.Left)
        result = true
    else if isVisibleFrom(matrix, x, y, Direction.Right) then
        visibleFrom = Some(Direction.Right)
        result = true
    else if isVisibleFrom(matrix, x, y, Direction.Top) then
        visibleFrom = Some(Direction.Top)
        result = true
    else if isVisibleFrom(matrix, x, y, Direction.Bottom) then
        visibleFrom = Some(Direction.Bottom)
        result = true
    result

def isVisibleFrom(matrix: Array[Array[Char]], x: Int, y: Int, dir: Direction): Boolean =
    (dir match
        case Direction.Left   => (0               until x      ).find(i => matrix(y)(i).toInt >= matrix(y)(x).toInt)
        case Direction.Right  => (matrix.size - 1 until x by -1).find(i => matrix(y)(i).toInt >= matrix(y)(x).toInt)
        case Direction.Top    => (0               until y      ).find(i => matrix(i)(x).toInt >= matrix(y)(x).toInt)
        case Direction.Bottom => (matrix.size - 1 until y by -1).find(i => matrix(i)(x).toInt >= matrix(y)(x).toInt)
    ).map(_ => false).getOrElse(true)
    
def getScenicScore(matrix: Array[Array[Char]], x: Int, y: Int): Int =
    val treeHeight = matrix(y)(x)
    val countLeft   = if x == 0 then 0 else               countVisibleInDirection(matrix, x - 1, y,     Direction.Left,   treeHeight)
    val countRight  = if x == matrix.size - 1 then 0 else countVisibleInDirection(matrix, x + 1, y,     Direction.Right,  treeHeight)
    val countTop    = if y == 0 then 0 else               countVisibleInDirection(matrix, x,     y - 1, Direction.Top,    treeHeight)
    val countBottom = if y == matrix.size - 1 then 0 else countVisibleInDirection(matrix, x,     y + 1, Direction.Bottom, treeHeight)
    countLeft * countRight * countTop * countBottom

def countVisibleInDirection(matrix: Array[Array[Char]], x: Int, y: Int, dir: Direction, maxHeight: Int): Int = 
    val treeHeight = matrix(y)(x)
    if treeHeight >= maxHeight then return 1
    dir match
        case Direction.Left   => if x == 0               then 1 else 1 + countVisibleInDirection(matrix, x - 1, y, dir, maxHeight)
        case Direction.Right  => if x == matrix.size - 1 then 1 else 1 + countVisibleInDirection(matrix, x + 1, y, dir, maxHeight)
        case Direction.Top    => if y == 0               then 1 else 1 + countVisibleInDirection(matrix, x, y - 1, dir, maxHeight)
        case Direction.Bottom => if y == matrix.size - 1 then 1 else 1 + countVisibleInDirection(matrix, x, y + 1, dir, maxHeight)
