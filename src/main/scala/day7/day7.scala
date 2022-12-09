package com.day7

import scala.io.Source

trait InputLine
case class Command(name: String, arg: String) extends InputLine
case class FileListing(name: String, size: Int) extends InputLine
case class DirectoryListing(name: String) extends InputLine

class FileSystemTreeNode(val name: String, var parent: Option[FileSystemTreeNode] = None, var children: List[FileSystemTreeNode] = List(), var size: Int = 0) {
    def root = getRoot(this)
    def cd(dir: String) = dir match
        case "/" => getRoot(this)
        case ".." => parent.get
        case _ => findNodeInList(name + "/" + dir, children)

    private def getRoot(node: FileSystemTreeNode): FileSystemTreeNode = node.parent match
        case None => node
        case Some(parentNode) => getRoot(parentNode)

    private def findNodeInList(dir: String, list: List[FileSystemTreeNode]): FileSystemTreeNode = list match
        case head :: tail => if head.name == dir then list.head else findNodeInList(dir, tail)
        case _ => throw new Exception("Node not found: " + dir + "; List: " + children.toString())
}

val inputFile = "src/main/resources/day7/input.txt"

def solution(): Unit =
    val fsTree = Source.fromFile(inputFile).getLines()
        .map(line => line.toInputLine)
        .foldLeft(FileSystemTreeNode("/"))(buildFsTree)
        .root
    val (totalSize, nodeSizeMap) = calculateNodeSizes(fsTree)
    println("part1: " + nodeSizeMap.values.filter(n => n <= 100000).sum)
    println("part2: " + nodeSizeMap.values.filter(n => n >= 8381165).toList.sorted.take(1))
    // RIGHT answers:
    // part1: 1428881
    // part2: List(10475598)

def buildFsTree(node: FileSystemTreeNode, inputLine: InputLine): FileSystemTreeNode = inputLine match
    case Command(name, _)   if name == "ls" => node
    case Command(name, arg) if name == "cd" => node.cd(arg)
    case FileListing(name, size) => { node.children = node.children :+ FileSystemTreeNode(node.name + "/" + name, Some(node), List(), size); node }
    case DirectoryListing(name)  => { node.children = node.children :+ FileSystemTreeNode(node.name + "/" + name, Some(node), List(), 0); node }

def calculateNodeSizes(node: FileSystemTreeNode, dirSizeMap: Map[String, Int] = Map()): (Int, Map[String, Int]) = 
    if node.children.isEmpty then
        (node.size, dirSizeMap)
    else
        val (childSize, childMap) = node.children.map(child => calculateNodeSizes(child)).reduce((a, b) => (a(0) + b(0), a(1) ++ b(1)))
        node.size = childSize
        val newMap = childMap + (node.name -> node.size)
        (node.size, newMap)

extension (s: String)
    def toInputLine: InputLine = 
        val tokens = s.split(" ")
        tokens(0) match
        case "$"   => new Command(tokens(1), if tokens.length == 3 then tokens(2) else "")
        case "dir" => new DirectoryListing(tokens(1))
        case _     => new FileListing(tokens(1), tokens(0).toInt)