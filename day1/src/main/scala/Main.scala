import scala.io.Source

@main def main: Unit = 
  var list = List.empty[Int]
  var runningTotal = 0
  Source.fromFile("C:/Users/slink/projects/adventofcode22/day1/src/main/resources/input.txt")
    .getLines()
    .foreach { line => if "".equals(line) then {
      list = runningTotal :: list
      runningTotal = 0
    } else {
      runningTotal += Integer.parseInt(line)
    }}
  
    println("Max calories: " + getMax3(list))

def getMax3(list: List[Int]): Int = list.sorted.takeRight(3).sum
  