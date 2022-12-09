import com.day1.{solution as day1Solution}
import com.day2.{solution as day2Solution}
import com.day3.{solution as day3Solution}
import com.day4.{solution as day4Solution}
import com.day5.{solution as day5Solution}
import com.day6.{solution as day6Solution}
import com.day7.{solution as day7Solution}
import com.day8.{solution as day8Solution}
import com.day9.{solution as day9Solution}

@main def main(solution: String): Unit = solution match {
  case "day1" => day1Solution()
  case "day2" => day2Solution()
  case "day3" => day3Solution()
  case "day4" => day4Solution()
  case "day5" => day5Solution()
  case "day6" => day6Solution()
  case "day7" => day7Solution()
  case "day8" => day8Solution()
  case "day9" => day9Solution()
  case _ => println("Cannot find solution " + solution)
}
