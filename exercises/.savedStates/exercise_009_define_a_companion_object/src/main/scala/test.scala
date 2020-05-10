class test

object test {

  def main(args: Array[String]): Unit = {
    println("hi" ne "null")
    //println(reverse(123456789))
  }

  def reverse(number: Int, reverseNumber: Int = 0): Int = if (number == 0) reverseNumber else reverse(number / 10, reverseNumber * 10 + number % 10 )
}
