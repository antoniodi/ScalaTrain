import scala.math.BigDecimal.RoundingMode

class Animal(val name: String)

class Bird(name: String) extends Animal(name)

class Test

object Test {

  def main(args: Array[String]): Unit = {
//    println("hi" ne "null")
//    val a = Array(1, 2, 3)
//    val b = Array(3, 2, 1)
//
//    val tuple = a.zip(b)
//    Array(tuple.count{ case (x, y) => x > y}, tuple.count{ case (x, y) => x > y})
//    print(compareTriplets(a, b))
//
//    def f(n: Int) = (0 until n).foreach(x => println("Hello World"))
//    def g(n: Int) = for (x <- 0 until n + 1) {println("Hello World")}
    //def f(n : Int) : Unit = { println("Hello World"); if (n > 1) f(n-1) }
    //for (i <- 1 to n) { print("Hello World\n") }
//    filterOddValues(List(1,2,3,4,5,6))
//    plusMinus(Array(-4,3,-9,0,4,1))
//    println(testFor())
//    println(drawPyramid(6))
//    (9972 to 10100).foreach(num => println(s"${num} - ${highestValuePalindrome(num.toString, num.toString.length, 10)} ${}"))
    println(highestValuePalindromeV2("1234", 4, 100))
  }

  def extraLongFactorialsV2(n: Int): BigInt = {
    if (n == 0) 1 else n * extraLongFactorialsV2(n - 1)
  }

  def extraLongFactorials(n: Int): BigInt = {
    def factorial(factorialAcum: BigInt = 1, i: Int = 1): BigInt = if (i <= n) factorial(factorialAcum * i, i + 1) else factorialAcum
    factorial()
  }

  /**
   * @param s: integer
   * @param n: long of integer
   * @param k: maximun changes allowed
   */
  def highestValuePalindromeV2(s: String, n: Int, k: Int): String = {
    val haftWord = n / 2
    def findPalindromeApply(world: String, i: Int = 0, differentValue: Int = 0, maxValue: Int = haftWord): Boolean = if (differentValue > k) false else {
      if (i < maxValue) { if (s.charAt(i) == s.charAt(n - i)) findPalindromeApply(world, i + 1, differentValue) else findPalindromeApply(world, i + 1, differentValue + 1) } else true
    }
    if (findPalindromeApply(s)) {
      def buildHighestValuePalindrome(world: String, i: Int = 0, leftWord: String = "", rightWord: String = "", maxValue: Int = haftWord): String = {
        if (i < maxValue) {
          val leftChar = s.charAt(i)
          val rightChar = s.charAt(n - i)
          if (leftChar == rightChar) buildHighestValuePalindrome(world, i + 1, s"${leftWord}${leftChar}", s"${rightChar}${rightWord}")
          else buildHighestValuePalindrome(world, i + 1, s"${leftWord}${9}", s"${rightChar}${9}")
        } else s"${leftWord}${rightWord}"
      }
      buildHighestValuePalindrome(s)
    } else "-1"
  }

  def highestValuePalindromeV3(s: String, n: Int, k: Int): String = {
    val haftWordLength = n / 2
    def mustFindPalindrome(i: Int = 0, differences: Int = 0): Boolean = if (differences > k) false
      else if (i < haftWordLength ) {
        if (s.charAt(i) == s.charAt(n - i - 1)) mustFindPalindrome(i + 1, differences) else {
          mustFindPalindrome(i + 1, differences + 1)
        }
      } else true

    if (mustFindPalindrome()) {
      val centerWord = if (n % 2 == 0) "" else "9"
      def buildHighestValuePalindrome(i: Int = 0, leftWord: String = "", rightWord: String = ""): (String, String) = {
        if (i < haftWordLength) {
          val leftChar = s.charAt(i)
          val rightChar = s.charAt(n - i - 1)
          if (leftChar == rightChar) buildHighestValuePalindrome(i + 1, s"${leftWord}${leftChar}", s"${rightWord}${rightChar}")
          else buildHighestValuePalindrome(i + 1, s"${leftWord}${"9"}", s"${rightWord}${"9"}")
        } else (leftWord, rightWord)
      }
      val leftAndRightWord = buildHighestValuePalindrome()
      val firstValue = leftAndRightWord._1
      val secondValue = leftAndRightWord._2
      if (firstValue >= secondValue) s"${firstValue}${centerWord}${firstValue.reverse}"
      else s"${secondValue}${centerWord}${secondValue.reverse}"
    } else "-1"
  }

  def highestValuePalindromeV4(s: String, n: Int, k: Int): String = {
    val haftWordLength = n / 2
    def mustFindPalindrome(i: Int = 0, differences: Int = 0): Boolean = if (differences > k) false
    else if (i < haftWordLength ) {
      if (s.charAt(i) == s.charAt(n - i - 1)) mustFindPalindrome(i + 1, differences) else {
        mustFindPalindrome(i + 1, differences + 1)
      }
    } else true

    if (mustFindPalindrome()) {
      if (n == 1) {
        "9"
      } else {
        val centerWord = if (n % 2 == 0) "" else s"${s.charAt(haftWordLength)}"

        def buildHighestValuePalindrome(i: Int = 0, leftWord: String = "", rightWord: String = ""): (String, String) = {
          if (i < haftWordLength) {
            val leftChar = s.charAt(i)
            val rightChar = s.charAt(n - i - 1)
            if (leftChar == rightChar) buildHighestValuePalindrome(i + 1, s"${leftWord}${leftChar}", s"${rightWord}${rightChar}")
            else buildHighestValuePalindrome(i + 1, s"${leftWord}${"9"}", s"${rightWord}${"9"}")
          } else (leftWord, rightWord)
        }

        val leftAndRightWord = buildHighestValuePalindrome()
        val firstValue = leftAndRightWord._1
        val secondValue = leftAndRightWord._2
        if (firstValue >= secondValue) s"${firstValue}${centerWord}${firstValue.reverse}"
        else s"${secondValue}${centerWord}${secondValue.reverse}"
      }
    } else "-1"
  }

  /**
   * @param s: integer
   * @param n: long of integer
   * @param k: maximun changes allowed
   */
  def highestValuePalindromeV1(s: String, n: Int, k: Int): String = {
    val halfLength = n / 2
    var changes = 0
    var world = "|"
    for (i <- 0 until halfLength) {
      val leftWord = s.substring(i, i + 1)
      val rightWord = s.substring(n - i - 1, n - i)
      if(leftWord != rightWord) {
        changes += 1
        world = world.replace("|", "9|9")
      } else {
        world = world.replace("|", s"${leftWord}|${rightWord}")
      }
    }
    if (changes > k) "-1"
    else {
      if (n % 2 == 0) world.replace("|", "") else world.replace("|", s.substring(halfLength, halfLength + 1))
    }
  }

  def timeConversion(s: String): String = {
    val time = s.substring(0, 8)
    val baseFormat = s.substring(8, 10)
    val hour = s.substring(0, 2).toInt
    if (baseFormat == "AM") {
      if (hour < 12 ) time else s"${0}${time.substring(2)}"
    } else {
      if (hour < 12 ) s"${hour.toInt + 12}${time.substring(2)}" else s"${hour}${time.substring(2)}"
    }
  }

  def birthdayCakeCandles(ar: Array[Int]): Int = {
    var maxValue = 0
    var count = 0
    for (a <- ar) {
      if (a > maxValue) {
        maxValue = a
        count = 1
      } else if( a == maxValue) { count += 1 }
    }
    count
  }

  def minAndMaxSum(arr: Array[Int]): Unit = {
    val longValues = arr.map(_.toLong)
    val sortedValues = longValues.sorted
    println(s"${sortedValues.init.sum} ${sortedValues.tail.sum}")

//    println(longArray.filterNot(_ == arr.max).sum + " " + longArray.filterNot(_ == arr.min).sum)
  }



  def drawPyramid(n: Int): Unit = for(num <- 1 to n) println(" " * (n - num) + "#" * num)

  def testFor(): Seq[Int] = {
    for {
      n <- 1 to 3 if n % 2 == 1
      m <- 1 to n
    } yield n * m
  }

  def plusMinus(arr: Array[Int]): Unit = {
    def divideWithScale(numberOne: Int, divideBy: Int): Unit = println((numberOne / BigDecimal(divideBy)).setScale(6, RoundingMode.CEILING))

    val arrSize = arr.length
    divideWithScale(arr.count(_ > 0), arrSize)
    divideWithScale(arr.count(_ < 0), arrSize)
    divideWithScale(arr.count(_ == 0), arrSize)
  }

  def diagonalDifference(arr: Array[Array[Int]]): Unit = {
    var firstDiagonal = 0
    var secondDiagonal = 0
    val dimension = arr.length
    for ((internalArray, i) <- arr.zipWithIndex) {
      firstDiagonal = firstDiagonal + internalArray(i)
      secondDiagonal += internalArray((dimension - 1) - i)
    }
    Math.abs(firstDiagonal - secondDiagonal)

    val arrSize = arr.length
    val arrZip = arr.zipWithIndex
    //Math.abs(arrZip.reduce((a, b) => a._1(a._2) + b._1(b._2)) - arrZip.reduce((a, b) => a._1(arrSize - 1 + a._2) + b._1(arrSize - 1 + b._2)))
  }

  def sumOddValues(): Unit = {
    def f(arr:List[Int]):Int = arr.filter(_ % 2 != 0).sum
  }

  def reverseList(): Unit = {
    val a = List(1,2,3).reverse
  }

  def filterOddValues(arr:List[Int]): Unit = {
    // for ((a,b) <- arr.zipWithIndex if b % 2 ==1) yield a
    // arr.zipWithIndex.filter(_._2 %2 == 1).map(_._1)
    // .collect{case (a,b) if b % 2 == 1 => a}
    //val a = (1 to 10).toList
      var a = List[Int]()
      for (i <- 1 to arr.size) { if (i % 2 == 0) {a = a :+ arr(i-1)}}
  }

  def filterValues(): Unit = {
    def f(delim:Int,arr:List[Int]):List[Int] = arr.filter(_ < delim)
  }

  def repeatNumbers(): Int = {
    //arr.flatMap(List.fill(num)(_))
    def f(num:Int, arr:List[Int]):List[Int] = arr.flatMap(n => (1 to num).map(x => n))
    1
  }

  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    def suma = (v1: Int, v2: Int) => v1 + v2
    //def function = (v1: (Int, Int), a1: Int = 0, a2: Int = 0) => 10

    val tuple = a.zip(b)

    Array(tuple.count{ case (x, y) => x > y}, tuple.count{ case (x, y) => x < y})

  }

  def sumTupleValues(tupleI: Array[(Int, Int)], value1: Int = 0, value2: Int = 0): (Int, Int) = {
    tupleI.headOption match {
      case Some(tuple) => if (tuple._1 > tuple._2) (value1 + 1, value2) else if (tuple._1 < tuple._2) (value1, value2 + 1) else (value1, value2)
      case _ => (value1, value2)
    }
  }

  def reverse(number: Int, reverseNumber: Int = 0): Int = if (number == 0) reverseNumber else reverse(number / 10, reverseNumber * 10 + number % 10 )
}

object Customer {
  private var defaultFName = "Lupita"
  private var defaultLName = "Perez"
}

class Customer(firstName: String = Customer.defaultFName, lastName: String = Customer.defaultLName) {
}

