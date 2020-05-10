import scala.math.BigDecimal.RoundingMode

class test

object test {

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
    println(testFor())
  }

  def drawPyramid(): Unit ={

  }

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

