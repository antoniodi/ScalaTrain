class test

object test {

  def main(args: Array[String]): Unit = {
    println("hi" ne "null")
    val a = Array(1, 2, 3)
    val b = Array(3, 2, 1)

    val tuple = a.zip(b)
    Array(tuple.count{ case (x, y) => x > y}, tuple.count{ case (x, y) => x > y})
    print(compareTriplets(a, b))
  }

  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    def suma = (v1: Int, v2: Int) => v1 + v2
    //def function = (v1: (Int, Int), a1: Int = 0, a2: Int = 0) => 10

    val tuple = a.zip(b)

    Array(tuple.count{ case (x, y) => x > y}, tuple.count{ case (x, y) => x < y})

  }

  // 3x3 (1,1) (2,2) (3,3) -> (1,3) (2,2) (3,1)
//  def getSumDiagonal(ar: Array[Array[Int]]): Int = {
//
//  }

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