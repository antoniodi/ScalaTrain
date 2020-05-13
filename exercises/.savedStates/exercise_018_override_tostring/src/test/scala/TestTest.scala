import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestTest extends AnyWordSpec with Matchers {

  "Must find palindrome" should {
    "return -1 if differences are upper than max differences" in {
      Test.highestValuePalindromeV3("1100", 4, 1) shouldEqual "-1"
      Test.highestValuePalindromeV3("111000", 6, 2) shouldEqual "-1"
      Test.highestValuePalindromeV3("123", 3, 0) shouldEqual "-1"
    }

//    TC-01 8 4 11111111 => 99111199
//    TC-02 8 5 11111111 => 99111199
//    TC-03 7 5 1111111 => 9919199
//    TC-04 8 1 11111111 => 11111111
//    TC-05 7 1 1111111 => 1119111

//    TC-01: Given 8 2 11119111
//    => Expected result: 11199111 with one replacement (same result if give k = 1)
//    TC-02: Given 7 1 1111111 => Expected result: 1119111
//    TC-03: Given 7 4 1111111 => Expected result: 9911199 with 4 replacements (one replacement operation left which cannot be used)
//    TC-04: Given 7 4 1191111 => Expected result: 9199919 with 4 replacements (one replacement operation left which cannot be used)
//    TC-05: Given 3 1 118 => Expected result: 818

//    From:
//
//      1 2 839 2 7594 3 0 12 4
//    9 2 939 4 9594 9 3 92 9
//    That's cost of 9 changes
//
//    128392759430124     +0
//    92...........29     +2
//    929.........929     +2
//    9293.......3929     +2
//    92939.....93929     +2
//    929394...493929     +1
//    929394959493929

    "return highest value palindrome" in {
      Test.highestValuePalindromeV3("3943", 4, 1) shouldEqual "3993"
      Test.highestValuePalindromeV3("1100", 4, 2) shouldEqual "9999"
      Test.highestValuePalindromeV3("123456", 6, 3) shouldEqual "999999"
      Test.highestValuePalindromeV3("092282", 6, 2) shouldEqual "992299"
      Test.highestValuePalindromeV3("0925282", 7, 2) shouldEqual "9929299"
      Test.highestValuePalindromeV3("5", 1, 1) shouldEqual "9"
//      Test.highestValuePalindromeV3("0925282", 99111, 50790) shouldEqual "9925299"
    }
  }

  "Extra long factorials" should {
    "return the factorials" in {
      Test.extraLongFactorials(1) shouldEqual 1
      Test.extraLongFactorials(2) shouldEqual 2
      Test.extraLongFactorials(4) shouldEqual 24
    }
  }

//  8 4 1111911 => 91199119
//
//  And a case of my own for the other part: 3 2 329 => 999

}
