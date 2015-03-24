import org.scalatest.FunSpec
import org.scalatest.Matchers._

class Chapter1Spec extends FunSpec {
  describe("0th") {
    it("test 1") {
      val str = "stresse"
      Chapter1.problem0(str) should be ("esserts")
    }
  }

  describe("1st") {
    it("test 1") {
      Chapter1.problem1("パタトクカシーー") should be ("タクシー")
    }
  }

  describe("2nd") {
    it("test 1") {
      Chapter1.problem2("パトカー", "タクシー") should be ("パタトクカシーー")
    }
  }

  describe("3rd") {
    it("test 1") {
    Chapter1.problem3("Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.") should be (Array(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9))
    }
  }

  describe("4th") {
    it("test 1") {
      Chapter1.problem4("Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can.") should be
      Map("H" -> 1, "He" -> 2,
        "Li" ->3, "Be" -> 4,
        "B" -> 5, "C" -> 6, "N" -> 7, "O" -> 8, "F" -> 9, "Ne" -> 10,
        "Na" -> 11, "Mi" -> 12, "Al" -> 13,
        "Si" -> 14, "P" -> 15, "S" -> 16, "Cl" -> 17, "Ar" -> 18,
        "K" -> 19, "Ca" -> 20
      )
    }
  }

  describe("5th") {
    val str = "I am an PROBLEMer"
    it("test 1") {
      Chapter1.problem5(str.split(" "), 2) should be
      (Array(
        Array("I", "am"),
        Array("am", "an"),
        Array("an", "PROBLEMer")
      ))
    }
    it("test 2") {
      Chapter1.problem5(str, 2) should be
      (Array(
        Array('I', ' '),
        Array(' ', 'a'),
        Array('a', 'm'),
        Array('m', ' '),
        Array(' ', 'a'),
        Array('a', 'n'),
        Array('n', ' '),
        Array(' ', 'N'),
        Array('N', 'L'),
        Array('L', 'P'),
        Array('P', 'e'),
        Array('e', 'r')
      ))
    }
  }

  describe("6th") {
    val (u, i, d1, d2, se_p1, se_p2) = Chapter1.problem6("paraparaparadise", "paragraph")
    it("test 1") {
      u should be (Set("pa", "ar", "ra", "ap", "ad", "di", "is", "se", "ag", "gr", "ph"))
    }
    it("test 2") {
      i should be (Set("pa", "ar", "ra", "ap"))
    }
    it("test 3") {
      d1 should be (Set("ad", "di", "is", "se"))
    }
    it("test 4") {
      d2 should be (Set("ag", "gr", "ph"))
    }
    it("test 5") {
      se_p1 should be (true)
    }
    it("test 6"){
      se_p2 should be (false)
    }

  }

  describe("7th") {
    it("test 1") {
      Chapter1.problem7(1, 2, 3) should be ("1時の2は3")
    }
    it("test 2") {
      Chapter1.problem7(12, "気温", 22.4) should be ("12時の気温は22.4")
    }
  }
}
