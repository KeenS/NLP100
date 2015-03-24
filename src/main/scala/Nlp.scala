import java.io._
import scala.io.Source

object Chapter1 {

  def problem0(str: String): String =
    str.reverse

  def problem1(str: String): String =
    new String(Array(str(1), str(3), str(5), str(7)))

  def problem2(str1:String, str2: String): String = {
    val buf = new StringBuilder
    for(i <- 0 until Array(str1.length, str2.length).min){
      buf += str1(i)
      buf += str2(i)
    }
    buf.toString
  }

  def problem3(str: String): Array[Int] =
    str.split(" ").map(_.filterNot(",." contains _)).map(_.length)

  def problem4(str: String): Map[String, Int] =
    str.split(" ").map(_.filterNot("." contains _)).zipWithIndex.map((x:(String, Int)) => {
      val (word, index) = x
      (index + 1) match {
        case 1 | 5 | 6 | 7 | 8 | 9 | 15 | 16 | 19 => (word.slice(0, 1), index + 1)
        case _ => (word.slice(0, 2), index + 1)
      }}
    ).toMap

  def problem5[S](seq: Traversable[S], n: Int): Array[Traversable[S]] =
    (0 to (seq.size - n)).toArray.map((i:Int) => seq.slice(i, i+n))

  def problem6(str1: String, str2: String): (Set[String], Set[String], Set[String], Set[String], Boolean, Boolean) = {
    val bigram1 = problem5(str1, 2).map(_.toString).toSet
    val bigram2 = problem5(str2, 2).map(_.toString).toSet
    val u = bigram1 union bigram2
    val i = bigram1 intersect bigram2
    val d1 = bigram1 diff bigram2
    val d2 = bigram2 diff bigram1
    val se_p1 = bigram1.contains("se")
    val se_p2 = bigram2.contains("se")
    return (u, i, d1, d2, se_p1, se_p2)
  }

  def problem7(x: Any, y:Any, z: Any): String =
    String.format("%s時の%sは%s",x.toString(), y.toString(), z.toString())

  def problem8(str: String): String =
    str.map((x:Char) => if (('a' to 'z') contains x) (216 - x).toChar else x)

  def problem9(str: String): String =
    str.split(" ").map((s:String) =>
      if(s.length <= 4)
        s
      else
        (s(0) +: scala.util.Random.shuffle(s.slice(1, s.length - 1).toSeq) :+ s(s.length - 1)).mkString).mkString(" ")

}

object Chapter2 {
  def probelem10(text: String): Int =
    text.count( _ == '\n')

  def problem11(text: String): String =
    text.map((c:Char) => if ( c == '\t') ' ' else c)

  def problem12(text: String): Unit = {
    val col1txt = new PrintWriter(new File("col1.txt" ))
    val col2txt = new PrintWriter(new File("col2.txt" ))
    for(Array(col1, col2) <- text.split("\n").map(_.split('\t'))) {
      col1txt.write(col1)
      col2txt.write(col2)
    }
    col1txt.close
    col2txt.close
  }

  def problem13(file1: String, file2: String, out: String) = {
    val outWriter = new PrintWriter(new File(out))
    for((col1, col2) <- (Source.fromFile(file1).mkString.split("\n") zip
      Source.fromFile(file2).mkString.split("\n"))){
      outWriter.println(String.format("%s\t%s", col1, col2))
    }
    outWriter.flush
    outWriter.close
  }
}
