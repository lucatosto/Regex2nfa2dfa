/**
  * Created by lucatosto on 20/03/17.
  */
import Array._
case class Unione (line: Any) {
  //println("----------------------"+line)
  object Tabulator {
    def format(table: Seq[Seq[Any]]) = table match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- table) yield formatRow(row, colSizes)
        formatRows(rowSeparator(colSizes), rows)
    }

    def formatRows(rowSeparator: String, rows: Seq[String]): String = (
      rowSeparator ::
        rows.head ::
        rowSeparator ::
        rows.tail.toList :::
        rowSeparator ::
        List()).mkString("\n")

    def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
      val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
      cells.mkString("|", "|", "|")
    }

    def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString("+", "+", "+")
  }

  println("NFA")
  val data1 = Tabulator.format(List(List("Stati", "a", "b", "ε", "flag"), List("0" , "/", "/", "1,3", "false"), List("1" , "2", "/", "/", "false"), List("2" , "/", "/", "5", "false"), List("3", "/", "4", "/", "false"), List("4", "/", "/","5", "false"), List("5", "/", "/", "/", "true") ))
  println(data1)
  println("DFA")
  val data2 = Tabulator.format(List(List("Stati", "a", "b", "flag"), List("S0(0,1,3)" , "S1(2,5)", "S2(4,5)", "false"), List("S1(2,5)" , "/", "/", "true"), List("S2(4,5)" , "/", "/", "true") ))
  println(data2)

  PrintToFile(line, data1, data2)

}
