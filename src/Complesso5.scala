//TODO (a*)|b
case class Complesso5 (line: Any) {
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

  //TODO: a dinamica, intestastione flag (true o false variabili)  //(a/b)*
  println("NFA")
  val data1 = Tabulator.format(List(List("Stati", "a", "b", "ε", "flag"), List("0" , "/", "/", "1,5", "false"), List("1" , "/", "/", "2,4", "false"), List("2" , "3", "/","/", "false"), List("3" , "/", "/","4,2", "false"),List("4" , "/", "/","7", "false"),List("5" , "/", "6","/", "false"),List("6" , "/", "/","7", "false"),List("7" , "/", "/","/", "true") ))
  println(data1)
  println("DFA")
  val data2 = Tabulator.format(List(List("Stati", "a", "b", "flag"), List("S0(0,1,2,4,5,7)","S1(2,3,4,7)", "S2(6,7)","true"), List("S1(2,3,4,7)", "S1(2,3,4,7)", "/", "true"), List("S2(6,7)", "/", "/", "true")))
  println(data2)
  PrintTofile(line, data1, data2)
}
