/**
  * Created by lucatosto on 20/03/17.
  */
import java.io.{File, FileWriter, PrintWriter}

import Array._
case class StellaB (line: Any) {
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

  val data1 = Tabulator.format(List(List("Stati", " b ", "ε", "flag"), List("0  " , "/ ", " 1,3 ", "false"), List("1  " , "2 ", " / ", "false"), List("2  " , "/ ", " 1,3 ", "false"), List("3  " , "/ ", " / ", "true") ))
  println("NFA\n"+data1)

  val data2 = Tabulator.format(List(List("Stati", "b", "flag"), List("S0(0,1,3)", "S1(1,2,3)", "true"), List("S1(1,2,3)", "S1(1,2,3)", "true")))
  println("DFA\n"+data2)
  
  PrintToFile(line, data1, data2)

 // println("ho scritto il file 'example.txt'")
}
