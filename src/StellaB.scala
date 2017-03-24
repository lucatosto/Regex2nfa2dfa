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



  //TODO: a dinamica, intestastione flag (true o false variabili) //Luca:secondo me non bisogna farlo, possiamo usare un pattern matching
  println("NFA")
  println(Tabulator.format(List(List("Stati", " b ", "ε", "flag"), List("0  " , "/ ", " 1,3 ", "false"), List("1  " , "2 ", " / ", "false"), List("2  " , "/ ", " 1,3 ", "false"), List("3  " , "/ ", " / ", "true") )))
  println("DFA")
  println(Tabulator.format(List(List("Stati", "b", "flag"), List("S0(0,1,3)", "S1(1,2,3)", "true"), List("S1(1,2,3)", "S1(1,2,3)", "true"))));


  /* val f= new File("esempio.txt")
   val fw=new FileWriter(f)
   val op=new PrintWriter(fw)
   val data = Tabulator.format(List(List("Stati", " a ", "ε", "flag"), List("0" , "/", "1,3", "false"), List("1" , "2", "/", "false"), List("2" , "/", "1,3", "false"), List("3" , "/", "/", "true") ))
   op(fw(op=>data.fo)) */




  //Questo è un esempio di scrittura su file. Dimmi che ne pensi. Non sono riuscito a tabularle bene, ma nemmeno ci ho provato tanto.
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }


  val data = Tabulator.format(List(List("Stati", " b ", "ε", "flag"), List("0" , "/", "1,3", "false"), List("1" , "2", "/", "false"), List("2" , "/", "1,3", "false"), List("3" , "/", "/", "true") ))
  printToFile(new File("example.txt")) {
    p => data.foreach(p.print)
  }

  println("ho scritto il file 'example.txt'")
}
