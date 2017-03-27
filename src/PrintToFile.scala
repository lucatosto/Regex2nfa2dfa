/**
  * Created by lucatosto on 27/03/17.
  */
import java.io.{File, FileWriter, PrintWriter}
case class PrintToFile(line: Any, data1: Any, data2:Any) {
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }


    val data ="Stringa: "+line+"\n inserimento valori:\n NFA:\n"+data1+"\n DFA: \n"+data2

    printToFile(new File("catalogo_conversioni.txt")) {
      p => data.foreach(p.print)
    }
}
