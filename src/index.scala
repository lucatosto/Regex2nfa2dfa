/**
  * Created by lucatosto on 20/03/17.
  */
object index {
  def main(args: Array[String]): Unit = {
    println("inserisci espressioni: ")
    val line=Console.readLine()
    println("hai inserito " + line)
    def casistica(esp: Any): Any= esp match {
      case "ab" => Unione(line)
      case "a*" => Stella(line)
      case "a|b" => Concatenazione(line)
      case "ab*" => Complesso(line)
      case _ => "errore"
    }
    casistica(line)
  }
}
