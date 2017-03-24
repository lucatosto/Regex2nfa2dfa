/**
  * Created by lucatosto on 20/03/17.
  */
object index {
  def main(args: Array[String]): Unit = {
    println("inserisci espressioni: ")
    val line=Console.readLine()
    println("hai inserito " + line)
    def casistica(esp: Any): Any= esp match {
        //daniele : ho modificato nelle stringhe sostituendo concatenazione con unione.
      case "a|b" => Unione(line)   //    modificare stringa, l'unione è a|b
      case "a*" => Stella(line)
      case "ab" => Concatenazione(line)    //modificare stringa : la concatenazione è ab
      case "(a|b)*" => Complesso1(line)   //
      case "(ab)*" => Complesso2(line)   //
      case _ => "errore"
    }
    casistica(line)
  }
}
