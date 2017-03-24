/**
  * Created by lucatosto on 20/03/17.
  */
object index {
  def main(args: Array[String]): Unit = {
    println("inserisci espressioni regolari per il linguaggio composto dall'alfabeto (a, b): ")
    val line=Console.readLine()
    println("hai inserito " + line)
    def casistica(esp: Any): Any= esp match {
        //daniele : ho modificato nelle stringhe sostituendo concatenazione con unione.
        //se guardi il primo println ho inserito che deve essere tutte le regex dell'alfabeto, quindi dovremmo inserire i casi
        //a*b, (a)b*, (a)*|b, a|b*, b* e così facendo abbiamo fatto una grammatica completa. Non c'è più il discorso che non ci sono dei
        //casi. Per questo linguaggio abbiamo fatto tutte le regex possibili.
      case "a|b" => Unione(line)   //    modificare stringa, l'unione è a|b
      case "a*" => Stella(line)
      case "b*"=> StellaB(line)
      case "ab" => Concatenazione(line)    //modificare stringa : la concatenazione è ab
      case "(a|b)*" => Complesso1(line)   //
      case "(ab)*" => Complesso2(line)   //
      case "a*b"=> Complesso3(line)
      case "ab*"=> Complesso4(line)
      case "(a*)|b"=>Complesso5(line)
      case "a|(b)*"=> Complesso6(line)
      case _ => "errore, la frase non appartiene al linguaggio"
    }
    casistica(line)
  }
}
