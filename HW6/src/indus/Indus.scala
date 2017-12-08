package indus
import collection.mutable.Map

object Indus {
  val inventory = Map[Description, Int]()
  def main(args: Array[String]): Unit = {
    val d1 = Description("The Matrix DVD", 15.50, "DVD World")
    val d2 = Description("The Terminator DVD", 13.25, "DVD World")
    val d3 = Description("Iron Man DVD", 18.00, "DVD Planet")
    inventory(d1) = 5
    inventory(d2) = 3
    inventory(d3) = 2
    println(inventory.toString)
    /* prints:
     * Map(The Terminator DVD price = $13.25 supplier = DVD World -> 3, 
     *     The Matrix DVD price = $15.5 supplier = DVD World -> 5, 
     *     Iron Man DVD price = $18.0 supplier = DVD Planet -> 2)
     */
  }
}