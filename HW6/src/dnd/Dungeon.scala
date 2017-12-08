package dnd

object Dungeon extends App {
    val r = new scala.util.Random()
    val puff = new Dragon("Puff")
    val thor = new Knight("Thor")
    
    while(puff.isAlive && thor.isAlive) {
      thor.attack(puff, r.nextInt(thor.getHealth))
      if(puff.isAlive) puff.attack(thor, r.nextInt(puff.getHealth))
    }
}