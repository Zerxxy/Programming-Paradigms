package dnd

class Character (val name: String){
  protected var health = 100
  var alive = true
  
  def loseHealth(amt: Int) =  {
      if (health < amt) {
        health = 0
        alive = false
      }
      else health = health - amt
    
  }
  
  def isAlive = alive
  def getName = name
  def getHealth = health
}





