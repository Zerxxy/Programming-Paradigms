package dnd

class Dragon(name: String) extends Character(name) {
  def attack(enemy: Knight, amt: Int) = {
    enemy.loseHealth(amt)
    println(getName + " is flaming " + enemy.getName + " for " + amt + " damage")
  }
}