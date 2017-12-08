package dnd

class Knight(name: String) extends Character(name) {
  def attack(enemy: Dragon, amt: Int) = {
    enemy.loseHealth(amt)
    println(getName + " is stabbing " + enemy.getName + " for " + amt + " damage")
  }
}