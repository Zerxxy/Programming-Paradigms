package indus

class Item(val description: Description) {
  val id = {Item.nextID += 1; Item.nextID}
  override def toString = "ID = " + id + ": " + description
}

object Item {
  def apply(description: Description) = new Item(description)
  var nextID = 500
}