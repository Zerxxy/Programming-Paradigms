package indus

class Description(val description: String, val price: Double, val supplier: String)  extends Equals {
  override def canEqual(other: Any) =  other.isInstanceOf[Description]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Description => 
         this.canEqual(other) && 
         this.## == other.## && 
         this.description == other.description &&
         this.price == other.price &&
         this.supplier == other.supplier
       case _ => false
    }
  override def hashCode = this.toString.##
	override def toString: String = {
    description + " price = $" + price + " supplier = " + supplier
  }
}

object Description {
  def apply(description: String, price: Double, supplier: String) =
     new Description(description, price, supplier)
}