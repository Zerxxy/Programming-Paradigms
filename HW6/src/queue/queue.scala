package queue

import scala.collection.mutable.ArrayBuffer

class Queue[T] {
  val elements = new ArrayBuffer[T]()
  def enqueue(e: T) { elements += e }
  def dequeue() { elements.remove(0) }
  def front = elements(0)
  def isEmpty = elements.isEmpty
  override def toString = {
    var result = "["
    for(e <- elements) result += (e.toString + " ")
    result += "]"
    result
  }
}

object Queue {
  def apply[T]() = new Queue[T]()
  def test {
    val waitingList = Queue[String]
    waitingList.enqueue("Bart")
    waitingList.enqueue("Lisa")
    waitingList.enqueue("Maggie")
    waitingList.enqueue("Marge")
    waitingList.enqueue("Homer")
    println(waitingList)
    while(!waitingList.isEmpty) {
      waitingList.dequeue()
      println(waitingList)
    }
  }
}