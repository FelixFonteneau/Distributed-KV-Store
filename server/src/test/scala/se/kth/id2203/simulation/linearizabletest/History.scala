package se.kth.id2203.simulation.linearizabletest

import se.kth.id2203.kvstore.{Operation, OperationResponse}

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class History {
  var listOperation: mutable.ListBuffer[(Operation, Long)] = mutable.ListBuffer.empty[(Operation, Long)]
  var listResponse: mutable.ListBuffer[(OperationResponse, Long)] = mutable.ListBuffer.empty[(OperationResponse, Long)]

  private var timestamp = 0

  def addEvent(op: Operation): Unit = {
    listOperation.addOne(op, timestamp)
    timestamp += 1
  }

  def addEvent(res: OperationResponse): Unit = {
    listResponse.addOne(res, timestamp)
    timestamp += 1
  }

  def getMinimalOperations: List[Operation] = {
    val (_, maxTimestamp) = listResponse.head
    val minimalOps = listOperation.filter(tupple => tupple._2 < maxTimestamp)
    minimalOps.map(ele => ele._1).result()
  }

  def remove(op: Operation): Boolean = {
    val tupple = listOperation.find(tupple => tupple._1 == op)
    if (tupple.isDefined) {
      val tupple2 = listResponse.find(tupple => tupple._1.id == op.id)
      if (tupple2.isDefined) {
        val indexOperation = listOperation.indexOf(tupple.get)
        listOperation.remove(indexOperation)
        val indexResponse = listResponse.indexOf(tupple2.get)
        listResponse.remove(indexResponse)
        return true
      }
    }
    false
  }

  def isEmpty: Boolean = {
    listOperation.isEmpty && listResponse.isEmpty
  }

  def getResult(op: Operation): Option[OperationResponse] = {
    val tupple = listResponse.find(tupple => tupple._1.id == op.id)
    if (tupple.isDefined) {
      Some(tupple.get._1)
    } else {
      None
    }
  }

  def export: (List[(Operation, Long)], List[(OperationResponse, Long)]) = {
    (listOperation.result(), listResponse.result())
  }

  def update(lists: (List[(Operation, Long)], List[(OperationResponse, Long)])): Unit = {
    listOperation = ListBuffer.empty
    listOperation ++= lists._1

    listResponse = ListBuffer.empty
    listResponse ++= lists._2
  }
}

