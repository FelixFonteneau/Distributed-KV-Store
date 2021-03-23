package se.kth.id2203.simulation.linearizable

import se.kth.id2203.kvstore.{Operation, OperationResponse}

import java.io._
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import scala.collection.mutable

@SerialVersionUID(100L)
class History(_listOperation: List[(Operation, Long)] = List.empty[(Operation, Long)],
              _listResponse: List[(OperationResponse, Long)] = List.empty[(OperationResponse, Long)]) extends Serializable {
  val listOperation: mutable.ListBuffer[(Operation, Long)] = mutable.ListBuffer.empty[(Operation, Long)] ++ _listOperation

  val listResponse: mutable.ListBuffer[(OperationResponse, Long)] = mutable.ListBuffer.empty[(OperationResponse, Long)] ++ _listResponse

  private var timestamp = 0
  override def toString : String = {
    var res = "operations:"
    for ((op, time) <- listOperation) {
      res += op + ":" + time +","
    }
    res += "responses:"
    for ((response, time) <- listResponse) {
      res += response + ":" + time +","
    }
    res
  }

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

  def serialise: String = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(this)
    oos.close
    new String(
      Base64.getEncoder().encode(stream.toByteArray),
      UTF_8
    )
  }

  def deserialise(encoded: String): History = {
    val bytes = Base64.getDecoder().decode(encoded.getBytes(UTF_8))
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close
    value.asInstanceOf[History]
  }
}

