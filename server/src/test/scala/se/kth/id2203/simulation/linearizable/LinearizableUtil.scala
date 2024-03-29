package se.kth.id2203.simulation.linearizable

import se.kth.id2203.kvstore.OpCode._
import se.kth.id2203.kvstore._

import java.io.{ByteArrayInputStream, ObjectInputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import scala.collection.mutable

object LinearizableUtil {
  // def applyOperation(op: Get, map: mutable.Map[String, String]): OperationResponse = {
  //   var response: OperationResponse = op.response(NotFound)
  //   if (map.contains(op.key)) {
  //     val value = map(op.key)
  //     response = op.response(Ok, value)
  //   }
  //   response
  // }

  // def applyOperation(op: Put, map: mutable.Map[String, String]): OperationResponse = {
  //   var response: OperationResponse = op.response(Created)
  //   if (map.contains(op.key)) {
  //     map(key) = op.value
  //     response = op.response(Updated)
  //   } else {
  //     map.addOne(op.key, op.value)
  //     response = op.response(Created)
  //   }
  //   response
  // }

  // def applyOperation(op: CAS, map: mutable.Map[String, String]): OperationResponse = {
  //   var response: OperationResponse = op.response(NotFound)
  //   if (map.contains(op.key)) {
  //     if (map(op.key) == op.oldValue) {
  //       map(op.key) = op.newValue
  //       response = op.response(Updated)
  //     } else {
  //       response = op.response(ValueDoesNotMatch)
  //     }
  //   }
  //   response
  // }

  def applyOperation(operation: Operation, map: mutable.Map[String, String]): OperationResponse = operation match {
    case op: Get => {
      var response: OperationResponse = op.response(NotFound)
      if (map.contains(op.key)) {
        val value = map(op.key)
        response = op.response(Ok, value)
      }
      response
    }
    case op: Put => {
      var response: OperationResponse = op.response(Created)
      if (map.contains(op.key)) {
        map(op.key) = op.value
        response = op.response(Updated)
      } else {
        map.addOne(op.key, op.value)
        response = op.response(Created)
      }
      response
    }
    case op: CAS => {
      var response: OperationResponse = op.response(NotFound)
      if (map.contains(op.key)) {
        if (map(op.key) == op.oldValue) {
          map(op.key) = op.newValue
          response = op.response(Updated)
        } else {
          response = op.response(ValueDoesNotMatch)
        }
      }
      response
    }
  }

  def isLinearizable(history: History, seq: mutable.Map[String, String]): Boolean = {
    /* if(h is empty) return true
        else{
          for each minimal operation op {
            // try linearizing op first
            let res be the result of op in h
            run op on S
            if(S gives result res && isLinearizable(h − op, S)) return true
            else undo op on S
          }
          return false
        }
     */
    var s = seq
    var h = history
    if (h.isEmpty) {
      return true
    }
    for (op <- h.getMinimalOperations) {
      val result = h.getResult(op)
      val oldS: Map[String, String] = s.toMap
      if (result.isDefined && applyOperation(op, s) == result.get) {
        val oldHistory = h.serialise
        if (h.remove(op) && isLinearizable(h, s)) {
          return true
        } else {
          h = deserialise(oldHistory) // undo the operation and get the old state
        }
      } else {
        s = collection.mutable.Map(oldS.toSeq: _*) // undo the operation and get the old state
      }
    }
    false
  }

  def deserialise(encoded: String): History = {
    val bytes = Base64.getDecoder().decode(encoded.getBytes(UTF_8))
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close
    value.asInstanceOf[History]
  }
}
