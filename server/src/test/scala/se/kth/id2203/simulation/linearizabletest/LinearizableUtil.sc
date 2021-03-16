import se.kth.id2203.kvstore.OpCode.{Created, NotFound, Ok, Updated, ValueDoesNotMatch}
import se.kth.id2203.kvstore.{CAS, Get, Operation, OperationResponse, Put}
import se.kth.id2203.simulation.linearizabletest.History

import scala.collection.mutable
import scala.sys.SystemProperties.preferIPv6Addresses.key


def applyOperation(op: Get, map: mutable.Map[String, String]): OperationResponse = {
  var response: OperationResponse = op.response(NotFound)
  if (map.contains(op.key)) {
    val value = map(op.key)
    response = op.response(Ok, value)
  }
  response
}

def applyOperation(op: Put, map: mutable.Map[String, String]): OperationResponse = {
  var response: OperationResponse = op.response(Created)
  if (map.contains(op.key)) {
    map(key) = op.value
    response = op.response(Updated)
  } else {
    map.addOne(op.key, op.value)
    response = op.response(Created)
  }
  response
}

def applyOperation(op: CAS, map: mutable.Map[String, String]): OperationResponse = {
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
      val oldHistory = h.export
      if (h.remove(op) && isLinearizable(h, s)) {
        return true
      } else {
        h.update(oldHistory)
      }
    } else {
      s = collection.mutable.Map(oldS.toSeq: _*)
    }
  }
  false
}
