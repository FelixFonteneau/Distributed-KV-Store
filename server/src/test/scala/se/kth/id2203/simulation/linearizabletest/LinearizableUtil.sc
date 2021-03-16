import se.kth.id2203.kvstore.{Operation, OperationResponse}

import scala.collection.mutable

def isLinearizable(h: History, s: mutable.Map[String, String]): Boolean {
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


}

class History {
  var listOperation: mutable.ListBuffer[(Operation, Long)] = mutable.ListBuffer.empty[(Operation, Long)]
  var listResponse: mutable.ListBuffer[(OperationResponse, Long)] = mutable.ListBuffer.empty[(Operation, Long)]

  def

}

abstract case class Event(var id: Long, var linkedId: Long)

case class EventOperation(event: Operation, var id: Long, var linkedId: Long) extends Event

case class EventResponse(event: Operation, id: Long, linkedId: Long) extends Event
