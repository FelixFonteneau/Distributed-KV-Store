package se.kth.id2203.engine

import se.sics.kompics.KompicsEvent
import se.sics.kompics.sl._
//Provided Primitives to use in your implementation

case class Prepare(nL: Long, ld: Int, na: Long) extends KompicsEvent

case class Promise(nL: Long, na: Long, suffix: List[RSM_Command], ld: Int) extends KompicsEvent

case class AcceptSync(nL: Long, suffix: List[RSM_Command], ld: Int) extends KompicsEvent

case class Accept(nL: Long, c: RSM_Command) extends KompicsEvent

case class Accepted(nL: Long, m: Int) extends KompicsEvent

case class Decide(ld: Int, nL: Long) extends KompicsEvent

object State extends Enumeration {
  type State = Value
  val PREPARE, ACCEPT, UNKOWN = Value
}

object Role extends Enumeration {
  type Role = Value
  val LEADER, FOLLOWER = Value
}

class SequenceConsensus extends Port {
  request[SC_Propose]
  indication[SC_Decide]
}

case class SC_Propose(value: RSM_Command) extends KompicsEvent

case class SC_Decide(value: RSM_Command) extends KompicsEvent

trait RSM_Command