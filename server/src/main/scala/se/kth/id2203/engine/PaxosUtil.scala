package se.kth.id2203.engine

import se.kth.id2203.kvstore.{Command}
import se.sics.kompics.KompicsEvent
import se.sics.kompics.sl._




//Provided Primitives to use in your implementation

case class Prepare(nL: Long, ld: Int, na: Long) extends KompicsEvent

case class Promise(nL: Long, na: Long, suffix: List[Command], ld: Int) extends KompicsEvent

case class AcceptSync(nL: Long, suffix: List[Command], ld: Int) extends KompicsEvent

case class Accept(nL: Long, c: Command) extends KompicsEvent

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

object SequenceConsensus extends Port {
  request[SC_Propose]
  indication[SC_Decide]
}

case class SC_Propose(value: Command) extends KompicsEvent

case class SC_Decide(value: Command) extends KompicsEvent
