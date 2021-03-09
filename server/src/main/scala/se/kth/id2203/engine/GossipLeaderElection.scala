package se.kth.id2203.engine

import se.kth.id2203.networking.{NetHeader, NetMessage}
import se.sics.kompics.network._
import se.sics.kompics.sl._
import se.sics.kompics.timer.{ScheduleTimeout, Timer}

import scala.collection.mutable

class GossipLeaderElection(init: Init[GossipLeaderElection]) extends ComponentDefinition {

  private val ballotOne = 0x0100000000l

  val ble = provides[BallotLeaderElection]
  // val pl = requires[PerfectLink]
  val net = requires[Network]
  val timer = requires[Timer]

  val self = init match {
    case Init(s: NetHeader) => s
  }

  val topology = List.empty[NetHeader]// todo
  // to determine cfg.getValue[List[Address]]("ble.simulation.topology");
  val delta = 3 // todo cfg.getValue[Long]("ble.simulation.delay")
  val majority = (topology.size / 2) + 1

  private var period = 3 //todo // cfg.getValue[Long]("ble.simulation.delay")
  private val ballots = mutable.Map.empty[NetHeader, Long]

  private var round = 0l
  private var ballot = ballotFromNAddress(0, self)

  private var leader: Option[(Long, NetHeader)] = None
  private var highestBallot: Long = ballot

  def ballotFromNAddress(n: Int, adr: NetHeader): Long = {
    val nBytes = com.google.common.primitives.Ints.toByteArray(n)
    val addrBytes = com.google.common.primitives.Ints.toByteArray(adr.hashCode())
    val bytes = nBytes ++ addrBytes
    val r = com.google.common.primitives.Longs.fromByteArray(bytes)
    assert(r > 0) // should not produce negative numbers!
    r
  }

  def incrementBallotBy(ballot: Long, inc: Int): Long = {
    ballot + inc.toLong * ballotOne
  }

  private def incrementBallot(ballot: Long): Long = {
    ballot + ballotOne
  }

  private def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period)
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout))
    trigger(scheduledTimeout -> timer)
  }

  private def checkLeader() {
    val (topProcess, topBallot) = (ballots + (self -> ballot)).maxBy{b => b._2}

    val top = (topBallot, topProcess)

    if (topBallot < highestBallot) {
      while (ballot <= highestBallot) {
        ballot = incrementBallotBy(ballot, 1)
      }
      leader = None
    } else {
      if (Some(top) != leader) {
        highestBallot = topBallot
        leader = Some(top)
        trigger(BLE_Leader(topProcess, topBallot) -> ble)
      }
    }
  }
  // todo !!!
  // ctrl uponEvent {
  //   case _: Start =>  {
  //     startTimer(period)
  //   }
  // }


  timer uponEvent {
    case CheckTimeout(_) => {
      if ((ballots.size + 1) >= (topology.size / 2)) {
        checkLeader()
      }
      ballots.clear();
      round = round + 1
      for (p <- topology) {
        if (p != self) {
          // trigger(PL_Send(p, HeartbeatReq(round, highestBallot)) -> pl);
          trigger(NetMessage(p, HeartbeatReq(round, highestBallot)) -> net)
        }
      }
      startTimer(period)
    }
  }

  net uponEvent {
    case NetMessage(src, HeartbeatReq(r, hb)) => {
      if (hb > highestBallot) {
        highestBallot = hb
      }
      trigger(NetMessage(src, HeartbeatResp(r, ballot)) -> net)
    }
    case NetMessage(src, HeartbeatResp(r, b)) => {
      if (r == round) {
        ballots += (src -> b)
      } else {
        period = period + delta
      }
    }
  }

  // pl uponEvent {
  //   case PL_Deliver(src, HeartbeatReq(r, hb)) => {
  //     /* INSERT YOUR CODE HERE */
  //     if (hb > highestBallot) {
  //       highestBallot = hb
  //     }
  //     trigger(PL_Send(src, HeartbeatResp(r, ballot)) -> pl)
  //   }
  //   case PL_Deliver(src, HeartbeatResp(r, b)) => {
  //     /* INSERT YOUR CODE HERE */
  //     if (r == round) {
  //       ballots += (src -> b)
  //     } else {
  //       period = period + delta
  //     }
  //   }
  // }
}