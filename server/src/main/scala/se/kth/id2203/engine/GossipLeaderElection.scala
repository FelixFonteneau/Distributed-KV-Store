package se.kth.id2203.engine

import se.kth.id2203.bootstrapping.{Booted, Bootstrapping}
import se.kth.id2203.networking.{FPL_Deliver, FPL_Send, FifoPerfectP2PLink, NetAddress}
import se.kth.id2203.overlay.LookupTable
import se.sics.kompics.sl._
import se.sics.kompics.timer._

import scala.collection.mutable

class GossipLeaderElection extends ComponentDefinition {

  private val ballotOne = 0x0100000000L

  val ble = provides(BallotLeaderElection)
  val timer = requires[Timer]
  val boot = requires(Bootstrapping)
  val fpl = requires(FifoPerfectP2PLink)


  val self = cfg.getValue[NetAddress]("id2203.project.address")

  var topology: Set[NetAddress] = Set(self)
  val delta = 100
  var majority = (topology.size / 2) + 1

  private var period = 250
  private val ballots = mutable.Map.empty[NetAddress, Long]

  private var round = 0L
  private var ballot = ballotFromNAddress(0, self)

  private var leader: Option[(Long, NetAddress)] = None
  private var highestBallot: Long = ballot

  def ballotFromNAddress(n: Int, adr: NetAddress): Long = {
    val nBytes = com.google.common.primitives.Ints.toByteArray(n)
    val addrBytes = com.google.common.primitives.Ints.toByteArray(adr.hashCode())
    val bytes = nBytes ++ addrBytes
    val r = com.google.common.primitives.Longs.fromByteArray(bytes)
    assert(r > 0) // should not produce negative numbers!
    r
  }

  // *** functions *** //

  def incrementBallotBy(ballot: Long, inc: Int): Long = {
    ballot + inc.toLong * ballotOne
  }

  def incrementBallot(ballot: Long): Long = {
    ballot + ballotOne
  }

  def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period)
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout))
    trigger(scheduledTimeout -> timer)
  }

  def checkLeader(): Unit = {
    val (topProcess, topBallot) = (ballots + (self -> ballot)).maxBy{b => b._2}

    val top = (topBallot, topProcess)

    if (topBallot < highestBallot) {
      while (ballot <= highestBallot) {
        ballot = incrementBallotBy(ballot, 1)
      }
      leader = None
    } else {
      if (!leader.contains(top)) {
        highestBallot = topBallot
        leader = Some(top)
        trigger(BLE_Leader(topProcess, topBallot) -> ble)
      }
    }
  }

  // *** Handlers *** //
  // ctrl uponEvent {
  //   case _: Start =>  {
  //     startTimer(period)
  //   }
  // }

  boot uponEvent {
    case Booted(assignment: LookupTable) => {
      log.info("Got NodeAssignment, ble ready.")
      topology = assignment.getNodes()
      majority = (topology.size / 2) + 1
      log.info("Start ble timer with period {}", period)
      startTimer(period)
    }
  }

  timer uponEvent {
    case CheckTimeout(_) => {
      // log.debug("CheckTimeout()")
      if ((ballots.size + 1) >= (topology.size / 2)) {
        checkLeader()
      }
      ballots.clear();
      round = round + 1
      for (p <- topology) {
        if (p != self) {
          // trigger(PL_Send(p, HeartbeatReq(round, highestBallot)) -> pl);
          trigger(FPL_Send(p, HeartbeatReq(round, highestBallot)) -> fpl)
        }
      }
      startTimer(period)
    }
  }

  fpl uponEvent {
    case FPL_Deliver(src, HeartbeatReq(r, hb)) => {
      if (hb > highestBallot) {
        highestBallot = hb
      }
      trigger(FPL_Send(src, HeartbeatResp(r, ballot)) -> fpl)
    }
    case FPL_Deliver(src, HeartbeatResp(r, b)) => {
      if (r == round) {
        ballots += (src -> b)
      } else {
        period = period + delta
      }
    }
  }
}
