package se.kth.id2203.engine

import se.kth.id2203.bootstrapping.{Booted, Bootstrapping}
import se.kth.id2203.kvstore.Command
import se.kth.id2203.networking.{NetAddress, NetMessage}
import se.kth.id2203.overlay.LookupTable
import se.sics.kompics.network._
import se.sics.kompics.sl._

import scala.collection.mutable

class SequencePaxos extends ComponentDefinition {
  import Role._
  import State._

  val sc = provides(SequenceConsensus)
  val ble = requires(BallotLeaderElection)
  // val pl = requires[FIFOPerfectLink]
  val net = requires[Network]
  val boot = requires(Bootstrapping)

  val self = cfg.getValue[NetAddress]("id2203.project.address")
  var pi: Set[NetAddress] = Set(self)
  // = init match {
  //   case Init(addr: NetAddress, pi: Set[NetAddress] @unchecked) => (addr, pi, pi - addr)
  // }

  var majority = (pi.size / 2) + 1

  var state = (FOLLOWER, UNKOWN)
  var nL = 0L
  var nProm = 0L
  var leader: Option[NetAddress] = None
  var na = 0L
  var va = List.empty[Command]
  var ld = 0
  // leader state
  var propCmds = List.empty[Command]
  val las = mutable.Map.empty[NetAddress, Int]
  val lds = mutable.Map.empty[NetAddress, Int]
  var lc = 0
  val acks = mutable.Map.empty[NetAddress, (Long, List[Command])]

  def suffix(s: List[Command], l: Int): List[Command] = {
    s.drop(l)
  }

  def prefix(s: List[Command], l: Int): List[Command] = {
    s.take(l)
  }

  // *** Handlers *** //

  boot uponEvent {
    case Booted(assignment: LookupTable) => {
      log.info("Got NodeAssignment, paxos ready.")
      pi = assignment.getNodes()
    }
  }

  ble uponEvent {
    case BLE_Leader(l, n) => {
      if (n > nL) {
        leader = Option(l)
        nL = n
        if (self == l && nL > nProm) {
          state = (LEADER, PREPARE)
          propCmds = List.empty[Command]

          for (p <- pi) {
            las(p) = 0
          }

          lds.clear()
          acks.clear()
          lc = 0
          for (p <- pi) {
            if (p != self) {
              trigger(NetMessage(self, p, Prepare(nL, ld, na)) -> net)
              // trigger(PL_Send(p, Prepare(nL, ld, na)) -> pl)
            }
          }
          acks(l) = (na, suffix(va, ld))
          lds(self) = ld
          nProm = nL
        } else {
          state = (FOLLOWER, state._2)
        }
      }
    }
  }

  /* pl uponEvent {
    case PL_Deliver(p, Prepare(np, ldp, n)) => {
      if (nProm < np) {
        nProm = np
        state = (FOLLOWER, PREPARE)
        var sfx: List[Command] = List.empty[Command]
        if (na >= n) {
          sfx = suffix(va, ldp)
        }
        trigger(PL_Send(p, Promise(np, na, sfx, ld)) -> pl)
      }
    }

    case PL_Deliver(a, Promise(n, na, sfxa, lda)) => {
      if ((n == nL) && (state == (LEADER, PREPARE))) {
        acks(a) = (na, sfxa)
        lds(a) = lda
        if (acks.size >= majority) { // change to ==
          var (k, sfx) = acks.values.maxBy(_._1)

          va = prefix(va, ld) ++ sfx ++ propCmds
          las(self) = va.size
          propCmds = List.empty[Command]
          state = (LEADER, ACCEPT)
          for (p <- pi) {
            if (p != self && lds.contains(p)) {
              val sfxp = suffix(va, lds(p)) // CHANGE TO LAS
              trigger(PL_Send(p, AcceptSync(nL, sfxp, lds(p))) -> pl)
            }
          }
        }
      } else if ((n == nL) && (state == (LEADER, ACCEPT))) {
        lds(a) = lda
        val sfx = suffix(va, lds(a))
        trigger(PL_Send(a, AcceptSync(nL, sfx, lds(a))) -> pl)
        if (lc != 0) {
          trigger(PL_Send(a, Decide(ld, nL)) -> pl)
        }
      }
    }
    case PL_Deliver(p, AcceptSync(nL, sfx, ldp)) => {
      if ((nProm == nL) && (state == (FOLLOWER, PREPARE))) {
        na = nL; // change nL si jamais
        va = prefix(va, ldp) ++ sfx
        trigger(PL_Send(p, Accepted(nL, va.size)) -> pl) // change nL si jamais
        state = (FOLLOWER, ACCEPT)
      }
    }

    case PL_Deliver(p, Accept(nL, c)) => {
      if ((nProm == nL) && (state == (FOLLOWER, ACCEPT))) {
        va = va ++ List(c)
        trigger(PL_Send(p, Accepted(nL, va.size)) -> pl)
      }
    }

    case PL_Deliver(_, Decide(l, nL)) => {
      if (nProm == nL) { // check the nL
        while (ld < l) {
          trigger(SC_Decide(va(ld)) -> sc);
          ld = ld + 1
        }
      }
    }

    case PL_Deliver(a, Accepted(n, m)) => {
      if ((n == nL) && (state == (LEADER, ACCEPT))) {
        las(a) = m
        val tmp = pi.filter(p => las(p) >= m)
        if (lc < m && tmp.size >= majority) {
          lc = m
          for (p <- pi.filter(p => lds.contains(p))) {
            trigger(PL_Send(p, Decide(lc, nL)) -> pl)
          }
        }

      }
    }
  }
  */

  // todo add a propose to all acceptors

  net uponEvent {
    case NetMessage(header, Prepare(np, ldp, n)) => { // todo find the ldp to put in the event function
      val p = header.src
      if (nProm < np) {
        nProm = np
        state = (FOLLOWER, PREPARE)
        var sfx: List[Command] = List.empty[Command]
        if (na >= n) {
          sfx = suffix(va, ldp)
        }
        trigger(NetMessage(self, p, Promise(np, na, sfx, ld)) -> net)
      }
    }

    case NetMessage(header, Promise(n, na, sfxa, lda)) => {
      val a = header.src
      if ((n == nL) && (state == (LEADER, PREPARE))) {
        acks(a) = (na, sfxa)
        lds(a) = lda
        if (acks.size >= majority) { // change to ==
          var (k, sfx) = acks.values.maxBy(_._1)

          va = prefix(va, ld) ++ sfx ++ propCmds
          las(self) = va.size
          propCmds = List.empty[Command]
          state = (LEADER, ACCEPT)
          for (p <- pi) {
            if (p != self && lds.contains(p)) {
              val sfxp = suffix(va, lds(p))
              trigger(NetMessage(self, p, AcceptSync(nL, sfxp, lds(p))) -> net)
            }
          }
        }
      } else if ((n == nL) && (state == (LEADER, ACCEPT))) {
        lds(a) = lda
        val sfx = suffix(va, lds(a))
        trigger(NetMessage(self, a, AcceptSync(nL, sfx, lds(a))) -> net)
        if (lc != 0) {
          trigger(NetMessage(self, a, Decide(ld, nL)) -> net)
        }
      }
    }
    case NetMessage(header, AcceptSync(nL, sfx, ldp)) => {
      val p = header.src
      if ((nProm == nL) && (state == (FOLLOWER, PREPARE))) {
        na = nL;
        va = prefix(va, ldp) ++ sfx // change to ldp si jamais
        trigger(NetMessage(self, p, Accepted(nL, va.size)) -> net)
        state = (FOLLOWER, ACCEPT)
      }
    }

    case NetMessage(header, Accept(nL, c)) => {
      val p = header.src
      if ((nProm == nL) && (state == (FOLLOWER, ACCEPT))) {
        va = va ++ List(c)
        trigger(NetMessage(self, p, Accepted(nL, va.size)) -> net)
      }
    }

    case NetMessage(_, Decide(l, nL)) => {
      if (nProm == nL) { // check the nL
        while (ld < l) {
          trigger(SC_Decide(va(ld)) -> sc)
          ld = ld + 1
        }
      }
    }

    case NetMessage(header, Accepted(n, m)) => {
      val a = header.src

      if ((n == nL) && (state == (LEADER, ACCEPT))) {
        las(a) = m
        val tmp = pi.filter(p => las(p) >= m)
        if (lc < m && tmp.size >= majority) {
          lc = m
          for (p <- pi.filter(p => lds.contains(p))) {
            trigger(NetMessage(self, p, Decide(lc, nL)) -> net)
          }
        }

      }
    }
  }

  sc uponEvent {
    case SC_Propose(c) => {
      if (state == (LEADER, PREPARE)) {
        propCmds = propCmds ++ List(c)
      }
      else if (state == (LEADER, ACCEPT)) {
        va = va ++ List(c)
        las(self) = las(self) + 1
        for (p <- pi.filter(p => lds.contains(p) && p != self)) {
          trigger(NetMessage(self, p, Accept(nL, c)) -> net)
        }
      }
    }
  }
}
