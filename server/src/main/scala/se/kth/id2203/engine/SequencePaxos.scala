package se.kth.id2203.engine

import se.kth.id2203.networking.{NetHeader, NetMessage}
import se.sics.kompics.network._
import se.sics.kompics.sl._

import scala.collection.mutable

class SequencePaxos(init: Init[SequencePaxos]) extends ComponentDefinition {
  import Role._
  import State._

  val sc = provides[SequenceConsensus]
  val ble = requires[BallotLeaderElection]
  // val pl = requires[FIFOPerfectLink]
  val net = requires[Network]

  val (self, pi, others) = init match {
    case Init(addr: NetHeader, pi: Set[NetHeader] @unchecked) => (addr, pi, pi - addr)
  }
  val majority = (pi.size / 2) + 1

  var state = (FOLLOWER, UNKOWN)
  var nL = 0l
  var nProm = 0l
  var leader: Option[NetHeader] = None
  var na = 0l
  var va = List.empty[RSM_Command]
  var ld = 0
  // leader state
  var propCmds = List.empty[RSM_Command]
  val las = mutable.Map.empty[NetHeader, Int]
  val lds = mutable.Map.empty[NetHeader, Int]
  var lc = 0
  val acks = mutable.Map.empty[NetHeader, (Long, List[RSM_Command])]

  def suffix(s: List[RSM_Command], l: Int): List[RSM_Command] = {
    s.drop(l)
  }

  def prefix(s: List[RSM_Command], l: Int): List[RSM_Command] = {
    s.take(l)
  }

  ble uponEvent {
    case BLE_Leader(l, n) => {
      if (n > nL) {
        leader = Option(l)
        nL = n
        if (self == l && nL > nProm) {
          state = (LEADER, PREPARE)
          propCmds = List.empty[RSM_Command]

          for (p <- pi) {
            las(p) = 0
          }

          lds.clear()
          acks.clear()
          lc = 0
          for (p <- pi) {
            if (p != self) {
              trigger(NetMessage(p, Prepare(nL, ld, na)) -> net)
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
        var sfx: List[RSM_Command] = List.empty[RSM_Command]
        if (na >= n) {
          sfx = suffix(va, ld) // todo change to ldp si jamais
        }
        trigger(PL_Send(p, Promise(np, na, sfx, ld)) -> pl) // todo change to ldp si jamais
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
          propCmds = List.empty[RSM_Command]
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
        va = prefix(va, ld) ++ sfx // change to ldp si jamais
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

  net uponEvent {
    case NetMessage(p, Prepare(np, ldp, n)) => {
      if (nProm < np) {
        nProm = np
        state = (FOLLOWER, PREPARE)
        var sfx: List[RSM_Command] = List.empty[RSM_Command]
        if (na >= n) {
          sfx = suffix(va, ld) // todo change to ldp si jamais
        }
        trigger(NetMessage(p, Promise(np, na, sfx, ld)) -> net) // todo change to ldp si jamais
      }
    }

    case NetMessage(a, Promise(n, na, sfxa, lda)) => {
      if ((n == nL) && (state == (LEADER, PREPARE))) {
        acks(a) = (na, sfxa)
        lds(a) = lda
        if (acks.size >= majority) { // change to ==
          var (k, sfx) = acks.values.maxBy(_._1)

          va = prefix(va, ld) ++ sfx ++ propCmds
          las(self) = va.size
          propCmds = List.empty[RSM_Command]
          state = (LEADER, ACCEPT)
          for (p <- pi) {
            if (p != self && lds.contains(p)) {
              val sfxp = suffix(va, lds(p)) // CHANGE TO LAS
              trigger(NetMessage(p, AcceptSync(nL, sfxp, lds(p))) -> net)
            }
          }
        }
      } else if ((n == nL) && (state == (LEADER, ACCEPT))) {
        lds(a) = lda
        val sfx = suffix(va, lds(a))
        trigger(NetMessage(a, AcceptSync(nL, sfx, lds(a))) -> net)
        if (lc != 0) {
          trigger(NetMessage(a, Decide(ld, nL)) -> net)
        }
      }
    }
    case NetMessage(p, AcceptSync(nL, sfx, ldp)) => {
      if ((nProm == nL) && (state == (FOLLOWER, PREPARE))) {
        na = nL; // change nL si jamais
        va = prefix(va, ld) ++ sfx // change to ldp si jamais
        trigger(NetMessage(p, Accepted(nL, va.size)) -> net) // change nL si jamais
        state = (FOLLOWER, ACCEPT)
      }
    }

    case NetMessage(p, Accept(nL, c)) => {
      if ((nProm == nL) && (state == (FOLLOWER, ACCEPT))) {
        va = va ++ List(c)
        trigger(NetMessage(p, Accepted(nL, va.size)) -> net)
      }
    }

    case NetMessage(_, Decide(l, nL)) => {
      if (nProm == nL) { // check the nL
        while (ld < l) {
          trigger(SC_Decide(va(ld)) -> sc);
          ld = ld + 1
        }
      }
    }

    case NetMessage(a, Accepted(n, m)) => {
      if ((n == nL) && (state == (LEADER, ACCEPT))) {
        las(a) = m
        val tmp = pi.filter(p => las(p) >= m)
        if (lc < m && tmp.size >= majority) {
          lc = m
          for (p <- pi.filter(p => lds.contains(p))) {
            trigger(NetMessage(p, Decide(lc, nL)) -> net)
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
          trigger(NetMessage(p, Accept(nL, c)) -> net)
        }
      }
    }
  }
}