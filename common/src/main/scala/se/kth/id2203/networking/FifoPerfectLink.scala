package se.kth.id2203.networking

import se.sics.kompics.network.Network
import se.sics.kompics.sl.{ComponentDefinition, Init}
import se.sics.kompics.KompicsEvent

import scala.collection.mutable


class FifoPerfectLink (init: Init[FifoPerfectLink]) extends ComponentDefinition {
  val net = requires[Network]
  val fpl = provides(FifoPerfectP2PLink)

  val self = cfg.getValue[NetAddress]("id2203.project.address")
  var pi: Set[NetAddress] = Set(self)

  val listen: mutable.Map[NetAddress, Long] = mutable.Map(self -> 0)
  val next: mutable.Map[NetAddress, Long] = mutable.Map(self -> 1)
  var pending: mutable.Set[(NetAddress, KompicsEvent, Long)] = mutable.Set.empty

  fpl uponEvent {
    case FPL_Send(dest, message) => {
      if (!listen.contains(dest)) {
        listen.addOne(dest, 0)
        next.addOne(dest, 1)
      }
      listen(dest) = listen(dest) + 1
      trigger(NetMessage(self, dest, MessageDated(message, listen(dest))) -> net)
    }

    case UpdateTopology(topology) => {
      val newNodes = topology.filter(p => !pi.contains(p))
      pi = topology
      for (p <- newNodes) {
        next(p) = 1
        listen(p) = 0
      }
    }
  }

  net uponEvent {
    case NetMessage(header, MessageDated(message, sn)) => {
      if (!listen.contains(header.src)) {
        listen.addOne(header.src, 0)
        next.addOne(header.src, 1)
      }
      pending.add((header.src, message, sn))
      var update: Boolean = true
      while (update) {
        update = false
        for ((p, m, snPrime) <- pending) {
          if (snPrime == next(p)) {
            update = true
            next(p) = next(p) + 1
            pending.remove((p, m, snPrime))
            trigger(FPL_Deliver(p, m) -> fpl)
          }
        }
      }
    }
  }
}
