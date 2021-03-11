package se.kth.id2203.networking

import se.kth.id2203.networking.NetAddress
import se.sics.kompics.KompicsEvent
import se.sics.kompics.sl._

case class FPL_Send(to: NetAddress, message: KompicsEvent) extends KompicsEvent

case class FPL_Deliver(src: NetAddress, message: KompicsEvent) extends KompicsEvent

case class UpdateTopology(topology: Set[NetAddress]) extends KompicsEvent

case class MessageDated(message: KompicsEvent, sequenceNumber: Long) extends KompicsEvent


object FifoPerfectP2PLink extends Port {
  request[FPL_Send]
  indication[FPL_Deliver]
  request[UpdateTopology]
}

