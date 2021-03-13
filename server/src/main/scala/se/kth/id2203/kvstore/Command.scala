package se.kth.id2203.kvstore

import se.kth.id2203.networking.NetAddress
import se.sics.kompics.sl.KompicsEvent

case class Command(operation: Operation, src: NetAddress, responsibleNode: NetAddress) extends KompicsEvent
