package se.kth.id2203.kvstore

import se.kth.id2203.networking.NetAddress

case class Command(operation: Operation, src: NetAddress)
