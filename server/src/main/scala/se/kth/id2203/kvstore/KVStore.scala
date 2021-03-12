/*
 * The MIT License
 *
 * Copyright 2017 Lars Kroll <lkroll@kth.se>.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package se.kth.id2203.kvstore

import se.kth.id2203.engine.{SC_Decide, SC_Propose, SequenceConsensus}
import se.kth.id2203.kvstore.OpCode.{Created, NotFound, Ok, Updated}
import se.kth.id2203.networking._
import se.kth.id2203.overlay.Routing
import se.sics.kompics.sl._
import se.sics.kompics.network.Network

import scala.collection.mutable;

class KVService extends ComponentDefinition {

  //******* Ports ******
  val net = requires[Network]
  val route = requires(Routing)

  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address")
  val consensus = requires(SequenceConsensus)

  val store: mutable.HashMap[String, String] = new mutable.HashMap[String, String]

  //******* Handlers ******
  net uponEvent {
    case NetMessage(header, op @ Get(key, _)) => {
      log.info("Got operation {}!", op)
      trigger(SC_Propose(Command(op, header.src)) -> consensus)
      // trigger(NetMessage(self, header.src, op.response(OpCode.NotImplemented)) -> net)
    }
    case NetMessage(header, op @ Put(key, value, _)) => {
      log.info("Got operation {}!", op)
      trigger(SC_Propose(Command(op, header.src)) -> consensus)
      // trigger(NetMessage(self, header.src, op.response(OpCode.NotImplemented)) -> net)
    }
  }

  consensus uponEvent {
    case SC_Decide(Command(op @ Get(key, _), address)) => {
      log.info("Paxos decided: {} {}", op, address)
      if (store.contains(key)) {
        val value = store(key)
        trigger(NetMessage(self, address, op.response(Ok, value)) -> net)
      } else {
        trigger(NetMessage(self, address, op.response(NotFound)) -> net)
      }
    }

    case SC_Decide(Command(op @ Put(key, value, _), address)) => {
      log.info("Paxos decided: {} {}", op, address)
      if (store.contains(key)) {
        store(key) = value
        trigger(NetMessage(self, address, op.response(Updated)) -> net)
      } else {
        store.addOne(key, value)
        trigger(NetMessage(self, address, op.response(Created)) -> net)
      }
    }
  }
}
