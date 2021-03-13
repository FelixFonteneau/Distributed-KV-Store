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
import se.sics.kompics.network.Network
import se.sics.kompics.sl._

import scala.collection.mutable;

class KVService extends ComponentDefinition {

  //******* Ports ******
  val fpl = requires(FifoPerfectP2PLink)
  val net = requires[Network]
  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address")
  val consensus = requires(SequenceConsensus)

  val store: mutable.HashMap[String, String] = new mutable.HashMap[String, String]

  //******* Handlers ******
  fpl uponEvent {
    case FPL_Deliver(src, command: Command) => {
      log.info("Got command {} {} {}.", command.operation, command.src, command.responsibleNode)
      trigger(SC_Propose(command) -> consensus)
    }
  }


  /**
   * After consensus, we register the operations
   */
  consensus uponEvent {
    case SC_Decide(Command(op @ Get(key, _), address, responsibleNode)) => {
      log.info("Paxos decided: {} {}", op, address)
      var response: OperationResponse = op.response(NotFound)
      if (store.contains(key)) {
        val value = store(key)
        response = op.response(Ok, value)
      }
      if (responsibleNode == self) {
        trigger(NetMessage(self, address, response) -> net)
      }
    }

    case SC_Decide(Command(op @ Put(key, value, _), address, responsibleNode)) => {
      log.info("Paxos decided: {} {}", op, address)
      var response: OperationResponse = op.response(Created)
      if (store.contains(key)) {
        store(key) = value
        response = op.response(Updated)
      } else {
        store.addOne(key, value)
        response = op.response(Created)
      }
      if (responsibleNode == self) {
        trigger(NetMessage(self, address, response) -> net)
      }
    }
  }
}
