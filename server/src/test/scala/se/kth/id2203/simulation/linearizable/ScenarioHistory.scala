package se.kth.id2203.simulation.linearizable

import se.kth.id2203.kvstore._
import se.kth.id2203.networking._
import se.sics.kompics.Start
import se.sics.kompics.network.Network
import se.sics.kompics.sl._
import se.sics.kompics.sl.simulator.SimulationResult
import se.sics.kompics.timer.Timer

class ScenarioHistory extends ComponentDefinition {

  //******* Ports ******
  val net = requires[Network]
  val timer = requires[Timer]

  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address")
  val history: History = new History()


  //******* Handlers ******
  ctrl uponEvent {
    case _: Start => {
      logger.info("history: Startup addr: {}", self)
    }
  }

  net uponEvent {
    case NetMessage(header, op: Operation) => {
      logger.debug(s"history: Got Op: $op")
      history.addEvent(op)
      SimulationResult += ("history" ->  history.serialise)
    }

    case NetMessage(header, or: OpResponse) => {
      logger.debug(s"history: Got OpResponse: $or")
      history.addEvent(or)
      SimulationResult += ("history" -> history.serialise)
    }
  }
}

