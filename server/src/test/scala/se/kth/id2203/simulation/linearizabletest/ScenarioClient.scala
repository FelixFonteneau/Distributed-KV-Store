package se.kth.id2203.simulation.linearizabletest

import se.kth.id2203.kvstore.{CAS, Get, OpResponse, Operation, Put}
import se.kth.id2203.networking.{NetAddress, NetMessage}
import se.kth.id2203.overlay.RouteMsg
import se.sics.kompics.Start
import se.sics.kompics.network.Network
import se.sics.kompics.sl.ComponentDefinition
import se.sics.kompics.sl.simulator.SimulationResult
import se.sics.kompics.timer.Timer
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}

import java.util.UUID
import scala.collection.mutable

class ScenarioClient extends ComponentDefinition {

  //******* Ports ******
  val net = requires[Network]
  val timer = requires[Timer]
  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address")
  val server = cfg.getValue[NetAddress]("id2203.project.bootstrap-address")
  val history = cfg.getValue[NetAddress]("id2203.project.history-address")

  private val pending = mutable.Map.empty[UUID, String]
  implicit val random = JSimulationScenario.getRandom

  def generateOperations(n: Int): List[Operation] = {
    var operationList = List.empty[Operation]
    for (i <- 0 to n) {
      val randomInt = random.nextInt(3)
      if (randomInt == 0) {
        val operation = Get(random.nextInt(10).toString)
        operationList = operationList ++ List(operation)
      } else if (randomInt == 1){
        val operation = Put(random.nextInt(10).toString, random.nextInt(10).toString)
        operationList = operationList ++ List(operation)
      } else {
        val operation = CAS(random.nextInt(10).toString, random.nextInt(10).toString, random.nextInt(10).toString)
        operationList = operationList ++ List(operation)
      }
    }
    operationList
  }

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start => {
      logger.info("Client: Startup addr: {}, history addr: {}", self, history)
      val messagesNumber = SimulationResult[Int]("nMessages")
      val operations = generateOperations(messagesNumber)
      for (op <- operations) {
        // Thread.sleep(random.nextInt(50))
        val routeMsg = RouteMsg(op.key, op) // don't know which partition is responsible, so ask the bootstrap server to forward it
        trigger(NetMessage(self, server, routeMsg) -> net)
        trigger(NetMessage(self, history, op) -> net) // send to the history

        pending += (op.id -> op.key)
        logger.info("Sending {}", op)
      }
    }
  }

  net uponEvent {
    case NetMessage(header, or @ OpResponse(id, status, value)) => {
      logger.debug(s"Got OpResponse: $or")
      pending.remove(id) match {
        case Some(key) => {
          trigger(NetMessage(self, history, or) -> net) // send to the history
        }
        case None      => logger.warn("ID $id was not pending! Ignoring response.")
      }
    }
  }
}
