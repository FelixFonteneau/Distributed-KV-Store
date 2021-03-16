package se.kth.id2203.simulation.linearizabletest

import se.kth.id2203.kvstore.{Get, OpResponse, Put}
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

class ScenarioClient2 extends ComponentDefinition {

  //******* Ports ******
  val net = requires[Network]
  val timer = requires[Timer]
  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address")
  val server = cfg.getValue[NetAddress]("id2203.project.bootstrap-address")
  private val pending = mutable.Map.empty[UUID, String]
  implicit val random = JSimulationScenario.getRandom()

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start => {
      val testType = SimulationResult[String]("type")
      val messages = SimulationResult[Int]("messages")

    }
  }

  net uponEvent {
    case NetMessage(header, or @ OpResponse(id, status, value)) => {
      logger.debug(s"Got OpResponse: $or")
      pending.remove(id) match {
        case Some(key) => SimulationResult += ("client2.received." + key -> status.toString())
        case None      => logger.warn("ID $id was not pending! Ignoring response.")
      }
    }
  }
}
