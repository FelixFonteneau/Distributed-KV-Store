package se.kth.id2203.simulation.linearizabletest

import org.scalatest._
import se.kth.id2203.kvstore.{Operation, OperationResponse}
import se.kth.id2203.simulation.linearizabletest.LinearizableUtil.isLinearizable
import se.sics.kompics.simulator.result.SimulationResultSingleton
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.sl.simulator._
import se.kth.id2203.simulation.linearizabletest.History
import scala.collection.mutable

class OpsTest extends FlatSpec with Matchers {

  private val nMessages = 20

  "Execution" should "be linearizable" in {
    val seed = 123l
    JSimulationScenario.setSeed(seed)
    val bootScenario = GlobalScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("nMessages" -> nMessages)

    bootScenario.simulate(classOf[LauncherComp])

    val historyOperation = res.get("historyOperation", classOf[List[(Operation, Long)]])
    val historyResponses = res.get("historyResponse", classOf[List[(OperationResponse, Long)]])
    // val historyOperation = SimulationResult.get[List[(Operation, Long)]]("historyOperation")
    // val historyResponses = SimulationResult.get[List[(OperationResponse, Long)]]("historyResponse")

    val history = new History(historyOperation, historyResponses)
    history.isEmpty should be (false)
    isLinearizable(history, mutable.Map.empty[String, String]) should be (true)

  }


}
