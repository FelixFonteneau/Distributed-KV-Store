package se.kth.id2203.simulation.linearizabletest

import org.scalatest._
import se.kth.id2203.simulation.linearizabletest.LinearizableUtil.isLinearizable
import se.sics.kompics.simulator.result.SimulationResultSingleton
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.sl.simulator._
import scala.collection.mutable
import se.kth.id2203.simulation.linearizabletest.History

class OpsTest extends FlatSpec with Matchers {

  private val nMessages = 20

  "Execution" should "be linearizable" in {
    val seed = 123l
    JSimulationScenario.setSeed(seed)
    val bootScenario = GlobalScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("nMessages" -> nMessages)
    SimulationResult += ("history" -> new History())
    bootScenario.simulate(classOf[LauncherComp])
    val history = SimulationResult.get[History]("history")
    if (history.isDefined) {
      isLinearizable(history.get, mutable.Map.empty[String, String]) should be (true)
    }
  }


}
