package se.kth.id2203.simulation.linearizabletest

import org.scalatest._
import se.sics.kompics.simulator.result.SimulationResultSingleton
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.sl.simulator._

class OpsTest extends FlatSpec with Matchers {

  private val nMessages = 20

  "Get Operations" should "Not be found" in {
    val seed = 123l
    JSimulationScenario.setSeed(seed)
    val simpleBootScenario = GlobalScenario.scenario(3)
    val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("nMessages" -> nMessages)
    SimulationResult += ("type" -> "get")
    simpleBootScenario.simulate(classOf[LauncherComp])
    for (i <- 0 to nMessages) {
      SimulationResult.get[String](s"test$i") should be (Some("NotFound"))
    }
  }


}
