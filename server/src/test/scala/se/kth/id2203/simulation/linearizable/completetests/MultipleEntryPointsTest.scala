package se.kth.id2203.simulation.linearizable.completetests

import org.scalatest._
import se.kth.id2203.simulation.linearizable.LinearizableUtil.isLinearizable
import se.kth.id2203.simulation.linearizable.{History, LinearizableUtil}
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.sl.simulator._

import scala.collection.mutable

class MultipleEntryPointsTest extends FlatSpec with Matchers {

  private val SERVER_NUMBER = 5

  def executeLinearizeTestPlan(seed: Long, nMessages: Int) : History = {
    JSimulationScenario.setSeed(seed)
    val bootScenario = GlobalScenario.scenario(SERVER_NUMBER)
    // val res = SimulationResultSingleton.getInstance()
    SimulationResult += ("nMessages" -> nMessages)

    bootScenario.simulate(classOf[LauncherComp])

    // val historyEncoded = res.get("history", classOf[String])
    val historyEncoded = SimulationResult.get[String]("history")
    LinearizableUtil.deserialise(historyEncoded.get)
  }

  "Execution1" should "be linearizable" in {
    val seed = 7384L
    val nMessage = 4

    val history = executeLinearizeTestPlan(seed, nMessage)
    history.isEmpty should be (false)
    isLinearizable(history, mutable.Map.empty[String, String]) should be (true)
  }

  "Execution2" should "be linearizable" in {
    val seed = 13034L
    val nMessage = 7

    val history = executeLinearizeTestPlan(seed, nMessage)
    history.isEmpty should be (false)
    isLinearizable(history, mutable.Map.empty[String, String]) should be (true)
  }

  "Execution3" should "be linearizable" in {
    val seed = 9853L
    val nMessage = 7

    val history = executeLinearizeTestPlan(seed, nMessage)
    history.isEmpty should be (false)
    isLinearizable(history, mutable.Map.empty[String, String]) should be (true)
  }
}
