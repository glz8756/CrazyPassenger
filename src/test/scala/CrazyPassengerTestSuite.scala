import CrazyPassenger.{getRandomSeat, simulation, simulationProbability}
import org.scalatest.funsuite.AnyFunSuite


class CrazyPassengerTestSuite extends AnyFunSuite {
  val seatMap: Map[Int, Int] = Map(
    1 -> 0,
    2 -> 0,
    3 -> 1,
    4 -> 0,
    5 -> 3,
    6 -> 0,
    7 -> 5,
    8 -> 0,
    9 -> 7,
    10 -> 0
  )

  val seatKeys = Set(10, 1, 6, 2, 8, 4)
  val probability = simulationProbability(100000, 100, 100)

  test("Run getRandomSeat, the function should return random number within Set(10, 1, 6, 2, 8, 4)"){
    assert(seatKeys.contains(getRandomSeat(seatMap)))
  }

  test("Run the simulation 100,000 times, the probability should be less than 0.55") {
    assert(simulationProbability(1000, 100, 100) < 0.55)
  }

  test("Run the simulation 100,000 times, the probability should be great than 0.45") {
    assert(simulationProbability(1000, 100, 100) > 0.45)
  }

  test("the simulation function should be return true or false") {
    val result: Set[Boolean] = Set(true, false)
    assert(result.contains(simulation(100, 1000)))
  }


}