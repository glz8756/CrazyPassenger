import scala.annotation.tailrec
import scala.util.Random

object CrazyPassenger {

  def getRandomSeat(seatMap: Map[Int, Int]): Int = {
    val seatKeys = seatMap.collect { case (s, p) if (p == 0) => s }.toList
    val random = new Random
    seatKeys(random.nextInt(seatKeys.length))
  }

  def simulation(seat: Int, passenge: Int): Boolean = {
    val passengerList = (1 to passenge).toList
    val seatMap = (1 to seat).map(s => (s, 0)).toMap

    @tailrec
    def simulationTailRec(remainingPassenger: List[Int], seatMap: Map[Int, Int]): Boolean = {
      if (remainingPassenger.isEmpty) false
      else {
        val nextPassenger = remainingPassenger.head
        if (nextPassenger == 100 && seatMap(nextPassenger) == 0 && getRandomSeat(seatMap) == 100) true
        else if (nextPassenger == 1) {
          val randomSeat = getRandomSeat(seatMap)
          if (randomSeat == 100) false
          else simulationTailRec(remainingPassenger.tail, seatMap + (randomSeat -> nextPassenger))
        }
        else {
          if (seatMap(nextPassenger) == 0) simulationTailRec(remainingPassenger.tail, seatMap + (nextPassenger -> nextPassenger))
          else {
            val randomSeat = getRandomSeat(seatMap)
            if (nextPassenger != 100 && randomSeat == 100) false
            else simulationTailRec(remainingPassenger.tail, seatMap + (randomSeat -> nextPassenger))
          }
        }
      }
    }
    simulationTailRec(passengerList, seatMap)
  }

  def simulationProbability(n: Int, numberOfSeats: Int, numberOfPassengers: Int): Double =
    (1 to n).map(_ => if (simulation(numberOfSeats, numberOfPassengers)) 1.00 else 0.00).sum / n.toDouble

}
