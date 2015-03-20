package dnmar;

import scala.collection.mutable.PriorityQueue

//A hypothesis has a score, in addition to an array of sucessors
abstract class Hypothesis extends Ordered[Hypothesis] {
  def score: Double
  def sucessors: Array[Hypothesis]

  def priority = score

  def compare(that:Hypothesis):Int = {
    if(this.score > that.score) {
      1
    } else if(this.score < that.score) {
      -1
    } else {
      0
    }
  }
}

class BeamSearch(h:Hypothesis, beamSize:Int) {
  def this(h:Hypothesis) = this(h, 3)

  val _BEAM_SIZE=beamSize
  var hypothesisQueue = new PriorityQueue[Hypothesis]
  this.hypothesisQueue += h

  def Head:Hypothesis = hypothesisQueue.head

  def UpdateQ() {
    val next = hypothesisQueue.dequeue
    hypothesisQueue ++= next.sucessors

    if(_BEAM_SIZE > 0 && hypothesisQueue.size > _BEAM_SIZE) {
      val newQ = hypothesisQueue.take(_BEAM_SIZE)
      hypothesisQueue = newQ
    }
    //println("queue size=\t" + hypothesisQueue.size)
  }
}
