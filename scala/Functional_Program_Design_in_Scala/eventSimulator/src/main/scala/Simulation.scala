
/** Class Hierarchy: Simulation > Gates (Wire, AND, OR, NOT) > 
  * Circuits (HalfAdder, FullAdder) > MySimulation and Param trait.
  */
trait Simulation {
  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]
  private var agenda: Agenda = List()

  //to handle time
  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  //Ensures the agenda is time-sorted
  private def insert(agenda: List[Event], item: Event): List[Event] = agenda match {
    case first :: rest if (first.time <= item.time) => first :: insert(rest, item)
    case _ => item :: agenda
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }

  //The event handling loop remove successive elements from the agenda and performs
  //the associated actions
  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }
}



