package DFG_Emulator

import akka.actor.{Actor, ActorLogging}

class UB(val ID: Int) extends Actor with ActorLogging with Emulator_Components
{
	def receive = {
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}
}