package DFG_Emulator

import DFG_Emulator.ECP.{Local_Cycle_Finished, Register_PU}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object ECP
{
	def props(ID: Int, num_of_PU: Int): Props = Props(new ECP(ID, num_of_PU))
	case class Local_Cycle_Finished(ID: Int)
	case class Register_PU(ID: Int)
}

class ECP(val ID: Int,
				 val num_of_PU: Int) extends Actor with ActorLogging with Emulator_Components
{
	val PU_list: Array[ActorRef] = new Array[ActorRef](num_of_PU)
	var counter = 0
	var cycles = 0
	def receive =
	{
		case Register_PU(id) ⇒
			{
				PU_list(id) = sender()
				println(s"PU ${id} successfully registered.")
			}
		case Local_Cycle_Finished(id) ⇒
			{
				counter += 1
				if (counter == num_of_PU)
					{
						cycles +=1
						for (pu ← PU_list) pu ! Synchronize
						counter = 0
						println(s"Cycle: ${cycles}")
					}
			}
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}
}