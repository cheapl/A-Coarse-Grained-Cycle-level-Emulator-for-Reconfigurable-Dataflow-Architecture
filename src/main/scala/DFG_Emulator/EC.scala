package DFG_Emulator

import DFG_Emulator.EC.{Local_Cycle_Finished, Register_PU, Register_UB}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object EC
{
	def props(ID: Int, num_of_PU: Int, UB: ActorRef): Props = Props(new EC(ID, num_of_PU, UB))
	case class Local_Cycle_Finished(ID: Int)
	case class Register_PU(ID: Int)
	case object Register_UB
}

class EC(val ID: Int,
				 val num_of_PU: Int,
				 val UB: ActorRef) extends Actor with ActorLogging with Emulator_Components
{
	val PU_list: Array[ActorRef] = new Array[ActorRef](num_of_PU)
	var counter = 0
	def receive =
	{
		case Register_PU(id) ⇒ PU_list(id) = sender()
		case Local_Cycle_Finished(id) ⇒
			{
				counter += 1
				if (counter == num_of_PU)
					{
						for (pu ← PU_list) pu ! Synchronize
						UB ! Synchronize
					}
			}
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}
}