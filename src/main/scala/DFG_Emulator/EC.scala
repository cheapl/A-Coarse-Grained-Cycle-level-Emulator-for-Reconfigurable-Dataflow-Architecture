package DFG_Emulator

import DFG_Emulator.EC.{Emulation_Start, Local_Cycle_Finished, Register_PU}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object EC
{
	def props(ID: Int, num_of_PU: Int, UB: ActorRef): Props = Props(new EC(ID, num_of_PU, UB))
	case class Local_Cycle_Finished(ID: Int)
	case class Register_PU(ID: Int)
	case object Emulation_Start
}

class EC(val ID: Int,
				 val num_of_PU: Int,
				 val UB: ActorRef) extends Actor with ActorLogging with Emulator_Components
{
	val PU_list: Array[ActorRef] = new Array[ActorRef](num_of_PU)
	var PU_reg_counter = 0
	var counter = 0
	var cycles = -1
	def receive =
	{
		case Register_PU(id) ⇒
			{
				println(s"${id} has registered")
				PU_list(id) = sender()
				PU_reg_counter += 1
				if (PU_reg_counter == num_of_PU)
					{
						for (pu ← PU_list) pu ! Synchronize
						UB ! Synchronize
					}
			}
		case Local_Cycle_Finished(id) ⇒
			{
				counter += 1
				//println(s"PU ${id} has finished its cycle. Current count: ${counter}")
				if (counter == num_of_PU)
					{
						cycles += 1
						counter = 0
						println(s"Current Cycle: ${cycles}")
						for (pu ← PU_list) pu ! Synchronize
						UB ! Synchronize
					}
			}
		case Emulation_Start ⇒
			{
				for (pu ← PU_list) pu ! Synchronize
				UB ! Synchronize
			}
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}
}