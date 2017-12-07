package DFG_Emulator

import DFG_Emulator.ECP.{Local_Cycle_Finished, Register_PU}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object PUP
{
	def props(ID: Int, EC: ActorRef): Props = Props(new PUP(ID, EC))
	case class Data_message(data: Array[Emulator_Numerics], receiver_port: Int)
	case class Set_Children(children: Array[ActorRef])
}

class PUP(val ID: Int,
				 val EC: ActorRef) extends Actor with ActorLogging with Emulator_Components
{
	var dummy = 0
	def invoke(): Unit =
	{
		for (x ← 1 to 999999999)
			{
				dummy += 1
			}
		dummy = 0
		println(s"PU ${ID} finished its work.")
		EC ! Local_Cycle_Finished(ID)
	}
	def receive =
	{
		case Synchronize ⇒ invoke()
		case Initialize ⇒ EC ! Register_PU(ID)
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}

}

