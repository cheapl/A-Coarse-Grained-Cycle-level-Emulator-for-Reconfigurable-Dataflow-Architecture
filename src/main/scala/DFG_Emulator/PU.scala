package DFG_Emulator

import DFG_Emulator.EC.Local_Cycle_Finished
import DFG_Emulator.PU.{Data_message, Set_Children}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

class Connection(val destination: Int, val port_id: Int)

object PU
{
	def props(arg_pack: PU_Arg_Pack, EC: ActorRef): Props = Props(new PU(arg_pack, EC))
	case class Data_message(data: Array[Emulator_Numerics], receiver_port: Int)
	case class Set_Children(children: Array[ActorRef])
}

class PU(val arg_pack: PU_Arg_Pack,
				 val EC: ActorRef) extends Actor with ActorLogging with Clocked_Components
{
	val ID: Int = arg_pack.ID
	val period: Int = arg_pack.period
	val input_port_length: Int = arg_pack.input_port_length
	val output_port_length: Int = arg_pack.output_port_length
	val input_port_width: Array[Int] = arg_pack.input_port_width
	val output_port_width: Array[Int] = arg_pack.output_port_width
	val register_length: Int = arg_pack.register_length
	val register_width: Array[Int] = arg_pack.register_width
	val code: String = arg_pack.code
	val input_ports: Array[IOPort] = new Array[IOPort](input_port_length)
	val output_ports: Array[IOPort] = new Array[IOPort](output_port_length)
	val input_data: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](input_port_length)
	val output_data: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](output_port_length)
	val Children: Array[ActorRef] = new Array[ActorRef](output_port_length)

	for(i ← 0 until input_port_length)
		{
			input_ports(i) = new IOPort(input_port_width(i))
			input_data(i) = new Array[Emulator_Numerics](input_port_width(i))
		}
	for(i ← 0 until output_port_length)
		{
			output_ports(i) = new IOPort(output_port_width(i))
			output_data(i) = new Array[Emulator_Numerics](output_port_width(i))
		}

	def invoke(): Unit =
	{
		EC ! Local_Cycle_Finished(ID)
	}
	def receive =
	{
		case Set_Children(children) ⇒ Array.copy(children, 0, Children, 0, children.length)
		case Synchronize ⇒ invoke()
		case Data_message(data, receiver_port) ⇒ input_ports(receiver_port).write(data)
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}

}

