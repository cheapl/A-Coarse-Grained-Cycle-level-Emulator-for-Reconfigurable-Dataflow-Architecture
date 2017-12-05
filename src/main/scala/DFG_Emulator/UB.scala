package DFG_Emulator

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object UB
{
	def props(ID: Int, data: Array[Array[Emulator_Numerics]], Sources: Array[PU_Arg_Pack]): Props = Props(new UB(ID, data, Sources))

}

class UB(val ID: Int,
				 val data: Array[Array[Emulator_Numerics]],
				 val Sources: Array[PU_Arg_Pack]) extends Actor with ActorLogging with Emulator_Components
{
	import PU.Data_message
	val data_buffer: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](data.length)
	var cycles = 0
	def pump_data(): Unit =
	{
		for (source ← Sources)
			{
				var source_PU = context.actorSelection("../PU_" + source.ID.toString())
				for(i ← 0 until source.input_port_length)
					{
						if (source.input_port_width(i) != -1)		source_PU ! new Data_message(Array(data(source.Data_Source_Index(i))(cycles)), i)
						//for (j ← 0 until source.input_port_width(i))
					}
			}
		cycles += 1
	}

	def receive =
	{
		case Synchronize ⇒ pump_data()
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}
}