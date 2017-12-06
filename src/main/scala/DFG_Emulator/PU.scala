package DFG_Emulator

import DFG_Emulator.EC.{Local_Cycle_Finished, Register_PU}
import DFG_Emulator.PU.Data_message
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.Settings

class Connection(val destination: Int, val port_id: Int)

object PU
{
	def props(arg_pack: PU_Arg_Pack, EC: ActorRef): Props = Props(new PU(arg_pack, EC))
	case class Data_message(data: Array[Emulator_Numerics], receiver_port: Int)
	//case class Set_Children(children: Array[ActorRef])
}

class PU(val arg_pack: PU_Arg_Pack,
				 val ec: ActorRef) extends Actor with ActorLogging with Clocked_Components
{

	val ID: Int = arg_pack.ID
	val period: Int = arg_pack.period
	val input_port_length: Int = arg_pack.input_port_length
	val output_port_length: Int = arg_pack.output_port_length
	val input_port_width: Array[Int] = arg_pack.input_port_width
	val output_port_width: Array[Int] = arg_pack.output_port_width
	val register_length: Int = arg_pack.register_length
	val register_width: Array[Int] = arg_pack.register_width
	val accumulator_length: Int = arg_pack.accumulator_length
	val accumulator_width: Array[Int] = arg_pack.accumulator_width
	val Output_Port_Delay: Array[Int] = arg_pack.output_port_delay
	val code: String = arg_pack.code
	val input_ports: Array[IOPort] = new Array[IOPort](input_port_length)
	val output_ports: Array[IOPort] = new Array[IOPort](output_port_length)
	val connections: Array[edge] = arg_pack.edges_list
	val registers: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](register_length)
	val accumulators: Array[Accumulator] = new Array[Accumulator](accumulator_length)
	val input_data: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](input_port_length)
	val output_data: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](output_port_length)
	val output_buffer: Array[Array[Emulator_Numerics]] = new Array[Array[Emulator_Numerics]](output_port_length)
	//val Children: Array[ActorRef] = new Array[ActorRef](output_port_length)

	for(i ← 0 until input_port_length)
		{
			input_ports(i) = new IOPort(input_port_width(i))
			input_ports(i).write(Array.fill[Emulator_Numerics](input_port_width(i))(new Identity))
			input_data(i) = new Array[Emulator_Numerics](input_port_width(i))
		}
	for(i ← 0 until output_port_length)
		{
			output_ports(i) = new IOPort(output_port_width(i))
			output_ports(i).write(Array.fill[Emulator_Numerics]((period - 1) * output_port_width(i) + Output_Port_Delay(i))(new Identity))
			output_data(i) = new Array[Emulator_Numerics](output_port_width(i))
			output_buffer(i) = new Array[Emulator_Numerics](output_port_width(i))
		}

	for (i ← 0 until register_length)		registers(i) = Array.fill[Emulator_Numerics](register_width(i))(new Emulator_Numerics(0))
	for (i ← 0 until accumulator_length)		accumulators(i) = new Accumulator(accumulator_width(i))

	def invoke(): Unit =
	{
		for(i ← 0 until input_port_length)	input_ports(i).read(input_data(i))
		val settings = new Settings()
		settings.usejavacp.value = true
		val executer = new IMain(settings)
		executer.beQuietDuring
			{
				executer.interpret("import DFG_Emulator.Emulator_Storage")
				executer.interpret("import DFG_Emulator.Emulator_Numerics")
				executer.interpret("import DFG_Emulator.NaN")
				executer.interpret("import DFG_Emulator.Identity")
				executer.interpret("import DFG_Emulator.printVal")
				executer.bind("id", "Int", ID)
				//for (i ← 0 until input_port_length)		executer.bind("Input(" + i + ")", "Array[Emulator_Numerics]", input_data(i))
				//for (i ← 0 until output_port_length)	executer.bind("Output(" + i + ")", "Array[Emulator_Numerics]", output_data(i))
				//for (i ← 0 until register_length)	executer.bind("Register(" + i + ")", "Array[Emulator_Numerics]", registers(i))
				executer.bind("Input", "Array[Array[DFG_Emulator.Emulator_Numerics]]", input_data)
				executer.bind("Output", "Array[Array[DFG_Emulator.Emulator_Numerics]]", output_data)
				executer.bind("Register", "Array[Array[DFG_Emulator.Emulator_Numerics]]", registers)
				executer.bind("Accumulator", "Array[DFG_Emulator.Accumulator]", accumulators)
				executer.interpret(code)
			}
		for(i ← 0 until output_port_length)
			{
				output_ports(i).write(output_data(i))
				output_ports(i).read(output_buffer(i))
			}

		for(connection ← connections)
			{
				context.actorSelection("../PU_" + connection.destination.toString()) ! new Data_message(output_buffer(connection.source_output_port).clone(), connection.destination_input_port)
			}
		ec ! Local_Cycle_Finished(ID)

	}

	def receive =
	{
		//case Set_Children(children) ⇒ Array.copy(children, 0, Children, 0, children.length)
		case Synchronize ⇒ invoke()
		case Initialize ⇒ {
			ec ! Register_PU(ID)
			println(s"PU ${ID} has period ${period}")
		}
		case Data_message(data, receiver_port) ⇒ input_ports(receiver_port).write(data)
		case "test" ⇒ log.info(s"${ID}received test")
		case _      ⇒ log.info("received unknown message")
	}

}

