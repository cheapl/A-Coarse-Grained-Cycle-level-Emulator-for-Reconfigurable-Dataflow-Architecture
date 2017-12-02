package DFG_Emulator

trait Emulator_Arg_Pack
{
	val ID: Int
}

class PU_Arg_Pack(val ID: Int,
									val period: Int,
									val input_port_length: Int,
									val output_port_length: Int,
									val input_port_width: Array[Int],
									val output_port_width: Array[Int],
									val register_length: Int,
									val register_width: Array[Int],
									val code: String) extends  Emulator_Arg_Pack

class EC_Arg_Pack(val ID: Int,
									val num_of_PU: Int) extends  Emulator_Arg_Pack

class UB_Arg_Pack(val ID: Int,
									val input_port_width: Int,
									val output_port_width: Int,
									val memory_size: Int) extends Emulator_Arg_Pack