package DFG_Emulator

class edge(Source:Int, Destination:Int, Source_output_port:Int, Destination_input_port:Int)
{
	var source:Int = Source
	var destination:Int = Destination
	var source_output_port:Int = Source_output_port
	var destination_input_port:Int = Destination_input_port
}

class PU_Arg_Pack(id:Int, Input_port_length:Int, Output_port_length:Int, Input_port_width:Array[Int], Output_port_width:Array[Int], Register_length:Int, Register_width:Array[Int], Code: String, EdgesList:Array[edge], pu_type:Int, data_source_index: Array[Int], Output_port_delay:Array[Int], Accumulator_length:Int, Accumulator_width:Array[Int], fileNameConfig:String)
{
	var ID:Int = id
	var period:Int = calTime.calTime(calTime.bindOperators(calTime.strToArray(Code)),fileNameConfig)
	var input_port_length:Int = Input_port_length
	var output_port_length:Int = Output_port_length
	var input_port_width:Array[Int] = Input_port_width
	var output_port_width:Array[Int] = Output_port_width
	var register_length:Int = Register_length
	var register_width:Array[Int] = Register_width
	var code:String = Code
	var edges_list:Array[edge] = EdgesList
	var PU_type:Int = pu_type
	var Data_Source_Index: Array[Int] = data_source_index
	var output_port_delay:Array[Int] = Output_port_delay
	var accumulator_length:Int = Accumulator_length
	var accumulator_width:Array[Int] = Accumulator_width
}



class Source(val PU_ID: Int,
						 val Input_Ports: Array[Int],
						 val Data_Sources: Array[Int])

//trait Emulator_Arg_Pack
//{
//	val ID: Int
//}
//
//class PU_Arg_Pack(val ID: Int,
//									val period: Int,
//									val input_port_length: Int,
//									val output_port_length: Int,
//									val input_port_width: Array[Int],
//									val output_port_width: Array[Int],
//									val register_length: Int,
//									val register_width: Array[Int],
//									val code: String) extends  Emulator_Arg_Pack
//
//class EC_Arg_Pack(val ID: Int,
//									val num_of_PU: Int) extends  Emulator_Arg_Pack
//
//class UB_Arg_Pack(val ID: Int,
//									val input_port_width: Int,
//									val output_port_width: Int,
//									val memory_size: Int) extends Emulator_Arg_Pack