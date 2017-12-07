 package DFG_Emulator

 import DFG_Emulator.EC.Emulation_Start
 import akka.actor.{ActorRef, ActorSystem}

 import scala.io.StdIn
 import scala.tools.nsc.interpreter.IMain
 import scala.tools.nsc.Settings

 class Fuck
 {
	 val value = 14
 }
 object Emulator_Main
 {
 	def main(args: Array[String]): Unit =
 	{

		//val s = new Settings
		//s.usejavacp.value = true
		//var i = new IMain(s)
		//var a = Array(new Fuck,new Fuck,new Fuck)
		//var b = new Array[Fuck](3)
		//i.beQuietDuring(Unit)
		//({
		//	i.interpret("import DFG_Emulator.Fuck")
		//	i.bind("input", "Array[DFG_Emulator.Fuck]", a)
		//	i.bind("output", "Array[DFG_Emulator.Fuck]", b)
		//
		//	i.interpret("for (i <- 0 until 3) {output(i) = input(i)}")
		//	i.interpret("val f = new DFG_Emulator.Fuck")
		//	i.interpret("println(f.value)")
		//})
		//println(b.mkString(" "))
 		 //def print_arg_pack(res: Array[PU_Arg_Pack], index: Int): Unit =
 		 //{
 		 //	println("ID:"+res(index).ID)
 		 //	println("period:"+res(index).period)
 		 //	println("input_port_length:"+res(index).input_port_length)
 		 //	println("output_port_length:"+res(index).output_port_length)
 		 //	println("input_port_width:"+res(index).input_port_width.mkString(" "))
 		 //	println("output_port_width:"+res(index).output_port_width.mkString(" "))
 		 //	println("register_length:"+res(index).register_length)
 		 //	println("register_width:"+res(index).register_width.mkString(" "))
 		 //	println("code:"+res(index).code)
 		 //	println("edgesList:"+res(index).edges_list(0).destination)
 		 //}
 		// First Read the DFG Configuration

 		var DFG_Args = DFG_Configuration_Parser.parse("M:\\Codes\\CS210\\Project\\A-Coarse-Grained-Cycle-level-Emulator-for-Reconfigurable-Dataflow-Architecture\\src\\main\\scala\\DFG_Emulator\\testcase\\matrix_nodes.json","M:\\Codes\\CS210\\Project\\A-Coarse-Grained-Cycle-level-Emulator-for-Reconfigurable-Dataflow-Architecture\\src\\main\\scala\\DFG_Emulator\\testcase\\matrix_edges.json")
 		var PUs = DFG_Args._1
		var Source_PUs = DFG_Args._2
		// print_arg_pack(res, 0)
 		val emulator = ActorSystem("emulator")
		val num_of_PUs = PUs.length
 		try
 		{
			val data = csv.get("M:\\Codes\\CS210\\Project\\A-Coarse-Grained-Cycle-level-Emulator-for-Reconfigurable-Dataflow-Architecture\\src\\main\\scala\\DFG_Emulator\\testcase\\matrix.csv")
			val ub = emulator.actorOf(UB.props(0, data, Source_PUs), "UB")
 			val ec = emulator.actorOf(EC.props(-1, num_of_PUs, ub), "EC")
			val PU_Actors: Array[ActorRef] = new Array[ActorRef](num_of_PUs)
 			for (i ← 0 until num_of_PUs)
				{
					PU_Actors(i) = emulator.actorOf(PU.props(PUs(i), ec), "PU_" + PUs(i).ID.toString())
					PU_Actors(i) ! Initialize
				}

			//val children_list: Array[Array[ActorRef]] = new Array[Array[ActorRef]](num_of_PUs)
			//for (i ← 0 until num_of_PUs)
			//{
			//	children_list(i) = new Array[ActorRef](DFG_Args(i).output_port_length)
			//
			//}
			println("Finished")
 			StdIn.readLine()
 		}
 		finally
 		{
 			emulator.terminate()
 		}
 	}
 }

//package DFG_Emulator
//
//import akka.actor.{ActorRef, ActorSystem}
//
//import scala.io.StdIn
//
//object Emulator_Main
//{
//	def main(args: Array[String]): Unit =
//	{
//		println("Hello World!")
//		val emulator = ActorSystem("emulator")
//
//		try
//		{
//			val ec = emulator.actorOf(ECP.props(-1, 5), "ECP")
//			val PU_List: Array[ActorRef] = new Array[ActorRef](5)
//			PU_List(0) = emulator.actorOf(PUP.props(0, ec))
//			PU_List(1) = emulator.actorOf(PUP.props(1, ec))
//			PU_List(2) = emulator.actorOf(PUP.props(2, ec))
//			PU_List(3) = emulator.actorOf(PUP.props(3, ec))
//			PU_List(4) = emulator.actorOf(PUP.props(4, ec))
//			PU_List(0) ! Initialize
//			PU_List(1) ! Initialize
//			PU_List(2) ! Initialize
//			PU_List(3) ! Initialize
//			PU_List(4) ! Initialize
//			PU_List(0) ! Synchronize
//			PU_List(1) ! Synchronize
//			PU_List(2) ! Synchronize
//			PU_List(3) ! Synchronize
//			PU_List(4) ! Synchronize
//			StdIn.readLine()
//		}
//		finally
//		{
//			emulator.terminate()
//		}
//	}
//}