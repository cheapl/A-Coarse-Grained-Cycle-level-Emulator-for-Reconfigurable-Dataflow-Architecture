package DFG_Emulator

import akka.actor.ActorSystem

import scala.io.StdIn

object Emulator_Main
{
	def main(args: Array[String]): Unit =
	{
		// First Read the DFG Configuration

		println("Hello World!")
		val emulator = ActorSystem("emulator")

		try
		{
			val ec = emulator.actorOf(EC.props(-1, 2), "EC")
			val pu_0 =
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