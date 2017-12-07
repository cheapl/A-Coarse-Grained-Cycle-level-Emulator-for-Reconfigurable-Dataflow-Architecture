package DFG_Emulator

import akka.actor.{Actor, ActorLogging, Props}

object Emulator_Supervisor {
	def props(): Props = Props(new Emulator_Supervisor)
}

class Emulator_Supervisor extends Actor with ActorLogging
{
	override def preStart(): Unit = log.info("DFG emulator started.")
	override def postStop(): Unit = log.info("DFG emulator stopped.")

	// No need to handle any messages
	override def receive = Actor.emptyBehavior
}


