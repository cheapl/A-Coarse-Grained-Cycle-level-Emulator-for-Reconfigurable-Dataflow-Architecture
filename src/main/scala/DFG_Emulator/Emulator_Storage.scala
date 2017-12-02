package DFG_Emulator

import scala.collection.mutable.Queue

trait Emulator_Storage
{
	val size: Int
}

class Buffer extends Emulator_Storage
{
	val size = 0
	val buffer = new Queue[Any]()
	def pop() : Any = buffer.dequeue()
	def push(item: Any): Unit = buffer.enqueue(item)
}

class Memory(val size: Int) extends Emulator_Storage
{
	val memory = new Array[Any](size)
}

class Register(val size: Int) extends Emulator_Storage
{
	val register = new Array[Any](size)
}