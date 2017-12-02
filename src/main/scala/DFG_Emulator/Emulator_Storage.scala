package DFG_Emulator

import scala.collection.mutable

trait Emulator_Storage
{
	val size: Int
}

class Buffer extends Emulator_Storage
{
	val size = 0
	val buffer = new mutable.Queue[Emulator_Numerics]()
	def pop() : Emulator_Numerics = buffer.dequeue()
	def push(item: Emulator_Numerics): Unit = buffer.enqueue(item)
}

class Memory(val size: Int) extends Emulator_Storage
{
	val memory = new Array[Emulator_Numerics](size)
}

class Register(val size: Int) extends Emulator_Storage
{
	val register = new Array[Emulator_Numerics](size)
}