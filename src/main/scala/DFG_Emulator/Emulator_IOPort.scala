package DFG_Emulator

trait Emulator_IOPort
{
	val width: Int
	val buffer: Buffer
}

class IOPort(val width: Int) extends Emulator_IOPort
{
	val buffer = new Buffer
	def read(destination: Array[Emulator_Numerics]) : Unit =
		{
			for(i ← 0 until width) destination(i) = buffer.pop()
		}
	def write(data: Array[Emulator_Numerics]) : Unit = data.foreach(buffer.push(_))
}
