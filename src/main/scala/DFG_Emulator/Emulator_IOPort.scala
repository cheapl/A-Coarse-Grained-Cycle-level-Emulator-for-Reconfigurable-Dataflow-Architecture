package DFG_Emulator

trait Emulator_IOPort
{
	val length: Int
	val buffer: Buffer
}

class IOPort(val length: Int) extends Emulator_IOPort
{
	val buffer = new Buffer
	def read(destination: Array[Emulator_Numerics]) : Unit =
		{
			for(i ← 0 until length) destination(i) = buffer.pop()
		}
	def write(output: Array[Emulator_Numerics]) : Unit = output.foreach(buffer.push)
}
