package DFG_Emulator

trait Emulator_IOPort
{
	val length: Int
	val buffer: Buffer
}

class IOPort(val length: Int,
						 val delay: Int,
						 val downstream: IOPort) extends Emulator_IOPort
{
	val buffer = new Buffer
	def read() : Unit = for(i ← 1 to length) yield buffer.pop
	def write(output: Array[Emulator_Numerics]) : Unit = output.foreach(buffer.push)
}
