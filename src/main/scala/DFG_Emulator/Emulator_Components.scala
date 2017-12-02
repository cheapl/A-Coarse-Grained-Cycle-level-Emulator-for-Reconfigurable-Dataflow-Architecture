package DFG_Emulator

trait Emulator_Components
{
	val ID: Int
}

trait Clocked_Components extends Emulator_Components
{
	val period: Int
}

