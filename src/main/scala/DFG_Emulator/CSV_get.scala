package  DFG_Emulator
import scala.io.Source 
import scala.collection.mutable.ArrayBuffer


object csv{

  def strClean(Str: String):String = {
    if (Str.head.toInt > 256) Str.tail
    else Str
  }

  def E_New(Str: String):Emulator_Numerics = { 
    if (Str == "NaN") new NaN
    else if (Str == "Identity") new Identity
    else if(Str.split(":")(1) == "Int") new Emulator_Numerics(Str.split(":")(0).toInt)
    else if(Str.split(":")(1) == "Long") new Emulator_Numerics(Str.split(":")(0).toLong)
    else if(Str.split(":")(1) == "Float") new Emulator_Numerics(Str.split(":")(0).toFloat)
    else if(Str.split(":")(1) == "Double") new Emulator_Numerics(Str.split(":")(0).toDouble)
    else if(Str.split(":")(1) == "Char") new Emulator_Numerics(Str.split(":")(0)(0))
    else if(Str.split(":")(1) == "Boolean") new Emulator_Numerics(Str.split(":")(0).toBoolean)
    else new Emulator_Numerics(-1)
  }

  def get(fileName: String):Array[Array[Emulator_Numerics]] = {
    
    var StrArray = new ArrayBuffer[ArrayBuffer[String]]()

    val file=Source.fromFile(fileName)
    for(line <- file.getLines) {
      var lineList = line.split(",")
      var lineArray = new ArrayBuffer[String]
      for (ele <- lineList) lineArray += ele
      StrArray += lineArray
    }
    file.close

    var E_AnyValArray = new ArrayBuffer[ArrayBuffer[Emulator_Numerics]]()

    for (line <- StrArray) {
      var lineArray = new ArrayBuffer[Emulator_Numerics]()
      for (ele <- line) {
        lineArray += E_New(strClean(ele))
      }
      E_AnyValArray += lineArray
    }

    var i:Int = 0
    var res = new Array[Array[Emulator_Numerics]](E_AnyValArray.length)
    while(i < E_AnyValArray.length){
      var j:Int = 0
      var lineArray = new Array[Emulator_Numerics](E_AnyValArray(i).length)
      while(j < E_AnyValArray(i).length){
        lineArray(j) = E_AnyValArray(i)(j)
        j += 1
      }
      res(i) = lineArray
      i += 1
    }
    res
  }
}

//object main{
//
//  def main(args: Array[String]): Unit = {
//    var res = csv.get("testcase/matrix.csv")
//    for (line <- res ){
//      for (ele <- line ) printVal.printVal(ele)
//      println("------------")
//    }
//
//  }
//}