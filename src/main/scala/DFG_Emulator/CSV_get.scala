import scala.io.Source 
import scala.collection.mutable.ArrayBuffer
//import etype._

object csv{

  def strClean(Str: String):String = {
    if (Str.head.toInt > 256) Str.tail
    else Str
  }

  def E_New(Str: String):E_AnyVal = { 
    if (Str == "NaN") new NaN
    else if(Str.split(":")(1) == "Int") new E_AnyVal(Str.split(":")(0).toInt)
    else if(Str.split(":")(1) == "Long") new E_AnyVal(Str.split(":")(0).toLong)
    else if(Str.split(":")(1) == "Float") new E_AnyVal(Str.split(":")(0).toFloat)
    else if(Str.split(":")(1) == "Double") new E_AnyVal(Str.split(":")(0).toDouble)
    else if(Str.split(":")(1) == "Char") new E_AnyVal(Str.split(":")(0)(0))
    else if(Str.split(":")(1) == "Boolean") new E_AnyVal(Str.split(":")(0).toBoolean)
    else new E_AnyVal(-1)
  }

  def get(fileName: String):Array[Array[E_AnyVal]] = {
    
    var StrArray = new ArrayBuffer[ArrayBuffer[String]]()

    val file=Source.fromFile(fileName)
    for(line <- file.getLines) {
      var lineList = line.split(",")
      var lineArray = new ArrayBuffer[String]
      for (ele <- lineList) lineArray += ele
      StrArray += lineArray
    }
    file.close

    var E_AnyValArray = new ArrayBuffer[ArrayBuffer[E_AnyVal]]()

    for (line <- StrArray) {
      var lineArray = new ArrayBuffer[E_AnyVal]()
      for (ele <- line) {
        lineArray += E_New(strClean(ele))
      }
      E_AnyValArray += lineArray
    }

    var i:Int = 0
    var res = new Array[Array[E_AnyVal]](E_AnyValArray.length)
    while(i < E_AnyValArray.length){
      var j:Int = 0
      var lineArray = new Array[E_AnyVal](E_AnyValArray(i).length)
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

object main{

  def main(args: Array[String]): Unit = {
    var res = csv.get("testcase/matrix.csv")
    for (line <- res ){
      for (ele <- line ) printVal.printVal(ele)
      println("------------")
    }
   
  }
}