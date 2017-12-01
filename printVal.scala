import etype._

object printVal{

  def printVal(x: E_AnyVal){
    if (x.value.isInstanceOf[Int]) println("Int:"+x.value.asInstanceOf[Int])
    else if (x.value.isInstanceOf[Long]) println("Long:"+x.value.asInstanceOf[Long])
    else if (x.value.isInstanceOf[Float]) println("Float:"+x.value.asInstanceOf[Float])
    else if (x.value.isInstanceOf[Double]) println("Double:"+x.value.asInstanceOf[Double])
    else if (x.value.isInstanceOf[Char]) println("Double:"+x.value.asInstanceOf[Char])
    else if (x.value.isInstanceOf[Boolean]) println("Boolean:"+x.value.asInstanceOf[Boolean])
    else if (x.isInstanceOf[NaN]) println("NaN")
    else println("Error")
  }

}