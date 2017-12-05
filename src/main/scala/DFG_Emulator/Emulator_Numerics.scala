package DFG_Emulator

import scala.math._

class Emulator_Numerics(in_value: AnyVal){
  var value:AnyVal = in_value

  //Arithmetic Operators

  def +(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int]+y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int]+y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int]+y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int]+y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int]+y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long]+y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long]+y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long]+y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long]+y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long]+y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float]+y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float]+y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float]+y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float]+y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float]+y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double]+y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double]+y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double]+y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double]+y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double]+y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char]+y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char]+y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char]+y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char]+y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char]+y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def -(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int]-y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int]-y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int]-y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int]-y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int]-y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long]-y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long]-y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long]-y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long]-y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long]-y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float]-y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float]-y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float]-y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float]-y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float]-y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double]-y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double]-y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double]-y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double]-y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double]-y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char]-y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char]-y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char]-y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char]-y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char]-y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def *(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int]*y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int]*y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int]*y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int]*y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int]*y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long]*y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long]*y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long]*y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long]*y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long]*y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float]*y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float]*y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float]*y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float]*y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float]*y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double]*y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double]*y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double]*y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double]*y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double]*y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char]*y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char]*y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char]*y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char]*y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char]*y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def /(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int]/y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int]/y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int]/y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int]/y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int]/y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long]/y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long]/y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long]/y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long]/y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long]/y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float]/y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float]/y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float]/y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float]/y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float]/y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double]/y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double]/y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double]/y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double]/y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double]/y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char]/y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char]/y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char]/y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char]/y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char]/y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def %(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int]%y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int]%y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int]%y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int]%y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int]%y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long]%y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long]%y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long]%y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long]%y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long]%y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float]%y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float]%y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float]%y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float]%y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float]%y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double]%y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double]%y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double]%y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double]%y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double]%y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char]%y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char]%y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char]%y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char]%y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char]%y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }


  //Relational Operators

  def ==(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    else new Emulator_Numerics(value == y.value)
  }

  def !=(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    else new Emulator_Numerics(value != y.value)
  }

  def >(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] > y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] > y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int] > y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int] > y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] > y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] > y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] > y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long] > y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long] > y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] > y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float] > y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float] > y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float] > y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float] > y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float] > y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double] > y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double] > y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double] > y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double] > y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double] > y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] > y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] > y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char] > y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char] > y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] > y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new Emulator_Numerics(value.asInstanceOf[Boolean] > y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def <(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] < y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] < y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int] < y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int] < y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] < y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] < y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] < y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long] < y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long] < y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] < y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float] < y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float] < y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float] < y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float] < y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float] < y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double] < y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double] < y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double] < y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double] < y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double] < y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] < y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] < y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char] < y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char] < y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] < y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new Emulator_Numerics(value.asInstanceOf[Boolean] < y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def >=(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] >= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] >= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int] >= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int] >= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] >= y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] >= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] >= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long] >= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long] >= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] >= y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float] >= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float] >= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float] >= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float] >= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float] >= y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double] >= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double] >= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double] >= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double] >= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double] >= y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] >= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] >= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char] >= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char] >= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] >= y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new Emulator_Numerics(value.asInstanceOf[Boolean] >= y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def <=(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] <= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] <= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Int] <= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Int] <= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] <= y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] <= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] <= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Long] <= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Long] <= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] <= y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Float] <= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Float] <= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Float] <= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Float] <= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Float] <= y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Double] <= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Double] <= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Double] <= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Double] <= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Double] <= y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] <= y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] <= y_value)
        case y_value:Float => new Emulator_Numerics(value.asInstanceOf[Char] <= y_value)
        case y_value:Double => new Emulator_Numerics(value.asInstanceOf[Char] <= y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] <= y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new Emulator_Numerics(value.asInstanceOf[Boolean] <= y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }


  //Logical Operators

  def &&(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new Emulator_Numerics(value.asInstanceOf[Boolean] && y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def ||(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new Emulator_Numerics(value.asInstanceOf[Boolean] || y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def unary_!() : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) this
    else if (value.isInstanceOf[Boolean]){
      new Emulator_Numerics( ! value.asInstanceOf[Boolean])
    }
    else new Emulator_Numerics(-1)
  }


  //Bitwise Operators

  def &(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] & y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] & y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] & y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] & y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] & y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] & y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] & y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] & y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] & y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def |(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] | y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] | y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] | y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] | y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] | y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] | y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] | y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] | y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] | y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def ^(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] ^ y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] ^ y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] ^ y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] ^ y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] ^ y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] ^ y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] ^ y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] ^ y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] ^ y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def unary_~() : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) this
    else if (value.isInstanceOf[Int]){
      new Emulator_Numerics( ~ value.asInstanceOf[Int])
    }
    else if (value.isInstanceOf[Long]){
      new Emulator_Numerics( ~ value.asInstanceOf[Long])
    }
    else if (value.isInstanceOf[Char]){
      new Emulator_Numerics( ~ value.asInstanceOf[Char])
    }
    else new Emulator_Numerics(-1)
  }

  def <<(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] << y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] << y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] << y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] << y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] << y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] << y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] << y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] << y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] << y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def >>(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] >> y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] >> y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] >> y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] >> y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] >> y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] >> y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] >> y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] >> y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] >> y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  def >>>(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Int] >>> y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Int] >>> y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Int] >>> y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Long] >>> y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Long] >>> y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Long] >>> y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(value.asInstanceOf[Char] >>> y_value)
        case y_value:Long => new Emulator_Numerics(value.asInstanceOf[Char] >>> y_value)
        case y_value:Char => new Emulator_Numerics(value.asInstanceOf[Char] >>> y_value)
      }
    }
    else new Emulator_Numerics(-1)
  }

  //pow and sqrt
  def sqrt() : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) this
    else if (value.isInstanceOf[Int])  new Emulator_Numerics(math.sqrt(value.asInstanceOf[Int]))
    else if (value.isInstanceOf[Long])  new Emulator_Numerics(math.sqrt(value.asInstanceOf[Long]))
    else if (value.isInstanceOf[Float])  new Emulator_Numerics(math.sqrt(value.asInstanceOf[Float]))
    else if (value.isInstanceOf[Double])  new Emulator_Numerics(math.sqrt(value.asInstanceOf[Double]))
    else if (value.isInstanceOf[Char])  new Emulator_Numerics(math.sqrt(value.asInstanceOf[Char]))
    else new Emulator_Numerics(-1)
  }

  def pow(y: Emulator_Numerics) : Emulator_Numerics = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Long => new Emulator_Numerics(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Float => new Emulator_Numerics(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Double => new Emulator_Numerics(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Char => new Emulator_Numerics(math.pow(value.asInstanceOf[Int],y_value))
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Long => new Emulator_Numerics(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Float => new Emulator_Numerics(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Double => new Emulator_Numerics(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Char => new Emulator_Numerics(math.pow(value.asInstanceOf[Long],y_value))
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Long => new Emulator_Numerics(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Float => new Emulator_Numerics(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Double => new Emulator_Numerics(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Char => new Emulator_Numerics(math.pow(value.asInstanceOf[Float],y_value))
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Long => new Emulator_Numerics(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Float => new Emulator_Numerics(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Double => new Emulator_Numerics(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Char => new Emulator_Numerics(math.pow(value.asInstanceOf[Double],y_value))
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new Emulator_Numerics(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Long => new Emulator_Numerics(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Float => new Emulator_Numerics(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Double => new Emulator_Numerics(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Char => new Emulator_Numerics(math.pow(value.asInstanceOf[Char],y_value))
      }
    }
    else new Emulator_Numerics(-1)
  }

}


class NaN extends Emulator_Numerics{}

class Identity extends Emulator_Numerics{}


object printVal{

  def printVal(x: Emulator_Numerics, id: Int){
    if (x.value.isInstanceOf[Int]) println(s"Result from PU${id}: Int:"+x.value.asInstanceOf[Int])
    else if (x.value.isInstanceOf[Long]) println(s"Result from PU${id}: Long:"+x.value.asInstanceOf[Long])
    else if (x.value.isInstanceOf[Float]) println(s"Result from PU${id}: Float:"+x.value.asInstanceOf[Float])
    else if (x.value.isInstanceOf[Double]) println(s"Result from PU${id}: Double:"+x.value.asInstanceOf[Double])
    else if (x.value.isInstanceOf[Char]) println(s"Result from PU${id}: Char:"+x.value.asInstanceOf[Char])
    else if (x.value.isInstanceOf[Boolean]) println(s"Result from PU${id}: Boolean:"+x.value.asInstanceOf[Boolean])
    else if (x.isInstanceOf[NaN]) println(s"Result from PU${id}: NaN")
    else if (x.isInstanceOf[Identity]) println(s"Result from PU${id}: Identity")
    else println(s"${id}: Error")
  }

}

