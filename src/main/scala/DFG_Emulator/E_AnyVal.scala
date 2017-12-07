//package etype

import scala.math._

class E_AnyVal(in_value: AnyVal){
  var value:AnyVal = in_value

  //Arithmetic Operators

  def +(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int]+y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int]+y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int]+y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int]+y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int]+y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long]+y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long]+y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long]+y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long]+y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long]+y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float]+y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float]+y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float]+y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float]+y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float]+y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double]+y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double]+y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double]+y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double]+y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double]+y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char]+y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char]+y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char]+y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char]+y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char]+y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def -(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int]-y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int]-y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int]-y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int]-y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int]-y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long]-y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long]-y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long]-y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long]-y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long]-y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float]-y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float]-y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float]-y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float]-y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float]-y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double]-y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double]-y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double]-y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double]-y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double]-y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char]-y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char]-y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char]-y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char]-y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char]-y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def *(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int]*y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int]*y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int]*y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int]*y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int]*y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long]*y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long]*y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long]*y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long]*y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long]*y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float]*y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float]*y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float]*y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float]*y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float]*y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double]*y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double]*y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double]*y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double]*y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double]*y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char]*y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char]*y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char]*y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char]*y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char]*y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def /(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int]/y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int]/y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int]/y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int]/y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int]/y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long]/y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long]/y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long]/y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long]/y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long]/y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float]/y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float]/y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float]/y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float]/y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float]/y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double]/y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double]/y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double]/y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double]/y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double]/y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char]/y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char]/y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char]/y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char]/y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char]/y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def %(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int]%y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int]%y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int]%y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int]%y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int]%y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long]%y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long]%y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long]%y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long]%y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long]%y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float]%y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float]%y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float]%y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float]%y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float]%y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double]%y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double]%y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double]%y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double]%y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double]%y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char]%y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char]%y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char]%y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char]%y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char]%y_value)
      }
    }
    else new E_AnyVal(-1)
  }


  //Relational Operators

  def ==(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    else new E_AnyVal(value == y.value)
  }

  def !=(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    else new E_AnyVal(value != y.value)
  }

  def >(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] > y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] > y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int] > y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int] > y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] > y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] > y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] > y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long] > y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long] > y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] > y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float] > y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float] > y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float] > y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float] > y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float] > y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double] > y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double] > y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double] > y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double] > y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double] > y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] > y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] > y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char] > y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char] > y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] > y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new E_AnyVal(value.asInstanceOf[Boolean] > y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def <(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] < y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] < y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int] < y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int] < y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] < y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] < y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] < y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long] < y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long] < y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] < y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float] < y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float] < y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float] < y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float] < y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float] < y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double] < y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double] < y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double] < y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double] < y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double] < y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] < y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] < y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char] < y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char] < y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] < y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new E_AnyVal(value.asInstanceOf[Boolean] < y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def >=(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] >= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] >= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int] >= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int] >= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] >= y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] >= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] >= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long] >= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long] >= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] >= y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float] >= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float] >= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float] >= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float] >= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float] >= y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double] >= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double] >= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double] >= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double] >= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double] >= y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] >= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] >= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char] >= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char] >= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] >= y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new E_AnyVal(value.asInstanceOf[Boolean] >= y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def <=(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] <= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] <= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Int] <= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Int] <= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] <= y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] <= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] <= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Long] <= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Long] <= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] <= y_value)
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Float] <= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Float] <= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Float] <= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Float] <= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Float] <= y_value)
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Double] <= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Double] <= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Double] <= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Double] <= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Double] <= y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] <= y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] <= y_value)
        case y_value:Float => new E_AnyVal(value.asInstanceOf[Char] <= y_value)
        case y_value:Double => new E_AnyVal(value.asInstanceOf[Char] <= y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] <= y_value)
      }
    }
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new E_AnyVal(value.asInstanceOf[Boolean] <= y_value)
      }
    }
    else new E_AnyVal(-1)
  }


  //Logical Operators

  def &&(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new E_AnyVal(value.asInstanceOf[Boolean] && y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def ||(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Boolean
    else if (value.isInstanceOf[Boolean]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Boolean => new E_AnyVal(value.asInstanceOf[Boolean] || y_value)
      }
    }
    else new E_AnyVal(-1)
  }

  def unary_!() : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) this
    else if (value.isInstanceOf[Boolean]){
      new E_AnyVal( ! value.asInstanceOf[Boolean])
    }
    else new E_AnyVal(-1)
  }


  //Bitwise Operators

  def &(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] & y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] & y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] & y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] & y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] & y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] & y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] & y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] & y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] & y_value)
      }
    } 
    else new E_AnyVal(-1)
  }

  def |(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] | y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] | y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] | y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] | y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] | y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] | y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] | y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] | y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] | y_value)
      }
    } 
    else new E_AnyVal(-1)
  }

  def ^(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] ^ y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] ^ y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] ^ y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] ^ y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] ^ y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] ^ y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] ^ y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] ^ y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] ^ y_value)
      }
    } 
    else new E_AnyVal(-1)
  }

  def unary_~() : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) this
    else if (value.isInstanceOf[Int]){
      new E_AnyVal( ~ value.asInstanceOf[Int])
    }
    else if (value.isInstanceOf[Long]){
      new E_AnyVal( ~ value.asInstanceOf[Long])
    }
    else if (value.isInstanceOf[Char]){
      new E_AnyVal( ~ value.asInstanceOf[Char])
    }
    else new E_AnyVal(-1)
  }

  def <<(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] << y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] << y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] << y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] << y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] << y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] << y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] << y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] << y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] << y_value)
      }
    } 
    else new E_AnyVal(-1)
  }

  def >>(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] >> y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] >> y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] >> y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] >> y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] >> y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] >> y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] >> y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] >> y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] >> y_value)
      }
    } 
    else new E_AnyVal(-1)
  }

  def >>>(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Int] >>> y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Int] >>> y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Int] >>> y_value)
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Long] >>> y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Long] >>> y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Long] >>> y_value)
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(value.asInstanceOf[Char] >>> y_value)
        case y_value:Long => new E_AnyVal(value.asInstanceOf[Char] >>> y_value)
        case y_value:Char => new E_AnyVal(value.asInstanceOf[Char] >>> y_value)
      }
    } 
    else new E_AnyVal(-1)
  }

  //pow and sqrt
  def sqrt() : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) this
    else if (value.isInstanceOf[Int])  new E_AnyVal(math.sqrt(value.asInstanceOf[Int]))
    else if (value.isInstanceOf[Long])  new E_AnyVal(math.sqrt(value.asInstanceOf[Long]))
    else if (value.isInstanceOf[Float])  new E_AnyVal(math.sqrt(value.asInstanceOf[Float]))
    else if (value.isInstanceOf[Double])  new E_AnyVal(math.sqrt(value.asInstanceOf[Double]))
    else if (value.isInstanceOf[Char])  new E_AnyVal(math.sqrt(value.asInstanceOf[Char]))
    else new E_AnyVal(-1)
  }

  def pow(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else if (this.isInstanceOf[Identity]) y
    else if (y.isInstanceOf[Identity]) this
    //For Int
    else if (value.isInstanceOf[Int]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Long => new E_AnyVal(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Float => new E_AnyVal(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Double => new E_AnyVal(math.pow(value.asInstanceOf[Int],y_value))
        case y_value:Char => new E_AnyVal(math.pow(value.asInstanceOf[Int],y_value))
      }
    }
    //For Long
    else if (value.isInstanceOf[Long]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Long => new E_AnyVal(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Float => new E_AnyVal(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Double => new E_AnyVal(math.pow(value.asInstanceOf[Long],y_value))
        case y_value:Char => new E_AnyVal(math.pow(value.asInstanceOf[Long],y_value))
      }
    }
    //For Float
    else if (value.isInstanceOf[Float]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Long => new E_AnyVal(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Float => new E_AnyVal(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Double => new E_AnyVal(math.pow(value.asInstanceOf[Float],y_value))
        case y_value:Char => new E_AnyVal(math.pow(value.asInstanceOf[Float],y_value))
      }
    }
    //For Double
    else if (value.isInstanceOf[Double]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Long => new E_AnyVal(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Float => new E_AnyVal(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Double => new E_AnyVal(math.pow(value.asInstanceOf[Double],y_value))
        case y_value:Char => new E_AnyVal(math.pow(value.asInstanceOf[Double],y_value))
      }
    }
    //For Char
    else if (value.isInstanceOf[Char]){
      var y_value:AnyVal = y.value
      y_value match{
        case y_value:Int => new E_AnyVal(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Long => new E_AnyVal(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Float => new E_AnyVal(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Double => new E_AnyVal(math.pow(value.asInstanceOf[Char],y_value))
        case y_value:Char => new E_AnyVal(math.pow(value.asInstanceOf[Char],y_value))
      }
    }
    else new E_AnyVal(-1)
  }

}


class NaN extends E_AnyVal{}

class Identity extends E_AnyVal{}


object printVal{

  def printVal(x: E_AnyVal){
    if (x.value.isInstanceOf[Int]) println("Int:"+x.value.asInstanceOf[Int])
    else if (x.value.isInstanceOf[Long]) println("Long:"+x.value.asInstanceOf[Long])
    else if (x.value.isInstanceOf[Float]) println("Float:"+x.value.asInstanceOf[Float])
    else if (x.value.isInstanceOf[Double]) println("Double:"+x.value.asInstanceOf[Double])
    else if (x.value.isInstanceOf[Char]) println("Char:"+x.value.asInstanceOf[Char])
    else if (x.value.isInstanceOf[Boolean]) println("Boolean:"+x.value.asInstanceOf[Boolean])
    else if (x.isInstanceOf[NaN]) println("NaN")
    else if (x.isInstanceOf[Identity]) println("Identity")
    else println("Error")
  }

}

