
class E_AnyVal(in_value: AnyVal){
  var value:AnyVal = in_value

  //Arithmetic Operators

  def +(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
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
    else new E_AnyVal(value == y.value)
  }

  def !=(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
    else new E_AnyVal(value != y.value)
  }

  def >(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
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
    else if (value.isInstanceOf[Boolean]){
      new E_AnyVal( ! value.asInstanceOf[Boolean])
    }
    else new E_AnyVal(-1)
  }


  //Bitwise Operators

  def &(y: E_AnyVal) : E_AnyVal = {
    if (this.isInstanceOf[NaN]) new NaN
    else if (y.isInstanceOf[NaN]) new NaN
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


}



class NaN extends E_AnyVal{}




object main{

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


  def main(args: Array[String]): Unit = {
  
    var NaN = new NaN
    var a_Int:Int = 2
    var a_Long:Long = 5
    var a_Char:Char = '1'

    var b_Int:Int = 8
    var b_Double:Double = 8.88

    var c_Boolean:Boolean = true

    var a = new E_AnyVal(a_Char)
    var b = new E_AnyVal(b_Double)
    var c = new E_AnyVal(c_Boolean)
    var d = new E_AnyVal(a_Int)


    printVal(NaN % NaN)
    printVal(a ^ a)
    printVal(a >>> d)
    printVal(a <= b)
    printVal(c || c)
    printVal(!NaN)

  }
}