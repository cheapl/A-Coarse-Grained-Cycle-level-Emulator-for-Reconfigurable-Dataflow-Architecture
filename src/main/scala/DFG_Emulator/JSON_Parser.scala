import scala.io.Source 
import scala.util.parsing.json.JSON
import scala.collection.mutable.ArrayBuffer

//package parser

class edge(Source:Int, Destination:Int, Source_output_port:Int, Destination_input_port:Int){
  var source:Int = Source
  var destination:Int = Destination 
  var source_output_port:Int = Source_output_port 
  var destination_input_port:Int = Destination_input_port 
}

class arg_pack(id:Int, Input_port_length:Int, Output_port_length:Int, Input_port_width:Array[Int], Output_port_width:Array[Int], Register_length:Int, Register_width:Array[Int], Code: String, EdgesList:Array[edge]){
  var ID:Int = id 
  var period:Int = calTime.calTime(calTime.bindOperators(calTime.strToArray(Code)))
  var input_port_length:Int = Input_port_length 
  var output_port_length:Int = Output_port_length 
  var input_port_width:Array[Int] = Input_port_width
  var output_port_width:Array[Int] = Output_port_width
  var register_length:Int = Register_length
  var register_width:Array[Int] = Register_width
  var code:String = Code
  var edgesList:Array[edge] = EdgesList
}


class CC[T]{ 
  def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) 
}

object M extends CC[Map[String, Any]]
object L extends CC[List[Any]]
object S extends CC[String]
object D extends CC[Double]


object parser{

  def strToList(Str:String) : Array[Int] = {
    Str.split(",").map(x => x.toInt) 
  }

  def parser(fileNameNode: String, fileNameEdge: String): Array[arg_pack] = {

    var nodesStr:String = ""
    var edgesStr:String = ""

    val file1=Source.fromFile(fileNameNode)
    for(line <- file1.getLines) nodesStr = nodesStr + line
    file1.close

    val file2=Source.fromFile(fileNameEdge)
    for(line <- file2.getLines) edgesStr = edgesStr + line
    file2.close

    val nodesList = for {
      Some(M(map)) <- List(JSON.parseFull(nodesStr))
      L(nodes) = map("nodes")
      M(node) <- nodes
      D(id) = node("ID")
      D(input_port_length) = node("input_port_length")
      D(output_port_length) = node("output_port_length")
      S(input_port_width) = node("input_port_width")
      S(output_port_width) = node("output_port_width")
      D(register_length) = node("register_length")
      S(register_width) = node("register_width")
      S(code) = node("code")
    } yield {
      (id, input_port_length, output_port_length, input_port_width, output_port_width, register_length, register_width, code)
    }

    val edgesList = for {
      Some(M(map)) <- List(JSON.parseFull(edgesStr))
      L(edges) = map("edges")
      M(edge) <- edges
      D(source) = edge("source")
      D(destination) = edge("destination")
      D(source_output_port) = edge("source_output_port")
      D(destination_input_port) = edge("destination_input_port")
    } yield {
      (source, destination, source_output_port, destination_input_port)
    }


    //println(nodesList.length)
    //println(edgesList.length)
    
    var edgesArray = new Array[edge](edgesList.length)
    var i:Int = 0
    while (i < edgesList.length){
      edgesArray(i) = new edge(edgesList(i)._1.toInt, edgesList(i)._2.toInt, edgesList(i)._3.toInt, edgesList(i)._4.toInt)
      i+=1
    }

    var Arg_packs = new Array[arg_pack](nodesList.length)
    var k:Int = 0
    while (k < nodesList.length) {
      var id:Int = nodesList(k)._1.toInt
      var counter:Int = 0
      var j:Int = 0
      for (Edge <- edgesArray){
        if (Edge.source == id) counter += 1
      }
      //println(counter)
      var EdgesList = new Array[edge](counter)
      for (Edge <- edgesArray){
        if (Edge.source == id) {EdgesList(j) =  Edge; j+=1}
      }
      Arg_packs(k) = new arg_pack(id, nodesList(k)._2.toInt, nodesList(k)._3.toInt, strToList(nodesList(k)._4), strToList(nodesList(k)._5), nodesList(k)._6.toInt, strToList(nodesList(k)._7), nodesList(k)._8, EdgesList)

      k+=1
    }

    Arg_packs
  
  }

}


object calTime{

  def strToArray(source:String) : ArrayBuffer[Char] = {
    var res = new ArrayBuffer[Char]()
    for (char <- source){
      if (char != ' ') res += char
    }
    res
  }

  def bindOperators(source:ArrayBuffer[Char]) : ArrayBuffer[String] = {
    var res = new ArrayBuffer[String]()
    var i:Int = 0
    while (i < source.length){
      //println(i)
      if(source(i) == '='){
        if(source(i+1) == '=') {res += "==";i+=2}
        else i+=1
      }
      else if(source(i) == '!'){
        if(source(i+1) == '=') {res += "!=";i+=2}
        else {res += "!";i+=1}
      }
      else if(source(i) == '>'){
        if(source(i+1) == '=') res += ">="
        else if(source(i+1) == '>' && source(i+2) != '>') {res += ">>";i+=2}
        else if(source(i+1) == '>' && source(i+2) == '>') {res += ">>>";i+=3}
        else {res += ">";i+=1}
      }
      else if(source(i) == '<'){
        if(source(i+1) == '=') {res += "<=";i+2}
        else if(source(i+1) == '<') {res += "<<";i+=2}
        else {res += "<";i+=1}
      }
      else if(source(i) == '&'){
        if(source(i+1) == '&') {res += "&&";i+=2}
        else {res += "&";i+=1}
      }
      else if(source(i) == '|'){
        if(source(i+1) == '|') {res += "||";i+=2}
        else {res += "|";i+=1}
      }
      else {res += source(i).toString;i+=1}
    }
    res
  }

  def calTime(code: ArrayBuffer[String]) : Int = {
    var res:Int = 0
    for (ele <- code) {
      //println(ele)
      ele match{
        case "+" => res += 1;
        case "-" => res += 1;
        case "*" => res += 1;
        case "/" => res += 1;
        case "%" => res += 1;
        case "==" => res += 1;
        case "!=" => res += 1;
        case ">" => res += 1;
        case "<" => res += 1;
        case ">=" => res += 1;
        case "<=" => res += 1;
        case "&&" => res += 1;
        case "||" => res += 1;
        case "!" => res += 1;
        case "&" => res += 1;
        case "|" => res += 1;
        case "^" => res += 1;
        case "~" => res += 1;
        case "<<" => res += 1;
        case ">>" => res += 1;
        case ">>>" => res += 1;
        case ele:String => res += 0;
      }
    }
    res
  } 

}


object main{

  def main(args: Array[String]): Unit = {
    var res =  parser.parser("testcase/sequential_nodes.json","testcase/sequential_edges.json")
    println("ID:"+res(0).ID)
    println("period:"+res(0).period)
    println("input_port_length:"+res(0).input_port_length)
    println("output_port_length:"+res(0).output_port_length)
    println("input_port_width(0):"+res(0).input_port_width(0))
    println("output_port_width(0):"+res(0).output_port_width(0))
    println("register_length:"+res(0).register_length)
    println("register_width(0):"+res(0).register_width(0))
    println("code:"+res(0).code)
    println("edgesList(0).destination:"+res(0).edgesList(0).destination)
  }

}