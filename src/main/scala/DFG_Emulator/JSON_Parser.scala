
package  DFG_Emulator

import scala.io.Source 
import scala.util.parsing.json.JSON
import scala.collection.mutable.ArrayBuffer

class edge(Source:Int, Destination:Int, Source_output_port:Int, Destination_input_port:Int){
  var source:Int = Source
  var destination:Int = Destination 
  var source_output_port:Int = Source_output_port 
  var destination_input_port:Int = Destination_input_port 
}

class arg_pack(id:Int, Input_port_length:Int, Output_port_length:Int, Input_port_width:Array[Int], Output_port_width:Array[Int], Register_length:Int, Register_width:Array[Int], Code: String, EdgesList:Array[edge], pu_type:Int, data_Source_Index:Array[Int], Output_port_delay:Array[Int]){
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
  var PU_type:Int = pu_type
  var Data_Source_Index:Array[Int] = data_Source_Index
  var output_port_delay:Array[Int] = Output_port_delay
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
    if(Str.length > 0) Str.split(",").map(x => x.toInt) 
    else new Array[Int](0)
  }

  def parser(fileNameNode: String, fileNameEdge: String): (Array[arg_pack],Array[arg_pack]) = {

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
      D(pu_type) = node("PU_type")
      S(data_Source_Index) = node("Data_Source_Index")
      S(output_port_delay) = node("output_port_delay")
    } yield {
      (id, input_port_length, output_port_length, input_port_width, output_port_width, register_length, register_width, code, pu_type, data_Source_Index, output_port_delay)
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

    
    var edgesArray = new Array[edge](edgesList.length)
    var i:Int = 0
    while (i < edgesList.length){
      edgesArray(i) = new edge(edgesList(i)._1.toInt, edgesList(i)._2.toInt, edgesList(i)._3.toInt, edgesList(i)._4.toInt)
      i+=1
    }

    var Arg_packs = new Array[arg_pack](nodesList.length)
    var source_counter:Int = 0
    var k:Int = 0
    while (k < nodesList.length) {
      if (nodesList(k)._9.toInt == 1) source_counter += 1
      var id:Int = nodesList(k)._1.toInt
      var counter:Int = 0
      var j:Int = 0
      for (Edge <- edgesArray){
        if (Edge.source == id) counter += 1
      }

      var EdgesList = new Array[edge](counter)
      for (Edge <- edgesArray){
        if (Edge.source == id) {EdgesList(j) =  Edge; j+=1}
      }
      Arg_packs(k) = new arg_pack(id, nodesList(k)._2.toInt, nodesList(k)._3.toInt, strToList(nodesList(k)._4), strToList(nodesList(k)._5), nodesList(k)._6.toInt, strToList(nodesList(k)._7), nodesList(k)._8, EdgesList, nodesList(k)._9.toInt, strToList(nodesList(k)._10), strToList(nodesList(k)._11))

      k+=1
    }

    var Source_arg_packs = new Array[arg_pack](source_counter)
    var n:Int = 0
    for (arg <- Arg_packs){
      if(arg.PU_type == 1){
        Source_arg_packs(n) = arg
        n += 1
      }
    }
    (Arg_packs, Source_arg_packs)
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
      else if(source(i) == 's'){
        if(source(i+1) == 'q' && source(i+2) == 'r' && source(i+3) == 't') {res += "sqrt";i+=4}
        else {res += "s";i+=1}
      }
      else if(source(i) == 'p'){
        if(source(i+1) == 'o' && source(i+2) == 'w') {res += "pow";i+=3}
        else {res += "p";i+=1}
      }
      else {res += source(i).toString;i+=1}
    }
    res
  }

  def calTime(code: ArrayBuffer[String]) : Int = {
    var res:Int = 0
    for (ele <- code) {
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
        case "sqrt" => res += 1;
        case "pow" => res += 1;
        case ele:String => res += 0;
      }
    }
    res
  } 
}


//object main{
//
//  def main(args: Array[String]): Unit = {
//    var res =  parser.parser("testcase/sequential_nodes.json","testcase/sequential_nodes.json")._1
//    var i:Int = 0
//    while (i < res.length){
//      println("ID:"+res(i).ID)
//      println("period:"+res(i).period)
//      println("input_port_length:"+res(i).input_port_length)
//      println("output_port_length:"+res(i).output_port_length)
//      println("input_port_width(0):"+res(i).input_port_width(0))
//      println("output_port_width(0):"+res(i).output_port_width(0))
//      println("register_length:"+res(i).register_length)
//      println("register_width(0):"+res(i).register_width(0))
//      println("code:"+res(i).code)
//      if(res(i).edges_list.length > 0) println("edges_list(0).destination:"+res(i).edges_list(0).destination)
//      else println("No edge start from this PU")
//      println("PU_type:"+res(i).PU_type)
//      if(res(i).PU_type == 1) println("Data_Source_Index(0):"+res(i).Data_Source_Index(0))
//      else println("This PU not get data from csv file")
//      println("---------------")
//      i+=1
//    }
//  }
//
//}

