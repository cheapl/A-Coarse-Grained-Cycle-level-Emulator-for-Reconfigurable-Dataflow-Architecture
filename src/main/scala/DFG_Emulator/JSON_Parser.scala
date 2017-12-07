
package  DFG_Emulator

import scala.io.Source
import scala.util.parsing.json.JSON
import scala.collection.mutable.ArrayBuffer

class CC[T]{ 
  def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) 
}

object M extends CC[Map[String, Any]]
object L extends CC[List[Any]]
object S extends CC[String]
object D extends CC[Double]


object DFG_Configuration_Parser{
  def strToList(Str:String) : Array[Int] = {
    if(Str.length > 0) Str.split(",").map(x => x.toInt) 
    else new Array[Int](0)
  }


  def parse(fileNameNode: String, fileNameEdge: String, fileNameConfig: String): (Array[PU_Arg_Pack],Array[PU_Arg_Pack]) = {
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
      D(accumulator_length) = node("accumulator_length")
      S(accumulator_width) = node("accumulator_width")
    } yield {
      (id, input_port_length, output_port_length, input_port_width, output_port_width, register_length, register_width, code, pu_type, data_Source_Index, output_port_delay, accumulator_length, accumulator_width)
    }

    val edges_list = for {
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

    
    var edgesArray = new Array[edge](edges_list.length)
    var i:Int = 0
    while (i < edges_list.length){
      edgesArray(i) = new edge(edges_list(i)._1.toInt, edges_list(i)._2.toInt, edges_list(i)._3.toInt, edges_list(i)._4.toInt)
      i+=1
    }

    var Arg_packs = new Array[PU_Arg_Pack](nodesList.length)
    var source_counter:Int = 0
    var k:Int = 0
    while (k < nodesList.length) {
      if (nodesList(k)._9.toInt == 1) source_counter += 1
      var id:Int = nodesList(k)._1.toInt
      var counter:Int = 0
      var j:Int = 0
      for (edg <- edgesArray){
        if (edg.source == id) counter += 1
      }
     
      var EdgesList = new Array[edge](counter)
      for (edg <- edgesArray){
        if (edg.source == id) {EdgesList(j) =  edg; j+=1}
      }
      Arg_packs(k) = new PU_Arg_Pack(id, nodesList(k)._2.toInt, nodesList(k)._3.toInt, strToList(nodesList(k)._4), strToList(nodesList(k)._5), nodesList(k)._6.toInt, strToList(nodesList(k)._7), nodesList(k)._8, EdgesList, nodesList(k)._9.toInt, strToList(nodesList(k)._10), strToList(nodesList(k)._11), nodesList(k)._12.toInt, strToList(nodesList(k)._13), fileNameConfig)

      k+=1
    }


    var Source_arg_packs = new Array[PU_Arg_Pack](source_counter)
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

			else if(source(i) == '-'){
			if(source(i-1) != '(') {res += "-";i+=1}
			else {i+=1}
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

  def calOperatorTime(fileNameConfig: String): Map[String,Int] = {
    var configStr:String = ""
    var file=Source.fromFile(fileNameConfig)
    for(line <- file.getLines) configStr = configStr + line
    file.close

    var configList = for {
      Some(M(map)) <- List(JSON.parseFull(configStr))
      L(operators) = map("operators")
      M(o) <- operators
      S(operator) = o("operator")
      D(period) = o("period")
    } yield {
      (operator, period)
    }

    var configMap:Map[String,Int] = Map()
    for (ele <- configList){
      configMap += (ele._1 -> ele._2.toInt)
    }

    configMap
  }

  def calTime(code: ArrayBuffer[String], fileNameConfig: String) : Int = {
    var configMap = calOperatorTime(fileNameConfig)
    //println(configMap.size)
    var length = configMap.size
    var resArray = new Array[Int](length)
    var operatorArray = new Array[String](length)
    resArray = resArray.map(x => 0)
    operatorArray = operatorArray.map(x => "none")

    var res:Int = 0
    for (ele <- code) {
      if(ele == "+") {resArray(0) += 1; operatorArray(0) = "+"}
      else if(ele == "-") {resArray(1) += 1; operatorArray(1) = "-"}
      else if(ele == "*") {resArray(2) += 1; operatorArray(2) = "*"}
      else if(ele == "/") {resArray(3) += 1; operatorArray(3) = "/"}
      else if(ele == "%") {resArray(4) += 1; operatorArray(4) = "%"}
      else if(ele == "==") {resArray(5) += 1; operatorArray(5) = "=="}
      else if(ele == "!=") {resArray(6) += 1; operatorArray(6) = "!="}
      else if(ele == ">") {resArray(7) += 1; operatorArray(7) = ">"}
      else if(ele == "<") {resArray(8) += 1; operatorArray(8) = "<"}
      else if(ele == ">=") {resArray(9) += 1; operatorArray(9) = ">="}
      else if(ele == "<=") {resArray(10) += 1; operatorArray(10) = "<="}
      else if(ele == "&&") {resArray(11) += 1; operatorArray(11) = "&&"}
      else if(ele == "||") {resArray(12) += 1; operatorArray(12) = "||"}
      else if(ele == "!") {resArray(13) += 1; operatorArray(13) = "!"}
      else if(ele == "&") {resArray(14) += 1; operatorArray(14) = "&"}
      else if(ele == "|") {resArray(15) += 1; operatorArray(15) = "|"}
      else if(ele == "^") {resArray(16) += 1; operatorArray(16) = "^"}
      else if(ele == "~") {resArray(17) += 1; operatorArray(17) = "~"}
      else if(ele == "<<") {resArray(18) += 1; operatorArray(18) = "<<"}
      else if(ele == ">>") {resArray(19) += 1; operatorArray(19) = ">>"}
      else if(ele == ">>>") {resArray(20) += 1; operatorArray(20) = ">>>"}
      else if(ele == "sqrt") {resArray(21) += 1; operatorArray(21) = "sqrt"}
      else if(ele == "pow") {resArray(22) += 1; operatorArray(22) = "pow"}
      else res += 0
    }

    var resList = resArray.toList
    var operatorList = operatorArray.toList
    var i:Int = 0

    while(i < length){
      if (resList(i) > 0){
        var current = math.ceil(resList(i).toDouble / configMap(operatorList(i)).toDouble)
        res += current.toInt
      }
      i += 1
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
//      i+=1
//    }
//  }
//
//}

