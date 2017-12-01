import scala.io.Source 
import scala.util.parsing.json.JSON
import scala.collection.mutable.ArrayBuffer

//package parser

class node(in_id:String, in_indegree:Int, in_outdegree:Int, in_node_type:Int, in_function:String){
  var id:String = in_id
  var indegree:Int = in_indegree
  var outdegree:Int = in_outdegree
  var node_type:Int = in_node_type
  var function:String = in_function
}

class edge(in_node_from:String, in_node_to:String, in_node_from_output:Int, in_node_to_input:Int, in_bandwidth:Int){
  var node_from:String = in_node_from
  var node_to:String = in_node_to
  var node_from_output:Int = in_node_from_output
  var node_to_input:Int = in_node_to_input
  var bandwidth:Int = in_bandwidth
}

class DFG(in_nodes:ArrayBuffer[node], in_edges:ArrayBuffer[edge]){
  var nodes:ArrayBuffer[node] = in_nodes
  var edges:ArrayBuffer[edge] = in_edges
}

class CC[T]{ 
  def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) 
}

object M extends CC[Map[String, Any]]
object L extends CC[List[Any]]
object S extends CC[String]
object D extends CC[Double]


object parser{

  def parser(fileNameNode: String, fileNameEdge: String): DFG = {

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
      S(id) = node("id")
      D(indegree) = node("indegree")
      D(outdegree) = node("outdegree")
      D(node_type) = node("node_type")
      S(function) = node("function")
    } yield {
      (id, indegree, outdegree, node_type, function)
    }

    val edgesList = for {
      Some(M(map)) <- List(JSON.parseFull(edgesStr))
      L(edges) = map("edges")
      M(edge) <- edges
      S(node_from) = edge("node_from")
      S(node_to) = edge("node_to")
      D(node_from_output) = edge("node_from_output")
      D(node_to_input) = edge("node_to_input")
      D(bandwidth) = edge("bandwidth")
    } yield {
      (node_from, node_to, node_from_output, node_to_input, bandwidth)
    }


    var E_nodesList = ArrayBuffer[node]()
    for (nodeTuple <- nodesList){
      var newNode = new node(nodeTuple._1, nodeTuple._2.toInt, nodeTuple._3.toInt, nodeTuple._4.toInt, nodeTuple._5)
      E_nodesList += newNode
    }

    var E_edgesList = ArrayBuffer[edge]()
    for (edgeTuple <- edgesList){
      var newEdge = new edge(edgeTuple._1, edgeTuple._2, edgeTuple._3.toInt, edgeTuple._4.toInt, edgeTuple._5.toInt)
      E_edgesList += newEdge
    }
    new DFG(E_nodesList,E_edgesList)
  
  }
}


object main{

  def main(args: Array[String]): Unit = {
    var res = parser.parser("nodes.json","edges.json")
    println(res.edges(0).node_from_output)
  }

}