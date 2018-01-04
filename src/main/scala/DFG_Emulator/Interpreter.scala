import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import etype._

class PU(){
  //registers
  var registers_1 = Array[E_AnyVal](new E_AnyVal(0))
  var registers_2 = Array[E_AnyVal](new E_AnyVal(0))
  var registers = Array[Array[E_AnyVal]](registers_1,registers_2)
  //accumulators
	var accumulators = Array[E_AnyVal](new E_AnyVal(0))
  //input_data
  var input_data_1 = Array[E_AnyVal](new E_AnyVal(1))
  var input_data_2 = Array[E_AnyVal](new E_AnyVal(2))
	var input_data = Array[Array[E_AnyVal]](input_data_1,input_data_2)
  //output_data
	var output_data_1 = Array[E_AnyVal](new E_AnyVal(0))
  var output_data_2 = Array[E_AnyVal](new E_AnyVal(0))
  var output_data = Array[Array[E_AnyVal]](output_data_1,output_data_2)
}

object Interpreter{

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
        else {res += "=";i+=1}
      }
      else if(source(i) == '+'){
        if(source(i+1) == '=') {res += "+=";i+=2}
        else {res += "+";i+=1}
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
      else if(source(i) == 'i'){
        if(source(i+1) == 'f') {res += "if";i+=2}
        else {res += "i";i+=1}
      }
      else {res += source(i).toString;i+=1}
    }
    res
  }

  def bind(source:ArrayBuffer[String]) : ArrayBuffer[String] = {
    var res = new ArrayBuffer[String]()
    var new_res = new ArrayBuffer[String]()
    var operators = Array("(",")",",","{","}","+","-","*","/","=","+=","==","!=",">","<",">=","<=","&&","||","!","sqrt","pow","if",";")
    var i:Int = 0
    var flag:Boolean = false
    var literal:String = ""
    while (i < source.length){
      for (operator <- operators){
        if(source(i) == operator) {
          flag = true
        }
      }
      if (flag == true) {
        res += literal
        literal = ""
        res += source(i)
        flag = false
      }
      else if(i == source.length - 1){
        literal += source(i)
        res += literal
        literal = ""
        flag = false
      }
      else literal += source(i)
      i += 1
    }
    for (ele <- res){
      if (ele != "") new_res += ele
    }
    new_res
  }


  def replace(input:String, pu:PU) : E_AnyVal = {
    var res = new E_AnyVal(-1)
    if (input == "NaN") res = new NaN
    else if (input == "Identity") res = new Identity
    else if(input(0) == 'I'){
      var i:Int = 0
      var counter:Int = 2
      var flag:Boolean = false
      var idx1_str:String = ""
      var idx2_str:String = ""
      while (i < input.length){
        if(input(i) == '[') flag = true
        else if (input(i) == ']'){
          flag = false
          counter -= 1
        }
        if(flag == true && input(i) != '[' && input(i) != ']' && counter == 2){
          idx1_str += input(i)
        }
        if(flag == true && input(i) != '[' && input(i) != ']' && counter == 1){
          idx2_str += input(i)
        }
        i+=1
      }
      var idx1 = idx1_str.toInt
      var idx2 = idx2_str.toInt
      res = pu.input_data(idx1)(idx2)
    }
    else if(input(0) == 'O'){
      var i:Int = 0
      var counter:Int = 2
      var flag:Boolean = false
      var idx1_str:String = ""
      var idx2_str:String = ""
      while (i < input.length){
        if(input(i) == '[') flag = true
        else if (input(i) == ']'){
          flag = false
          counter -= 1
        }
        if(flag == true && input(i) != '[' && input(i) != ']' && counter == 2){
          idx1_str += input(i)
        }
        if(flag == true && input(i) != '[' && input(i) != ']' && counter == 1){
          idx2_str += input(i)
        }
        i+=1
      }
      var idx1 = idx1_str.toInt
      var idx2 = idx2_str.toInt
      res = pu.output_data(idx1)(idx2)
    }
    else if(input(0) == 'R'){
      var i:Int = 0
      var counter:Int = 2
      var flag:Boolean = false
      var idx1_str:String = ""
      var idx2_str:String = ""
      while (i < input.length){
        if(input(i) == '[') flag = true
        else if (input(i) == ']'){
          flag = false
          counter -= 1
        }
        if(flag == true && input(i) != '[' && input(i) != ']' && counter == 2){
          idx1_str += input(i)
        }
        if(flag == true && input(i) != '[' && input(i) != ']' && counter == 1){
          idx2_str += input(i)
        }
        i+=1
      }
      var idx1 = idx1_str.toInt
      var idx2 = idx2_str.toInt
      res = pu.registers(idx1)(idx2)
    }
    else if(input(0) == 'A'){
      var i:Int = 0
      var flag:Boolean = false
      var idx_str:String = ""
      while (i < input.length){
        if(input(i) == '[') flag = true
        else if (input(i) == ']'){
          flag = false
        }
        if(flag == true && input(i) != '[' && input(i) != ']'){
          idx_str += input(i)
        }
        i+=1
      }
      var idx = idx_str.toInt
      res = pu.accumulators(idx)
    }
    else if(input(0) == '$'){
      var num_str:String = ""
      var flag:Int = 1
      var j:Int = 1
      while(flag > 0){
        if (input(j) == ':') flag -= 1
        else{
          num_str += input(j)
        }
        j += 1
      }
      //println(j)
      //println(num_str)
      var type_str:String = ""
      flag = 1
      var k:Int = 0
      while(flag > 0){
        if (input(j+k) == '$') flag -= 1
        else{
          type_str += input(j+k)
        }
        k += 1
      }
      //println(j+k)
      //println(type_str)
      type_str match{
        case "Int" => res = new E_AnyVal(num_str.toInt)
        case "Long" => res = new E_AnyVal(num_str.toLong)
        case "Float" => res = new E_AnyVal(num_str.toFloat)
        case "Double" => res = new E_AnyVal(num_str.toDouble)
        case "Char" => res = new E_AnyVal(num_str(0))
        case "Boolean" => res = new E_AnyVal(num_str.toBoolean)
      }
    }
    //printVal.printVal(res)
    res
  }

  def calculator(equa:ArrayBuffer[String], pu:PU):E_AnyVal = {
    var bool_operators = Array("==","!=",">","<",">=","<=","&&","||","!")
    var num_operators = Array("+","-","*","/","sqrt","pow")
    var res = new E_AnyVal(0)
    var is_bool:Boolean = false

    for (ele <- equa){
      for (bool_operator <- bool_operators){
        if(ele == bool_operator) is_bool = true
      }
    }

    if(equa.length == 1){
      res = replace(equa(0),pu)
    }
    else if(is_bool){
      var operator_stack = new Stack[String]()
      var ele_stack = new Stack[String]()
      var i:Int = 0
      while (i < equa.length){
        var flag:Boolean = false
        for (bool_operator <- bool_operators){
          if(equa(i) == bool_operator) flag = true
        }

        if(flag) {
          operator_stack.push(equa(i))
          i+=1
        }
        else if(equa(i) == "("){
          var res = find_sub_equa(equa,i)
          var new_equa = res._1
          ele_stack.push(printVal.toStr(calculator(new_equa,pu)))
          i = res._2 + 1
        }
        else {
          ele_stack.push(equa(i))
          i+=1
        }
      }
      //println(equa)
      //println(operator_stack)
      //println(ele_stack)
      while(ele_stack.length > 0){
        var operand_1 = replace(ele_stack.pop(),pu)
        var operand_2 = replace(ele_stack.pop(),pu)
        var operator = operator_stack.pop()
        operator match{
          case "==" => res = operand_1 == operand_2
          case "!=" => res = operand_1 != operand_2
          case ">" => res = operand_1 > operand_2
          case "<" => res = operand_1 < operand_2
          case ">=" => res = operand_1 >= operand_2
          case "<=" => res = operand_1 <= operand_2
          case "&&" => res = operand_1 && operand_2
          case "||" => res = operand_1 || operand_2
        }
      }
    }
    else{
      //println(equa)
      var ele_stack = new Stack[String]()
      var i:Int = 0
      var top:Int = 0
      //push first ele into stack
      if(equa(i) == "("){
        var res = find_sub_equa(equa,i)
        var new_equa = res._1
        ele_stack.push(printVal.toStr(calculator(new_equa,pu)))
        i = res._2 + 1
      }
      else {
        ele_stack.push(equa(i))
        i += 1
      }
      //println(ele_stack)
      //from the second element to the last, push them to stack
      while (i < equa.length){
        if (equa(i) == "+"){
          i += 1
          if(equa(i) == "("){
            var res = find_sub_equa(equa,i)
            var new_equa = res._1
            ele_stack.push(printVal.toStr(calculator(new_equa,pu)))
            i = res._2 + 1
            top += 1
          }
          else {
            ele_stack.push(equa(i))
            i += 1
            top += 1
          }
        }
        else if (equa(i) == "-"){
          var coef = new E_AnyVal(-1)
          i += 1
          if(equa(i) == "("){
            var res = find_sub_equa(equa,i)
            var new_equa = res._1
            ele_stack.push(printVal.toStr(coef * calculator(new_equa,pu)))
            i = res._2 + 1
            top += 1
          }
          else {
            ele_stack.push(printVal.toStr(coef * replace(equa(i),pu)))
            i += 1
            top += 1
          }
        }
        else if (equa(i) == "*"){
          i += 1
          if(equa(i) == "("){
            var res = find_sub_equa(equa,i)
            var new_equa = res._1
            var temp = printVal.toStr( calculator(new_equa,pu) * replace(ele_stack(top),pu) )
            ele_stack(top) = temp
            i = res._2 + 1
          }
          else {
            var temp = printVal.toStr( replace(equa(i),pu) * replace(ele_stack(top),pu) )
            ele_stack(top) = temp
            i += 1
          }
        }
        else if (equa(i) == "/"){
          i += 1
          if(equa(i) == "("){
            var res = find_sub_equa(equa,i)
            var new_equa = res._1
            var temp = printVal.toStr( calculator(new_equa,pu) / replace(ele_stack(top),pu) )
            ele_stack(top) = temp
            i = res._2 + 1
          }
          else {
            var temp = printVal.toStr( replace(equa(i),pu) / replace(ele_stack(top),pu) )
            ele_stack(top) = temp
            i += 1
          }
        }
      }
      //println(ele_stack)

      for (ele_fin <- ele_stack){
        var temp = res + replace(ele_fin,pu)
        res = temp
      }

      res
    }
    
    res
  }

  def find_sub_equa(equa:ArrayBuffer[String], idx:Int):(ArrayBuffer[String],Int) = {
    var new_equa = new ArrayBuffer[String]()
    var counter:Int = 1
    var i = idx
    i += 1
    while(counter > 0){
      if(equa(i) == "(") {
        counter += 1
        new_equa += equa(i)
        i += 1
      }
      else if(equa(i) == ")") {
        counter -= 1
        if (counter > 0){
          new_equa += equa(i)
          i += 1
        }
      }
      else{
        new_equa += equa(i)
        i += 1
      }
    }
    (new_equa,i)
  }

  def replace_pow_sqrt(source:ArrayBuffer[String], pu:PU):ArrayBuffer[String] = {
    var res = new ArrayBuffer[String]()
    var i:Int = 0

    while (i < source.length){
      if(source(i) == "pow") {
        var ele1 = replace(source(i+2),pu)
        var ele2 = replace(source(i+4),pu)
        var rp = printVal.toStr(ele1.pow(ele2))
        res += rp
        i += 6
      }
      else if (source(i) == "sqrt"){
        var ele = replace(source(i+2),pu)
        var rp = printVal.toStr(ele.sqrt())
        res += rp
        i += 4
      }
      else{
        res += source(i)
        i += 1
      }
    }
    res
  }


  def replace_if_else(source:ArrayBuffer[String], pu:PU):ArrayBuffer[String] = {
    var res = new ArrayBuffer[String]()
    var equa = new ArrayBuffer[String]()
    var i:Int = 0
    var flag:Int = 1
    var enable:Boolean = true

    while (i < source.length){
      if(source(i) == "if") {
        i += 2
        while(flag > 0){
          if(source(i) == "(") {
            flag += 1
            equa += source(i)
            i += 1
          }
          else if(source(i) == ")") {
            flag -= 1
            if (flag > 0){
              equa += source(i)
              i += 1
            }
          }
          else{
            equa += source(i)
            i += 1
          }
        }
        //println(printVal.toStr(calculator(equa,pu)))
        if (printVal.toStr(calculator(equa,pu)) == "$true:Boolean$"){
          i += 2
          while(source(i) != "}"){
            res += source(i)
            i += 1
          }
          enable = false
        }
        else{
          while(source(i) != "else") i += 1
          i += 2
          while(source(i) != "}"){
            res += source(i)
            i += 1
          }
          enable = false
        }
      }
      else {
        if(enable) res += source(i)
        i += 1
      }
  
    }
    res
  }

  def invoke(code:String, pu:PU){
    var code_split =  bind(bindOperators(strToArray(code)))
    //println(code_split)
    var code_split_ps = replace_pow_sqrt(code_split,pu)
    //println(code_split_ps)
    var code_split_ps_if = replace_if_else(code_split_ps,pu)
    var code_fin = code_split_ps_if
    //println(code_fin)
    var codes = new ArrayBuffer[ArrayBuffer[String]]()
    codes += new ArrayBuffer[String]()
    var i:Int = 0
    for(ele <- code_fin){
      if (ele == ";"){
        i += 1
        codes += new ArrayBuffer[String]()
      }
      else codes(i) += ele
    }
    for (line <- codes){
      var equa = ArrayBuffer[String]()
      var j:Int = 2
      while (j < line.length){
        equa += line(j)
        j += 1
      }
      //println(equa)
      if (line(0)(0) == 'I') println("You can not change input!")
      else if (line(0)(0) == 'O'){
        var i:Int = 0
        var counter:Int = 2
        var flag:Boolean = false
        var idx1_str:String = ""
        var idx2_str:String = ""
        while (i < line(0).length){
          if(line(0)(i) == '[') flag = true
          else if (line(0)(i) == ']'){
            flag = false
            counter -= 1
          }
          if(flag == true && line(0)(i) != '[' && line(0)(i) != ']' && counter == 2){
            idx1_str += line(0)(i)
          }
          if(flag == true && line(0)(i) != '[' && line(0)(i) != ']' && counter == 1){
            idx2_str += line(0)(i)
          }
          i+=1
        }
        var idx1 = idx1_str.toInt
        var idx2 = idx2_str.toInt
        //printVal.printVal(calculator(equa,pu))
        pu.output_data(idx1)(idx2) = calculator(equa,pu)
      }
      else if (line(0)(0) == 'R'){
        var i:Int = 0
        var counter:Int = 2
        var flag:Boolean = false
        var idx1_str:String = ""
        var idx2_str:String = ""
        while (i < line(0).length){
          if(line(0)(i) == '[') flag = true
          else if (line(0)(i) == ']'){
            flag = false
            counter -= 1
          }
          if(flag == true && line(0)(i) != '[' && line(0)(i) != ']' && counter == 2){
            idx1_str += line(0)(i)
          }
          if(flag == true && line(0)(i) != '[' && line(0)(i) != ']' && counter == 1){
            idx2_str += line(0)(i)
          }
          i+=1
        }
        var idx1 = idx1_str.toInt
        var idx2 = idx2_str.toInt
        pu.registers(idx1)(idx2) = calculator(equa,pu)
      }
      else if (line(0)(0) == 'A'){
        var i:Int = 0
        var flag:Boolean = false
        var idx_str:String = ""
        while (i < line(0).length){
          if(line(0)(i) == '[') flag = true
          else if (line(0)(i) == ']'){
            flag = false
          }
          if(flag == true && line(0)(i) != '[' && line(0)(i) != ']'){
            idx_str += line(0)(i)
          }
          i+=1
        }
        var idx = idx_str.toInt
        if (line(1) == "=") pu.accumulators(idx) = calculator(equa,pu)
        else if (line(1) == "+=") pu.accumulators(idx) = pu.accumulators(idx) + calculator(equa,pu)
      }
    }

  }


}


object main{

  def main(args: Array[String]): Unit = {
    //var code:String = "O[0][0] = (pow(I[0][0],$2:Int$) + sqrt(I[1][0])) * $2:Int$; if ( (O[0][0] == I[0][0]) && (O[0][0] == I[1][0]) ) {O[1][0] = Identity} else {O[1][0] = NaN}"
    //var code:String = "O[0][0] = I[0][0] + $1:Int$"
    var code:String = "A[0] += I[0][0] * I[1][0]; O[0][0] = I[0][0]; O[1][0] = I[1][0]"
    var PU_test:PU = new PU
    Interpreter.invoke(code,PU_test)
    printVal.printVal(PU_test.input_data(0)(0))
    printVal.printVal(PU_test.input_data(1)(0))
    printVal.printVal(PU_test.output_data(0)(0))
    printVal.printVal(PU_test.output_data(1)(0))
    printVal.printVal(PU_test.accumulators(0))
  }

}