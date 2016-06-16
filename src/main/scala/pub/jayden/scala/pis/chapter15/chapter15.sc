def tupleDemo(expr:Any) =
  expr match {
    case (a:Integer, b:Integer, c:Integer) => "Integer three tuple"
    case (a:String, b:String, c:String) => "String three tuple"
    case _ => "Anything"
  }

def tupleDemo2(expr:Any) =
  expr match {
    case m: Tuple3[Integer,Integer, Integer] => "Integer three tuple"
    case (a:String, b:String, c:String) => "String three tuple"
    case _ => "Anything"
  }

def isIntIntMap(x:Any) =
  x match {
    case m:Map[Int, Int] => "Int Map"
    case _ => "this is not Int Map"
  }


tupleDemo((1,2,3))
tupleDemo(("aaa","bbb","ccc"))

isIntIntMap(Map( 1->1))
isIntIntMap(Map( "String"->"String"))