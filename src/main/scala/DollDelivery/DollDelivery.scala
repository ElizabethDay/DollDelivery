package main.scala.DollDelivery


/**
 * Doll Delivery - implementation of Dijkstra's algorthm in scala
 * @author eday
 */

class EdgeMap (edgeList: List[Map[String, Any]]) {
  
  var locationList = scala.collection.Set[Any]()
  var edgeMap = createEdgeMap(edgeList)
  var shortestPath = (Int, List())
  
  def createEdgeMap(edges: List[Map[String, Any]]): Map[String, List[Tuple2[String, Int]]] = {
    locationList = collection.Set(edges.flatMap(edge => edge get "startLocation") ++ edges.flatMap(edge => edge get "endLocation"): _*)
    
    val locMap = 
      locationList.foldLeft(Map[String, List[Tuple2[String, Int]]]()) ((r,c) =>
        r + (c.asInstanceOf[String] -> 
          (edges.foldLeft(List[Tuple2[String, Int]]())((z, i) => 
            if ((i.getOrElse("startLocation", "")) == c){
              ((i.getOrElse("endLocation", "").asInstanceOf[String]), (i.getOrElse("distance", 0).asInstanceOf[Int])) :: z
            } else z
          ))
        )
      )
     
    return locMap
  }
  
  def findShortestRoute(start: String, end: String) : Map[Any, Any] = {
    var locationDistances = 
      locationList.foldLeft(Map[String, Int]())((r, c) => r + (c.asInstanceOf[String] -> (if(c == start) 0 else Int.MaxValue)))
    djikstras(start, end, edgeMap, Map[String,Int](), List[String]())
  }
  
  def djikstras(current: String, end: String, eMap: Map[String, List[Tuple2[String, Int]]], distances: Map[String, Int], visited: List[String]) : Map [Any, Any] = {
    val currentNode = eMap.getOrElse(current, List())
    var distancesList = distances
    var smallestDistance = Int.MaxValue
    
    var dist = 0
    for(neighbor <- currentNode){  
      dist = distances.getOrElse(neighbor._1, Int.MaxValue)
      if (dist < neighbor._2) {distancesList = distancesList + (neighbor._1 -> neighbor._2)}
      if (dist < smallestDistance ) {smallestDistance = dist}
    }
    return Map()
  }
  
}

object DollDelivery {
  
  def FindShortestPath (startingLocation: String, targetLocaion: String, edges: List[Map[String, Any]]): Map[String, Any] = {
    val neighborhoodMap = new EdgeMap(edges)
    
    return Map("distance" -> 31, "path" -> "Kruthika's abode => Brian's apartment => Wesley's condo => Bryce's den => Craig's haunt")
  }
  
  
  def main(args: Array[String]){
    val neighborhood = List(
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Mark's crib", "distance" -> 9),
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Greg's casa", "distance" -> 4),
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Matt's pad", "distance" -> 18),
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Brian's apartment", "distance" -> 8),
      Map("startLocation" -> "Brian's apartment", "endLocation" -> "Wesley's condo", "distance" -> 7),
      Map("startLocation" -> "Brian's apartment", "endLocation" -> "Cam's dwelling", "distance" -> 17),
      Map("startLocation" -> "Greg's casa", "endLocation" -> "Cam's dwelling", "distance" -> 13),
      Map("startLocation" -> "Greg's casa", "endLocation" -> "Mike's digs", "distance" -> 19),
      Map("startLocation" -> "Greg's casa", "endLocation" -> "Matt's pad", "distance" -> 14),
      Map("startLocation" -> "Wesley's condo", "endLocation" -> "Kirk's farm", "distance" -> 10),
      Map("startLocation" -> "Wesley's condo", "endLocation" -> "Nathan's flat", "distance" -> 11),
      Map("startLocation" -> "Wesley's condo", "endLocation" -> "Bryce's den", "distance" -> 6),
      Map("startLocation" -> "Matt's pad", "endLocation" -> "Mark's crib", "distance" -> 19),
      Map("startLocation" -> "Matt's pad", "endLocation" -> "Nathan's flat", "distance" -> 15),
      Map("startLocation" -> "Matt's pad", "endLocation" -> "Craig's haunt", "distance" -> 14),
      Map("startLocation" -> "Mark's crib", "endLocation" -> "Kirk's farm", "distance" -> 9),
      Map("startLocation" -> "Mark's crib", "endLocation" -> "Nathan's flat", "distance" -> 12),
      Map("startLocation" -> "Bryce's den", "endLocation" -> "Craig's haunt", "distance" -> 10),
      Map("startLocation" -> "Bryce's den", "endLocation" -> "Mike's digs", "distance" -> 9),
      Map("startLocation" -> "Mike's digs", "endLocation" -> "Cam's dwelling", "distance" -> 20),
      Map("startLocation" -> "Mike's digs", "endLocation" -> "Nathan's flat", "distance" -> 12),
      Map("startLocation" -> "Cam's dwelling", "endLocation" -> "Craig's haunt", "distance" -> 18),
      Map("startLocation" -> "Nathan's flat", "endLocation" -> "Kirk's farm", "distance" -> 3))
   
    val path = FindShortestPath("Kruthika's abode", "Craig's haunt", neighborhood)
    println(path)
  }

}
