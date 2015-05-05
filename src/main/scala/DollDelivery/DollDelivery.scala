package main.scala.DollDelivery

/**
 * Doll Delivery - implementation of Dijkstra's algorthm in scala
 * @author eday
 */

class EdgeMap(edgeList: List[Map[String, Any]]) {

  type Path[Key] = (Int, List[Key])

  var locationList = createLocationList(edgeList)
  var edgeMap = createEdgeMap(edgeList)
  var shortestPath = (Int, List())

  def createLocationList(edges: List[Map[String, Any]]): scala.collection.Set[String] = {
    collection.Set(edges.map(edge => (edge.getOrElse("startLocation", "")).asInstanceOf[String]) ++ edges.map(edge => (edge.getOrElse("endLocation", "")).asInstanceOf[String]): _*)
  }

  def createEdgeMap(edges: List[Map[String, Any]]): Map[String, List[Tuple2[Int, String]]] = {
    locationList.foldLeft(Map[String, List[Tuple2[Int, String]]]())((r, c) =>
      r + (c.asInstanceOf[String] ->
        (edges.foldLeft(List[Tuple2[Int, String]]())((z, i) =>
          if ((i.getOrElse("startLocation", "")) == c) {
            ( (i.getOrElse("distance", 0).asInstanceOf[Int]), (i.getOrElse("endLocation", "").asInstanceOf[String])) :: z
          } else z))))
  }

  def findShortestRoute(start: String, end: String): (Int, List[String]) = {
    if (!(locationList.contains(start) && locationList.contains(end))) { return (0, List("Location does not exist")) }

    var locationDistances =
      locationList.foldLeft(Map[String, Int]())((r, c) => r + (c.asInstanceOf[String] -> (if (c == start) 0 else Int.MaxValue)))

    dijkstra[String](edgeMap, List((0, List(start))), end, Set())
  }

  def dijkstra[Key](edges: Map[Key, List[(Int, Key)]], trace: List[Path[Key]], end: Key, visited: Set[Key]): Path[Key] = trace match {
    case (distance, path) :: trace_remaining => path match {
      case key :: paths_remaining =>
        if (key == end) (distance, path.reverse)
        else {
          val paths = edges(key).flatMap {
            case (dist, key) => if (!visited.contains(key)) List((distance + dist, key :: path)) else Nil }
          val sorted = (paths ++ trace_remaining).sortWith {
            case ((dist1, _), (dist2, _)) => dist1 < dist2 }
          dijkstra(edges, sorted, end, visited + key)
        }
    }
    case Nil => (0, List())
  }
}

object DollDelivery {

  def FindShortestPath(startingLocation: String, targetLocation: String, edges: List[Map[String, Any]]): Map[Any, Any] = {
    val neighborhoodMap = new EdgeMap(edges)

    val finalPath = neighborhoodMap.findShortestRoute(startingLocation, targetLocation)
    
    Map("distance" -> finalPath._1, "path" -> finalPath._2.mkString(" => "))
  }

  def main(args: Array[String]) {
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
