package test.scala.DollDelivery

import org.scalatest.FunSpec
import main.scala.DollDelivery.DollDelivery

class DeliverySpec extends FunSpec {

  describe("A Delivery") {
    
    describe("when has only two linked nodes") {
      it("should display the first and last node") {
        assertResult(Map("distance" -> 2, "path" -> "a => b")){
          DollDelivery.FindShortestPath("a", "b", List(Map("startLocation" -> "a", "endLocation" -> "b", "distance" -> 2)))
        }
      }
    }
    
    describe("when uses nodes it does not contain") {
      it("should display 'Location does not exist'") {
        assertResult(Map("distance" -> 0, "path" -> "Location does not exist")){
          DollDelivery.FindShortestPath("a", "c", List(Map("startLocation" -> "a", "endLocation" -> "b", "distance" -> 2)))
        }
        assertResult(Map("distance" -> 0, "path" -> "Location does not exist")){
          DollDelivery.FindShortestPath("a", "c", List(Map("startLocation" -> "b", "endLocation" -> "c", "distance" -> 2)))
        }
      }
    }    
    describe("when contains no proper route") {
      it("should display no path") {
        assertResult(Map("distance" -> 0, "path" -> "")){
          DollDelivery.FindShortestPath("a", "e", List(Map("startLocation" -> "a", "endLocation" -> "b", "distance" -> 2),
                                                       Map("startLocation" -> "b", "endLocation" -> "c", "distance" -> 2),
                                                       Map("startLocation" -> "d", "endLocation" -> "e", "distance" -> 2)))
        }
      }
    }
    
    describe("when has a sink node") {
      it("should not route through the sink node") {
        assertResult(Map("distance" -> 15, "path" -> "a => c => d => e")){
          DollDelivery.FindShortestPath("a", "e", List(Map("startLocation" -> "a", "endLocation" -> "b", "distance" -> 1),
                                                       Map("startLocation" -> "a", "endLocation" -> "c", "distance" -> 5),
                                                       Map("startLocation" -> "c", "endLocation" -> "d", "distance" -> 5),
                                                       Map("startLocation" -> "d", "endLocation" -> "e", "distance" -> 5)))
        }
      }
    }
    
    describe("when has a source node") {
      it("should not route through the source node") {
        assertResult(Map("distance" -> 15, "path" -> "a => b => d => e")){
          DollDelivery.FindShortestPath("a", "e", List(Map("startLocation" -> "a", "endLocation" -> "b", "distance" -> 5),
                                                       Map("startLocation" -> "b", "endLocation" -> "d", "distance" -> 5),
                                                       Map("startLocation" -> "c", "endLocation" -> "d", "distance" -> 1),
                                                       Map("startLocation" -> "c", "endLocation" -> "e", "distance" -> 1),
                                                       Map("startLocation" -> "d", "endLocation" -> "e", "distance" -> 5)))
        }
      }
    }
    
    describe("when an early node has a short route") {
      it("should find the shorter route that does not include that node") {
        assertResult(Map("distance" -> 20, "path" -> "a => d => c => f")){
          DollDelivery.FindShortestPath("a", "f", List(Map("startLocation" -> "a", "endLocation" -> "b", "distance" -> 7),
                                                       Map("startLocation" -> "a", "endLocation" -> "c", "distance" -> 14),
                                                       Map("startLocation" -> "a", "endLocation" -> "d", "distance" -> 9),
                                                       Map("startLocation" -> "b", "endLocation" -> "d", "distance" -> 10),
                                                       Map("startLocation" -> "b", "endLocation" -> "e", "distance" -> 15),
                                                       Map("startLocation" -> "c", "endLocation" -> "f", "distance" -> 9),
                                                       Map("startLocation" -> "d", "endLocation" -> "c", "distance" -> 2),
                                                       Map("startLocation" -> "d", "endLocation" -> "e", "distance" -> 11),
                                                       Map("startLocation" -> "e", "endLocation" -> "f", "distance" -> 6)))
        }
      }
    }
  }
}
