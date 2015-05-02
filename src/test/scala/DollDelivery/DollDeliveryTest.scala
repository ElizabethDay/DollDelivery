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
    
    describe("when contains no proper route") {
      it("should display an error") {
        assert(true)
      }
    }
    
    describe("when has a sink node") {
      it("should not route through the sink node") {
        assert(true)
      }
    }
    
    describe("when has a source node") {
      it("should not route through the source node") {
        assert(true)
      }
    }
  }
}
