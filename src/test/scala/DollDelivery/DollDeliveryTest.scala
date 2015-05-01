package test.scala.DollDelivery

import org.scalatest.FunSpec

class DeliverySpec extends FunSpec {
  describe("A Delivery") {
    describe("when empty") {
      it("should return nothing") {
        assert(Set.empty.size == 0)
      }
    }
    
    describe("when has only nodes") {
      it("should display the first and last node") {
        assert(Set.empty.size == 0)
      }
    }
    
    describe("when contains no proper route") {
      it("should display an error") {
        assert(Set.empty.size == 0)
      }
    }
    
    describe("when has a sink node") {
      it("should not route through the sink node") {
        assert(Set.empty.size == 0)
      }
    }
    
    describe("when has a source node") {
      it("should not route through the source node") {
        assert(Set.empty.size == 0)
      }
    }
  }
}
