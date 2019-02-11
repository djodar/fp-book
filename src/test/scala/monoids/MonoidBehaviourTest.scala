package monoids

import org.scalatest.{FlatSpec, Matchers}

class MonoidBehaviourTest extends FlatSpec with Matchers {
  "A Monoid" should "count" in {
    Monoid.count("daniel is cool") should be(3)
  }
}
