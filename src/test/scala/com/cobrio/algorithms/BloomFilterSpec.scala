package com.cobrio.algorithms

import org.specs._
import com.cobrio.algorithms.{MurmurHash => MH}

/**
 * @author Ilya Sterin
 * @version 1.0       
 */

object BloomFilterSpec extends Specification {

  "Values are added to BloomFilter" should {
    "BloomFilter must find these values" in {
      val filter = createBloomFilter(10000)
      filter.addValue("Ilya Sterin")
      filter.addValue("Elijah Sterin")

      filter.exists_?("Ilya Sterin") must beTrue
      filter.exists_?("Elijah Sterin") must beTrue
    }
  }

  "Values are added to BloomFilter" should {
    "BloomFilter must not find values which haven't been added" in {
      val filter = createBloomFilter(10000)
      filter.addValue("Ilya Sterin")
      filter.addValue("Elijah Sterin")

      filter.exists_?("Shouldn't find") must beFalse
    }
  }

  private def createBloomFilter(capacity:Int):BloomFilter = {
    new BloomFilter(1000, new MurmurHashable())
  }

}