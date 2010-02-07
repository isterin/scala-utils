package com.cobrio.algorithms

import org.specs._
import com.cobrio.algorithms.{MurmurHash => MH}

/**
 * @author Ilya Sterin
 * @version 1.0       
 */

object MurmurHashSpec extends Specification {

  "MurmurHash is passed the same byte array and seed twice" should {
    "return the same hash" in {
      MH.hash("Some string".getBytes, 0) must_== MH.hash("Some string".getBytes, 0)
      MH.hash("Some other string".getBytes, 100) must_== MH.hash("Some other string".getBytes, 100)
    }
  }
  
  "MurmurHash is passed two different byte arrays and same seed" should {
    "return two different hashes" in {
      MH.hash("Some string".getBytes, 0) must_!= MH.hash("Some other string".getBytes, 0)
    }
  }

  "MurmurHash is passed two similar byte arrays and different seeds" should {
    "return two different hashes" in {
      MH.hash("Some string".getBytes, 0) must_!= MH.hash("Some string".getBytes, 100)
    }
  }

}