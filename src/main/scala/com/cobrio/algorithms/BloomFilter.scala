package com.cobrio.algorithms

import scala.collection.mutable.BitSet

class BloomFilter(capacity:Int, hashable:Hashable, hashCount:Int = 5) {

  private val buckets:BitSet = { new BitSet(capacity) }
  private val hashFunc = hashable.hashes(hashCount)(capacity) _

  def addValue(value:String) {
    hashFunc(value).foreach( buckets += _ )
  }

  def exists_?(value:String):Boolean = {
    for ( i <- hashFunc(value) ) if (!buckets.contains(i)) return false
    return true
  }
}

trait Hashable {
  def hashes(hashCount:Int)(max:Int)(value:String):Array[Int]
}

class MurmurHashable extends Hashable {
  import com.cobrio.algorithms.{MurmurHash => MH}
  def hashes(hashCount:Int)(max:Int)(value:String):Array[Int] = {
    val hash1 = MH.hash(value.getBytes, 0)
    val hash2 = MH.hash(value.getBytes, hash1)
    ( for ( i <- 0 until hashCount) yield Math.abs((hash1 + i * hash2) % max) ).toArray
  }
}

object Runner {
  def main(args: Array[String]) {
    val bloom = new BloomFilter(2000, new MurmurHashable())
    bloom.addValue("Ilya Sterin")
    bloom.addValue("Elijah Sterin")

    assert(bloom.exists_?("Ilya Sterin"))
    assert(bloom.exists_?("Elijah Sterin"))
    assert(!bloom.exists_?("Don't Exist"))
  }
}