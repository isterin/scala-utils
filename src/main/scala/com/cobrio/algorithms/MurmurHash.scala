package com.cobrio.algorithms
/**
 * This is a very fast, non-cryptographic hash suitable for general hash-based
 * lookup.  See http://murmurhash.googlepages.com/ for more details.
 * 
 * <p>The C version of MurmurHash 2.0 found at that site was ported
 * to Java by Andrzej Bialecki (ab at getopt org) and 
 * then ported to Scala by Ilya Sterin</p>
 */
object MurmurHash {  
  
  def hash(data:Array[Byte], seed:Int):Int = {
    val length = data.length
    val m:Int = 0x5bd1e995;
    val r:Int = 24;
 
    var h:Int = seed ^ length;
 
    val len_4:Int = length >> 2;
 
    for (i <- 0 until len_4) {
      val i_4:Int = i << 2;
      var k:Int = data(i_4 + 3);
      k = k << 8;
      k = k | (data(i_4 + 2) & 0xff);
      k = k << 8;
      k = k | (data(i_4 + 1) & 0xff);
      k = k << 8;
      k = k | (data(i_4 + 0) & 0xff);
      k *= m;
      k ^= k >>> r;
      k *= m;
      h *= m;
      h ^= k;
    }
 
    // avoid calculating modulo
    val len_m:Int = len_4 << 2;
    val left:Int = length - len_m;
 
    if (left != 0) {
      if (left >= 3) {
        h ^= (data(length - 3) << 16).asInstanceOf[Int];
      }
      if (left >= 2) {
        h ^= (data(length - 2) << 8).asInstanceOf[Int];
      }
      if (left >= 1) {
        h ^= data(length - 1).asInstanceOf[Int];
      }
 
      h *= m;
    }
 
    h ^= h >>> 13;
    h *= m;
    h ^= h >>> 15;
 
    return h;
  }
}