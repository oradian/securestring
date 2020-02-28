package de.choffmeister.securestring

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.{ByteBuffer, CharBuffer}
import java.security.{MessageDigest, SecureRandom}
import java.util.Random

class SecureString private(seed: Long, shaked: Array[Byte]) {
  private[this] var wiped = false

  def readBytes[T](inner: Array[Byte] => T): T = {
    if (wiped) {
      sys.error("This secret has already been wiped")
    }
    val bytes = SecureString.shake(seed, shaked)
    try {
      inner(bytes)
    } finally {
      SecureString.clear(bytes)
    }
  }
  def readChars[T](inner: Array[Char] => T): T = {
    if (wiped) {
      sys.error("This secret has already been wiped")
    }
    readBytes { bytes =>
      val chars = SecureString.decode(bytes)
      try {
        inner(chars)
      } finally {
        SecureString.clear(chars)
      }
    }
  }

  def wipe(): Unit = {
    SecureString.clear(shaked)
    wiped = true
  }

  val isEmpty: Boolean = shaked.isEmpty

  override def hashCode: Int = shaked.length
  override def equals(o: Any): Boolean = o match {
    case other: SecureString => (other eq this) ||
      readBytes { plain1 =>
        other.readBytes { plain2 =>
          MessageDigest.isEqual(plain1, plain2)
        }
      }
    case _ =>
      false
  }
}

object SecureString {
  /** Once a string is created, it may be interned into the JVM String pool.
   * There is no obvious way to wipe it other than ugly reflection hacks to destroy
   * the underlying supposedly immutable array of characters.
   *
   * Avoid this factory method as it was only created to enumerate wrong usages in one place */
  @deprecated("SecureString cannot consume Strings as they are immutable")
  def apply(string: String): SecureString =
    consume(string.toCharArray)

  def consume(bytes: Array[Byte]): SecureString = {
    val seed = random.nextLong()
    val shaked = shake(seed, bytes)
    clear(bytes)
    new SecureString(seed, shaked)
  }
  def consume(chars: Array[Char]): SecureString = {
    val bytes = encode(chars)
    val secure = consume(bytes)
    clear(chars)
    secure
  }

  private def shake(seed: Long, bytes: Array[Byte]): Array[Byte] = {
    val rnd = new Random(globalSeed ^ seed)
    val result = new Array[Byte](bytes.length)
    rnd.nextBytes(result)
    var i = 0
    while (i < result.length) {
      result(i) = (result(i) ^ bytes(i)).toByte
      i += 1
    }
    result
  }

  private def clear(bytes: Array[Byte]): Unit =
    java.util.Arrays.fill(bytes, 0: Byte)
  private def clear(chars: Array[Char]): Unit =
    java.util.Arrays.fill(chars, 0: Char)

  private def encode(chars: Array[Char]): Array[Byte] = {
    val wrapped = CharBuffer.wrap(chars)
    val encoded = UTF_8.encode(wrapped)
    val array = new Array[Byte](encoded.remaining)
    encoded.get(array)
    clear(encoded.array)
    array
  }
  private def decode(bytes: Array[Byte]): Array[Char] = {
    val wrapped = ByteBuffer.wrap(bytes)
    val decoded = UTF_8.decode(wrapped)
    val array = new Array[Char](decoded.remaining)
    decoded.get(array)
    clear(decoded.array)
    array
  }

  private[this] val random = new SecureRandom()

  /** Returns a seed based on the Java VM start time and the instantiation time of a singleton object. This seed is
   * (a) stable over the lifetime of the Java VM,
   * (b) not receivable from a memory dump and
   * (c) not trivial to receive from process information. */
  private[this] def seedStartTime: Long = ManagementFactory.getRuntimeMXBean.getStartTime
  private[this] val seedObject: Any = new Object()
  private[this] def globalSeed: Long = ((seedObject.hashCode.toLong << 32) | seedObject.hashCode) ^ seedStartTime
}
