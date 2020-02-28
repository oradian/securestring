package de.choffmeister.securestring

class SecureStringSpec extends org.specs2.mutable.Specification {
  "clear input arrays" in {
    val chars = "test".toCharArray
    chars.mkString ==== "test"
    val ss1 = SecureString.consume(chars)
    chars.mkString ==== "\u0000" * 4
    ss1.readChars(_.mkString ==== "test")

    val bytes = "test2".getBytes("UTF-8")
    val ss2 = SecureString.consume(bytes)
    bytes.toList ==== List[Byte](0, 0, 0, 0, 0)
    ss2.readChars(_.mkString ==== "test2")
  }

  "detect equality" in {
    val ss1 = SecureString.consume("test".toCharArray)
    val ss2 = SecureString.consume("test".toCharArray)
    val ss3 = SecureString.consume("test1".toCharArray)
    val ss4 = SecureString.consume("test2".toCharArray)

    ss1 ==== ss2
    ss1 !=== ss3
    ss3 !=== ss4
    ss1 !== new Object()
    ss1 !== 1
    ss1 !== "test"
  }

  "allow temporary reading" in {
    val ss1 = SecureString.consume("test1".toCharArray)
    val ss2 = SecureString.consume("test2".toCharArray)

    ss1.readChars(_ ==== "test1".toCharArray)
    ss2.readChars(_ ==== "test2".toCharArray)
    ss1.readBytes(_ ==== "test1".getBytes("UTF-8"))
    ss2.readBytes(_ ==== "test2".getBytes("UTF-8"))
  }

  "wipe" in {
    val ss = SecureString.consume("password".toCharArray)
    ss.readChars(_.mkString ==== "password")
    ss.readBytes(_.toList ==== "password".getBytes("UTF-8").toList)
    ss.wipe()
    ss.readChars(_.mkString !=== "password") must throwA
    ss.readBytes(_.toList != "password".getBytes("UTF-8").toList) must throwA
  }
}
