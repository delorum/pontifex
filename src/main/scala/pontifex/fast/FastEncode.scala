package pontifex.fast

import pontifex.PontifexCodec.loadFromConfigStream

import scala.annotation.tailrec
import scala.sys.process._

object FastEncode {
  private val (pontifex, _) = loadFromConfigStream(this.getClass.getResourceAsStream("/default2.conf"))

  def doWork(text: String, date: String, count: Long, mode: CodeMode, pin: String): WorkResult = {
    val keyBytes = readKeyBytes(pin)
    val keyPrefix = new String(keyBytes)
    val key = keyPrefix + date.replace("-", "") + (if (count > 1) count.toString else "")
    mode match {
      case CodeMode.Encode =>
        val preparedText = pad("." + text.filter(c => pontifex.containsOpen(c)) + ".")
        val encrypted = pontifex.encrypt(preparedText, key)
        val blocks = encrypted.length / 5
        val blocksDividedBy = if (blocks % 5 == 0) 5 else if (blocks % 6 == 0) 6 else 7
        val result = encrypted.grouped(5 * blocksDividedBy).map(s => s.grouped(5).mkString(" ")).mkString("\n")
        val control = pontifex.decrypt(encrypted, key)
        WorkResult(result, control)
      case CodeMode.Decode =>
        val result = pontifex.decrypt(text, key)
        val encrypted = pontifex.encrypt(result, key)
        val blocks = encrypted.length / 5
        val blocksDividedBy = if (blocks % 5 == 0) 5 else if (blocks % 6 == 0) 6 else 7
        val control = encrypted.grouped(5 * blocksDividedBy).map(s => s.grouped(5).mkString(" ")).mkString("\n")
        WorkResult(result, control)
    }
  }

  private def readKeyBytes(pin: String): Array[Byte] = {
    val processLogger = new ProcessLogger {
      override def out(s: => String): Unit = ()

      override def err(s: => String): Unit = ()

      override def buffer[T](f: => T): T = f
    }
    val str = List(
      "openssl",
      "enc",
      "-d",
      "-aes-192-cbc",
      "-pbkdf2",
      "-pass",
      s"pass:$pin",
      "-in",
      "/home/aborunov/Yandex.Disk/backup/fast"
    ).!!(processLogger)
    str.init.split(" ").map(_.toByte)
  }

  @tailrec private def pad(s: String, firstPartCount: Int = 0, lastPartCount: Int = 0): String = {
    def blocksCountDividedBy(by: Int): Boolean = {
      (s.length / 5) % by == 0
    }
    val dividedBy5 = s.length % 5 == 0
    val blocksCountOk = blocksCountDividedBy(5) || blocksCountDividedBy(6) || blocksCountDividedBy(7)
    val firstPartOk = firstPartCount >= 5
    val lastPartOk = lastPartCount >= 5
    if (dividedBy5 && blocksCountOk && firstPartOk && lastPartOk) s
    else {
      if (lastPartCount >= firstPartCount) {
        pad(pontifex.getRandomOpenSymbol.toString + s, firstPartCount + 1, lastPartCount)
      } else {
        pad(s + pontifex.getRandomOpenSymbol.toString, firstPartCount, lastPartCount + 1)
      }

    }
  }
}
