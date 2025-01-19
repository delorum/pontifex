package pontifex.fast

import pontifex.utils.PontifexUtils

import scala.annotation.tailrec
import scala.sys.process._

object FastEncode {
  private val (pontifex, _) = PontifexUtils.loadFromConfigStream(this.getClass.getResourceAsStream("/default2.conf"))

  def containsOpen(c: Char): Boolean = pontifex.containsOpen(c)

  def doWork(text: String, date: String, count: Long, mode: CodeMode, pin: String): WorkResult = {
    val keyBytes = readKeyBytes(pin)
    val keyPrefix = new String(keyBytes)
    val key = keyPrefix + date.replace("-", "") + (if (count > 1) count.toString else "")
    mode match {
      case CodeMode.Encode =>
        val (preparedText, blocksDividedBy) = pad("." + text.filter(c => pontifex.containsOpen(c)) + ".")
        val encrypted = pontifex.encrypt(preparedText, key)
        val result = encrypted.grouped(5 * blocksDividedBy).map(s => s.grouped(5).mkString(" ")).mkString(" ")
        val control = pontifex.decrypt(encrypted, key)
        WorkResult(result, control)
      case CodeMode.Decode =>
        val result = pontifex.decrypt(text, key)
        val encrypted = pontifex.encrypt(result, key)
        val blocks = encrypted.length / 5
        val blocksDividedBy = (21 to 1 by -1).find(by => blocks % by == 0).getOrElse(blocks)
        val control = encrypted.grouped(5 * blocksDividedBy).map(s => s.grouped(5).mkString(" ")).mkString(" ")
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

  private def pad(s: String, firstPartCount: Int = 0, lastPartCount: Int = 0): (String, Int) = {
    val blocksCount = s.length / 5

    def blocksCountDividedBy(by: Int): Boolean = {
      blocksCount % by == 0
    }
    val dividedBy5 = s.length % 5 == 0
    val firstPartOk = firstPartCount >= 5
    val lastPartOk = lastPartCount >= 5

    def continue = {
      if (lastPartCount >= firstPartCount) {
        pad(pontifex.getRandomOpenSymbol.toString + s, firstPartCount + 1, lastPartCount)
      } else {
        pad(s + pontifex.getRandomOpenSymbol.toString, firstPartCount, lastPartCount + 1)
      }
    }

    if (dividedBy5 && firstPartOk && lastPartOk) {
      if (blocksCount < 22) (s, blocksCount)
      else if (blocksCountDividedBy(22)) (s, 22)
      else continue
    } else continue
  }
}
