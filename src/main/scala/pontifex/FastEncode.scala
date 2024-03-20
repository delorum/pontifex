package pontifex

import pontifex.PontifexCodec.loadFromConfigStream

import java.awt.Component
import javax.swing.event.{AncestorEvent, AncestorListener}
import javax.swing.{JComponent, JOptionPane, JPasswordField, SwingUtilities}
import scala.sys.process._
import scala.annotation.tailrec

object FastEncode extends App {
  sealed trait Mode
  case object Encode extends Mode
  case object Decode extends Mode

  private val text = ""
  private val date = "16-03-2024"
  private val count = 2
  private val mode: Mode = Encode
  // private val mode: Mode = Decode

  private val keyBytes = readKeyBytes

  private def enterPin(): String = {
    val pf = new JPasswordField()
    pf.addAncestorListener( new RequestFocusListener() )
    val result = JOptionPane.showConfirmDialog(null, pf, "pin", JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE)
    if (result == JOptionPane.OK_OPTION) new String(pf.getPassword) else ""
  }

  private def readKeyBytes: Array[Byte] = {
    val pin = enterPin()
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
    ).!!
    str.init.split(" ").map(_.toByte)
  }

  private val keyPrefix = new String(keyBytes)

  private val (pontifex, _) = loadFromConfigStream(this.getClass.getResourceAsStream("/default2.conf"))

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

  private val key = keyPrefix + date.replace("-", "") + (if (count > 1) count.toString else "")

  mode match {
    case Encode =>
      val preparedText = pad("." + text.filter(c => pontifex.containsOpen(c)) + ".")
      val encrypted = pontifex.encrypt(preparedText, key)
      val blocks = encrypted.length / 5
      val blocksDividedBy = if (blocks % 5 == 0) 5 else if (blocks % 6 == 0) 6 else 7
      println(encrypted.grouped(5 * blocksDividedBy).map(s => s.grouped(5).mkString(" ")).mkString("\n"))

    case Decode =>
      println(pontifex.decrypt(text, key))
  }

  class RequestFocusListener extends AncestorListener {

    def ancestorAdded(e: AncestorEvent) {
      val al: AncestorListener = this
      var result = false
      val component: JComponent = e.getComponent
      new Thread(new Runnable {
        override def run(): Unit = {
          while(!result) {
            result = component.requestFocusInWindow()
            if (!result) Thread.sleep(100)
          }
        }
      }).start()
      component.removeAncestorListener(al)
    }

    def ancestorMoved(e: AncestorEvent): Unit = {}

    def ancestorRemoved(e: AncestorEvent): Unit = {}
  }
}
