package pontifex.fast

import pontifex.fast.FastEncode.doWork

import javax.swing.{JOptionPane, JPasswordField}

object FastEncodeApp extends App {
  private val text = """тест""".stripMargin
  private val date = "31-05-2024"
  private val count = 1
  private val mode: CodeMode = CodeMode.Encode

  println(doWork(text, date, count, mode, enterPin()))

  private def enterPin(): String = {
    val pf = new JPasswordField()
    pf.addAncestorListener(new RequestFocusListener())
    val result =
      JOptionPane.showConfirmDialog(null, pf, "pin", JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE)
    if (result == JOptionPane.OK_OPTION) new String(pf.getPassword) else ""
  }
}
