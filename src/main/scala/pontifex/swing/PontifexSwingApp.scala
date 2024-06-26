package pontifex.swing

import pontifex.fast.{CodeMode, FastEncode}

import javax.swing.UIManager

object PontifexSwingApp extends App {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  private val frame = new PontifexFrame

  private val thread = new Thread(new Runnable {

    override def run(): Unit = {
      var count: Long = 0L
      var date: String = ""
      var pin: String = ""
      var mode: CodeMode = CodeMode.Unknown
      var inputText: String = ""
      while (true) {
        if (
          frame.getPin.nonEmpty && (mode != getCodeMode || pin != frame.getPin || date != frame.getDate ||
            inputText != frame.getInputText || count != frame.getCount)
        ) {
          mode = getCodeMode
          pin = frame.getPin
          date = frame.getDate
          inputText = frame.getInputText
          count = frame.getCount
          try {
            val workResult = FastEncode.doWork(inputText, date, count, mode, pin)
            frame.setResultText(workResult.result)
            frame.setControlText(workResult.control)
          } catch {
            case t: Throwable =>
              /*frame.setResultText(t.getClass.toString + ": " + t.getLocalizedMessage)
              frame.setControlText(t.getClass.toString + ": " + t.getLocalizedMessage)*/
              frame.setResultText("error")
              frame.setControlText("error")
          }
        }
        Thread.sleep(1000)
      }
    }
  })

  private def getCodeMode: CodeMode = {
    if (frame.getIsEncoding) CodeMode.Encode else CodeMode.Decode
  }

  thread.start()
}
