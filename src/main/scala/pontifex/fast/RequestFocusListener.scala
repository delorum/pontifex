package pontifex.fast

import javax.swing.JComponent
import javax.swing.event.{AncestorEvent, AncestorListener}

class RequestFocusListener extends AncestorListener {

  def ancestorAdded(e: AncestorEvent) {
    val al: AncestorListener = this
    var result = false
    val component: JComponent = e.getComponent
    new Thread(new Runnable {
      override def run(): Unit = {
        while (!result) {
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
