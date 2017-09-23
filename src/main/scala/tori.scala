import javax.swing.{JPanel, JFrame}
import java.awt.BorderLayout

object Tori {
  def main(args: Array[String]) :Unit = {
    println("Hello World")

    val frame : MainFrame = new MainFrame()
  }
}

class MainFrame() extends JFrame{
  val panel = new JPanel()
  val contentPane = this.getContentPane()
  contentPane.add(panel, BorderLayout.CENTER)
  this.setBounds(100, 100, 300, 250)
  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  this.setVisible(true)
}