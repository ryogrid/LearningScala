import javax.swing.{JFrame, JPanel, WindowConstants}
import java.awt.{BorderLayout, Graphics}
import scala.util.Random
import java.util.{Timer, TimerTask}

object BoidAlgorithm {
  val SCREEN_SIZE = 500          // 画面サイズ

  def main(args: Array[String]) :Unit = {
    val frame : MainFrame = new MainFrame()
  }
}

class MainFrame() extends JFrame{
  val panel = new BoidPanel()
  val contentPane = this.getContentPane()
  contentPane.add(panel, BorderLayout.CENTER)
  this.setBounds(100, 100, BoidAlgorithm.SCREEN_SIZE, BoidAlgorithm.SCREEN_SIZE)
  this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  this.setVisible(true)
}

class Tori(var x: Double, var y: Double, var vx:Double, var vy:Double)

class BoidPanel() extends JPanel{
  val FPS = 60
  val NUM_BOIDS = 100            // ボイドの数
  val BOID_SIZE = 5              // ボイドの大きさ
  val MAX_SPEED = 7

  private var boids = initBoids()

  private val t = new Timer()
  t.schedule(new RepaintTimer(), 0, 1000 / FPS)

  private def initBoids(): Vector[Tori] = {
    boids = Vector.empty[Tori]
    val r = new Random()
    (1 to NUM_BOIDS).foldLeft(boids){ (seq ,index) =>
      val x = r.nextInt(BoidAlgorithm.SCREEN_SIZE)
      val y = r.nextInt(BoidAlgorithm.SCREEN_SIZE)
      seq :+ new Tori(x, y, 0, 0)
    }
  }

  class RepaintTimer extends TimerTask {
    def run: Unit = {
      move()
      repaint()
    }
  }

  override def paint(g: Graphics): Unit = {
    g.clearRect(0, 0, BoidAlgorithm.SCREEN_SIZE, BoidAlgorithm.SCREEN_SIZE)
    for(boid <- boids){
      g.drawOval(boid.x.asInstanceOf[Int], boid.y.asInstanceOf[Int], BOID_SIZE, BOID_SIZE)
    }
  }

  private def move(): Unit = {
    // 全てのボイドに3つのルールを適用
    // 速度の微調整(壁から出た場合と最大速度を超えていた場合の処理)
    // ボイドの速度より座標を更新
    for(n <- 0 until NUM_BOIDS){
      rule1(n)
      rule2(n)
      rule3(n)
      // limit speed
      val b: Tori = boids(n)
      val speed = Math.sqrt(b.vx * b.vx + b.vy * b.vy)
      if (speed >= MAX_SPEED) {
        val r = MAX_SPEED / speed
        b.vx *= r
        b.vy *= r
      }
      // 座標の更新
      b.x += b.vx
      b.y += b.vy
      // 壁の外に出てしまった場合速度を内側へ向ける
      if ((b.x < 0 && b.vx < 0) || (b.x > BoidAlgorithm.SCREEN_SIZE && b.vx > 0)){
        b.vx *= -1
      }
      if ((b.y < 0 && b.vy < 0) || (b.y > BoidAlgorithm.SCREEN_SIZE && b.vy > 0)){
        b.vy *= -1
      }
    }

  }

  private def rule1(index: Int): Unit = {
    // ボイドは群れの中心へ向かおうとする

    // 自分を除いた群れの真ん中
    var c_x = 0.0
    var c_y = 0.0
    for (n <- 0 until NUM_BOIDS if n != index) {
      c_x += boids(n).x
      c_y += boids(n).y
    }
    c_x /= NUM_BOIDS - 1
    c_y /= NUM_BOIDS - 1
    boids(index).vx += (c_x - boids(index).x) / 100
    boids(index).vy += (c_y - boids(index).y) / 100
  }

  private def rule2(index: Int): Unit = {
    // ボイドは他のボイドと最低限の距離を取ろうとする
    for (n <- 0 until NUM_BOIDS if n != index) {
        val d = distance(boids(n), boids(index)) // ボイド間の距離
        if (d < 5) {
          boids(index).vx -= boids(n).x - boids(index).x
          boids(index).vy -= boids(n).y - boids(index).y
        }
    }
  }

  private def rule3(index: Int): Unit = {
    // ボイドは群れの平均速度ベクトルに合わせようとする
    var pv_x = 0.0
    var pv_y = 0.0 // 自分を除いた群れの平均速度
    for (n <- 0 until NUM_BOIDS if n != index) {
      pv_x += boids(n).vx
      pv_y += boids(n).vy
    }
    pv_x /= NUM_BOIDS - 1
    pv_y /= NUM_BOIDS - 1
    boids(index).vx += (pv_x - boids(index).vx) / 8
    boids(index).vy += (pv_y - boids(index).vy) / 8
  }

  private def distance(a: Tori, b: Tori): Double =  {
    val x = a.x - b.x
    val y = a.y - b.y
    Math.sqrt(x * x + y * y)
  }
}
