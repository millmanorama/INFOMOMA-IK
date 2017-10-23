
import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._
import javafx.animation.AnimationTimer
import javafx.application.Application
import javafx.event.EventHandler
import javafx.scene.layout.VBox
import javafx.scene.paint.Color

import javafx.application.Application
import javafx.event.ActionEvent

import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.scene.control.Button
import javafx.scene.input.MouseEvent
import javafx.scene.layout.StackPane
import javafx.stage.Stage

object IKSolver {
  def main(args: Array[String]) {
    Application.launch(classOf[IKSolver], args: _*)
  }
}

class IKSolver extends Application {
  val H = 900
  val W = 900
  val oX = 50
  val oY = 50

  var q = DenseVector[Double](0, 0, 1)
  val lengths = List(398, 224, 158.0)
  val thetas = DenseVector[Double](0, 22, -63).map(_ * Pi / 180)
  val thetas2 = DenseVector[Double](15.05, -20, -37).map(_ * Pi / 180)

  val o = DenseVector(0.0, 0.0, 1)

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("IK!")

    val root = new VBox
    val canvas = new Canvas(W, H)
    val gc = canvas.getGraphicsContext2D

    gc.setLineWidth(2)
    var target = DenseVector[Double](100.0, 100.0, 1)

    canvas.setOnMouseClicked(new EventHandler[MouseEvent] {
      override def handle(e: MouseEvent) {
        val worldMouse = screenToWorld(e.getX, e.getY)
        target = DenseVector[Double](worldMouse._1, worldMouse._2, 1)
        println(target)
      }
    })

    root.getChildren.add(canvas)
    primaryStage.setScene(new Scene(root, W, H))
    primaryStage.show

    //    val tip1 = fk(lengths, thetas, gc)
    //    gc.setStroke(Color.BLUE)
    //    val tip2 = fk(lengths, thetas2, gc, 0)

    //    println(tip1 - tip2)

    new AnimationTimer {
      override def handle(now: Long) {
        gc.setFill(Color.color(1, 1, 1, .1))
        gc.fillRect(0, 0, W, H)
        //        for (i <- 1 until 10) {
        //          gc.setFill(Color.color(0, 0, 0, .1))
        //          gc.fillRect(0, 0, 900, 900)

        q = q +.1* pinv(jacobian(lengths, q)) * (target - fk(lengths, q, gc))
        //        q(0) = clamp(q(0), -Pi / 2, Pi / 20) % Pi
        //        q(1) = clamp(q(1), -Pi / 2, Pi / 20) % Pi
                q(0) = q(0) % (2*Pi)
                q(1) = q(1) % (2*Pi)
        q(2) = q(1) * 2 / 3.0
        //        }
        gc.setFill(Color.color(0, 0, 0, 1))

        val screenTarget = worldToScreen(target(0), target(1))
        gc.fillOval(screenTarget._1 - 5, screenTarget._2 - 5, 10.0, 10.0)

        gc.fillText("?s: " + q(0) + ",  " + q(1), 20, 20)
        println(q)
        //        gc.fillText("TIP: " + v2(0) + ", " + v2(1), 20, 40)
      }
    }.start
  }

  def clamp(x: Double, min: Double, max: Double): Double = {
    return Math.min(Math.max(min, x), max)
  }
  def fk(lengths: List[Double], thetas: DenseVector[Double], gc: GraphicsContext): DenseVector[Double] = {
    var v1 = o
    var M = DenseMatrix.eye[Double](3)
    var v2: DenseVector[Double] = DenseVector(3)
    for ((l, i) <- lengths.zipWithIndex) {
      val R = rotation(thetas(i))
      val T = translation(l, 0)
      M = M * R * T
      v2 = M * o

      val v1Screen = worldToScreen(v1(0), v1(1))
      val v2Screen = worldToScreen(v2(0), v2(1))
      gc.strokeLine(v1Screen._1, v1Screen._2, v2Screen._1, v2Screen._2)
      v1 = v2
    }
    return v2

    //    gc.fillText("?m: " + thetas(0) + " ?i: " + thetas(1) + " ?d: " + thetas(2), 20, 20)
    //    gc.fillText("TIP: " + v2(0) + ", " + v2(1), 20, 40)
  }

  def translation(x: Double, y: Double): DenseMatrix[Double] = {
    DenseMatrix.create(3, 3, Array(
      1, 0, 0,
      0, 1, 0,
      x, y, 1
    ))
  }

  def worldToScreen(x: Double, y: Double): (Double, Double) = {
    return (x + oX, H - oY - y);
  }

  def screenToWorld(x: Double, y: Double): (Double, Double) = {
    return (x - oX, H - y - oY)
  }

  def rotation(theta: Double): DenseMatrix[Double] = {
    val c = cos(theta)
    val s = sin(theta)
    DenseMatrix.create(3, 3, Array(
      c, s, 0,
      -s, c, 0,
      0, 0, 1
    ))
  }

  def jacobian(ls: List[Double], q: DenseVector[Double]): DenseMatrix[Double] = {
    val ldi = ls(2) + ls(1)
    DenseMatrix.create(3, 3, Array(
      -ldi * sin(q(0)), ldi * cos(q(0)), 0,
      -ls(2) * sin(q(1)), ls(2) * cos(q(1)), 0,
      0, 0, 0
    ))
  }
}