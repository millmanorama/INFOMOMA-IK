
import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

import javafx.animation.AnimationTimer
import javafx.application.Application

import javafx.event.ActionEvent
import javafx.event.EventHandler

import javafx.scene.paint.Color
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.scene.input.MouseEvent
import javafx.scene.layout.StackPane
import javafx.scene.layout.VBox
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
  val oY = 450

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
    var target = DenseVector[Double](100.0, -100.0, 1)

    canvas.setOnMouseClicked(new EventHandler[MouseEvent] {
      override def handle(e: MouseEvent) {
        val worldMouse = screenToWorld(e.getX, e.getY)
        target = DenseVector[Double](worldMouse._1, worldMouse._2, 1)
      }
    })

    root.getChildren.add(canvas)
    primaryStage.setScene(new Scene(root, W, H))
    primaryStage.show

    new AnimationTimer {
      override def handle(now: Long) {
        gc.setFill(Color.color(1, 1, 1, .1))
        gc.fillRect(0, 0, W, H)
        gc.setFill(Color.color(.5, .5, .5, 1))
        gc.fillRect(0, oY + 20, W, H)

        val targetDistance = norm(target)
        val length = lengths.reduce(_ + _)

        var threshHold = 1.0
        if (targetDistance > length) {
          threshHold = targetDistance - length + threshHold
        }
        var points = fk(lengths, q)
        var error = (target - points.last)
        var errorMag = norm(error)
        var i = 0
        while (errorMag > threshHold && i < 100) {
          q = q + .1 * pinv(jacobian(lengths, q)) * error
          q(0) = q(0) % (2 * Pi)
          q(1) = q(1) % (2 * Pi)
          q(2) = q(1) * 2 / 3.0

          points = fk(lengths, q)
          error = (target - points.last)
          errorMag = norm(error)

          i = i + 1
        }

        draw(points, gc)
        gc.setFill(Color.color(0, 0, 0, 1))

        val screenTarget = worldToScreen(target(0), target(1))
        gc.fillOval(screenTarget._1 - 5, screenTarget._2 - 5, 10.0, 10.0)
        if (i > 0) {
          println("itration: " + i)
          println("errorMag: " + errorMag)
          println(q)
        }
      }
    }.start
  }

  def clamp(x: Double, min: Double, max: Double): Double = {
    return Math.min(Math.max(min, x), max)
  }

  def fk(lengths: List[Double], thetas: DenseVector[Double]): List[DenseVector[Double]] = {
    var points = List(o)
    var v1 = o
    var M = DenseMatrix.eye[Double](3)
    var v2: DenseVector[Double] = DenseVector(3)
    for ((l, i) <- lengths.zipWithIndex) {
      val R = rotation(thetas(i))
      val T = translation(l, 0)
      M = M * R * T
      v2 = M * o
      points = points :+ v2
      v1 = v2
    }
    return points
  }

  def draw(points: List[DenseVector[Double]], gc: GraphicsContext) {
    var v1 = worldToScreen(points(0))
    for (v2 <- points.tail.map(worldToScreen(_))) {
      gc.strokeLine(v1._1, v1._2, v2._1, v2._2)
      v1 = v2
    }
  }

  def translation(x: Double, y: Double): DenseMatrix[Double] = {
    DenseMatrix.create(3, 3, Array(
      1, 0, 0,
      0, 1, 0,
      x, y, 1
    ))
  }

  def worldToScreen(p: DenseVector[Double]): (Double, Double) = {
    return worldToScreen(p(0), p(1))
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