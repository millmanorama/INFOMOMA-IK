
import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

import javax.imageio.ImageIO

import java.awt.image.BufferedImage
import java.io.File
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

  val initialQ = DenseVector[Double](0, 0, 0)
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

    println(fk(lengths, thetas).last)
    println(fk(lengths, thetas2).last)

    drawReachability

    var q = initialQ
    canvas.setOnMouseDragged(new EventHandler[MouseEvent] {
      override def handle(e: MouseEvent) {
        val worldMouse = screenToWorld(e.getX, e.getY)
        target = DenseVector[Double](worldMouse._1, worldMouse._2, 1)
        gc.setFill(Color.color(1, 1, 1, 1))
        gc.fillRect(0, 0, W, H)
        gc.setFill(Color.color(.5, .5, .5, 1))
        gc.fillRect(0, oY + 20, W, H)
        val (points, q2, errorMag, i) = ik(q, target, 100)
        q = q2
        draw(points, gc)
        gc.setFill(Color.color(0, 0, 0, 1))

        val screenTarget = worldToScreen(target(0), target(1))
        gc.fillOval(screenTarget._1 - 5, screenTarget._2 - 5, 10.0, 10.0)
      }
    })


    root.getChildren.add(canvas)
    primaryStage.setScene(new Scene(root, W, H))
    primaryStage.show

  }

  def ik(initialQ: DenseVector[Double], target: DenseVector[Double], maxIterations: Integer): (List[DenseVector[Double]], DenseVector[Double], Double, Integer) = {
    var q = initialQ

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

    while (errorMag > threshHold & i < maxIterations) {
      q = q + .1 * pinv(jacobian(lengths, q)) * error
      q(0) = clamp(q(0), -Pi / 3, Pi / 3)
      q(1) = clamp(q(1), -2 * Pi / 3, 0)
      q(2) = q(1) * 2 / 3.0

      points = fk(lengths, q)
      error = (target - points.last)
      errorMag = norm(error)

      i = i + 1
    }
    return (points, q, errorMag, i)
  }

  def drawReachability() {

    var bi: BufferedImage = new BufferedImage(W, H, BufferedImage.TYPE_3BYTE_BGR)
    val big = bi.createGraphics
    for (x <- 0 until W) {
      for (y <- 0 until H) {
        var (wX, wY) = screenToWorld(x, y)
        val target = DenseVector[Double](wX, wY, 1)

        val (points, q, errorMag, i) = ik(initialQ, target, 256)
        if (errorMag <= 1.0) {
          big.setPaint(java.awt.Color.WHITE)
          big.fillRect(x, y, 1, 1)
        } 
        if (x % 10 == 0 & y % 10 == 0)
          println(x, y)
      }
    }
    ImageIO.write(bi, "png", new File("reachable.png"))

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

  def jacobian(l: List[Double], q: DenseVector[Double]): DenseMatrix[Double] = {
    val thetaSum = q(0) + (5 / 3.0) * q(1)
    val l2cosThetaSum = l(2) * cos(thetaSum)
    val l2sinThetaSum = l(2) * sin(thetaSum)

    val thetaSum2 = q(0) + q(1)
    val l1cosThetaSum2 = l(1) * cos(thetaSum2)
    val l1sinThetaSum2 = l(1) * sin(thetaSum2)

    DenseMatrix.create(3, 3, Array(
      -l2sinThetaSum - l1sinThetaSum2 - l(0) * sin(q(0)), l2cosThetaSum + l1cosThetaSum2 + l(0) * cos(q(0)), 0,
      -(5 / 3.0) * l2sinThetaSum - l1sinThetaSum2, (5 / 3.0) * l2cosThetaSum + l1cosThetaSum2, 0,
      0, 0, 0
    ))
  }
}