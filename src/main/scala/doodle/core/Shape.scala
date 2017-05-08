package doodle.core

import doodle.backend.Canvas
import doodle.jvm.Java2DCanvas

/**
  * Created by graig on 5/7/17.
  */
sealed trait Shape {

  val boundingBox: BoundingBox = this match{
    case Circle(r) => BoundingBox(-r,r,r,-r)
    case Rectangle(w,h) => BoundingBox(-w/2d,h/2d,w/2d,-h/2d)
    case Above(t,b) => t.boundingBox.above(b.boundingBox)
    case Beside(l,r) => l.boundingBox.beside(r.boundingBox)
    case On(t,b) => t.boundingBox.on(b.boundingBox)
  }

  def on(bottom: Shape): Shape = On(this,bottom)
  def above(bottom: Shape): Shape = Above(this,bottom)
  def beside(right: Shape): Shape = Beside(this,right)

  def draw(canvas: Canvas): Unit = draw(canvas,0.0,0.0)

  def draw(canvas: Canvas, originX: Double, originY: Double): Unit = this match{
    case Circle(r) => canvas.circle(originX,originY,r)
    case Rectangle(w,h) => canvas.rectangle(originX - w/2d,originY - h/2d,w,h)
    case Above(t,b) => {
      val box = this.boundingBox
      val tBox = t.boundingBox
      val bBox = b.boundingBox
      val aboveOriginY = originY + box.top - (tBox.height / 2d)
      val belowOriginY = originY + box.bottom + (bBox.height / 2d)
      t.draw(canvas,originX,aboveOriginY)
      b.draw(canvas,originX,belowOriginY)
    }
    case On(t,b) => {
      t.draw(canvas,originX,originY)
      b.draw(canvas,originX,originY)
    }
    case Beside(l,r) => {
      val box = this.boundingBox
      val lBox = l.boundingBox
      val rBox = r.boundingBox
      val leftOriginX = originX + box.left + (lBox.width / 2)
      val rightOriginX = originX + box.right - (rBox.width / 2)
      l.draw(canvas,leftOriginX,originY)
      r.draw(canvas,rightOriginX,originY)
    }
  }


}

final case class Circle(radius: Double) extends Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class On(top: Shape, bottom: Shape) extends Shape
final case class Above(top: Shape, bottom: Shape) extends Shape
final case class Beside(left: Shape, right: Shape) extends Shape

object test{
  val canvas = Java2DCanvas.canvas
  canvas.setSize(500,500)
  canvas.setStroke(Stroke(1,Color.black,Line.Cap.Round,Line.Join.Round))
  val drawingA = Circle(10) beside Circle(10)
  val drawingB = drawingA above drawingA
  drawingB.draw(canvas)
  canvas.stroke()
}