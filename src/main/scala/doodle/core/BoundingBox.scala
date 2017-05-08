package doodle.core

/**
  * Created by graig on 5/7/17.
  */
case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double){

  def height = top - bottom
  def width = right - left

  def above(that: BoundingBox): BoundingBox = BoundingBox(this.left min that.left, (this.height+that.height)/2d, this.right max that.right, -(this.height+that.height)/2d)
  def beside(that: BoundingBox): BoundingBox = BoundingBox(-(this.width + that.width)/2d, this.top max that.top,(this.width + that.width)/2d,this.bottom min that.bottom)
  def on(that: BoundingBox): BoundingBox = BoundingBox(this.left min that.left, this.top max that.top, this.right max that.right, this.bottom min that.bottom)




}
