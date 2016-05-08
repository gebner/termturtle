import at.logic.gapt.expr.{Apps, Const, LambdaExpression}
import doodle.core._
import doodle.syntax._

object drawTerm {

  def apply(term: LambdaExpression, angle: Angle, length: Double, color: Map[LambdaExpression, Color]): Image = term match {
    case Apps(c, Seq()) =>
      OpenPath(Seq(
        MoveTo(Point.zero),
        LineTo(Point.polar(length, angle))
      )) lineColor color(c)

    case Apps(head, args) =>
      val subTrees = for ((arg, i) <- args.zipWithIndex) yield
        apply(arg, angle + (30.degrees * (0.5 - i)), length * 0.8, color)
      apply(head, angle, length, color) on (subTrees.reduce(_ on _) at Vec.polar(angle, length))
  }

  def apply(term: LambdaExpression, colors: Map[LambdaExpression, Color]): Image = apply(term, -90.degrees, 50, colors)

}
