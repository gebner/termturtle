import at.logic.gapt.expr.{Apps, Const, LambdaExpression}
import doodle.core._

object drawTerm {

  def apply(term: LambdaExpression, angle: Angle, length: Double, color: Map[Const, Color]): Image = term match {
    case c: Const =>
      Path(Seq(
        MoveTo(Vec.zero),
        LineTo(Vec.polar(angle, length))
      )) lineColor color(c)

    case Apps(head, args) =>
      val subTrees = for ((arg, i) <- args.zipWithIndex) yield
        apply(arg, angle + (30.degrees * (0.5 - i)), length * 0.8, color)
      apply(head, angle, length, color) on (subTrees.reduce(_ on _) at Vec.polar(angle, length))
  }

  def apply(term: LambdaExpression, colors: Map[Const, Color]): Image = apply(term, -90.degrees, 50, colors)

}
