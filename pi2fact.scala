import java.nio.file.{Files, Paths}

import at.logic.gapt.expr._
import at.logic.gapt.expr.fol.Numeral
import at.logic.gapt.expr.hol.existsclosure
import at.logic.gapt.grammars.RecursionScheme
import at.logic.gapt.proofs.Sequent
import at.logic.gapt.proofs.expansion.InstanceTermEncoding
import at.logic.gapt.provers.smtlib.Z3
import doodle.core._
import doodle.jvm._
import doodle.syntax._
import org.jfree.graphics2d.svg.SVGGraphics2D

object pi2fact extends scala.App {
  val A = Const( "A", Ti -> To )
  val B = Const( "B", Ti -> ( Ti -> ( ( Ti -> To ) -> To ) ) )
  val C = Const( "C", Ti -> To )
  val D = Const( "D", ( Ti -> To ) -> ( Ti -> ( Ti -> ( Ti -> To ) ) ) )

  val O = Const( "0", Ti )
  val s = Const( "s", Ti -> Ti )
  val plus = Const( "+", Ti -> ( Ti -> Ti ) )
  val times = Const( "*", Ti -> ( Ti -> Ti ) )
  val g = Const( "g", Ti -> ( Ti -> Ti ) )
  val f = Const( "f", Ti -> Ti )

  val X = Var( "X", Ti -> To )
  val x = Var( "x", Ti )
  val y = Var( "y", Ti )
  val z = Var( "z", Ti )
  val w = Var( "w", Ti )

  val hors = RecursionScheme(
    A,
    Set( A, B, C, D ),

    A( z ) -> B( z, s( O ), C ),
    A( z ) -> -Eq( times( s( O ), z ), z ),
    A( z ) -> Eq( f( z ), g( s( O ), z ) ),
    B( s( x ), y, X ) -> B( x, times( y, s( x ) ), D( X, x, y ) ),
    D( X, x, y, w ) -> -Eq( times( times( y, s( x ) ), w ), times( y, times( s( x ), w ) ) ),
    D( X, x, y, w ) -> -Eq( g( y, s( x ) ), g( times( y, s( x ) ), x ) ),
    D( X, x, y, w ) -> -Eq( f( s( x ) ), times( s( x ), f( x ) ) ),
    D( X, x, y, w ) -> X( times( s( x ), w ) ),
    B( O, y, X ) -> -Eq( g( y, O ), y ),
    B( O, y, X ) -> -Eq( f( O ), s( O ) ),
    B( O, y, X ) -> -Eq( times( s( O ), s( O ) ), s( O ) ),
    B( O, y, X ) -> X( s( O ) )
  )

  implicit class RichExpr(expr: LambdaExpression) {
    def *(that: LambdaExpression): LambdaExpression = times(expr, that)
    def +(that: LambdaExpression): LambdaExpression = plus(expr, that)
  }

  val endSequent = existsclosure(
    ((s(O) * x) === x) +:
      (((x * y) * z) === (x * (y * z))) +:
      (g(x, s(y)) === g(x * s(y), y)) +:
      (g(x, O) === x) +:
      (f(s(x)) === (s(x) * f(x))) +:
      (f(O) === s(O)) +:
      Sequent()) :+ (f(x) === g(s(O), x))
  val encoding = InstanceTermEncoding(endSequent)

  def lang(i: Int) = hors.parametricLanguage(Numeral(i)).map(_.asInstanceOf[HOLFormula]) map encoding.encode

  def grid(images: Seq[Image]): Image = {
    val columns = math.sqrt(images.size).toInt
    allAbove(images.grouped(columns).map(allBeside).toSeq)
  }

  val n = 4
  val l = lang(4)

  println(Z3 isValid Substitution(x -> Numeral(4))(encoding.decodeToInstanceSequent(l)))

  val allConsts = l flatMap {subTerms(_)} collect { case c: Const => c }
  val colors = for ((c,i) <- allConsts.toSeq.zipWithIndex)
    yield c -> Color.hsl((35*i).degrees, .5.normalized, .5.normalized)

  val img = grid(l.toSeq
    sortBy { expressionSize(_) }
    map { drawTerm(_, -90.degrees, 40, colors.toMap) }) lineWidth 0.5

  val bbox = BoundingBox(img)
  val svgGraphics = new SVGGraphics2D(bbox.width.toInt,bbox.height.toInt)
  DoodlePanel(img).paintImage(img, Vec(bbox.right,bbox.bottom), DrawingContext.blackLines)(svgGraphics)
  Files.write(Paths.get("pi2fact.svg"), svgGraphics.getSVGDocument.getBytes)

  draw(img)
}