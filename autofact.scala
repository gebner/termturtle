import at.logic.gapt.expr.fol.Numeral
import at.logic.gapt.expr.hol.existsclosure
import at.logic.gapt.expr._
import at.logic.gapt.grammars.RecursionScheme
import at.logic.gapt.proofs.Sequent
import at.logic.gapt.proofs.expansion.InstanceTermEncoding
import at.logic.gapt.provers.eprover.EProver
import at.logic.gapt.provers.prover9.Prover9
import at.logic.gapt.provers.vampire.Vampire
import doodle.core._
import doodle.jvm._
import doodle.syntax._

object autofact extends scala.App {
  val O = FOLConst( "0" )
  val s = FOLFunctionConst( "s", 1 )
  val g = FOLFunctionConst("g", 2)
  val f = FOLFunctionConst("f", 1)
  val plus = FOLFunctionConst("+", 2)
  val times = FOLFunctionConst("*", 2)

  implicit class RichExpr(expr: LambdaExpression) {
    def *(that: LambdaExpression): LambdaExpression = times(expr, that)
    def +(that: LambdaExpression): LambdaExpression = plus(expr, that)
  }

  val x = FOLVar( "x" )
  val y = FOLVar( "y" )
  val z = FOLVar( "z" )

  val n = 7

  val endSequent = existsclosure(
    ((s(O) * x) === x) +:
    (((x * y) * z) === (x * (y * z))) +:
    (g(x, s(y)) === g(x * s(y), y)) +:
    (g(x, O) === x) +:
    (f(s(x)) === (s(x) * f(x))) +:
    (f(O) === s(O)) +:
    Sequent()
    :+ (f(Numeral(n)) === g(s(O), Numeral(n)))
  )

  val Some(expansionProof) = EProver getExpansionProof endSequent
  val (lang, encoding) = InstanceTermEncoding(expansionProof.expansionSequent)

//  val l = lang map encoding.decodeToSignedFormula
  val l = lang
  l foreach println

  def grid(images: Seq[Image]): Image = {
    val columns = math.sqrt(images.size).toInt
    allAbove(images.grouped(columns).map(allBeside).toSeq)
  }

  val allConsts = l flatMap {subTerms(_)} collect { case c: Const => c }
  val colors = for ((c,i) <- allConsts.toSeq.zipWithIndex)
    yield c -> Color.hsl((35*i).degrees, .5.normalized, .5.normalized)

  val img = grid(l.toSeq
    sortBy { expressionSize(_) }
    map { drawTerm(_, -90.degrees, 20, colors.toMap) }) lineWidth 0.5

//  val bbox = BoundingBox(img)
//  val svgGraphics = new SVGGraphics2D(bbox.width.toInt,bbox.height.toInt)
//  DoodlePanel(img).paintImage(img, Vec(bbox.right,bbox.bottom), DrawingContext.blackLines)(svgGraphics)
//  Files.write(Paths.get("pi2fact.svg"), svgGraphics.getSVGDocument.getBytes)

  draw(img)

}