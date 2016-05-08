import at.logic.gapt.expr._
import at.logic.gapt.expr.hol.instantiate
import at.logic.gapt.formats.tip.TipSmtParser
import at.logic.gapt.grammars.{RecSchemTemplate, RecursionScheme}
import at.logic.gapt.proofs.expansion.InstanceTermEncoding
import at.logic.gapt.proofs.{Context, FiniteContext}
import at.logic.gapt.provers.escargot.Escargot
import doodle.core._
import doodle.syntax._
import utils._

import scala.language.postfixOps

object appnil extends scala.App {

  val tipbench =
    """
(declare-sort i 0)
(declare-datatypes () ((list (nil) (cons (head i) (tail list)))))

(define-fun-rec app ((xs list) (ys list)) list
  (match xs
    (case (cons x xs) (cons x (app xs ys)))
    (case nil ys)))

(assert-not (forall ((x list))
  (= (app x nil) x)))
(check-sat)
    """

  val problem = TipSmtParser.parse(tipbench)
  implicit var ctx = problem.context

  val enc = InstanceTermEncoding(problem.toSequent)

  def mkInst(n: Int): LambdaExpression =
    if (n == 0) le"nil"
    else le"cons ${Var(s"x$n", TBase("i"))} ${mkInst(n-1)}"

  val instLangs = 0 to 4 map { i =>
    val inst = mkInst(i)
    val instSeq = problem.toSequent.map(identity, instantiate(_, inst))
    val Some(instProof) = Escargot.getExpansionProof(instSeq)
    enc.encode(instProof)
  }

  ctx += Context.Sort(enc.instanceTermType.asInstanceOf[TBase])
  ctx += hoc"A: list > InstanceTermType"
  ctx += hoc"B: list > InstanceTermType"
  val rs = RecursionScheme(le"A".asInstanceOf[Const],
    le"A x" -> enc.encode(hof"app nil nil != nil"),
    le"A x" -> enc.encode(hof"app x nil = x"),
    le"A x" -> le"B x",
    le"B (cons x xs)" -> le"B xs",
    le"B (cons x xs)" -> enc.encode(hof"app (cons x xs) nil != cons x (app xs nil)")
  )

  val l2 = rs.rules.map(_.rhs)

  val allConsts = allConstsOrVars(instLangs.toSet + l2 flatten)
  val colors = for ((c,i) <- allConsts.toSeq.zipWithIndex)
    yield c -> Color.hsl((35*i).degrees, .5.normalized, .5.normalized)

  val img1 = grid(for (l1 <- instLangs) yield grid(l1.toSeq
    sortBy { expressionSize(_) }
    map { drawTerm(_, 90.degrees, 90, colors.toMap) on Rectangle(120,120).noFill.noLine }) lineWidth 3
    on Rectangle(300,300).noFill.lineColor(Color.gray.lightenBy(0.5.normalized))
    on Rectangle(350,350).noFill.noLine)
  val img2 = grid(l2.toSeq
    sortBy { expressionSize(_) }
    map { drawTerm(_, 90.degrees, 140, colors.toMap) on Rectangle(150,150).noFill.noLine }) lineWidth 3
  val img = img1 beside Rectangle(200,100).noFill.noLine beside img2

  exportSVG(img1, "appnil-instproofs.svg")
  exportSVG(img2, "appnil-grammar.svg")
  exportSVG(img, "appnil.svg")

  img.draw
}
