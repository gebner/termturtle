import java.nio.file.{Files, Paths}
import javax.swing.WindowConstants

import at.logic.gapt.expr._
import doodle.core._
import doodle.jvm.{CanvasFrame, CanvasPanel}
import doodle.syntax._
import org.jfree.graphics2d.svg.SVGGraphics2D

class SVGExporter {
  val canvasPanel = new CanvasPanel

  var w = 0
  var h = 0

  def result(): String = {
    val CanvasPanel.SetSize(w,h) = canvasPanel.queue.poll()
    val svgGraphics = new SVGGraphics2D(w,h)
    canvasPanel.paintComponent(svgGraphics)
    svgGraphics.getSVGDocument
  }
}

object utils {

  def exportSVG(img: Image): String = {
    val exporter = new SVGExporter
    implicit def canvas = exporter.canvasPanel.canvas

    val i = doodle.backend.Image.compile(img, DrawingContext.blackLines)
    val bb = i.boundingBox
    interpreter.draw(img, canvas, bb.width.ceil.toInt+40, bb.height.ceil.toInt+40,
      Point.cartesian(bb.left-20,bb.top+20)
    )

    exporter.result()
  }

  def exportSVG(img: Image, filename: String): Unit =
    Files.write(Paths.get(filename), exportSVG(img).getBytes)

  implicit def interpreter = doodle.backend.StandardInterpreter.interpreter
  implicit def canvas = {
    val frame = new CanvasFrame()
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.panel.canvas
  }

  def grid(images: Seq[Image]): Image = {
    val columns = math.sqrt(images.size).toInt
    allAbove(images.grouped(columns).map(allBeside).toSeq)
  }

  def allConstants(expr: Iterable[LambdaExpression]): Set[Const] =
    expr.view.flatMap(allConstants(_)).toSet
  def allConstants(expr: LambdaExpression): Set[Const] =
    subTerms(expr) collect { case c: Const => c }

}
