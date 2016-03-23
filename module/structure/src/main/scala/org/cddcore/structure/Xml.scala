package org.cddcore.structure

import scala.xml.{Node, NodeSeq}


object XMLStructure extends Structure[Node, NodeSeq] {
  def findResult(pathRootAndSteps: PathRootAndSteps[Node, NodeSeq]): NodeSeq = {
    import pathRootAndSteps._
    steps.foldLeft(structureAsResult(root)) { case (acc, step) =>
      step.linked match {
        case true => acc \ step.element
        case false => acc \\ step.element
      }
    }
  }

  def structureAsResult(s: Node): NodeSeq = s
}


class Xml extends StructureHolder[Node, NodeSeq] {
  protected def structureTitle: String = "xml"

  implicit val structure = XMLStructure

  implicit def xml(s: Node) = new PathRoot(s)

  def string = PathResult((nodeSeq: NodeSeq) => nodeSeq.text)

  def optString = PathResult((nodeSeq: NodeSeq) => if (nodeSeq.isEmpty) None else Some(nodeSeq.text))

  def int = PathResult((nodeSeq: NodeSeq) => nodeSeq.text.toInt)

  def fold[Acc, T](raw: PathResult[NodeSeq, T], initial: => Acc, foldFn: (Acc, T) => Acc): PathResult[NodeSeq, Acc] =
    PathResult(_.map(raw.convertor).foldLeft(initial)(foldFn)
    )
}
