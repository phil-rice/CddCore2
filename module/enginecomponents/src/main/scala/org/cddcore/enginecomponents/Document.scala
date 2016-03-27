package org.cddcore.enginecomponents

trait Document {
  def name: String

  def ref: String
}

object Document {
  def internet(ref: String) = InternetDocument(ref, ref)

  def internet(name: String, ref: String) = InternetDocument(name, ref)

  def paper(ref: String) = PaperDocument(ref, "")

  def paper(name: String, ref: String) = PaperDocument(name, ref)
}

case class InternetDocument(name: String, ref: String) extends Document

case class PaperDocument(name: String, ref: String) extends Document

object Reference {
  def apply(document: Document): Reference = Reference(document, None)

  def apply(document: Document, internalRef: String): Reference = Reference(document, Some(internalRef))
}

case class Reference(document: Document, internalRef: Option[String] = None)
