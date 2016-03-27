package org.cddcore.rendering

import org.cddcore.enginecomponents.HasDefinedInSourceCodeAt


class DuplicateTitleException(duplicates: Map[String, List[HasDefinedInSourceCodeAt]]) extends Exception(s"Duplicate titles in engines ${
  duplicates.map{case (title, d) => title +"->" + d.map(_.definedInSourceCodeAt).mkString(",")}}")