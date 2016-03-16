package org.cddcore.rendering

class DuplicateTitleException(duplicates: Map[String, List[String]]) extends Exception(s"Duplicate titles in engines ${duplicates}")