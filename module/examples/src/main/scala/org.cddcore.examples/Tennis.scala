/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.examples

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{Document, InternetDocument}
import org.cddcore.rendering.Renderer


object Tennis {
//  def main(args: Array[String]) {
//    Renderer.makeReportFilesFor(tennis)
//  }

  val wikiPage = Document.internet("https://en.wikipedia.org/wiki/Tennis")

  val (serverWon, receiverWon) = ("server won", "receiver won")
  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")
  lazy val tennis = new Engine[(Int, Int), String]() {
    title("Tennis Kata")
    useCase("Winning", "A game is won by the first player to have won at least four points in total and at least two points more than the opponent.") {
      useCase("Server Winning") {
        (4, 0) produces serverWon when { case (l, r) => (l - r) >= 2 && l >= 4 } ref wikiPage
        (4, 1) produces serverWon withComment "This was 4,1"
        (4, 2) produces serverWon
        (5, 3) produces serverWon
      }
      useCase("Received Winning") {
        (0, 4) produces receiverWon when { case (l, r) => (r - l) >= 2 && r >= 4 }
        (1, 4) produces receiverWon
        (2, 4) produces receiverWon
        (3, 5) produces receiverWon
      }
    }

    useCase("Running Score", "The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as 'love', 'fifteen', 'thirty', and 'forty' respectively.") {
      useCase("different scores") {
        (2, 3) produces "thirty, forty" because { case (l, r) if l < 4 && r < 4 => s"${lookup(l)}, ${lookup(r)}" }
        (2, 1) produces "thirty, fifteen"
        //        (3, 2) produces something where (_.contains("thirty"))
      }

      useCase("When both have the same running score", "The running score, if both scores are the same, is called xx all") {
        (0, 0) produces "love all" when { case (l, r) => l == r && l < 3 } by (x => s"${lookup(x._1)} all")
        (2, 2) produces "thirty all"
      }
    }

    useCase("End game") {
      useCase("Deuce", "If at least three points have been scored by each player, and the scores are equal, the score is 'deuce'.") {
        (3, 3) produces "deuce" when { case (l, r) => l >= 3 && r >= 3 && l == r }
        (4, 4) produces "deuce"
        (5, 5) produces "deuce"
      }

      useCase("Advantage", "If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is 'advantage' for the player in the lead.") {
        (5, 4) produces "advantage server" when { case (l, r) => l >= 3 && r >= 3 && l == r + 1 }
        (6, 5) produces "advantage server"
        (4, 5) produces "advantage receiver" when { case (l, r) => l >= 3 && r >= 3 && r == l + 1 }
        (5, 6) produces "advantage receiver"
      }
    }
  }

}
