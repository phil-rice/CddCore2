/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.examples

import scala.language.implicitConversions
import scala.xml.Elem
import scala.xml.NodeSeq
import org.cddcore.engine.Engine
import org.cddcore.structure.{PathResult, Situation, Xml}
import org.junit.runner.RunWith

case class MapWithDefault[K, V](map: Map[K, V], default: V) extends Function[K, V] {
  def apply(k: K) = map.getOrElse(k, default)
}

case class World(thisBank: BankId, customerIdToCustomer: (CustomerId) => Elem, acceptedBanks: List[BankId] = List(BankId.hsbc, BankId.rbs, BankId.thisBank))

case class BankId(id: String)

object BankId {
  def thisBank = BankId("this")

  def hsbc = BankId("HSBC")

  def rbs = BankId("RBS")

  def dodgyBank = BankId("DodgyBank")
}

object GBP {
  implicit def intToGBP(i: Int) = GBP(i, 0)

  implicit def stringToGBP(s: String) = {
    val d: Double = java.lang.Double.parseDouble(s);
    val pounds = Math.floor(d).toInt;
    GBP(pounds, (d - pounds).toInt)
  }

  implicit def GBPToDouble(g: GBP) = g.pounds + g.pence / 100.0
}

case class GBP(pounds: Integer, pence: Integer)

case class CustomerId(id: String, bank: BankId)

case class Customer(id: CustomerId, balance: GBP, overdraftLimit: GBP, premiumCustomer: Boolean)

case class Cheque(refNo: String, from: CustomerId, to: CustomerId, amount: GBP)

object Message {
  implicit def stringToMessage(s: String) = Message(s)

  implicit def tupleToMessage(t: Tuple2[String, _]) = Message(t._1, t._2)
}

case class Message(pattern: String, keys: Any*)

case class ProcessChequeResult(pay: Boolean, message: Message)

object ProcessChequeTestMother {
  val dodgyDaveId = CustomerId("12", BankId.thisBank)
  val dodgyDaveAtDodgyBankId = CustomerId("12", BankId.dodgyBank)
  val dodgyDave = <Customer>
    <Id>12</Id>
    <Bank>this</Bank>
    <Balance>100.0</Balance>
    <OverdraftLimit>0</OverdraftLimit>
    <PremiumCustomer>false</PremiumCustomer>
  </Customer>

  val richRogerId = CustomerId("34", BankId.thisBank)
  val richRoger = <Customer>
    <Id>34</Id>
    <Bank>this</Bank>
    <Balance>10000.0</Balance>
    <OverdraftLimit>4000.0</OverdraftLimit>
    <PremiumCustomer>true</PremiumCustomer>
  </Customer>
  val richRogerAtHsbcId = CustomerId("123", BankId.hsbc)

  val world = World(BankId.thisBank, MapWithDefault(Map(dodgyDaveId -> dodgyDave, richRogerId -> richRoger), <NoCustomerRecord/>))

  def cheque(ref: String, from: CustomerId, to: CustomerId, amount: Double) = {
    val e =
      <cheque>
        <From><Bank>{ from.bank.id }</Bank><Id>{ from.id }</Id></From>
        <To><Bank>{ to.bank.id }</Bank><Id>{ to.id }</Id></To>
        <Amount> { amount }</Amount>
      </cheque>
    e
  }
}

case class ChequeSituation(world: World, cheque: Elem) extends Xml {

  import GBP._

  def gbp = PathResult(stringToGBP, OneAndOnlyOneString)

  def bankId = PathResult((id: String) => BankId(id), OneAndOnlyOneString)

  val chequeContents = root(cheque)
  val chequeFromContents = chequeContents \ "From"
  lazy val chequeFromBank = chequeFromContents \ "Bank" \ bankId
  lazy val chequeFromId = chequeFromContents \ "Id" \ string

  lazy val chequeFrom = {
    val result = CustomerId(chequeFromId(), chequeFromBank())
    result
  }

  val chequeToContents = chequeContents \ "To"
  lazy val chequeToBank = chequeToContents \ "Bank" \ bankId
  lazy val chequeToId = chequeToContents \ "Id" \ string
  lazy val chequeTo = CustomerId(chequeToId(), chequeToBank())

  lazy val chequeAmount = chequeContents \ "Amount" \ gbp

  lazy val customer = root(world.customerIdToCustomer(chequeFrom))
  lazy val customerBalance = customer \ "Balance" \ gbp
  lazy val customerOverdraftLimit = customer \ "OverdraftLimit" \ gbp

  lazy val customerWouldBeOverDrawn = chequeAmount() > customerBalance()
  lazy val customerHasNoOverdraftLimit = customerOverdraftLimit() == GBP(0, 0)
  lazy val customerWouldExceedOverdraftLimit = chequeAmount() >= customerBalance() + customerOverdraftLimit()
}

class ProcessChequeXml

object ProcessChequeXml {

  import ProcessChequeTestMother._

  val processCheque = new Engine[ChequeSituation, ProcessChequeResult]("Process Cheques") {
    useCase("Cheques that are for a different bank should be rejected") {
      ChequeSituation(world, cheque("1", richRogerAtHsbcId, richRogerId, 1000)).
        produces(ProcessChequeResult(false, "processCheque.reject.fromBankNotThisBank")).
        withComment("One thousand pounds from rich roger at HSBC to rich roger at this bank. But the 'FromBank' isn't this bank")
        .when((s: ChequeSituation) => s.chequeFromBank() != s.world.thisBank)
    }
    useCase("Cheques that are to a bank not on the white list should be rejected") {
      ChequeSituation(world, cheque("1", dodgyDaveId, dodgyDaveAtDodgyBankId, 50)).
        produces(ProcessChequeResult(false, ("processCheque.reject.toBank.notInWhiteList", BankId.dodgyBank))).
        withComment("Dodgy Dave is moving half his funds to a bank that isn't on the accepted list").
        when((s: ChequeSituation) => !s.world.acceptedBanks.contains(s.chequeToBank()))
    }


    useCase("Cheques that will take the customer over the overdraft limit will should be rejected") {
      ChequeSituation(world, cheque("1", dodgyDaveId, richRogerId, 110)).
        produces(ProcessChequeResult(false, "processCheque.reject.noOverdraft")).
        withComment("Dodgy Dave sending more money than he has").
        when((s: ChequeSituation) => s.customerWouldBeOverDrawn && s.customerHasNoOverdraftLimit)

      ChequeSituation(world, cheque("1", richRogerId, richRogerAtHsbcId, 15000)).
        produces(ProcessChequeResult(false, "processCheque.reject.exceedsOverdraftLimit")).
        withComment("Rich Roger sending more money than he has, taking him over his limit").
        when((s: ChequeSituation) => s.customerWouldExceedOverdraftLimit)
    }

    useCase("Cheques that are to to customers in an accepted bank, when the cheque writer has sufficient funds, should be allowed") {
      ChequeSituation(world, cheque("1", dodgyDaveId, richRogerId, 50)).
        produces(ProcessChequeResult(true, "processCheque.accept")).
        withComment("Dodgy Dave sending an OK cheque to someone in this bank").
        when((s: ChequeSituation) => s.world.acceptedBanks.contains(s.chequeToBank()))

      ChequeSituation(world, cheque("1", dodgyDaveId, richRogerAtHsbcId, 50)).
        produces(ProcessChequeResult(false, "processCheque.defaultResult.shouldntHappen")).
        withComment("Dodgy Dave sending an OK cheque to someone in an accepted bank")
    }

    useCase("Cheques that are for a different bank should be rejected") {
      ChequeSituation(world, cheque("1", richRogerAtHsbcId, richRogerId, 1000)).
        produces(ProcessChequeResult(false, "processCheque.reject.fromBankNotThisBank")).
        withComment("One thousand pounds from rich roger at HSBC to rich roger at this bank. But the 'FromBank' isn't this bank").
        when((s: ChequeSituation) => s.chequeFromBank() != s.world.thisBank)

      useCase("Cheques that are to a bank not on the white list should be rejected") {
        ChequeSituation(world, cheque("1", dodgyDaveId, dodgyDaveAtDodgyBankId, 50)).
          produces(ProcessChequeResult(false, ("processCheque.reject.toBank.notInWhiteList", BankId.dodgyBank))).
          withComment("Dodgy Dave is moving half his funds to a bank that isn't on the accepted list").
          when((s: ChequeSituation) => {
            val c = s.chequeToBank()
            !s.world.acceptedBanks.contains(c)
          })
      }
    }


    useCase("Cheques that will take the customer over the overdraft limit will should be rejected") {
      ChequeSituation(world, cheque("1", dodgyDaveId, richRogerId, 110)).
        produces(ProcessChequeResult(false, "processCheque.reject.noOverdraft")).
        withComment("Dodgy Dave sending more money than he has").
        when((s: ChequeSituation) => s.customerWouldBeOverDrawn && s.customerHasNoOverdraftLimit)

      ChequeSituation(world, cheque("1", richRogerId, richRogerAtHsbcId, 15000)).
        produces(ProcessChequeResult(false, "processCheque.reject.exceedsOverdraftLimit")).
        withComment("Rich Roger sending more money than he has, taking him over his limit").
        when((s: ChequeSituation) => s.customerWouldExceedOverdraftLimit)
    }
    useCase("Cheques that are to to customers in an accepted bank, when the cheque writer has sufficient funds, should be allowed") {
      ChequeSituation(world, cheque("1", dodgyDaveId, richRogerId, 50)).
        produces(ProcessChequeResult(true, "processCheque.accept")).
        withComment("Dodgy Dave sending an OK cheque to someone in this bank").
        when((s: ChequeSituation) => s.world.acceptedBanks.contains(s.chequeToBank()))

      ChequeSituation(world, cheque("1", dodgyDaveId, richRogerAtHsbcId, 50)).
        produces(ProcessChequeResult(true, "processCheque.accept")).
        withComment("Dodgy Dave sending an OK cheque to someone in an accepted bank")
    }
  }

//  def main(args: Array[String]) {
//    println(processCheque(ChequeSituation(world, cheque("1", dodgyDaveId, richRogerAtHsbcId, 50))))
//  }

}
