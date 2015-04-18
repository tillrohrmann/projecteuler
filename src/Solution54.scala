import scala.io.Source

import Solution54.Card.Card
import Solution54.Color.Color

object Solution54 {
  object Card extends Enumeration {
    type Card = Value
    val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
  }

  object Color extends Enumeration {
    type Color = Value
    val Diamond, Heart, Spade, Cross = Value
  }

  trait Rank extends Ordered[Rank]{
    val rank: Int

    def compare(hand: Rank): Int
  }

  case class HighCard(card: (Card, Color)) extends Rank {
    val rank = 1

    def compare(hand: Rank): Int = {
      hand match {
        case HighCard(hC) => {
          card._1.id - hC._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class OnePair(pair: List[(Card, Color)]) extends Rank {
    val rank = 2

    override def compare(hand: Rank): Int = {
      hand match {
        case OnePair(p) => {
          pair.head._1.id - p.head._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class TwoPairs(high: List[(Card, Color)], low: List[(Card, Color)]) extends Rank {
    val rank = 3

    override def compare(hand: Rank): Int = {
      hand match {
        case TwoPairs(hp, lp) => {
          if(high.head._1.id - hp.head._1.id == 0){
            low.head._1.id - lp.head._1.id
          } else {
            high.head._1.id - hp.head._1.id
          }
        }
        case x => rank - x.rank
      }
    }
  }

  case class ThreeOfAKind(three: List[(Card, Color)]) extends Rank {
    val rank = 4

    override def compare(hand: Rank): Int = {
      hand match {
        case ThreeOfAKind(t) => {
          three.head._1.id - t.head._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class Straight(straight: List[(Card, Color)]) extends Rank {
    val rank = 5

    override def compare(hand: Rank): Int = {
      hand match {
        case Straight(s) => {
          straight.last._1.id - s.last._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class Flush(flush: List[(Card, Color)]) extends Rank {
    val rank = 6

    override def compare(hand: Rank): Int = {
      hand match {
        case Flush(f) => {
          flush.last._1.id - f.last._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class FullHouse(triple: List[(Card, Color)], pair: List[(Card, Color)]) extends Rank {
    val rank = 7

    override def compare(hand: Rank): Int = {
      hand match {
        case FullHouse(t, p) => {
          if(triple.head._1.id - t.head._1.id == 0){
            pair.head._1.id - p.head._1.id
          } else {
            triple.head._1.id - t.head._1.id
          }
        }

        case x => rank - x.rank
      }
    }
  }

  case class FourOfAKind(four: List[(Card, Color)]) extends Rank {
    val rank = 8

    override def compare(hand: Rank): Int = {
      hand match {
        case FourOfAKind(f) => {
          four.head._1.id - f.head._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class StraightFlush(flush: List[(Card, Color)]) extends Rank {
    val rank = 9

    override def compare(hand: Rank): Int = {
      hand match {
        case StraightFlush(f) => {
          flush.last._1.id - f.last._1.id
        }

        case x => rank - x.rank
      }
    }
  }

  case class RoyalFlush(flush: List[(Card, Color)]) extends Rank {
    val rank = 10

    override def compare(hand: Rank): Int = {
      hand match {
        case RoyalFlush(f) => {
          0
        }

        case x => rank - x.rank
      }
    }
  }

  import Card._

  case class Hand(cards: List[(Card, Color)])

  case class RH(rank: Rank, rest: List[(Card, Color)])

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("p054_poker.txt").getLines()
//    val lines = List("5H 5C 6S 7S KD 2C 3S 8S 8D TD", "5D 8C 9S JS AC 2C 5C 7D 8S QH",
//"2D 9C AS AH AC 3D 6D 7D TD QD", "4D 6S 9H QH QC 3D 6D 7H QD QS", "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")
    val hands = lines map {
      line => {
        val cards = line.split(" ")

        val playerOne = createHand(cards.slice(0, 5))
        val playerTwo = createHand(cards.slice(5, cards.length))

        (playerOne, playerTwo)
      }
    }

    val rankedHands = hands map {
      case (left, right) => {
        (createRankedHand(left), createRankedHand(right))
      }
    }

    val wins = rankedHands map {
      case (a, b) => (a, b, checkWin(a, b))
    } toList

    println(wins.mkString("\n"))

    println(wins.map(_._3).reduce(_+_))
  }

  def checkWin(a: RH, b: RH): Int = {
    if(a.rank > b.rank) {
      1
    } else if(b.rank > a.rank) {
      0
    } else {
      val result = a.rest.reverse.zip(b.rest.reverse).dropWhile{
        case (l,r) => l == r
      }

      if(result.isEmpty) {
        if(a.rest.length < b.rest.length){
          0
        } else if(a.rest.length > b.rest.length) {
          1
        } else {
          0
        }
      } else {
        val (l,r) = result.head

        if(l < r) {
          0
        } else if(l>r) {
          1
        } else {
          0
        }
      }
    }
  }

  def createHand(cards: Array[String]): Hand = {
    val mappedCards = cards.map{
      pair => (getCard(pair(0)), getColor(pair(1)))
    }

    Hand(mappedCards.toList)
  }

  def createRankedHand(hand: Hand): RH = {
    val sortedCards = hand.cards.sortBy(_._1)

    if(containsRoyalFlush(sortedCards)) {
      RH(RoyalFlush(sortedCards), List())
    }
    else if (containsStraightFlush(sortedCards)) {
      RH(StraightFlush(sortedCards), List())
    } else if(containsFourOfAKind(sortedCards)) {
      val (four, rest) = extractFourOfAKind(sortedCards)
      RH(FourOfAKind(four), rest)
    } else if(containsFullHouse(sortedCards)) {
      val groups = sortedCards.groupBy(_._1)

      val triple = groups.filter(_._2.size == 3).values.reduce(_ ::: _)
      val pair = groups.filter(_._2.size == 2).values.reduce(_ ::: _)

      RH(FullHouse(triple, pair), List())
    } else if(containsFlush(sortedCards)) {
      RH(Flush(sortedCards), List())
    } else if(containsStraight(sortedCards)) {
      RH(Straight(sortedCards), List())
    } else if(containsThreeOfAKind(sortedCards)) {
      val (three, rest) = extractThreeOfAKind(sortedCards)
      RH(ThreeOfAKind(three), rest)
    } else if(containsTwoPairs(sortedCards)) {

      val groups = sortedCards.groupBy(_._1)

      val pairs = groups.filter(_._2.size == 2).values.toList.sortBy(_.head._1)

      val rest = groups.filterNot(_._2.size == 2).values.reduce(_:::_)

      RH(TwoPairs(pairs(1), pairs(0)), rest)
    } else if(containsOnePair(sortedCards)) {
      val (onePair, rest) = extractOnePair(sortedCards)
      RH(OnePair(onePair), rest)
    } else{
      RH(HighCard(sortedCards.last), sortedCards.init)
    }
  }

  def containsRoyalFlush(cards: List[(Card, Color)]): Boolean = {
    val head = cards.head

    cards.tail.forall( head._2 == _._2) && head._1 == Ten && cards.last._1 == Ace
  }

  def containsStraightFlush(cards: List[(Card, Color)]): Boolean = {
    val head = cards.head

    val straight = cards.tail.zip(cards.init).map{case (a,b) => a._1.id - b._1.id}.forall(_ == 1)

    cards.tail.forall(head._2 == _._2) && straight
  }

  def containsFourOfAKind(cards: List[(Card, Color)]): Boolean = {
    cards.groupBy(_._1).exists(x => x._2.size == 4)
  }

  def extractFourOfAKind(cards: List[(Card, Color)]): (List[(Card, Color)], List[(Card, Color)]) = {
    val (four, rest) = cards.groupBy(_._1).partition(_._2.length == 4)

    val f = four.values.reduce(_ ::: _)
    val r = rest.values.reduce(_:::_).sortBy(_._1)

    (f, r)
  }

  def containsFullHouse(cards: List[(Card, Color)]): Boolean = {
    val groups = cards.groupBy(_._1)

    groups.size == 2 && groups.forall(x => x._2.size == 3 || x._2.size == 2)
  }

  def containsFlush(cards: List[(Card, Color)]): Boolean = {
    val head = cards.head

    cards.tail.forall(_._2 == head._2)
  }

  def containsStraight(cards: List[(Card, Color)]): Boolean = {
    val last = cards.last
    val t = cards.init

    val aceStraight = last._1 == Ace && t.tail.zip(t.init).map{case (a,b) => a._1.id - b._1.id}.forall(_ == 1)

    cards.tail.zip(cards.init).map{case (a,b) => a._1.id - b._1.id}.forall(_ == 1) ||
      aceStraight
  }

  def containsThreeOfAKind(cards: List[(Card, Color)]): Boolean = {
    cards.groupBy(_._1).exists(_._2.size == 3)
  }

  def extractThreeOfAKind(cards: List[(Card, Color)]): (List[(Card, Color)], List[(Card, Color)]) = {
    val groups = cards.groupBy(_._1)

    val (t, r) = groups.partition(_._2.size == 3)

    (t.values.reduce(_:::_), r.values.reduce(_:::_).sortBy(_._1))
  }

  def containsTwoPairs(cards: List[(Card, Color)]): Boolean = {
    cards.groupBy(_._1).filter(_._2.size == 2).size == 2
  }

  def containsOnePair(cards: List[(Card, Color)]): Boolean = {
    var lastCard = cards.head._1
    var pairFound = false

    cards.tail.foreach {
      x => {
        if(x._1 == lastCard) {
          pairFound = true
        }

        lastCard = x._1
      }
    }

    pairFound
  }

  def extractOnePair(cards: List[(Card, Color)]): (List[(Card, Color)], List[(Card, Color)]) = {
    val groups = cards.groupBy(_._1)

    val (p, r) = groups.partition(_._2.size == 2)

    (p.values.reduce(_ ::: _), r.values.reduce(_ ::: _).sortBy(_._1))
  }

  def getCard(card: Char): Card = {
    import Card._

    card match {
      case '2' => Two
      case '3' => Three
      case '4' => Four
      case '5' => Five
      case '6' => Six
      case '7' => Seven
      case '8' => Eight
      case '9' => Nine
      case 'T' => Ten
      case 'J' => Jack
      case 'Q' => Queen
      case 'K' => King
      case 'A' => Ace
    }
  }

  def getColor(color: Char): Color = {
    import Color._

    color match {
      case 'D' => Diamond
      case 'H' => Heart
      case 'S' => Spade
      case 'C' => Cross
    }
  }
}
