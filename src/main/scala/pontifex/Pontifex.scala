package pontifex

import scala.annotation.tailrec

class Pontifex(val alphabet1: String, val alphabet2: String, cards: Array[(Char, Char)]) {
  def this(alphabet1: String, alphabet2: String) =
    this(
      alphabet1,
      alphabet2,
      (alphabet2.map(c => (c, 'G')).toList :::
        alphabet2.map(c => (c, 'B')).toList :::
        alphabet2.take(2).map(c => (c, 'R')).toList).toArray
    )
  def this(alphabet: String) = this(alphabet, alphabet)

  private val alphabet1Set: Set[Char] = alphabet1.toSet
  private val alphabet2Set: Set[Char] = alphabet2.toSet

  private val letter2number1 = alphabet1.zipWithIndex.map(kv => (kv._1, kv._2 + 1)).toMap
  private val letter2number2 = alphabet2.zipWithIndex.map(kv => (kv._1, kv._2 + 1)).toMap

  private val number2letter1 = letter2number1.map(_.swap)
  private val number2letter2 = letter2number2.map(_.swap)

  private val AlphabetLen = alphabet1.length
  private val DeckLen: Int = AlphabetLen * 2 + 2
  val Joker1: Int = DeckLen - 1
  val Joker2: Int = DeckLen

  require(alphabet1.length == alphabet2.length, "alphabet2 length should be equal to alphabet1 length")
  require(alphabet1.toSet.size == alphabet1.length, "alphabet1 symbols should be unique")
  require(alphabet2.toSet.size == alphabet2.length, "alphabet2 symbols should be unique")
  require(cards.length == alphabet2.length * 2 + 2, "wrong cards length. Should be: alphabet * 2 + 2")
  require(cards.toSet.size == cards.length, "cards must be unique")
  require(cards.map(_._2).forall(c => "RGBCY".contains(c)), "supported card colors: RGBCY")

  def getCard(c: Int): (Char, Char) = {
    cards(c - 1)
  }

  def containsOpen(c: Char): Boolean = {
    alphabet1Set.contains(replace(c.toUpper))
  }

  def containsEncrypted(c: Char): Boolean = {
    alphabet2Set.contains(c.toUpper)
  }

  def step1(deck: List[Int]): List[Int] = {
    val (before, after) = deck.span(_ != Joker1)
    if (after.length > 1) before ::: after.tail.head :: Joker1 :: after.tail.tail
    else Joker1 :: before
  }

  def reverseStep1(deck: List[Int]): List[Int] = {
    step1(deck.reverse).reverse
  }

  def step1Joker2(deck: List[Int]): List[Int] = {
    val (before, after) = deck.span(_ != Joker2)
    if (after.length > 1) before ::: after.tail.head :: Joker2 :: after.tail.tail
    else Joker2 :: before
  }

  def step2(deck: List[Int]): List[Int] = {
    val (before, after) = deck.span(_ != Joker2)
    if (after.length > 2) before ::: after.tail.head :: after.tail.tail.head :: Joker2 :: after.tail.tail.tail
    else if (after.length > 1) before.head :: Joker2 :: before.tail ::: after.tail
    else before.head :: before.tail.head :: Joker2 :: before.tail.tail ::: after.tail
  }

  def reverseStep2(deck: List[Int]): List[Int] = {
    step2(deck.reverse).reverse
  }

  def step3(deck: List[Int]): List[Int] = {
    val before = deck.takeWhile(x => x != Joker1 && x != Joker2)
    val inside =
      deck
        .dropWhile(x => x != Joker1 && x != Joker2)
        .intersect(deck.reverse.dropWhile(x => x != Joker1 && x != Joker2).reverse)
    val after = deck.dropWhile(x => x != Joker1 && x != Joker2).tail.dropWhile(x => x != Joker1 && x != Joker2).tail
    after ::: inside ::: before
  }

  def countCut(amount: Int, deck: List[Int]): List[Int] = {
    val first = deck.take(amount)
    val beforeLast = deck.drop(amount).init
    beforeLast ::: first ::: deck.last :: Nil
  }

  def reverseCountCut(amount: Int, deck: List[Int]): List[Int] = {
    val otherAmount = deck.length - 1 - amount
    val first = deck.take(otherAmount)
    val beforeLast = deck.drop(otherAmount).init
    beforeLast ::: first ::: deck.last :: Nil
  }

  def step4(deck: List[Int]): List[Int] = {
    val last = deck.last
    val amount = last match {
      case Joker1 | Joker2 => Joker1
      case x => x
    }
    countCut(amount, deck)
  }

  def reverseStep4(deck: List[Int]): List[Int] = {
    val last = deck.last
    val amount = last match {
      case Joker1 | Joker2 => Joker1
      case x => x
    }
    reverseCountCut(amount, deck)
  }

  def step5(deck: List[Int]): (Int, List[Int]) = {
    val position = deck.head match {
      case Joker1 | Joker2 => Joker1
      case x => x
    }
    (deck(position), deck)
  }

  def keySequence(num: Int, deck: List[Int]): List[Int] = {
    @tailrec
    def _keySequence(curDeck: List[Int], result: List[Int]): List[Int] = {
      //println(result.length + " : " + curDeck.mkString(","))
      if (result.length == num) result.reverse
      else {
        val (nextSymbol, newDeck) = step5(step4(step3(step2(step1(curDeck)))))
        nextSymbol match {
          case Joker1 | Joker2 => _keySequence(newDeck, result)
          case x => _keySequence(newDeck, x :: result)
        }
      }
    }
    _keySequence(deck, Nil)
  }

  def straightDeck: List[Int] = (1 to DeckLen).toList

  def randomDeck: List[Int] = {
    val straight = (1 to DeckLen).toBuffer
    (for {
      _ <- 1 to DeckLen
      randomCard = straight.remove((math.random * straight.length).toInt)
    } yield randomCard).toList
  }

  def deck(key: String): List[Int] = {
    val prepared = key.toUpperCase.map(replace).filter(c => alphabet1Set.contains(c)).trim
    @tailrec
    def _deck(curDeck: List[Int], keyPart: List[Char]): List[Int] = {
      keyPart match {
        case letter :: rest =>
          val number = letter2number1(letter)
          val newDeck = countCut(number, step4(step3(step2(step1(curDeck)))))
          _deck(newDeck, rest)
        case Nil => curDeck
      }
    }
    if (prepared.isEmpty) straightDeck
    else _deck(straightDeck, prepared.toList)
  }

  @tailrec
  final def mod(a: Int, b: Int): Int = {
    if (a > 0) {
      if (a <= b) a else mod(a - b, b)
    } else mod(a + b, b)
  }

  private def replace(c: Char): Char = {
    c.toUpper match {
      case 'Ъ' => '6'
      case 'Б' => '6'
      case 'Ь' => '6'
      case 'В' => '8'
      case 'Ё' => 'Е'
      case 'Й' => 'И'
      case 'Э' => 'З'
      case '3' => 'З'
      case 'Щ' => 'Ш'
      case '0' => 'О'
      case '4' => 'Ч'
      case x => x
    }
  }

  def encryptSymbol(d: Char, s: Int): Char = {
    val r = mod(letter2number1(replace(d.toUpper)) + s, AlphabetLen)
    val l = number2letter2(r)
    l
  }

  def decryptSymbol(d: Char, s: Int): Char = {
    val r = mod(letter2number2(d.toUpper) - s, AlphabetLen)
    val l = number2letter1(r)
    l
  }

  def getNumberForOpenSymbol(d: Char): Option[Int] = {
    val prepared = replace(d.toUpper)
    if (alphabet1Set.contains(prepared)) Some(letter2number1(prepared))
    else None
  }

  def getRandomEncryptedSymbol: Char = {
    alphabet2((math.random() * alphabet2.length).toInt)
  }

  def getRandomOpenSymbol: Char = {
    alphabet1((math.random() * alphabet1.length).toInt)
  }

  def encrypt(message: String, deck: List[Int]): String = {
    val prepared = message.toUpperCase.map(replace).filter(c => alphabet1Set.contains(c)).trim
    val sequence = keySequence(prepared.length, deck)
    (for {
      (d, s) <- prepared.zip(sequence)
    } yield {
      encryptSymbol(d, s)
    }).mkString
  }

  def encrypt(message: String, key: String): String = {
    encrypt(message, deck(key))
  }

  def decrypt(cipherText: String, deck: List[Int]): String = {
    val prepared = cipherText.toUpperCase().filter(c => alphabet2Set.contains(c))
    val sequence = keySequence(prepared.length, deck)
    (for {
      (d, s) <- prepared.zip(sequence)
    } yield {
      decryptSymbol(d, s)
    }).mkString
  }

  def decrypt(cipherText: String, key: String): String = {
    decrypt(cipherText, deck(key))
  }
}
