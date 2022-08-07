package pontifex

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

/**
 * https://www.schneier.com/wp-content/uploads/2015/12/sol-test.txt
 */
@RunWith(classOf[JUnitRunner])
class PontifexTest extends AnyFunSuite {
  test("straight deck, AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.straightDeck
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("4 49 10 24 8 51 44 6 4 33 20 39 19 34 42"))
    assert(p.encrypt(message, deck) == withoutSpaces("EXKYI ZSGEH UNTIQ"))
    assert(p.decrypt(withoutSpaces("EXKYI ZSGEH UNTIQ"), deck) == message)
  }

  test("deck(f), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val key = "f"
    val deck = p.deck(key)
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("49 24 8 46 16 1 12 33 10 10 9 27 4 32 24"))
    assert(p.encrypt(message, deck) == withoutSpaces("XYIUQ BMHKK JBEGY"))
    assert(p.encrypt(message, key) == withoutSpaces("XYIUQ BMHKK JBEGY"))
    assert(p.decrypt(withoutSpaces("XYIUQ BMHKK JBEGY"), deck) == message)
    assert(p.decrypt(withoutSpaces("XYIUQ BMHKK JBEGY"), key) == message)
  }

  test("deck(fo), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val key = "fo"
    val deck = p.deck(key)
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("19 46 9 24 12 1 4 43 11 32 23 39 29 34 22"))
    assert(p.encrypt(message, deck) == withoutSpaces("TUJYM BERLG XNDIW"))
    assert(p.encrypt(message, key) == withoutSpaces("TUJYM BERLG XNDIW"))
    assert(p.decrypt(withoutSpaces("TUJYM BERLG XNDIW"), deck) == message)
    assert(p.decrypt(withoutSpaces("TUJYM BERLG XNDIW"), key) == message)
  }

  test("deck(foo), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("foo")
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("8 19 7 25 20 9 8 22 32 43 5 26 17 38 48"))
    assert(p.encrypt(message, deck) == withoutSpaces("ITHZU JIWGR FARMW"))
    assert(p.decrypt(withoutSpaces("ITHZU JIWGR FARMW"), deck) == message)
  }

  test("deck(a), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val key = "a"
    val deck = p.deck(key)
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("49 14 3 26 11 32 18 2 46 37 34 42 13 18 28"))
    assert(p.encrypt(message, deck) == withoutSpaces("XODAL GSCUL IQNSC"))
    assert(p.encrypt(message, key) == withoutSpaces("XODAL GSCUL IQNSC"))
    assert(p.decrypt(withoutSpaces("XODAL GSCUL IQNSC"), deck) == message)
    assert(p.decrypt(withoutSpaces("XODAL GSCUL IQNSC"), key) == message)
  }

  test("deck(aa), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val key = "aa"
    val deck = p.deck(key)
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("14 7 32 22 38 23 23 2 26 8 12 2 34 16 15"))
    assert(p.encrypt(message, deck) == withoutSpaces("OHGWM XXCAI MCIQP"))
    assert(p.encrypt(message, key) == withoutSpaces("OHGWM XXCAI MCIQP"))
    assert(p.decrypt(withoutSpaces("OHGWM XXCAI MCIQP"), deck) == message)
    assert(p.decrypt(withoutSpaces("OHGWM XXCAI MCIQP"), key) == message)
  }

  test("deck(aaa), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("aaa")
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("3 28 18 42 24 33 1 16 51 39 6 29 43 46 45"))
    assert(p.encrypt(message, deck) == withoutSpaces("DCSQY HBQZN GDRUT"))
    assert(p.decrypt(withoutSpaces("DCSQY HBQZN GDRUT"), deck) == message)
  }

  test("deck(b), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("b")
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("49 16 4 30 12 40 8 19 37 25 47 29 18 16 18"))
    assert(p.encrypt(message, deck) == withoutSpaces("XQEEM OITLZ VDSQS"))
    assert(p.decrypt(withoutSpaces("XQEEM OITLZ VDSQS"), deck) == message)
  }

  test("deck(bc), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("bc")
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("16 13 32 17 10 42 34 7 2 37 6 48 44 28 4"))
    assert(p.encrypt(message, deck) == withoutSpaces("QNGRK QIHCL GWSCE"))
    assert(p.decrypt(withoutSpaces("QNGRK QIHCL GWSCE"), deck) == message)
  }

  test("deck(bcd), AAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("bcd")
    val message = "AAAAAAAAAAAAAAA"
    assert(p.keySequence(message.length, deck) == toListInt("5 38 20 27 50 1 38 26 49 33 39 42 49 2 35"))
    assert(p.encrypt(message, deck) == withoutSpaces("FMUBY BMAXH NQXCJ"))
    assert(p.decrypt(withoutSpaces("FMUBY BMAXH NQXCJ"), deck) == message)
  }

  test("deck(cryptonomicon), AAAAAAAAAAAAAAAAAAAAAAAAA") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("cryptonomicon")
    val message = "AAAAAAAAAAAAAAAAAAAAAAAAA"
    assert(p.encrypt(message, deck) == withoutSpaces("SUGSR SXSWQ RMXOH IPBFP XARYQ"))
    assert(p.decrypt(withoutSpaces("SUGSR SXSWQ RMXOH IPBFP XARYQ"), deck) == message)
  }

  test("deck(cryptonomicon), SOLITAIREX") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val deck = p.deck("cryptonomicon")
    val message = "SOLITAIREX"
    assert(p.encrypt(message, deck) == withoutSpaces("KIRAK SFJAN"))
    assert(p.decrypt(withoutSpaces("KIRAK SFJAN"), deck) == message)
  }

  test("containsOpen") {
    val alphabet1 = "АБГДЕЖЗИКЛМНОПРСТУФХЦЧШЫЮЯ1256789.,"
    val alphabet2 = "АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679"
    val p = new Pontifex(alphabet1, alphabet2)
    alphabet1.foreach { c =>
      assert(p.containsOpen(c))
    }
    assert(!p.containsOpen('W'))
  }

  test("containsEncrypted") {
    val alphabet1 = "АБГДЕЖЗИКЛМНОПРСТУФХЦЧШЫЮЯ1256789.,"
    val alphabet2 = "АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679"
    val p = new Pontifex(alphabet1, alphabet2)
    alphabet2.foreach { c =>
      assert(p.containsEncrypted(c))
    }
    assert(!p.containsEncrypted('W'))
  }

  test("randomDeck") {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val p = new Pontifex(alphabet)
    val deck = p.randomDeck
    val message = "TEST"
    val encrypted = p.encrypt(message, deck)
    val decrypted = p.decrypt(encrypted, deck)
    assert(decrypted == message)
  }

  test("getRandomOpenSymbol") {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val p = new Pontifex(alphabet)
    (1 to 100).foreach { _ =>
      assert(alphabet.contains(p.getRandomOpenSymbol))
    }
  }

  test("getRandomEncryptedSymbol") {
    val alphabet1 = "АБГДЕЖЗИКЛМНОПРСТУФХЦЧШЫЮЯ1256789.,"
    val alphabet2 = "АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679"
    val p = new Pontifex(alphabet1, alphabet2)
    (1 to 100).foreach { _ =>
      assert(alphabet2.contains(p.getRandomEncryptedSymbol))
    }
  }

  test("getNumberForOpenSymbol: None") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    assert(p.getNumberForOpenSymbol(1).isEmpty)
  }

  test("reverse") {
    val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val key = "cryptonomicon"
    val deck = p.deck(key)
    val resultDeck = key.reverse.foldLeft(deck) {case (deck, c) =>
      p.reverseStep1(p.reverseStep2(p.step3(p.reverseStep4(p.reverseCountCut(p.getNumberForOpenSymbol(c).get, deck)))))
    }
    assert(resultDeck == p.straightDeck)
  }

  private def withoutSpaces(s: String): String = {
    s.replace(" ", "")
  }

  private def toListInt(s: String): List[Int] = {
    s.split(" ").toList.map(_.toInt)
  }
}
