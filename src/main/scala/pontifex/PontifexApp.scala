package pontifex

import scala.annotation.tailrec

object PontifexApp extends App {
  //val pontifex = new Pontifex("АБГДЕЖЗИКЛМНОПРСТУФХЦЧШЫЮЯ1256789.,", "АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679")
  //val pontifex = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  //import pontifex._

  /*val s = keySequence(100000, straightDeck)
    s.groupBy(c => c).mapValues(_.length).toSeq.sortBy(_._2).foreach(println)*/
  /*println(keySequence(10, straightDeck).mkString(", ") + " (shouldBe: 4 49 10 24 8 51 44 6 4 33)")
  println(encrypt("AAAAAAAAAA", straightDeck) + " (shouldBe: EXKYI  ZSGEH)")
  println(encrypt("AAAAAAAAAAAAAAA", "FOO") + " (shouldBe: ITHZU  JIWGR  FARMW)")
  println(encrypt("SOLITAIREX", "CRYPTONOMICON") + " (shouldBe: KIRAK SFJAN)")
  println(decrypt("KIRAKSFJAN", "CRYPTONOMICON") + " (shouldBe: SOLITAIREX)")
  println(encrypt("SOLITAIRE", "CRYPTONOMICONN"))
  println(encrypt("SOLITAIRE", "CRYPTONOMICONNN"))
  println(encrypt("SOLITAIRE", "CRYPTONOMICONNNN"))
  println(decrypt("DONOTUSEPC", "CRYPTONOMICON"))*/

  //private val text = "ВСТАЛВСЕМЬ.ВАННА.ВЫШЕЛВВОСЕМЬ.ВИКАВСАДИК.НЕПЛАКАЛА.ЛЕНАСВАРИЛААНЕКАШУ.ПОБРИЛСЯ.ИРРИГАТОР.ПРИЛОЖЕНИЕДЛЯПИТЬЯВОДЫНАЧАСАХ.ПОЕХАЛИСАНЕЙНАРАБОТУ.ЗАЕХАЛИВГАЛАРЕЮ,ВЕРНУЛЧАСЫ........."

  /*val text = {
    scala.io.Source
      .fromFile("open.txt")
      .getLines
      .mkString("\n")
      .replace("\n", "")
      .replace(" ", "")
  }

  println(deck("ключ1").mkString(", "))

  private val encryptedText: String = encrypt(text, "ключ1")
  printText(encryptedText)
  println()
  private val openText: String = decrypt(encryptedText, "ключ1")
  printText(openText)

  def printText(encrypted: String, fivesInRow: Int = 7, println: String => Unit = println): Unit = {
    println(encrypted.grouped(5).grouped(fivesInRow).map(part => part.mkString(" ")).mkString("\n"))
  }

  def matchIndex(text: String): Double = {
    val n = text.length
    val letters = text.groupBy(x => x).mapValues(_.length)
    letters.map {
      case (_, f) =>
        1.0 * f * (f - 1) / (n * (n - 1))
    }.sum
  }

  def printStats(encrypted: String, println: String => Unit = println): Unit = {
    val m = encrypted.groupBy(x => x).mapValues(_.length).toSeq.sortBy(_._2)
    //m.foreach(println)
    val s = s"${m.head} : ${m.last} : ${m.last._2 - m.head._2}"
    println(s)
  }

  printStats(openText)
  printStats(encryptedText)*/

  val p = new Pontifex("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  val deck = p.straightDeck
  val s = p.keySequence(10000, deck)

  s.sliding(3).map { triple =>
    (triple, countTuples(s, triple))
  }.toSeq.sortBy(_._2.length).foreach(println)

  @tailrec
  private def countTuples(seq: List[Int], subSeq: List[Int], res: List[Int] = Nil): List[Int] = {
    if (res.isEmpty) {
      val idx = seq.indexOfSlice(subSeq)
      if (idx == -1) res.reverse
      else countTuples(seq, subSeq, idx :: res)
    } else {
      val idx = seq.indexOfSlice(subSeq, res.head)
      if (idx == -1) res.reverse
      else countTuples(seq, subSeq, idx :: res)
    }
  }
}
