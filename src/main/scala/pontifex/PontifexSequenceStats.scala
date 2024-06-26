package pontifex

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object PontifexSequenceStats extends App {

  private val pontifex: Pontifex =
    PontifexCodec.loadFromConfigStream(this.getClass.getResourceAsStream("/default2.conf"))._1

  private val key = pontifex.randomKey(80)
  println(key)
  private val deck = pontifex.deck(key)

  private val sequenceLength = 10000
  private val pontifexSequence = pontifex.keySequence(sequenceLength, deck)

  FileUtils.save("pontifex.txt") { println =>
    println(pontifexSequence.mkString(" "))
  }

  private val randomSequence = getRandomSequence(sequenceLength)

  private val nonRandomSequence = (1 to 2500).flatMap(_ => pontifexSequence.take(40)).toList

  val key1 = "Б23"
  val deck1 = pontifex.deck(key1)
  val key2 = "Б24"
  val deck2 = pontifex.deck(key2)

  val pontifexSequence1 = pontifex.keySequence(sequenceLength, deck1)
  val pontifexSequence2 = pontifex.keySequence(sequenceLength, deck2)

  val pontifexDiff = pontifexSequence1.zip(pontifexSequence2).map(x => x._1 - x._2)

  private val randomSequence1 = getRandomSequence(sequenceLength)
  private val randomSequence2 = getRandomSequence(sequenceLength)

  val randomDiff = randomSequence1.zip(randomSequence2).map(x => x._1 - x._2)

  FileUtils.save("random.txt") { println =>
    println(randomSequence.mkString(" "))
  }

  private def getRandomSequence(length: Int): List[Int] = {
    (1 to length).map(_ => (math.random() * 70).toInt + 1).toList
  }

  // сколько раз встретилось каждое число
  private def stat1(sequence: List[Int]): Unit = {
    val a = sequence.groupBy(x => x).mapValues(_.length).toSeq.sortBy(_._1)
    a.foreach(println)
    val times = a.map(_._2).sorted
    println(
      s"min=${times.min} " +
        s"avg=${1.0 * times.sum / times.length} " +
        s"q50=${times(times.length / 2)} " +
        s"max=${times.max} " +
        s"diff=${times.max - times.min}"
    )
  }

  // какие числа и сколько раз встретились после каждого числа
  def stat2(sequence: List[Int]): Unit = {
    val uniqueNumbers = sequence.distinct.sorted
    uniqueNumbers.foreach { num =>
      val a = sequence
        .sliding(2)
        .filter(_.head == num)
        .map(_.last)
        .toSeq
        .groupBy(x => x)
        .mapValues(_.length)
        .toSeq
        .sortBy(_._2)
      val times = a.map(_._2).sorted
      println(
        s"$num: length=${times.length} " +
          s"min=${times.min} (${a.filter(_._2 == times.min).mkString(", ")}) " +
          s"avg=${1.0 * times.sum / times.length} " +
          s"q50=${times(times.length / 2)} " +
          s"max=${times.max} (${a.filter(_._2 == times.max).mkString(", ")}) " +
          s"diff=${times.max - times.min}"
      )
    }
  }

  // играем в кости: бросаем 5 кубиков, считаем сумму. Распределение частот выпавших сумм должны подчиняться нормальному распределению
  // данные сохраняются в файле, совместимом с gnuplot: plot 'stat3_pontifex.dat' with linespoints; replot 'stat3_random.dat' with linespoints;
  def stat3(sequence: List[Int], fileName: String = "stat3.dat"): Unit = {
    val a = sequence
      .grouped(5)
      .map { result =>
        result.sum
      }
      .toSeq
      .groupBy(x => x)
      .mapValues(_.length)
      .toSeq
      .sortBy(_._1)
    FileUtils.save(fileName) { println =>
      a.foreach { case (x, y) =>
        println(s"$x $y")
      }
    }
  }

  // смещаем последовательность на разные величины и прикладываем к исходной.
  // Ищем совпадение наибольшей длины, проверяем, сколько таких
  def stat4(sequence: List[Int]): Unit = {
    @tailrec def findMaxSubSequenceLength(
        drop: Int = 0,
        a: List[(Int, Int)],
        curMax: ArrayBuffer[Int] = ArrayBuffer(),
        result: (Int, ArrayBuffer[Int]) = (0, ArrayBuffer())): (Int, ArrayBuffer[Int]) = {
      if (a.isEmpty) result
      else {
        val (x, y) = a.head
        if (x == y) {
          findMaxSubSequenceLength(
            drop + 1,
            a.tail,
            curMax += x,
            if (curMax.length > result._2.length) (drop, curMax) else result
          )
        } else {
          findMaxSubSequenceLength(drop + 1, a.tail, ArrayBuffer(), result)
        }
      }
    }

    val a = (1 until sequence.length).map { offset =>
      val a = sequence.zip(sequence.drop(offset))
      val (drop, result) = findMaxSubSequenceLength(offset, a)
      if (result.length > 3) println(s"($offset, $drop): ${result.mkString(" ")}")
      result
    }
    println(a.map(_.length).distinct.mkString(" "))
    a.groupBy(x => x).mapValues(_.length).toSeq.filter(_._1.length > 3).filter(_._2 > 1).sortBy(_._2).foreach(println)
  }

  // println(decrypt(key))
  println("======stat1:pontifex======")
  stat1(pontifexDiff)
  println("======stat1:random======")
  stat1(randomDiff)
  /*  println("======nonrandom======")
  stat1(nonRandomSequence)*/

  println("======stat2:pontifex======")
  stat2(pontifexDiff)
  println("======stat2:random======")
  stat2(randomDiff)
  /*  println("======nonrandom======")
  stat2(nonRandomSequence)*/

  println("======stat3:pontifex======")
  stat3(pontifexDiff, fileName = "stat3_pontifex.dat")
  println("======stat3:random======")
  stat3(randomDiff, fileName = "stat3_random.dat")
  /*  println("======nonrandom======")
  stat3(nonRandomSequence, fileName = "stat3_nonrandom.dat")*/

  println("======stat4:pontifex======")
  stat4(pontifexDiff)
  println("======stat4:random======")
  stat4(randomDiff)
  /*  println("======nonrandom======")
  stat4(nonRandomSequence)*/
}
