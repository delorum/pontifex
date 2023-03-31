package pontifex

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object PontifexSequenceStats extends App {

  private val pontifex: Pontifex =
    PontifexCodec.loadFromConfigStream(this.getClass.getResourceAsStream("/default2.conf"))._1

  private val key = pontifex.randomKey(80)
  println(key)
  private val deck = pontifex.deck(key)

  private val sequenceLength = 100000
  private val pontifexSequence = pontifex.keySequence(sequenceLength, deck)

  FileUtils.save("pontifex.txt") { println =>
    println(pontifexSequence.mkString(" "))
  }

  private val randomSequence = getRandomSequence(sequenceLength)

  private val nonRandomSequence = (1 to 2500).flatMap(_ => pontifexSequence.take(40)).toList

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
  // данные сохраняются в файле, совместимом с gnuplot: pplot 'stat3.dat' with linespoints; replot 'stat3_random.dat' with linespoints;
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
  println("======pontifex======")
  stat1(pontifexSequence)
  println("======random======")
  stat1(randomSequence)
  /*  println("======nonrandom======")
  stat1(nonRandomSequence)*/

  println("======pontifex======")
  stat2(pontifexSequence)
  println("======random======")
  stat2(randomSequence)
  /*  println("======nonrandom======")
  stat2(nonRandomSequence)*/

  println("======pontifex======")
  stat3(pontifexSequence, fileName = "stat3_pontifex.dat")
  println("======random======")
  stat3(randomSequence, fileName = "stat3_random.dat")
  /*  println("======nonrandom======")
  stat3(nonRandomSequence, fileName = "stat3_nonrandom.dat")*/

  println("======pontifex======")
  stat4(pontifexSequence)
  println("======random======")
  stat4(randomSequence)
  /*  println("======nonrandom======")
  stat4(nonRandomSequence)*/
}
