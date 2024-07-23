package pontifex.utils

import pontifex.Pontifex

import java.io.{InputStream, InputStreamReader}
import java.nio.charset.Charset
import java.util.Properties

object PontifexUtils {
  def loadFromConfigStream(in: InputStream): (Pontifex, String) = {
    val config = new Properties()
    config.load(new InputStreamReader(in, Charset.forName("UTF-8")))
    val alphabet1 = config.getProperty("alphabet1")
    require(alphabet1 != null, "alphabet required")
    val alphabet2 = config.getProperty("alphabet2", alphabet1)
    val cards = {
      val cardSymbols =
        config.getProperty("cards", alphabet2 + alphabet2 + alphabet2.take(2))
      val cardColors = config.getProperty(
        "cardColors",
        alphabet2.map(_ => 'C') + alphabet2
          .map(_ => 'B') + alphabet2.take(2).map(_ => 'R')
      )
      cardSymbols.zip(cardColors).toArray
    }
    val lang = config.getProperty("lang", "en")
    require(lang == "ru" || lang == "en", "only ru and en lang are supported")

    val replaces = config.getProperty("replaces", "")
    val replacesMap: Map[Char, Char] = loadReplaces(replaces)
    (new Pontifex(alphabet1, alphabet2, cards, replacesMap), lang)
  }

  private def loadReplaces(replacesRaw: String): Map[Char, Char] = {
    val replaces = replacesRaw.replace("\"", "")
    val replacesMap: Map[Char, Char] =
      if (replaces.isEmpty) Map.empty
      else {
        replaces.toSeq.grouped(3).map {
          case Seq(letter, replaceWith, _) => letter -> replaceWith
          case Seq(letter, replaceWith) => letter -> replaceWith
        }.toMap
      }
    replacesMap
  }
}
