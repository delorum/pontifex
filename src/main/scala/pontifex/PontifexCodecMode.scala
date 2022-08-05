package pontifex

sealed trait PontifexCodecMode {
  def name(lang: String): String
}

object PontifexCodecMode {
  case object Encoding extends PontifexCodecMode {
    override def name(lang: String): String = if (lang == "ru") "Шифрование" else "Encoding mode"
  }
  case object Decoding extends PontifexCodecMode {
    override def name(lang: String): String =  if (lang == "ru") "Дешифрование" else "Decoding mode"
  }
  case object Key extends PontifexCodecMode {
    override def name(lang: String): String = if (lang == "ru") "Выбор ключа" else "Key selection mode"
  }
}
