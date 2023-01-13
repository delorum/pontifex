package pontifex

sealed trait PontifexCodecMode {
  def name(lang: String): String
  def hotkey(lang: String): String
}

object PontifexCodecMode {

  case object Encoding extends PontifexCodecMode {
    override def name(lang: String): String = if (lang == "ru") "Шифрование" else "Encoding mode"
    override def hotkey(lang: String): String = if (lang == "ru") "Ctrl-E:Деш" else "Ctrl-E:Dec"
  }

  case object Decoding extends PontifexCodecMode {
    override def name(lang: String): String = if (lang == "ru") "Дешифрование" else "Decoding mode"
    override def hotkey(lang: String): String = if (lang == "ru") "Ctrl-E:Шиф" else "Ctrl-E:Enc"
  }

  case object Key extends PontifexCodecMode {
    override def name(lang: String): String = if (lang == "ru") "Выбор ключа" else "Key selection mode"
    override def hotkey(lang: String): String = if (lang == "ru") "Ctrl-E:Шиф" else "Ctrl-E:Enc"
  }
}
