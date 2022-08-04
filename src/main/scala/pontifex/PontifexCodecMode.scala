package pontifex

sealed trait PontifexCodecMode {
  def name: String
}

object PontifexCodecMode {
  case object Encoding extends PontifexCodecMode {
    override def name: String = "шифрование"
  }
  case object Decoding extends PontifexCodecMode {
    override def name: String = "дешифрование"
  }
  case object Key extends PontifexCodecMode {
    override def name: String = "выбор ключа"
  }
}
