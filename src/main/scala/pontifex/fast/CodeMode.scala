package pontifex.fast

sealed trait CodeMode

object CodeMode {
  case object Unknown extends CodeMode
  case object Encode extends CodeMode
  case object Decode extends CodeMode
}
