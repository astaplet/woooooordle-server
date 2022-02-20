package woooooordle.server.objects

sealed trait StartMode
object StartModes {
  case object CLI extends StartMode
  case object Server extends StartMode
  case object Empty extends StartMode
}

case class AppArgs(startMode: StartMode)
