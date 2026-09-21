// ─────────────────────────────────────────────────────────────────────────────
// MODULO — BLOQUE G3: ALERTAS POR TELEGRAM
// ─────────────────────────────────────────────────────────────────────────────
object TelegramService {
  private val botToken = sys.env.getOrElse("TELEGRAM_BOT_TOKEN", "")
  private val chatId   = sys.env.getOrElse("TELEGRAM_CHAT_ID", "")

  def enviar(mensaje: String): Unit = {
    if (botToken.isEmpty || chatId.isEmpty) return
    try {
      val url = s"https://api.telegram.org/bot$botToken/sendMessage"
      val body = s"""{"chat_id":"$chatId","text":"${mensaje.replace("\"", "'")}","parse_mode":"HTML"}"""
      val conn = new java.net.URL(url).openConnection().asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("POST")
      conn.setRequestProperty("Content-Type", "application/json")
      conn.setDoOutput(true)
      conn.getOutputStream.write(body.getBytes("UTF-8"))
      conn.getResponseCode
      conn.disconnect()
    } catch { case _: Exception => () }
  }
}
