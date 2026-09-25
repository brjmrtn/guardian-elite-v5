// ─────────────────────────────────────────────────────────────────────────────
// MODULO — BLOQUE G3: ALERTAS POR TELEGRAM / BLOQUE N: BOT BIDIRECCIONAL (webhook)
// ─────────────────────────────────────────────────────────────────────────────
object TelegramService {
  private val botToken = sys.env.getOrElse("TELEGRAM_BOT_TOKEN", "")
  val chatIdConfigurado: String = sys.env.getOrElse("TELEGRAM_CHAT_ID", "")
  val baseUrl: String = sys.env.getOrElse("PUBLIC_BASE_URL", "https://guardian-elite-v5.onrender.com").stripSuffix("/")

  def configurado: Boolean = botToken.nonEmpty && chatIdConfigurado.nonEmpty

  /** Mensaje al chat configurado (alertas, recordatorios). */
  def enviar(mensaje: String): Unit = enviarA(chatIdConfigurado, mensaje)

  /** Texto plano: los mensajes llevan corchetes y texto libre del usuario, que romperian parse_mode HTML. */
  def enviarA(chatId: String, mensaje: String): Unit = {
    if (botToken.isEmpty || chatId.isEmpty || mensaje.trim.isEmpty) return
    try {
      val body = ujson.write(ujson.Obj("chat_id" -> chatId, "text" -> mensaje.take(4000), "disable_web_page_preview" -> true))
      val conn = new java.net.URL(s"https://api.telegram.org/bot$botToken/sendMessage").openConnection().asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("POST")
      conn.setRequestProperty("Content-Type", "application/json; charset=utf-8")
      conn.setConnectTimeout(10000); conn.setReadTimeout(15000)
      conn.setDoOutput(true)
      conn.getOutputStream.write(body.getBytes("UTF-8"))
      conn.getResponseCode
      conn.disconnect()
    } catch { case _: Exception => () }
  }

  /**
   * Secreto que Telegram reenvia en la cabecera X-Telegram-Bot-Api-Secret-Token de cada update.
   * Se deriva del token del bot para no necesitar otra variable de entorno.
   */
  lazy val secretoWebhook: String =
    if (botToken.isEmpty) ""
    else java.security.MessageDigest.getInstance("SHA-256").digest(s"guardian-webhook:$botToken".getBytes("UTF-8"))
      .map("%02x".format(_)).mkString.take(48)

  /** Registra el webhook una vez al arrancar (GET a setWebhook). */
  def registrarWebhook(): Unit = {
    if (botToken.isEmpty) return
    try {
      val webhookUrl = java.net.URLEncoder.encode(s"$baseUrl/telegram/webhook", "UTF-8")
      val registerUrl = s"https://api.telegram.org/bot$botToken/setWebhook?url=$webhookUrl&secret_token=$secretoWebhook&allowed_updates=%5B%22message%22%5D"
      val conn = new java.net.URL(registerUrl).openConnection().asInstanceOf[java.net.HttpURLConnection]
      conn.setConnectTimeout(10000); conn.setReadTimeout(15000)
      val code = conn.getResponseCode
      println(s"[Telegram] setWebhook -> HTTP $code")
      conn.disconnect()
    } catch { case e: Exception => println(s"[Telegram] setWebhook ERROR: ${e.getMessage.take(200)}") }
  }
}
