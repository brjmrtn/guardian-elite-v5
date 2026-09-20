import java.util.Properties
import jakarta.mail._
import jakarta.mail.internet._
import jakarta.mail.util.ByteArrayDataSource
import jakarta.activation.DataHandler

// ─────────────────────────────────────────────────────────────────────────────
// MODULO — SISTEMA DE BACKUPS AUTOMATICOS: envio del dump SQL por email
// ─────────────────────────────────────────────────────────────────────────────
object BackupService {
  def enviarPorEmail(destino: String, filename: String, contenido: Array[Byte]): Unit = {
    val smtpHost = sys.env.getOrElse("SMTP_HOST", "smtp.gmail.com")
    val smtpPort = sys.env.getOrElse("SMTP_PORT", "587")
    val smtpUser = sys.env.getOrElse("SMTP_USER", "")
    val smtpPass = sys.env.getOrElse("SMTP_PASS", "")

    if (smtpUser.isEmpty || smtpPass.isEmpty) {
      println("[Backup Email] SMTP no configurado — saltando envío por email")
      return
    }

    val props = new Properties()
    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.starttls.enable", "true")
    props.put("mail.smtp.host", smtpHost)
    props.put("mail.smtp.port", smtpPort)

    val session = Session.getInstance(props, new Authenticator() {
      override def getPasswordAuthentication =
        new PasswordAuthentication(smtpUser, smtpPass)
    })

    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(smtpUser))
    message.setRecipients(Message.RecipientType.TO, destino)
    message.setSubject(s"Guardian Elite — Backup semanal $filename")

    val multipart = new MimeMultipart()

    // Cuerpo del email
    val textPart = new MimeBodyPart()
    textPart.setText(
      s"Backup automático semanal de Guardian Elite.\n\n" +
      s"Archivo: $filename\n" +
      s"Tamaño: ${contenido.length / 1024}KB\n\n" +
      s"Este backup contiene todos los datos de seguimiento de Héctor.\n" +
      s"Guárdalo en un lugar seguro."
    )
    multipart.addBodyPart(textPart)

    // Adjunto SQL
    val attachPart = new MimeBodyPart()
    attachPart.setDataHandler(new DataHandler(
      new ByteArrayDataSource(contenido, "application/sql")))
    attachPart.setFileName(filename)
    multipart.addBodyPart(attachPart)

    message.setContent(multipart)
    Transport.send(message)
    println(s"[Backup Email] Enviado a $destino")
  }
}
