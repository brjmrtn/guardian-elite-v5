import cask._

/*
 * REGLAS DE RUTAS CASK — LEER ANTES DE AÑADIR CUALQUIER RUTA
 *
 * 1. NUNCA mezclar rutas literales y wildcards al mismo nivel de árbol
 *    (para el mismo método HTTP).
 *    MAL:  /match-center/clima    (literal)
 *          /match-center/:id/card (wildcard)
 *    BIEN: /match-center/clima    (literal)
 *          /partido-card/:id      (prefijo diferente para el wildcard)
 *
 * 2. Si necesitas añadir una ruta nueva con wildcard bajo un prefijo
 *    que ya tiene rutas literales: usar un prefijo diferente para
 *    la ruta con wildcard.
 *
 * 3. Ejemplos de prefijos seguros para wildcards:
 *    /partido/:id, /entreno/:id, /hito/:id, /correlacion/:id
 *
 * El conflicto no lo detecta el compilador: solo aparece al arrancar el servidor.
 * Arrancar el servidor en local tras añadir rutas.
 */

// GUARDIAN ELITE -- Punto de entrada del servidor.
//
// Logica por controlador:
//   - AuthController       -> /login, /logout
//   - DashboardController  -> /
//   - MatchController      -> /match-center, /match/:id, /video/:id, /tournament/:id
//   - BioController        -> /bio/*, /oracle, /distribution
//   - HistoryController    -> /history, /scouting
//   - CareerController     -> /career/*, /gear, /penalties, /career/legacy
//   - AdminController      -> /admin/*, /settings, /tactics
//
// Utilidades compartidas -> SharedLayout

object GuardianServer extends cask.Main {

  DatabaseManager.initDB()
  AmateurDatabaseManager.initTables()

  // ── AUTO-SYNC ENGINE ────────────────────────────────────────────────────────
  // Arranca en background tras 10s de startup.
  // Verifica todos los usuarios con liga configurada y sincroniza si >24h sin sync.
  AmateurDatabaseManager.startAutoSyncEngine()

  // ── BACKUP ENGINE ────────────────────────────────────────────────────────────
  // Se ejecuta automáticamente cada domingo a las 3:00 AM.
  // Genera un dump SQL y lo guarda en la propia BD; lo envía por email si esta configurado.
  val backupExecutor = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "backup-engine")
    t.setDaemon(true)
    t
  })

  // Calcula milisegundos hasta el proximo domingo a las 3:00 AM
  val ahora = java.time.LocalDateTime.now()
  val proximoDomingo = ahora
    .`with`(java.time.temporal.TemporalAdjusters.next(java.time.DayOfWeek.SUNDAY))
    .withHour(3).withMinute(0).withSecond(0)
  val msHastaBackup = java.time.Duration.between(ahora, proximoDomingo).toMillis

  backupExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try {
          println(s"[Guardian Backup] Iniciando backup semanal ${java.time.LocalDate.now()}")
          val sql = DatabaseManager.generarBackupSQL()
          val bytes = sql.getBytes("UTF-8")
          val fecha = java.time.LocalDate.now().toString
          val filename = s"guardian_backup_$fecha.sql"

          // Destino 1: guardar en tabla de backups en la propia BD
          DatabaseManager.guardarBackupEnBD(sql, fecha)

          // Destino 2: enviar por email si BACKUP_EMAIL esta configurado
          val emailDest = sys.env.getOrElse("BACKUP_EMAIL", "")
          if (emailDest.nonEmpty) {
            BackupService.enviarPorEmail(emailDest, filename, bytes)
          }

          println(s"[Guardian Backup] Backup completado: ${bytes.length / 1024}KB")
        } catch { case e: Exception =>
          println(s"[Guardian Backup] ERROR: ${e.getMessage.take(200)}")
        }
      }
    },
    msHastaBackup,
    7 * 24 * 60 * 60 * 1000L, // cada 7 dias
    java.util.concurrent.TimeUnit.MILLISECONDS
  )

  // ── RFFM BENCHMARK ENGINE ────────────────────────────────────────────────────
  // Se ejecuta automaticamente cada lunes a las 6:00 AM, antes del resumen semanal.
  // El sync es lento (muchas URLs a rffm.es) — siempre en un hilo de fondo, nunca
  // bloquea el servidor. Si falla, se loguea y se continua con los ultimos datos.
  val rffmExecutor = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "rffm-benchmark-engine")
    t.setDaemon(true)
    t
  })
  val ahoraRffm = java.time.LocalDateTime.now()
  val proximoLunesRffm = ahoraRffm
    .`with`(java.time.temporal.TemporalAdjusters.next(java.time.DayOfWeek.MONDAY))
    .withHour(6).withMinute(0).withSecond(0)
  val msHastaRffm = java.time.Duration.between(ahoraRffm, proximoLunesRffm).toMillis

  rffmExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try {
          println(s"[RFFM Benchmark] Iniciando sync semanal ${java.time.LocalDate.now()}")
          val resultado = DatabaseManager.syncRFFMBenchmark()
          println(s"[RFFM Benchmark] $resultado")
        } catch { case e: Exception =>
          println(s"[RFFM Benchmark] ERROR: ${e.getMessage.take(200)}")
        }
      }
    },
    msHastaRffm,
    7 * 24 * 60 * 60 * 1000L, // cada 7 dias
    java.util.concurrent.TimeUnit.MILLISECONDS
  )

  // ── RESUMEN SEMANAL ENGINE (BLOQUE G) ───────────────────────────────────────
  // Se ejecuta automáticamente cada lunes a las 8:00 AM. SQL puro, sin Gemini.
  val resumenExecutor = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "resumen-engine")
    t.setDaemon(true)
    t
  })
  val ahora2 = java.time.LocalDateTime.now()
  val proximoLunes = ahora2
    .`with`(java.time.temporal.TemporalAdjusters.next(java.time.DayOfWeek.MONDAY))
    .withHour(8).withMinute(0).withSecond(0)
  val msHastaResumen = java.time.Duration.between(ahora2, proximoLunes).toMillis

  resumenExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try {
          val html = DatabaseManager.generarResumenSemanal()
          val emailDest = sys.env.getOrElse("BACKUP_EMAIL", "")
          if (emailDest.nonEmpty) {
            BackupService.enviarResumenEmail(emailDest, html)
          }
          // BLOQUE G3: mismo contenido del resumen, en texto plano, por Telegram
          // (sin la alerta de carga: el bot ya la manda como mensaje propio a las 8:00, BLOQUE R)
          val textoPlano = DatabaseManager.generarResumenSemanal(incluirAlertaCarga = false)
            .replaceAll("<[^>]+>", " ").replaceAll("\\s+", " ").trim
          TelegramService.enviar(textoPlano)
        } catch { case e: Exception =>
          println(s"[Resumen Email] ERROR: ${e.getMessage.take(200)}")
        }
      }
    },
    msHastaResumen,
    7 * 24 * 60 * 60 * 1000L,
    java.util.concurrent.TimeUnit.MILLISECONDS
  )

  // ── AVISO DIA DE PARTIDO POR TELEGRAM (BLOQUE G3) ───────────────────────────
  // Se comprueba TODOS los dias a las 9:00 AM — el dia de partido nunca esta hardcodeado,
  // se lee dinamicamente de weekly_structure en cada ejecucion (soporta cambios desde /settings).
  val avisoPartidoExecutor = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "aviso-partido-engine")
    t.setDaemon(true)
    t
  })
  val ahora3 = java.time.LocalDateTime.now()
  var proximasNueve = ahora3.withHour(9).withMinute(0).withSecond(0)
  if (!proximasNueve.isAfter(ahora3)) proximasNueve = proximasNueve.plusDays(1)
  val msHastaAvisoPartido = java.time.Duration.between(ahora3, proximasNueve).toMillis

  avisoPartidoExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try {
          DatabaseManager.getAvisoDiaPartido().foreach(TelegramService.enviar)
        } catch { case e: Exception =>
          println(s"[Aviso Partido] ERROR: ${e.getMessage.take(200)}")
        }
      }
    },
    msHastaAvisoPartido,
    24 * 60 * 60 * 1000L, // cada 24h
    java.util.concurrent.TimeUnit.MILLISECONDS
  )

  // ── TELEGRAM BOT BIDIRECCIONAL (BLOQUE N) ──────────────────────────────────
  // Registra el webhook una vez al arrancar y revisa cada 10 min si toca algun recordatorio
  // (la hora se evalua en Europe/Madrid y cada aviso se envia como mucho una vez al dia).
  new Thread(() => TelegramService.registrarWebhook(), "telegram-webhook-register").start()
  val telegramExecutor = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "telegram-recordatorios")
    t.setDaemon(true)
    t
  })
  telegramExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try DatabaseManager.tgRecordatoriosPendientes().foreach(TelegramService.enviar)
        catch { case e: Exception => println(s"[Telegram recordatorios] ERROR: ${e.getMessage.take(200)}") }
      }
    },
    60 * 1000L,
    10 * 60 * 1000L,
    java.util.concurrent.TimeUnit.MILLISECONDS
  )

  override def host: String = "0.0.0.0"
  override def port: Int    = sys.env.getOrElse("PORT", "8081").toInt

  override def allRoutes: Seq[cask.Routes] = Seq(
    AuthController,
    DashboardController,
    MatchController,
    BioController,
    HistoryController,
    CareerController,
    AdminController,
    AmateurController,
    PublicController
  )
}
