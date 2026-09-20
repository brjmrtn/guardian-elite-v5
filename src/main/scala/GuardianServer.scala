import cask._

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
