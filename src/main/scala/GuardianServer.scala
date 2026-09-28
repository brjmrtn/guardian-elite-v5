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

  /** Milisegundos hasta la proxima `hora`:00 en GUARDIAN_TZ (del dia `dia` si se indica), nunca en la zona del servidor. */
  private def msHastaProxima(dia: Option[java.time.DayOfWeek], hora: Int): Long = {
    val ahora = DatabaseManager.ahoraGuardian()
    var objetivo = ahora.withHour(hora).withMinute(0).withSecond(0).withNano(0)
    dia.foreach(d => objetivo = objetivo.`with`(java.time.temporal.TemporalAdjusters.nextOrSame(d)))
    if (!objetivo.isAfter(ahora)) objetivo = objetivo.plusDays(if (dia.isDefined) 7 else 1)
    java.time.Duration.between(ahora, objetivo).toMillis
  }

  // ── BACKUP ENGINE ────────────────────────────────────────────────────────────
  // Domingo a las 3:00 (y al arrancar, ver CATCH-UP): backup si el ultimo tiene mas de 7 dias.
  // Genera un dump SQL y lo guarda en la propia BD; lo envía por email si esta configurado.
  val backupExecutor = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "backup-engine")
    t.setDaemon(true)
    t
  })

  val msHastaBackup = msHastaProxima(Some(java.time.DayOfWeek.SUNDAY), 3)

  backupExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try {
          println(s"[Guardian Backup] Programado: ${DatabaseManager.ejecutarBackupSemanal()}")
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
  val msHastaResumen = msHastaProxima(Some(java.time.DayOfWeek.MONDAY), 8)

  resumenExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try DatabaseManager.ejecutarResumenSemanal()
        catch { case e: Exception =>
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
  val msHastaAvisoPartido = msHastaProxima(None, 9)

  avisoPartidoExecutor.scheduleAtFixedRate(
    new Runnable {
      def run(): Unit = {
        try {
          DatabaseManager.enviarAvisoPartidoSiToca()
        } catch { case e: Exception =>
          println(s"[Aviso Partido] ERROR: ${e.getMessage.take(200)}")
        }
      }
    },
    msHastaAvisoPartido,
    24 * 60 * 60 * 1000L, // cada 24h
    java.util.concurrent.TimeUnit.MILLISECONDS
  )

  // ── CATCH-UP AL ARRANCAR ─────────────────────────────────────────────────────
  // Render apaga el servidor cuando no se usa y los schedulers de arriba se pierden sus horas.
  // Al despertar se ponen al dia las tareas aplazables (cada funcion es idempotente).
  // Hilo de fondo: nunca bloquea el arranque; un fallo en un paso no impide los demas.
  val catchUpArranque = new Thread(() => {
    val inicio = System.currentTimeMillis()
    def esperarHasta(msDesdeInicio: Long): Unit = {
      val restante = inicio + msDesdeInicio - System.currentTimeMillis()
      if (restante > 0) try Thread.sleep(restante) catch { case _: InterruptedException => () }
    }
    def paso(nombre: String)(tarea: => Unit): Unit =
      try tarea catch { case e: Exception => println(s"[Catch-up] $nombre ERROR: ${e.getMessage.take(200)}") }

    esperarHasta(90 * 1000L)
    paso("backup")(println(s"[Catch-up] backup: ${DatabaseManager.ejecutarBackupSemanal(forzar = false)}"))

    esperarHasta(120 * 1000L)
    paso("resumen semanal") {
      val ahora = DatabaseManager.ahoraGuardian()
      val lunesOcho = ahora.`with`(java.time.temporal.TemporalAdjusters.previousOrSame(java.time.DayOfWeek.MONDAY))
        .withHour(8).withMinute(0).withSecond(0).withNano(0)
      // solo la semana en curso: no se envian semanas atrasadas
      if (!ahora.isBefore(lunesOcho) && !DatabaseManager.resumenSemanalEnviadoEstaSemana())
        println(s"[Catch-up] resumen semanal: ${DatabaseManager.ejecutarResumenSemanal()}")
    }
    paso("aviso partido") {
      val hora = DatabaseManager.ahoraGuardian().getHour
      if (hora >= 9 && hora < 15) DatabaseManager.enviarAvisoPartidoSiToca()
    }
  }, "catch-up-arranque")
  catchUpArranque.setDaemon(true)
  catchUpArranque.start()

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
        // Protocolo de recuperacion: se genera aqui (nunca en el render del dashboard) si la carga lo requiere
        try DatabaseManager.comprobarProtocoloRecuperacion()
        catch { case e: Exception => println(s"[Recuperacion] ERROR: ${e.getMessage.take(200)}") }
        // Alertas positivas: se registran (y pasan al Legado la primera vez de cada tipo)
        try DatabaseManager.registrarAlertasPositivas()
        catch { case e: Exception => println(s"[Alertas positivas] ERROR: ${e.getMessage.take(200)}") }
        // Reto semanal de Hector: se asegura el de la semana en curso (desde el lunes a las 7:00, hora de Madrid)
        try {
          val hora = java.time.ZonedDateTime.now(java.time.ZoneId.of(sys.env.getOrElse("GUARDIAN_TZ", "Europe/Madrid"))).getHour
          if (hora >= 7 && hora < 22) DatabaseManager.generarRetoHector() match {
            case Left(e) => println(s"[Reto Hector] ${e.take(200)}")
            case Right(_) =>
          }
        } catch { case e: Exception => println(s"[Reto Hector] ERROR: ${e.getMessage.take(200)}") }
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
