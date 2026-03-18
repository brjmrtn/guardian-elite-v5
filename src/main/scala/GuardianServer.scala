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
    AmateurController
  )
}
