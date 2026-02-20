import cask._

/**
 * GUARDIAN ELITE — Punto de entrada del servidor.
 *
 * Este objeto solo se encarga de:
 *   1. Configurar host/puerto.
 *   2. Registrar todos los controladores en el router de Cask.
 *   3. Inicializar la base de datos al arrancar.
 *
 * Lógica por controlador:
 *   - AuthController       → /login, /logout
 *   - DashboardController  → /
 *   - MatchController      → /match-center, /match/*, /video/*, /tournament/*
 *   - BioController        → /bio/*, /oracle, /distribution
 *   - HistoryController    → /history, /scouting
 *   - CareerController     → /career/*, /gear, /penalties, /career/legacy
 *   - AdminController      → /admin/*, /settings, /tactics
 *
 * Utilidades compartidas → SharedLayout
 */
 object GuardianServer extends cask.Main {

 override def host: String = "0.0.0.0"
 override def port: Int    = sys.env.getOrElse("PORT", "8081").toInt

 override def allRoutes: Seq[cask.Routes] = Seq(
 AuthController,
 DashboardController,
 MatchController,
 BioController,
 HistoryController,
 CareerController,
 AdminController
 )

 // Inicializar tablas de BD una sola vez al arrancar
 DatabaseManager.initDB()
 }







