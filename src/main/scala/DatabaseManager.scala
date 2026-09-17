import java.sql.{Connection, DriverManager, Date}
import java.util.Properties
import java.time.{LocalDate, Period}
import requests._
import ujson._
import org.jsoup.Jsoup
import scala.jdk.CollectionConverters._

// --- DATA MODELS ---
case class PlayerCardData(nombre: String, media: Int, posicion: String, fotoUrl: String, clubUrl: String, flagUrl: String, clubNombre: String, div: Int, han: Int, kic: Int, ref: Int, spd: Int, pos: Int, divRaw: Double, hanRaw: Double, kicRaw: Double, refRaw: Double, spdRaw: Double, posRaw: Double, fechaNacimiento: String, rffmUrl: String, rffmName: String)
// MatchLog completo para Moneyball
case class MatchLog(id: Int, rival: String, resultado: String, minutos: Int, nota: Double, fecha: String, clima: String, estadio: String, notas: String, video: String, reaccion: String, status: String, tipo: String, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int, analisisVoz: String, torneoNombre: String, fase: String, paradas: Int, p1v1: Int, pAir: Int, pPie: Int, zTiros: String, zGoles: String)
case class SeasonSummary(id: Int, categoria: String, clubUrl: String, fotoUrl: String, partidosJugados: Int, golesContra: Int, porteriasCero: Int, mediaFinal: Int)
case class Achievement(icono: String, nombre: String, cantidad: Int, descripcion: String)
case class GearItem(id: Int, nombre: String, tipo: String, usos: Int, maxUsos: Int, estado: String, img: String)
case class Objective(id: Int, tipo: String, actual: Double, meta: Int, descripcion: String)
case class Drill(id: Int, nombre: String, desc: String, actual: Int, objetivo: Int)
case class VideoTag(id: Int, matchId: Int, minuto: Int, segundo: Int, tipo: String, desc: String)
case class VideoClip(tagId: Int, matchId: Int, rival: String, fecha: String, videoUrl: String, minuto: Int, segundo: Int, tipo: String)
case class RivalInfo(nombre: String, estilo: String, claves: String, notas: String)
case class PenaltyStat(zona: String, total: Int, goles: Int)
case class RPGStatus(nivel: Int, xp: Int, nextLevelXp: Int, titulo: String, cinturonJudo: String)
case class TechReview(id: Int, fecha: String, blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String)
// Academic notes
case class AcademicNote(id: Int, fecha: String, asignatura: String, nota: Double, tipo: String)
// Vault Medico
case class MedicalReport(id: Int, fecha: String, tipo: String, diagnostico: String, recomendaciones: String, esPrevio: Boolean)
// Footbar (sensor GPS de rendimiento fisico/tecnico)
case class FootbarSession(
  matchId: Int, distanciaKm: Double, altaIntensidadM: Double, sprintMaxKmh: Double,
  pctActividad: Double, tiempoActividadMin: Int, aceleraciones: Int, desaceleraciones: Int,
  balones: Int, pases: Int, tiempoBalonSeg: Int, disparos: Int, tiroMaxKmh: Double
)

object DatabaseManager {
  private val dbHost = sys.env.getOrElse("DB_HOST", "ep-fancy-cherry-abkfneqp-pooler.eu-west-2.aws.neon.tech")
  private val dbName = sys.env.getOrElse("DB_NAME", "neondb")
  private val dbUser = sys.env.getOrElse("DB_USER", "neondb_owner")
  private val dbPass = sys.env.getOrElse("DB_PASS", "")

  val url = s"jdbc:postgresql://$dbHost/$dbName?sslmode=require&options=-c%20client_encoding=UTF8"

  // --- POOL DE CONEXIONES (HikariCP) ---
  // Se inicializa UNA sola vez al arrancar. Neon free tier soporta ~10 conexiones;
  // con maximumPoolSize=5 dejamos margen para el dashboard de Neon.
  private val pool: com.zaxxer.hikari.HikariDataSource = {
    if (dbPass.isEmpty) throw new IllegalStateException("DB_PASS no configurada. Anadela como variable de entorno.")
    val config = new com.zaxxer.hikari.HikariConfig()
    config.setJdbcUrl(url)
    config.setUsername(dbUser)
    config.setPassword(dbPass)
    config.setMaximumPoolSize(10)
    config.setMinimumIdle(1)
    config.setConnectionTimeout(10000)   // 10s esperando conexion libre del pool
    config.setIdleTimeout(300000)        // Cierra idle tras 5 min
    config.setMaxLifetime(600000)        // Vida max de conexion: 10 min
    config.setKeepaliveTime(120000)      // Ping cada 2 min para mantener vivas las conexiones
    config.addDataSourceProperty("ssl", "true")
    config.setConnectionInitSql("SET client_encoding TO 'UTF8'")
    new com.zaxxer.hikari.HikariDataSource(config)
  }

  // API identica a antes: todos los metodos del DatabaseManager siguen igual.
  // La unica diferencia es que ahora devuelve una conexion del pool, no una nueva.
  def getConnection(): Connection = pool.getConnection()

  // --- INICIALIZACION DE TABLAS (se llama UNA vez al arrancar el servidor) ---
  // Centraliza todos los CREATE TABLE IF NOT EXISTS que antes estaban dispersos
  // por cada metodo de consulta, eliminando el overhead en cada request.
  def initDB(): Unit = {
    val conn = getConnection()
    try {
      val stmt = conn.createStatement()

      // Temporadas y partidos (nucleo del sistema)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS seasons (
        id               SERIAL PRIMARY KEY,
        nombre_club      TEXT,
        categoria        TEXT,
        foto_jugador_url TEXT,
        club_escudo_url  TEXT,
        media            DOUBLE PRECISION DEFAULT 59,
        stat_div         DOUBLE PRECISION DEFAULT 80,
        stat_han         DOUBLE PRECISION DEFAULT 60,
        stat_kic         DOUBLE PRECISION DEFAULT 55,
        stat_ref         DOUBLE PRECISION DEFAULT 60,
        stat_spd         DOUBLE PRECISION DEFAULT 62,
        stat_pos         DOUBLE PRECISION DEFAULT 58,
        fecha_inicio     DATE,
        fecha_nacimiento DATE DEFAULT '2020-06-19',
        rffm_url         TEXT,
        rffm_team_name   TEXT,
        judo_belt        TEXT DEFAULT 'Blanco'
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS matches (
        id               SERIAL PRIMARY KEY,
        season_id        INT,
        fecha            DATE,
        rival            TEXT,
        tipo_partido     TEXT DEFAULT 'LIGA',
        status           TEXT DEFAULT 'SCHEDULED',
        goles_favor      INT DEFAULT 0,
        goles_contra     INT DEFAULT 0,
        minutos          INT DEFAULT 0,
        nota             DOUBLE PRECISION DEFAULT 0,
        paradas          INT DEFAULT 0,
        paradas_1v1      INT DEFAULT 0,
        paradas_aereas   INT DEFAULT 0,
        acciones_pie     INT DEFAULT 0,
        clima            TEXT,
        estadio          TEXT,
        temperatura      INT,
        notas_partido    TEXT,
        video_url        TEXT,
        reaccion_goles   TEXT,
        analisis_voz     TEXT,
        zona_goles       TEXT,
        zona_paradas     TEXT,
        zona_tiros       TEXT,
        mapa_campo       TEXT,
        pc_t             INT DEFAULT 0,
        pc_ok            INT DEFAULT 0,
        pl_t             INT DEFAULT 0,
        pl_ok            INT DEFAULT 0,
        media_historica  DOUBLE PRECISION DEFAULT 0,
        torneo_nombre    TEXT,
        fase             TEXT
      )""")

      // Biometria y salud
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS wellness (
        id            SERIAL PRIMARY KEY,
        fecha         DATE DEFAULT CURRENT_DATE,
        sueno         INT,
        horas_sueno   DOUBLE PRECISION,
        energia       INT,
        dolor         INT,
        zona_dolor    TEXT,
        altura        DOUBLE PRECISION,
        peso          DOUBLE PRECISION,
        animo         INT,
        notas_conducta TEXT,
        estado_fisico TEXT DEFAULT 'DISPONIBLE'
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS match_goals (
        id              SERIAL PRIMARY KEY,
        match_id        INT NOT NULL,
        minuto          INT DEFAULT 0,
        origen          TEXT,
        situacion       TEXT,
        responsabilidad TEXT DEFAULT 'Media',
        era_parable     TEXT DEFAULT 'Dudoso',
        zona_gol        TEXT,
        notas           TEXT
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS growth_history (
        id     SERIAL PRIMARY KEY,
        fecha  DATE DEFAULT CURRENT_DATE,
        altura DOUBLE PRECISION,
        peso   DOUBLE PRECISION
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS medical_records (
        id               SERIAL PRIMARY KEY,
        fecha            DATE,
        tipo             TEXT,
        diagnostico      TEXT,
        recomendaciones  TEXT,
        es_previo        BOOLEAN DEFAULT FALSE
      )""")

      // Entrenamiento y progresion tecnica
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS trainings (
        id             SERIAL PRIMARY KEY,
        fecha          DATE DEFAULT CURRENT_DATE,
        tipo           TEXT,
        foco           TEXT,
        rpe            INT,
        calidad        INT,
        atencion       INT,
        rutina_detalle TEXT
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS drills (
        id                SERIAL PRIMARY KEY,
        nombre            TEXT,
        descripcion       TEXT,
        sesiones_objetivo INT DEFAULT 10,
        sesiones_actuales INT DEFAULT 0,
        activo            BOOLEAN DEFAULT TRUE
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS technical_reviews (
        id             SERIAL PRIMARY KEY,
        fecha          DATE DEFAULT CURRENT_DATE,
        blocaje        INT,
        pies           INT,
        aereo          INT,
        valentia       INT,
        concentracion  INT,
        coordinacion   INT,
        notas          TEXT
      )""")

      // Material deportivo
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS gear (
        id                   SERIAL PRIMARY KEY,
        nombre               TEXT,
        tipo                 TEXT,
        vida_util_estimada   INT DEFAULT 30,
        usos_actuales        INT DEFAULT 0,
        activo               BOOLEAN DEFAULT TRUE,
        imagen_url           TEXT
      )""")
      stmt.executeUpdate("ALTER TABLE gear ADD COLUMN IF NOT EXISTS precio_compra DOUBLE PRECISION DEFAULT 0")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS injuries (
        id              SERIAL PRIMARY KEY,
        fecha_inicio    DATE DEFAULT CURRENT_DATE,
        fecha_alta      DATE,
        zona            TEXT,
        tipo            TEXT,
        gravedad        TEXT DEFAULT 'LEVE',
        descripcion     TEXT,
        dias_baja       INT DEFAULT 0,
        activa          BOOLEAN DEFAULT TRUE
      )""")

      // Scouting y rivales
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS rivals (
        id              SERIAL PRIMARY KEY,
        nombre          TEXT UNIQUE,
        estilo_juego    TEXT,
        jugadores_clave TEXT,
        notas_scouting  TEXT
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS penalties (
        id          SERIAL PRIMARY KEY,
        fecha       DATE DEFAULT CURRENT_DATE,
        rival       TEXT,
        zona_tiro   TEXT,
        zona_salto  TEXT,
        es_gol      BOOLEAN DEFAULT FALSE
      )""")

      // Video y analisis
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS video_tags (
        id          SERIAL PRIMARY KEY,
        match_id    INT,
        minuto      INT,
        segundo     INT,
        tipo        TEXT,
        descripcion TEXT
      )""")

      // Objetivos y logros
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS objectives (
        id          SERIAL PRIMARY KEY,
        tipo        TEXT,
        objetivo    INT,
        descripcion TEXT
      )""")

      // Academico
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS academic_performance (
        id              SERIAL PRIMARY KEY,
        fecha           DATE DEFAULT CURRENT_DATE,
        asignatura      TEXT,
        nota            DOUBLE PRECISION,
        tipo_evaluacion TEXT,
        comentarios     TEXT
      )""")

      // Leyendas (comparativa historica)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS legends_milestones (
        id     SERIAL PRIMARY KEY,
        nombre TEXT,
        edad   INT,
        hito   TEXT
      )""")

      // Cache de respuestas IA (evita llamadas duplicadas a Gemini)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS ai_cache (
        prompt_hash TEXT PRIMARY KEY,
        respuesta   TEXT,
        creado_en   TIMESTAMP DEFAULT NOW()
      )""")
      // Limpiar errores cacheados de versiones anteriores en cada arranque
      stmt.executeUpdate("""DELETE FROM ai_cache WHERE
        respuesta LIKE 'Error:%' OR
        respuesta LIKE '%status code%' OR
        respuesta LIKE '%NOT_FOUND%' OR
        respuesta LIKE '%INVALID_ARGUMENT%' OR
        respuesta LIKE '%Error tras agotar%'
      """)

      // Columnas opcionales anadidas en versiones posteriores (ALTER IF NOT EXISTS es idempotente)
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS judo_belt TEXT DEFAULT 'Blanco'")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS rffm_url TEXT")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS rffm_team_name TEXT")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS fecha_nacimiento DATE DEFAULT '2020-06-19'")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS mapa_campo TEXT")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS analisis_voz TEXT")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS lineas_superadas INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS acciones_preventivas INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS scanning_rate INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS es_local BOOLEAN DEFAULT NULL")

      // ── v7.2: NLP Scouting Aggregator ──────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS scouting_reports (
        id           SERIAL PRIMARY KEY,
        fecha        DATE DEFAULT CURRENT_DATE,
        ojeador      TEXT DEFAULT '',
        club_origen  TEXT DEFAULT '',
        texto_raw    TEXT NOT NULL,
        nivel_tecnico    INT DEFAULT 0,
        nivel_tactico    INT DEFAULT 0,
        nivel_fisico     INT DEFAULT 0,
        nivel_mental     INT DEFAULT 0,
        nivel_distribucion INT DEFAULT 0,
        nivel_global     INT DEFAULT 0,
        proyeccion       TEXT DEFAULT '',
        recomendacion    TEXT DEFAULT '',
        fortalezas       TEXT DEFAULT '',
        areas_mejora     TEXT DEFAULT '',
        resumen_ia       TEXT DEFAULT '',
        created_at   TIMESTAMP DEFAULT NOW()
      )""")

      // ── v7.2: Nutrition plans cache ─────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS nutrition_plans (
        id           SERIAL PRIMARY KEY,
        semana       DATE DEFAULT CURRENT_DATE,
        acwr         DOUBLE PRECISION DEFAULT 1.0,
        rpe_media    DOUBLE PRECISION DEFAULT 5.0,
        nota_ultimo  DOUBLE PRECISION DEFAULT 6.0,
        plan_ia      TEXT DEFAULT '',
        created_at   TIMESTAMP DEFAULT NOW()
      )""")

      // ── Footbar: sensor GPS de rendimiento fisico/tecnico (opcional por partido) ──
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS footbar_sessions (
        id                   SERIAL PRIMARY KEY,
        match_id             INT REFERENCES matches(id) ON DELETE CASCADE,
        distancia_km         DOUBLE PRECISION DEFAULT 0,
        alta_intensidad_m    DOUBLE PRECISION DEFAULT 0,
        sprint_max_kmh       DOUBLE PRECISION DEFAULT 0,
        pct_actividad        DOUBLE PRECISION DEFAULT 0,
        tiempo_actividad_min INT DEFAULT 0,
        aceleraciones        INT DEFAULT 0,
        desaceleraciones     INT DEFAULT 0,
        balones              INT DEFAULT 0,
        pases                INT DEFAULT 0,
        tiempo_balon_seg     INT DEFAULT 0,
        disparos             INT DEFAULT 0,
        tiro_max_kmh         DOUBLE PRECISION DEFAULT 0,
        created_at           TIMESTAMP DEFAULT NOW(),
        UNIQUE(match_id)
      )""")

      println("[OK] initDB: todas las tablas verificadas.")
    } catch {
      case e: Exception => println(s"[!] initDB error: ${e.getMessage}")
    } finally {
      conn.close()
    }
  }

  def fixEncoding(s: String): String = { try { if (s == null) "" else if (s.contains("A")) new String(s.getBytes("ISO-8859-1"), "UTF-8") else s } catch { case e: Exception => s } }

  /** Escapa caracteres HTML peligrosos en strings que provienen de la BD
   *  y van a ser embebidos en HTML (raw()). Previene XSS.
   *  Usalo siempre que hagas: s"...$variableDeBD..." dentro de un bloque HTML.
   */
  def escHtml(s: String): String = {
    if (s == null) ""
    else s.replace("&", "&amp;")
          .replace("<", "&lt;")
          .replace(">", "&gt;")
          .replace("\"", "&quot;")
          .replace("'", "&#x27;")
  }
  def calcularEdadExacta(fechaStr: String): Int = { try { Period.between(LocalDate.parse(fechaStr), LocalDate.now()).getYears } catch { case _: Exception => 5 } }

  // --- NUEVO: SISTEMA DE AUDITORIA TECNICA ---
  def saveTechnicalReview(blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String): Unit = {
    val conn = getConnection(); try {
      val ps = conn.prepareStatement("INSERT INTO technical_reviews (fecha, blocaje, pies, aereo, valentia, concentracion, coordinacion, notas) VALUES (?,?,?,?,?,?,?,?)")
      ps.setDate(1, Date.valueOf(LocalDate.now()))
      ps.setInt(2, blocaje); ps.setInt(3, pies); ps.setInt(4, aereo); ps.setInt(5, valentia); ps.setInt(6, concentracion); ps.setInt(7, coordinacion)
      ps.setString(8, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getTechnicalReviews(): List[TechReview] = {
    var l = List[TechReview](); val conn = getConnection(); try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM technical_reviews ORDER BY fecha ASC")
      while(rs.next()) {
        l = l :+ TechReview(rs.getInt("id"), rs.getDate("fecha").toString, rs.getInt("blocaje"), rs.getInt("pies"), rs.getInt("aereo"), rs.getInt("valentia"), rs.getInt("concentracion"), rs.getInt("coordinacion"), rs.getString("notas"))
      }
    } finally { conn.close() }; l
  }

  def getTechEvolutionChart(): String = {
    val reviews = getTechnicalReviews()
    if (reviews.isEmpty) return "{ \"labels\": [], \"datasets\": [] }"
    val labels = reviews.map(r => s"'${r.fecha}'").mkString(",")
    val d1 = reviews.map(_.blocaje).mkString(",")
    val d2 = reviews.map(_.valentia).mkString(",")
    val d3 = reviews.map(_.concentracion).mkString(",")
    s"""{
      "labels": [$labels],
      "datasets": [
        { "label": "Blocaje/Manos", "data": [$d1], "borderColor": "#0dcaf0", "tension": 0.3, "fill": false },
        { "label": "Valentia", "data": [$d2], "borderColor": "#dc3545", "tension": 0.3, "fill": false },
        { "label": "Concentracion", "data": [$d3], "borderColor": "#ffc107", "tension": 0.3, "fill": false }
      ]
    }"""
  }

  // --- LEYENDAS Y COMPARATIVA (Mantenido) ---
  def initLegendsTable(): String = {
    val conn = getConnection(); try {
      val stmt = conn.createStatement()
      stmt.executeUpdate("DELETE FROM legends_milestones")
      // Corregir nombres corruptos que puedan existir en DB de versiones anteriores
      val fixStmt = conn.createStatement()
      fixStmt.execute("UPDATE legends_milestones SET nombre = 'Marc-Andre ter Stegen' WHERE nombre LIKE 'Marc-Andr%ter Stegen'")
      fixStmt.execute("UPDATE legends_milestones SET hito = REPLACE(hito, 'a\u00f1os', 'anos') WHERE hito LIKE '%a_os%'")
      fixStmt.close()

      val ps = conn.prepareStatement("INSERT INTO legends_milestones (nombre, edad, hito) VALUES (?,?,?)")
      val data = Seq(("Marc-Andre ter Stegen", 5, "Jugaba de DELANTERO. No se puso de portero hasta los 10 anos."), ("Thibaut Courtois", 5, "Su deporte principal era el VOLEIBOL."), ("Iker Casillas", 6, "Jugaba en el patio del colegio El Recuerdo sobre cemento."), ("Gianluigi Buffon", 6, "Jugaba de centrocampista. Le gustaba correr y marcar goles."), ("Manuel Neuer", 5, "Llevaba un osito de peluche a la porteria."))
      data.foreach { case (n, e, h) => ps.setString(1, n); ps.setInt(2, e); ps.setString(3, fixEncoding(h)); ps.executeUpdate() }
      "Base de datos de Leyendas actualizada."
    } catch { case e: Exception => s"Error: ${e.getMessage}" } finally { conn.close() }
  }

  def getLegendComparison(): String = {
    val card = getLatestCardData(); val edad = calcularEdadExacta(card.fechaNacimiento); val conn = getConnection()
    try {
      val rsLeague = conn.createStatement().executeQuery("SELECT AVG(goles_contra) as media_liga FROM matches WHERE status='PLAYED'")
      var mediaLiga = 0.0; if(rsLeague.next()) mediaLiga = rsLeague.getDouble("media_liga")
      val rsMyStats = conn.createStatement().executeQuery("SELECT AVG(goles_contra) as mi_media FROM matches WHERE status='PLAYED'")
      var miMedia = 0.0; if(rsMyStats.next()) miMedia = rsMyStats.getDouble("mi_media")
      val rsLegend = conn.createStatement().executeQuery(s"SELECT * FROM legends_milestones WHERE edad <= $edad ORDER BY edad DESC LIMIT 1")
      val legendHtml = if (rsLegend.next()) {
        val safeNombre = escHtml(fixEncoding(rsLegend.getString("nombre")))
        val safeHito   = escHtml(fixEncoding(rsLegend.getString("hito")))
        "<div class='mb-3'>" +
        "<h6 class='text-warning text-uppercase mb-1'>A TU EDAD (" + edad + " A\u00d1OS)...</h6>" +
        "<h4 class='text-white fw-bold mb-1'>" + safeNombre + "</h4>" +
        "<p class='text-light small fst-italic'>&quot;" + safeHito + "&quot;</p>" +
        "</div>"
      } else ""
      val diff = mediaLiga - miMedia; val color = if(diff >= 0) "text-success" else "text-danger"
      f"""<div class="card bg-secondary bg-opacity-10 border-warning shadow mb-4"><div class="card-header bg-dark text-warning fw-bold text-center small">CONTEXTO & LEYENDAS</div><div class="card-body">$legendHtml<hr class="border-secondary"><h6 class="text-info text-uppercase text-center mb-2 small fw-bold">COMPARATIVA RFFM</h6><div class="row text-center align-items-center"><div class="col-6 border-end border-secondary"><div class="small text-muted fw-bold">TU MEDIA</div><div class="display-6 fw-bold $color">${f"$miMedia%1.1f"}</div></div><div class="col-6"><div class="small text-muted fw-bold">MEDIA LIGA</div><div class="display-6 fw-bold text-white">${f"$mediaLiga%1.1f"}</div></div></div></div></div>"""
    } catch { case e: Exception => "" } finally { conn.close() }
  }

  // --- IA CONFIG ---
  val modelList = Seq("gemini-2.5-flash", "gemini-flash-latest")
  object AIProvider {
    import java.security.MessageDigest

    private def getHash(s: String): String =
      MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString

    // Funcion principal: intenta cache, si no, llama a Gemini
    def ask(prompt: String, media: Option[(String, String)] = None, bypassCache: Boolean = false): String = {
      val conn = DatabaseManager.getConnection()
      // Creamos un hash unico combinando el prompt y los primeros bytes del archivo (si existe)
      val combinedKey = prompt + media.map(_._2.take(100)).getOrElse("")
      val hash = getHash(combinedKey)

      try {
        if (!bypassCache) {
          val ps = conn.prepareStatement("SELECT respuesta FROM ai_cache WHERE prompt_hash = ?")
          ps.setString(1, hash)
          val rs = ps.executeQuery()
          if (rs.next()) return rs.getString("respuesta")
        }

        // Llamada real a la API (Unificada)
        val response = callGeminiUnified(prompt, media)

        // Guardar en cache solo si la respuesta es valida (no un error)
        if (!response.startsWith("Error:") && !response.contains("status code") && !response.contains("NOT_FOUND")) {
          val save = conn.prepareStatement(
            "INSERT INTO ai_cache (prompt_hash, respuesta) VALUES (?, ?) ON CONFLICT (prompt_hash) DO UPDATE SET respuesta = EXCLUDED.respuesta"
          )
          save.setString(1, hash)
          save.setString(2, response)
          save.executeUpdate()
        } else {
          // Limpiar cualquier error cacheado anteriormente para este hash
          val del = conn.prepareStatement("DELETE FROM ai_cache WHERE prompt_hash = ?")
          del.setString(1, hash)
          del.executeUpdate()
        }

        response
      } finally { conn.close() }
    }

    private def callGeminiUnified(prompt: String, media: Option[(String, String)]): String = {
      val apiKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim
      if (apiKey.isEmpty) return "Error: GEMINI_API_KEY no configurada"

      val isPdf = media.exists(_._1 == "application/pdf")

      // Usar siempre v1beta — soporta PDF y es compatible con cualquier API key de Google AI Studio
      val urls = Seq(
        s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=$apiKey",
        // s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-exp:generateContent?key=$apiKey",
        //s"https://generativelanguage.googleapis.com/v1beta/models/:generateContent?key=$apiKey"
      )

      println(s"DEBUG: isPdf=$isPdf key=[${apiKey.take(4)}...${apiKey.takeRight(4)}]")

      val parts = ujson.Arr(ujson.Obj("text" -> prompt))

      media.foreach { case (mime, data) =>
        val cleanData = if (data.contains(",")) data.split(",")(1) else data
        if (mime == "application/pdf") {
          // PDFs: inlineData con mime correcto — funciona en gemini-1.5-flash
          parts.value.append(ujson.Obj(
            "inlineData" -> ujson.Obj("mimeType" -> "application/pdf", "data" -> cleanData)
          ))
        } else {
          // Imagenes: inlineData normal
          parts.value.append(ujson.Obj(
            "inlineData" -> ujson.Obj("mimeType" -> mime, "data" -> cleanData)
          ))
        }
      }

      val payload = ujson.Obj("contents" -> ujson.Arr(ujson.Obj("parts" -> parts)))

      var lastError = ""
      for (url <- urls) {
        try {
          val r = requests.post(
            url,
            data = ujson.write(payload),
            headers = Map("Content-Type" -> "application/json"),
            readTimeout = 60000  // PDF necesita mas tiempo
          )
          if (r.statusCode == 200)
            return ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str
          else {
            lastError = s"Status ${r.statusCode}: ${r.text().take(300)}"
            println(s"DEBUG URL fallida: $url -> $lastError")
          }
        } catch { case e: Exception =>
          lastError = e.getMessage
          println(s"DEBUG excepcion: $lastError")
        }
      }
      s"Error: $lastError"
    }
  }

  // --- EN: DatabaseManager.scala ---

  def generateTrainingSession(mode: String, focus: String): String = {
    var ctx = ""
    val conn = getConnection()
    try {
      // 1. Contexto Academico (Fase 2: Detector de Fatiga Mental)
      val rsAcad = conn.createStatement().executeQuery(
        "SELECT nota FROM academic_performance ORDER BY fecha DESC LIMIT 1"
      )
      if(rsAcad.next()) {
        val ultimaNota = rsAcad.getDouble("nota")
        // Logica de fatiga cognitiva inyectada al prompt
        if(ultimaNota < 6.0) ctx += "ESTADO COGNITIVO: Carga academica alta o estres detectado. Priorizar sesion ludica y de baja frustracion. "
        else ctx += "ESTADO COGNITIVO: Optimo. Se puede exigir alta concentracion tactica. "
      }

      // 2. Dojo Synergy (Fase 2: Judo Integration)
      val rsJudo = conn.createStatement().executeQuery("SELECT judo_belt FROM seasons ORDER BY id DESC LIMIT 1")
      if(rsJudo.next()) {
        val belt = rsJudo.getString("judo_belt")
        ctx += s"CONOCIMIENTO DOJO: Cinturon $belt. Incorporar dinamicas de caidas y agilidad de judo a la porteria. "
      }

      // 3. Alertas Tecnicas y Clima (Lo que ya teniamos de Fase 1)
      val alerts = getTechnicalAlerts()
      if(alerts.nonEmpty) ctx += s"ALERTAS TECNICAS: ${alerts.mkString(", ")}. "
    } finally {
      conn.close()
    }

    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)

    // Prompt enriquecido con Fatiga Mental y Dojo Synergy
    val prompt = s"""
    Eres Entrenador Elite. Crea una sesion de 45min (Padre/Hijo).
    ROL: PORTERO ($edad anos).
    OBJETIVO: $focus.
    CONTEXTO MULTI-DISCIPLINA: $ctx.

    ESTRUCTURA:
    1. Calentamiento (Ludico + Caidas tipo Judo).
    2. Bloque Principal (Ajustar dificultad segun ESTADO COGNITIVO).
    3. Reto Final.
    SOLO TEXTO PLANO.
  """

    AIProvider.ask(prompt).replace("```html","").replace("```","").trim
  }
  // --- NUEVO: CENTRO DE PREDICCION BIOMETRICA (EL ORACULO) ---
  def getOracleInsights(): String = {
    val conn = getConnection()
    try {
      val sb = new StringBuilder()

      // 1. Obtener datos de crecimiento (ultimos 2 para comparar)
      val rsG = conn.createStatement().executeQuery("SELECT altura, peso FROM physical_growth ORDER BY fecha DESC LIMIT 2")

      // Variables para guardar los datos y usarlos luego
      var currentH = 110.0
      var currentW = 20.0
      var hasPrev = false
      var prevH = 0.0
      var prevW = 0.0

      if (rsG.next()) {
        currentH = rsG.getDouble("altura")
        currentW = rsG.getDouble("peso")

        if (rsG.next()) {
          prevH = rsG.getDouble("altura")
          prevW = rsG.getDouble("peso")
          hasPrev = true
        }
      }

      // 2. Analisis de Biotipo y Composicion
      val imc = if(currentH > 0) currentW / Math.pow(currentH/100, 2) else 0.0
      sb.append(s"<div class='mb-3 text-white'><b>[stats] COMPOSICION:</b> ${currentH}cm / ${currentW}kg</div>")

      val (perfilNombre, perfilDesc) = if (imc < 15) {
        ("<span class='text-info fw-bold'>VELOCISTA</span>", "Peso ligero que favorece la <b>agilidad pura</b> y velocidad de desplazamiento.")
      } else if (imc >= 15 && imc <= 17) {
        ("<span class='text-success fw-bold'>EQUILIBRADO</span>", "Relacion potencia-peso optima. Buen equilibrio entre <b>salto y velocidad</b>.")
      } else {
        ("<span class='text-warning fw-bold'>TANQUE</span>", "Mayor masa corporal. Ventaja en <b>proteccion de balon</b> y duelos 1v1.")
      }
      sb.append(s"<div class='mb-3 small text-light'><b>[scout] Perfil Fisico:</b> $perfilNombre. $perfilDesc</div>")

      // 3. Alerta de Estiron (Solo si hay historial)
      if (hasPrev && currentH > prevH && currentW <= prevW) {
        sb.append("<div class='alert alert-warning p-2 small mb-3'>")
        sb.append("<b>[hueso] ESTIRON DETECTADO:</b> Ha crecido en altura sin aumentar masa. ")
        sb.append("Es probable que este algo mas impreciso. Trabajar <b>propiocepcion</b>.</div>")
      }

      // 4. Calculo de Cargas (ACWR) — inline para no abrir segunda conexion
      val rsAc = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAc.setInt(1, 7); val rsAcR = rsAc.executeQuery()
      val acuteLoads = Seq(if (rsAcR.next()) rsAcR.getDouble(1) else 0.0)
      val rsCh = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCh.setInt(1, 28); val rsChR = rsCh.executeQuery()
      val chronicLoads = Seq(if (rsChR.next()) rsChR.getDouble(1) else 0.0)
      val acuteAvg = if (acuteLoads.nonEmpty) acuteLoads.sum / 7.0 else 0.0
      val chronicAvg = if (chronicLoads.nonEmpty) chronicLoads.sum / 28.0 else 1.0
      val acwr = if (chronicAvg > 0) acuteAvg / chronicAvg else 0.0

      // 5. Grafico de Barras ACWR
      val maxVal = Math.max(acuteAvg, chronicAvg).max(100.0)
      val acuteWidth = (acuteAvg / maxVal * 100).toInt
      val chronicWidth = (chronicAvg / maxVal * 100).toInt
      val barColor = if(acwr > 1.5) "bg-danger" else if(acwr < 0.8) "bg-info" else "bg-success"

      sb.append("<div class='mb-4 p-3 bg-black bg-opacity-25 rounded border border-secondary'>")
      sb.append("<h6 class='text-uppercase x-small fw-bold text-muted mb-3'>Estado de Carga (ACWR)</h6>")
      sb.append(s"<div class='mb-2'><div class='progress' style='height: 6px; background:#111;'><div class='progress-bar bg-secondary' style='width: $chronicWidth%'></div></div><div class='x-small text-muted'>Carga Cronica</div></div>")
      sb.append(s"<div class='mb-2'><div class='progress' style='height: 12px; background:#111;'><div class='progress-bar $barColor progress-bar-striped progress-bar-animated' style='width: $acuteWidth%'></div></div><div class='x-small text-muted'>Carga Aguda (Semana)</div></div>")
      sb.append(f"<div class='text-center mt-2'><span class='badge bg-dark border border-secondary'>Ratio: $acwr%.2f</span></div>")
      sb.append("</div>")

      // 6. Plan de Trabajo Dinamico
      sb.append("<div class='card bg-primary bg-opacity-10 border-primary p-3 mb-2'>")
      sb.append("<h6 class='text-primary fw-bold'><i class='fas fa-clipboard-list'></i> Plan Recomendado:</h6>")
      if (acwr > 1.5) {
        sb.append("<p class='small text-warning mb-0'><b>[!] FATIGA DETECTADA:</b> Sesion teorica o tecnica manual sentado.</p>")
      } else {
        sb.append("<p class='small text-light mb-0'><b>[OK] LISTO:</b> Coordinacion de pies y blocajes en movimiento.</p>")
      }
      sb.append("</div>")

      sb.toString()
    } catch {
      case e: Exception => s"Analizando datos bioptometricos... (${e.getMessage})"
    } finally {
      conn.close()
    }
  }

  def getRPGStatus(): RPGStatus = {
    var xp = 0
    var belt = "Blanco"
    val conn = getConnection()

    try {
      // 1. Calculo de XP basado en rendimiento real
      val rs = conn.createStatement().executeQuery("SELECT COUNT(*) as pj, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as cs, SUM(paradas) as sv, AVG(nota) as avg_n FROM matches WHERE status='PLAYED'")
      if(rs.next()){
        val pj = rs.getInt("pj")
        val cs = rs.getInt("cs")
        val sv = rs.getInt("sv")
        val avg = rs.getDouble("avg_n")
        xp = (pj * 50) + (cs * 100) + (sv * 5) + (if(avg > 7.0) ((avg - 7.0) * 100).toInt else 0)
      }

      // 2. Obtencion del cinturon de Judo
      val rsBelt = conn.createStatement().executeQuery("SELECT judo_belt FROM seasons ORDER BY id DESC LIMIT 1")
      if(rsBelt.next()) {
        belt = Option(rsBelt.getString("judo_belt")).getOrElse("Blanco")
      }
    } finally { conn.close() }

    // 3. Logica de progresion (Se calcula una sola vez aqui)
    val level = 1 + (xp / 1000)
    val nextLevelXp = level * 1000 // Usamos el nombre exacto de tu Case Class

    val title = level match {
      case 1 => "Novato Promesa"
      case 2 => "Portero Local"
      case 3 => "Muro Regional"
      case 4 => "Candado Nacional"
      case _ => "Leyenda Mundial"
    }

    // Retornamos el objeto con el orden y nombres correctos
    RPGStatus(level, xp, nextLevelXp, title, belt)
  }

  def getOraclePrediction(hDad: Double, hMom: Double): String = { val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT altura FROM physical_growth ORDER BY fecha DESC LIMIT 1"); val currentHeight=if(rs.next()) rs.getDouble("altura") else 115.0; val midParent=(hDad+hMom+13)/2.0; val projected=(currentHeight*(180.0/110.0)+midParent)/2.0+5.0; val minH=projected-4; val maxH=projected+4; f"<div class='text-center'><h1 class='display-1 text-warning fw-bold'>${projected.toInt} cm</h1><p class='text-muted'>Proyeccion Adulta Estimada</p><div class='progress mb-2' style='height:10px;'><div class='progress-bar bg-success' style='width:${(projected/200.0)*100}%%'></div></div><p class='small'>Rango probable: <b>${minH.toInt}cm - ${maxH.toInt}cm</b></p><hr><p class='small text-info'>Comparativa Elite: <b>189 cm</b> (Media Pro)</p></div>" } catch { case _:Exception => "Error calculando." } finally { conn.close() } }

  // --- CORE MATCH LOGIC ---
  def logMatch(
                riv: String, gf: Int, gc: Int, min: Int, n: Double, med: Double, par: Int,
                zG: String, zT: String, zP: String, p1v1: Int, pAir: Int, pPie: Int,
                clima: String, estadio: String, temp: Int, notas: String, video: String,
                reaccion: String, fechaStr: String, tipo: String,
                pcTot: Int, pcOk: Int, plTot: Int, plOk: Int,
                mapaCampo: String,
                lineasSup: Int = 0, scanningRate: Int = 0, esLocal: Option[Boolean] = None
              ): Unit = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons")
      if(rs.next()){
        val s = conn.prepareStatement("""
        INSERT INTO matches (
          season_id, rival, goles_favor, goles_contra, minutos, nota, media_historica,
          paradas, zona_goles, zona_tiros, zona_paradas, paradas_1v1, paradas_aereas,
          acciones_pie, clima, estadio, temperatura, notas_partido, video_url,
          reaccion_goles, fecha, status, tipo_partido, pc_t, pc_ok, pl_t, pl_ok,
          torneo_nombre, fase, mapa_campo, lineas_superadas, scanning_rate, es_local
        ) VALUES (
          ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
          'PLAYED', ?, ?, ?, ?, ?, '', '', ?, ?, ?, ?
        )
      """)
        s.setInt(1, rs.getInt("id"))
        s.setString(2, fixEncoding(riv))
        s.setInt(3, gf)
        s.setInt(4, gc)
        s.setInt(5, min)
        s.setDouble(6, n)
        s.setDouble(7, med)
        s.setInt(8, par)
        s.setString(9, zG)
        s.setString(10, zT)
        s.setString(11, zP)
        s.setInt(12, p1v1)
        s.setInt(13, pAir)
        s.setInt(14, pPie)
        s.setString(15, clima)
        s.setString(16, fixEncoding(estadio))
        s.setInt(17, temp)
        s.setString(18, fixEncoding(notas))
        s.setString(19, video)
        s.setString(20, fixEncoding(reaccion))
        s.setDate(21, Date.valueOf(fechaStr))
        s.setString(22, tipo)
        s.setInt(23, pcTot)
        s.setInt(24, pcOk)
        s.setInt(25, plTot)
        s.setInt(26, plOk)
        s.setString(27, mapaCampo)
        s.setInt(28, lineasSup)
        s.setInt(29, scanningRate)
        esLocal match {
          case Some(v) => s.setBoolean(30, v)
          case None    => s.setNull(30, java.sql.Types.BOOLEAN)
        }
        s.executeUpdate()
      }
    } finally {
      conn.close()
    }
  }
  // --- FOOTBAR (SENSOR GPS DE RENDIMIENTO FISICO/TECNICO) ---
  def saveFootbar(
    matchId: Int, distanciaKm: Double, altaIntensidadM: Double, sprintMaxKmh: Double,
    pctActividad: Double, tiempoActividadMin: Int, aceleraciones: Int, desaceleraciones: Int,
    balones: Int, pases: Int, tiempoBalonSeg: Int, disparos: Int, tiroMaxKmh: Double
  ): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO footbar_sessions
          (match_id, distancia_km, alta_intensidad_m, sprint_max_kmh, pct_actividad,
           tiempo_actividad_min, aceleraciones, desaceleraciones, balones, pases,
           tiempo_balon_seg, disparos, tiro_max_kmh)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT (match_id) DO UPDATE SET
          distancia_km = EXCLUDED.distancia_km, alta_intensidad_m = EXCLUDED.alta_intensidad_m,
          sprint_max_kmh = EXCLUDED.sprint_max_kmh, pct_actividad = EXCLUDED.pct_actividad,
          tiempo_actividad_min = EXCLUDED.tiempo_actividad_min, aceleraciones = EXCLUDED.aceleraciones,
          desaceleraciones = EXCLUDED.desaceleraciones, balones = EXCLUDED.balones, pases = EXCLUDED.pases,
          tiempo_balon_seg = EXCLUDED.tiempo_balon_seg, disparos = EXCLUDED.disparos, tiro_max_kmh = EXCLUDED.tiro_max_kmh
      """)
      ps.setInt(1, matchId)
      ps.setDouble(2, distanciaKm)
      ps.setDouble(3, altaIntensidadM)
      ps.setDouble(4, sprintMaxKmh)
      ps.setDouble(5, pctActividad)
      ps.setInt(6, tiempoActividadMin)
      ps.setInt(7, aceleraciones)
      ps.setInt(8, desaceleraciones)
      ps.setInt(9, balones)
      ps.setInt(10, pases)
      ps.setInt(11, tiempoBalonSeg)
      ps.setInt(12, disparos)
      ps.setDouble(13, tiroMaxKmh)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getFootbar(matchId: Int): Option[FootbarSession] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM footbar_sessions WHERE match_id = ?")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      if (rs.next()) Some(FootbarSession(
        matchId, rs.getDouble("distancia_km"), rs.getDouble("alta_intensidad_m"), rs.getDouble("sprint_max_kmh"),
        rs.getDouble("pct_actividad"), rs.getInt("tiempo_actividad_min"), rs.getInt("aceleraciones"), rs.getInt("desaceleraciones"),
        rs.getInt("balones"), rs.getInt("pases"), rs.getInt("tiempo_balon_seg"), rs.getInt("disparos"), rs.getDouble("tiro_max_kmh")
      )) else None
    } finally { conn.close() }
  }

  // Correlacion Pearson entre distancia recorrida (Footbar) y nota del partido
  def getFootbarCorrelacion(): Double = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT f.distancia_km, m.nota FROM footbar_sessions f
        JOIN matches m ON m.id = f.match_id
        WHERE f.distancia_km > 0
      """)
      var pairs = List[(Double, Double)]()
      while (rs.next()) pairs = pairs :+ (rs.getDouble("distancia_km"), rs.getDouble("nota"))
      calcCorrelation(pairs)
    } finally { conn.close() }
  }

  // Datos para la pagina /footbar: KPIs medios, tabla por partido y serie distancia/nota
  def getFootbarPageData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT f.*, m.rival, m.fecha, m.nota
        FROM footbar_sessions f
        JOIN matches m ON m.id = f.match_id
        WHERE m.status='PLAYED'
        ORDER BY m.fecha DESC
      """)
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "matchId"        -> rs.getInt("match_id"),
          "rival"          -> Option(rs.getString("rival")).getOrElse(""),
          "fecha"          -> rs.getDate("fecha").toString,
          "nota"           -> rs.getDouble("nota"),
          "distanciaKm"    -> rs.getDouble("distancia_km"),
          "sprintMaxKmh"   -> rs.getDouble("sprint_max_kmh"),
          "pases"          -> rs.getInt("pases"),
          "disparos"       -> rs.getInt("disparos")
        )
      }

      def avg(f: Map[String, Any] => Double): Double =
        if (rows.isEmpty) 0.0 else rows.map(f).sum / rows.size

      Map(
        "rows"            -> rows,
        "totalSesiones"   -> rows.size,
        "avgDistanciaKm"  -> avg(_("distanciaKm").asInstanceOf[Double]),
        "maxSprintKmh"    -> (if (rows.isEmpty) 0.0 else rows.map(_("sprintMaxKmh").asInstanceOf[Double]).max),
        "avgPases"        -> avg(_("pases").asInstanceOf[Int].toDouble),
        "correlacionNota" -> getFootbarCorrelacion()
      )
    } finally { conn.close() }
  }

  // ACWR fisico real basado en Footbar: carga GPS objetiva (distancia x % actividad)
  // en lugar de la estimacion por minutos jugados. Devuelve 0.0 si no hay datos
  // suficientes en footbar_sessions durante los ultimos 28 dias.
  def getFootbarACWR(): Double = {
    val conn = getConnection()
    try {
      val rsAcute = conn.createStatement().executeQuery("""
        SELECT COALESCE(SUM(f.distancia_km * (f.pct_actividad / 100.0)), 0) AS carga
        FROM matches m JOIN footbar_sessions f ON f.match_id = m.id
        WHERE m.status='PLAYED' AND m.fecha >= CURRENT_DATE - 7
      """)
      val acuteSum = if (rsAcute.next()) rsAcute.getDouble("carga") else 0.0

      val rsChronic = conn.createStatement().executeQuery("""
        SELECT COALESCE(SUM(f.distancia_km * (f.pct_actividad / 100.0)), 0) AS carga
        FROM matches m JOIN footbar_sessions f ON f.match_id = m.id
        WHERE m.status='PLAYED' AND m.fecha >= CURRENT_DATE - 28
      """)
      val chronicSum = if (rsChronic.next()) rsChronic.getDouble("carga") else 0.0
      val chronicAvg = chronicSum / 4.0  // 28 dias = 4 semanas, equivalente semanal

      if (chronicAvg > 0) acuteSum / chronicAvg else 0.0
    } finally { conn.close() }
  }

  def playScheduledMatch(
                          id: Int, gf: Int, gc: Int, min: Int, nota: Double, paradas: Int,
                          notas: String, video: String, reaccion: String, clima: String, estadio: String,
                          zonaGoles: String, zonaTiros: String, zonaParadas: String,
                          p1v1: Int, pAir: Int, pPie: Int, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int,
                          mapaCampo: String, // <--- NUEVO PARAMETRO
                          distanciaKm: Double = 0.0 // <--- FOOTBAR
                        ): Unit = {
    val conn = getConnection()
    try {
      val c = getLatestCardData()
      val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas, pcTot, pcOk, plTot, plOk, distanciaKm)
      updateStats(n)

      val ps = conn.prepareStatement("""
      UPDATE matches SET
        status='PLAYED', goles_favor=?, goles_contra=?, minutos=?, nota=?, paradas=?,
        notas_partido=?, video_url=?, reaccion_goles=?, clima=?, estadio=?,
        zona_goles=?, zona_tiros=?, zona_paradas=?, paradas_1v1=?, paradas_aereas=?,
        acciones_pie=?, pc_t=?, pc_ok=?, pl_t=?, pl_ok=?, mapa_campo=?
      WHERE id=?
    """)

      ps.setInt(1, gf); ps.setInt(2, gc); ps.setInt(3, min); ps.setDouble(4, nota)
      ps.setInt(5, paradas); ps.setString(6, fixEncoding(notas)); ps.setString(7, video)
      ps.setString(8, fixEncoding(reaccion)); ps.setString(9, clima); ps.setString(10, fixEncoding(estadio))
      ps.setString(11, zonaGoles); ps.setString(12, zonaTiros); ps.setString(13, zonaParadas)
      ps.setInt(14, p1v1); ps.setInt(15, pAir); ps.setInt(16, pPie)
      ps.setInt(17, pcTot); ps.setInt(18, pcOk); ps.setInt(19, plTot); ps.setInt(20, plOk)
      ps.setString(21, mapaCampo) // <--- NUEVO
      ps.setInt(22, id)

      ps.executeUpdate()
    } finally {
      conn.close()
    }
  }

  // --- LECTURA DE PARTIDOS EXTENDIDA ---
  def getMatchesList(): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='PLAYED' ORDER BY fecha DESC"); while(rs.next()){ l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString, Option(rs.getString("clima")).getOrElse(""), Option(rs.getString("estadio")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"), Option(rs.getString("analisis_voz")).getOrElse(""), Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"), rs.getInt("acciones_pie"), Option(rs.getString("zona_tiros")).getOrElse(""), Option(rs.getString("zona_goles")).getOrElse("")) } } finally {conn.close()}; l }
  def getUpcomingMatches(): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC"); while(rs.next()){ l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), "-", 0, 0, rs.getDate("fecha").toString, "", Option(rs.getString("estadio")).getOrElse(""), "", "", "", rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"),0,0,0,0, "", Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), 0,0,0,0,"","") } } finally {conn.close()}; l }
  def getMatchById(id: Int): Option[MatchLog] = { var m:Option[MatchLog]=None; val conn=getConnection(); try { val s=conn.prepareStatement("SELECT * FROM matches WHERE id = ?"); s.setInt(1,id); val rs=s.executeQuery(); if(rs.next()){ m=Some(MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString, Option(rs.getString("clima")).getOrElse("Sol"), Option(rs.getString("estadio")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"), Option(rs.getString("analisis_voz")).getOrElse(""), Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"), rs.getInt("acciones_pie"), Option(rs.getString("zona_tiros")).getOrElse(""), Option(rs.getString("zona_goles")).getOrElse(""))) } } finally { conn.close() }; m }
  def getRivalScouting(rivalBusqueda: String): (List[MatchLog], Map[String, Int]) = { var matches = List[MatchLog](); var stats = scala.collection.mutable.Map("pj"->0, "gf"->0, "gc"->0, "ganados"->0, "empatados"->0, "perdidos"->0); val conn = getConnection(); try { val query = s"SELECT * FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED' ORDER BY fecha DESC"; val stmt = conn.prepareStatement(query); stmt.setString(1, s"%$rivalBusqueda%"); val rs = stmt.executeQuery(); while(rs.next()) { val (gf, gc) = (rs.getInt("goles_favor"), rs.getInt("goles_contra")); matches = matches :+ MatchLog(rs.getInt("id"), rs.getString("rival"), s"$gf-$gc", rs.getInt("minutos"), rs.getDouble("nota"), rs.getString("fecha"), Option(rs.getString("clima")).getOrElse(""), Option(rs.getString("estadio")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"), Option(rs.getString("analisis_voz")).getOrElse(""), Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"), rs.getInt("acciones_pie"), Option(rs.getString("zona_tiros")).getOrElse(""), Option(rs.getString("zona_goles")).getOrElse("")); stats("pj") += 1; stats("gf") += gf; stats("gc") += gc; if(gf > gc) stats("ganados") += 1 else if(gf == gc) stats("empatados") += 1 else stats("perdidos") += 1 } } finally { conn.close() }; (matches, stats.toMap) }

  // --- FUNCIONES EXTRA ---
  def createTournament(nombre: String, estructura: String): String = { val conn=getConnection(); var count=0; try{ val rsId=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rsId.next()){ val sId=rsId.getInt("id"); val lines=estructura.split("\n").map(_.trim).filter(_.nonEmpty); val ps=conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas, clima, estadio, torneo_nombre, fase) VALUES (?, ?, ?, 'TORNEO', 'SCHEDULED', 0, 0, 0, 0, 0, 'Sol', 'Sede Torneo', ?, ?)"); lines.foreach { l => val p=l.split("\\|").map(_.trim); if(p.length>=2){ ps.setInt(1, sId); ps.setDate(2, if(p.length>2) try Date.valueOf(p(2)) catch {case _:Exception=>Date.valueOf(LocalDate.now())} else Date.valueOf(LocalDate.now())); ps.setString(3, fixEncoding(p(1))); ps.setString(4, fixEncoding(nombre)); ps.setString(5, fixEncoding(p(0))); ps.executeUpdate(); count += 1 } } } else return "Error: Crea una temporada primero." } catch { case e: Exception => return s"Error: ${e.getMessage}" } finally { conn.close() }; s"Torneo '$nombre' creado ($count partidos)." }
  def syncRFFMCalendar(): String = { var logs=new StringBuilder(); var count=0; val conn=getConnection(); try{ val rsCfg=conn.createStatement().executeQuery("SELECT id, rffm_url, rffm_team_name FROM seasons ORDER BY id DESC LIMIT 1"); if(!rsCfg.next()) return "Error: Sin temporada."; val (sid,url,myTeam)=(rsCfg.getInt("id"), Option(rsCfg.getString("rffm_url")).getOrElse(""), Option(rsCfg.getString("rffm_team_name")).getOrElse("").toUpperCase); if(url.isEmpty || myTeam.isEmpty) return "Error Config."; val doc=Jsoup.connect(url).userAgent("Mozilla/5.0").timeout(10000).get(); val ps=conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas, clima, estadio, torneo_nombre, fase) VALUES (?, ?, ?, 'LIGA', 'SCHEDULED', 0, 0, 0, 0, 0, 'Sol', ?, '', 'Regular')"); for(row<-doc.select("table tbody tr").asScala){ val cols=row.select("td"); if(cols.size()>=4){ val (loc,vis)=(cols.get(0).text().toUpperCase.trim, cols.get(2).text().toUpperCase.trim); if(loc.contains(myTeam)||vis.contains(myTeam)){ val rival=if(loc.contains(myTeam)) vis else loc; val campo=if(cols.get(3).text().length>50) cols.get(3).text().take(50) else cols.get(3).text(); if(conn.createStatement().executeQuery(s"SELECT count(*) FROM matches WHERE season_id=$sid AND rival='${fixEncoding(rival)}'").next()){ ps.setInt(1, sid); ps.setDate(2, Date.valueOf(LocalDate.now().plusDays(7))); ps.setString(3, fixEncoding(rival)); ps.setString(4, fixEncoding(campo)); ps.executeUpdate(); count+=1; logs.append(s"+ $rival\n") } } } } } catch { case e: Exception => logs.append(s"Err: ${e.getMessage}") } finally { conn.close() }; logs.toString() }
  def updateMatch(id: Int, rival: String, gf: Int, gc: Int, min: Int, nota: Double, clima: String, estadio: String, temp: Int, notas: String, video: String, reaccion: String, fechaStr: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE matches SET rival=?, goles_favor=?, goles_contra=?, minutos=?, nota=?, clima=?, estadio=?, temperatura=?, notas_partido=?, video_url=?, reaccion_goles=?, fecha=? WHERE id=?"); s.setString(1,fixEncoding(rival)); s.setInt(2,gf); s.setInt(3,gc); s.setInt(4,min); s.setDouble(5,nota); s.setString(6,clima); s.setString(7,fixEncoding(estadio)); s.setInt(8,temp); s.setString(9,fixEncoding(notas)); s.setString(10,video); s.setString(11,fixEncoding(reaccion)); s.setDate(12,Date.valueOf(fechaStr)); s.setInt(13,id); s.executeUpdate() } finally { conn.close() } }
  def updateMatchExtra(id: Int, tipo: String, esLocal: String): Unit = { val conn=getConnection(); try { val esLocalVal: java.lang.Boolean = esLocal match { case "true" => true; case "false" => false; case _ => null }; val ps = conn.prepareStatement("UPDATE matches SET tipo_partido=? WHERE id=?"); ps.setString(1, if (tipo.nonEmpty) tipo else "LIGA"); ps.setInt(2, id); ps.executeUpdate(); if (esLocalVal != null) { val ps2 = conn.prepareStatement("UPDATE matches SET es_local=? WHERE id=?"); ps2.setBoolean(1, esLocalVal); ps2.setInt(2, id); ps2.executeUpdate() } } finally { conn.close() } }
  def deleteMatch(id: Int): Unit = { val conn=getConnection(); try { conn.createStatement().executeUpdate(s"DELETE FROM matches WHERE id=$id") } finally { conn.close() } }
  def updateRFFMSettings(url: String, teamName: String): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("UPDATE seasons SET rffm_url=?, rffm_team_name=? WHERE id=(SELECT MAX(id) FROM seasons)"); ps.setString(1,url); ps.setString(2,teamName); ps.executeUpdate() } finally { conn.close() } }
  def updateSeasonSettings(f: String, c: String, n: String, fecha: String): String = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE seasons SET foto_jugador_url=COALESCE(NULLIF(?,''), foto_jugador_url), club_escudo_url=COALESCE(NULLIF(?,''), club_escudo_url), nombre_club=COALESCE(NULLIF(?,''), nombre_club), fecha_nacimiento=? WHERE id=(SELECT MAX(id) FROM seasons)"); s.setString(1,f); s.setString(2,c); s.setString(3,fixEncoding(n)); s.setDate(4, Date.valueOf(fecha)); s.executeUpdate(); "DATOS ACTUALIZADOS" } finally { conn.close() } }
  def getLatestCardData(): PlayerCardData = { var conn: Connection=null; try { conn=getConnection(); val rs=conn.createStatement().executeQuery("SELECT * FROM seasons ORDER BY id DESC LIMIT 1"); if(rs.next()){ val fecha=Option(rs.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2020-06-19"); PlayerCardData("HECTOR", rs.getDouble("media").toInt, "GK", Option(rs.getString("foto_jugador_url")).getOrElse(""), Option(rs.getString("club_escudo_url")).getOrElse(""), "", Option(rs.getString("nombre_club")).getOrElse(""), rs.getDouble("stat_div").toInt, rs.getDouble("stat_han").toInt, rs.getDouble("stat_kic").toInt, rs.getDouble("stat_ref").toInt, rs.getDouble("stat_spd").toInt, rs.getDouble("stat_pos").toInt, rs.getDouble("stat_div"), rs.getDouble("stat_han"), rs.getDouble("stat_kic"), rs.getDouble("stat_ref"), rs.getDouble("stat_spd"), rs.getDouble("stat_pos"), fecha, Option(rs.getString("rffm_url")).getOrElse(""), Option(rs.getString("rffm_team_name")).getOrElse("")) } else { PlayerCardData("HECTOR", 59, "GK", "", "", "", "", 80, 60, 55, 60, 62, 58, 80, 60, 55, 60, 62, 58, "2020-06-19", "", "") } } finally { if(conn!=null) conn.close() } }
  def getDeepAnalysis(): String = {
    var conn:Connection=null;
    try {
      conn=getConnection(); val sb=new StringBuilder(); val card=getLatestCardData(); val edad=calcularEdadExacta(card.fechaNacimiento);
      sb.append(s"Analista Elite ($edad anos). Tendencias:\n");
      val rs=conn.createStatement().executeQuery("""
        SELECT m.fecha, m.rival, m.nota,
               COALESCE(f.distancia_km, 0)      AS dist_km,
               COALESCE(f.sprint_max_kmh, 0)    AS sprint_max,
               COALESCE(f.pases, 0)             AS pases
        FROM matches m
        LEFT JOIN footbar_sessions f ON f.match_id = m.id
        WHERE m.status='PLAYED'
        ORDER BY m.fecha ASC
      """);
      var c=0; while(rs.next()){ c+=1; sb.append(s"${rs.getString("fecha")}|${rs.getString("rival")}|${rs.getDouble("nota")}|${rs.getDouble("dist_km")}|${rs.getDouble("sprint_max")}|${rs.getInt("pases")}\n") };
      if(c<2) return "Pocos datos.";
      // Cambio aqui: Llamamos a AIProvider.ask
      val prompt = s"""Eres un analista de rendimiento de porteros de élite.
Tienes los siguientes partidos de Hector (portero, ${edad} años), con formato fecha|rival|nota|distanciaKm|sprintMaxKmh|pases (los tres ultimos son datos del sensor Footbar; 0 si no se registraron para ese partido):

${sb.toString()}

Escribe un análisis narrativo en HTML limpio (sin markdown, sin bloques de código). Usa exactamente esta estructura:
<h4>ANÁLISIS</h4>
<p><strong>Tendencia general:</strong> [un párrafo describiendo la evolución de las notas a lo largo del tiempo, si va subiendo, bajando, o irregular]</p>
<p><strong>Mejor racha:</strong> [describe el período de mejor rendimiento y contra qué rivales]</p>
<p><strong>Punto de atención:</strong> [describe el momento más bajo y posibles causas]</p>
<p><strong>Conclusión:</strong> [una frase motivadora y concreta sobre qué trabajar para la próxima semana]</p>

No reproduzcas la tabla de datos. Escribe siempre en párrafos. Habla en segunda persona dirigiéndote a Hector directamente. Si hay datos de distancia y sprint, analiza si hay correlación entre carga física y rendimiento. Si distancia > 3km con nota baja, o sprint alto con nota baja, menciónalo como señal de fatiga."""
      AIProvider.ask(prompt).replace("```html","").replace("```","").trim
    } catch {
      case e:Exception =>
        e.printStackTrace() // Esto hara que el error aparezca en el log de Render/Consola
        "Error: " + e.getMessage
    }
  }
  def getChartData(): String = { var l=List[String](); var d=List[Double](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT rival, media_historica FROM matches WHERE status='PLAYED' ORDER BY fecha ASC LIMIT 15"); while(rs.next()){ l=l:+s"'${rs.getString("rival")}'"; d=d:+rs.getDouble("media_historica") } } finally {conn.close()}; s"""{ "labels": [${l.mkString(",")}], "data": [${d.mkString(",")}] }""" }
  def getAchievements(): List[Achievement] = { var l=List[Achievement](); val conn=getConnection(); try { val s=conn.createStatement(); val r1=s.executeQuery("SELECT COUNT(*) FROM matches WHERE goles_contra=0 AND status='PLAYED'"); if(r1.next()&&r1.getInt(1)>=5) l=l:+Achievement("(M)","El Muro",r1.getInt(1)/5,""); val r2=s.executeQuery("SELECT COUNT(*) FROM matches WHERE nota>=9 AND status='PLAYED'"); if(r2.next()&&r2.getInt(1)>0) l=l:+Achievement("(E)","MVP",r2.getInt(1),"") } finally { conn.close() }; l }
  def getSeasonObjectives(): List[Objective] = { var l=List[Objective](); val conn=getConnection(); try { val rsObj=conn.createStatement().executeQuery("SELECT id, tipo, objetivo, descripcion FROM objectives"); val objs=new scala.collection.mutable.ListBuffer[(Int,String,Int,String)](); while(rsObj.next()) objs+=((rsObj.getInt("id"),rsObj.getString("tipo"),rsObj.getInt("objetivo"),rsObj.getString("descripcion"))); val rsStats=conn.createStatement().executeQuery("SELECT COUNT(*) as pj, COUNT(CASE WHEN goles_contra=0 THEN 1 END) as cs, AVG(nota) as media FROM matches WHERE status='PLAYED'"); var (cs,pj,md)=(0,0,0.0); if(rsStats.next()){cs=rsStats.getInt("cs");pj=rsStats.getInt("pj");md=rsStats.getDouble("media")}; objs.foreach { case (id,t,m,d) => val act=t match { case "CleanSheets"=>cs.toDouble case "MediaNota"=>md case "PartidosJugados"=>pj.toDouble case _=>0.0 }; l=l:+Objective(id,t,act,m,d) } } finally { conn.close() }; l }
  def getGoalHeatmap(temporada: String = ""): Map[String, Int] = {
    val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
    val counts = scala.collection.mutable.Map(zones.map(_ -> 0): _*)
    val conn = getConnection()
    try {
      val where = if (temporada.nonEmpty) s"AND fecha >= '$temporada-01-01' AND fecha <= '$temporada-12-31'" else ""
      val rs = conn.createStatement().executeQuery(
        s"SELECT zona_goles FROM matches WHERE status='PLAYED' AND zona_goles IS NOT NULL AND zona_goles != '' $where"
      )
      while (rs.next()) {
        val zg = rs.getString("zona_goles")
        zg.split(",").filter(_.nonEmpty).foreach { z =>
          val key = z.trim.toUpperCase
          if (counts.contains(key)) counts(key) += 1
        }
      }
    } finally { conn.close() }
    counts.toMap
  }

  def getGoalHeatmapByRival(rival: String): Map[String, Int] = {
    val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
    val counts = scala.collection.mutable.Map(zones.map(_ -> 0): _*)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT zona_goles FROM matches WHERE status='PLAYED' AND LOWER(rival) LIKE LOWER(?) AND zona_goles IS NOT NULL AND zona_goles != ''"
      )
      ps.setString(1, s"%$rival%")
      val rs = ps.executeQuery()
      while (rs.next()) {
        rs.getString("zona_goles").split(",").filter(_.nonEmpty).foreach { z =>
          val key = z.trim.toUpperCase
          if (counts.contains(key)) counts(key) += 1
        }
      }
    } finally { conn.close() }
    counts.toMap
  }

  def getPenaltyDetailedStats(): (Map[String, (Int,Int)], List[(String,Int,Int)]) = {
    // (zona -> (total, parados)), List[(rival, total, goles)]
    val byZone  = scala.collection.mutable.Map[String,(Int,Int)]()
    val byRival = scala.collection.mutable.ListBuffer[(String,Int,Int)]()
    val conn = getConnection()
    try {
      val rs1 = conn.createStatement().executeQuery(
        "SELECT zona_tiro, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 0 ELSE 1 END) as parados FROM penalties GROUP BY zona_tiro ORDER BY total DESC"
      )
      while (rs1.next()) byZone(rs1.getString("zona_tiro")) = (rs1.getInt("total"), rs1.getInt("parados"))
      val rs2 = conn.createStatement().executeQuery(
        "SELECT rival, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 1 ELSE 0 END) as goles FROM penalties WHERE rival IS NOT NULL AND rival != '' GROUP BY rival ORDER BY total DESC LIMIT 10"
      )
      while (rs2.next()) byRival += ((rs2.getString("rival"), rs2.getInt("total"), rs2.getInt("goles")))
    } finally { conn.close() }
    (byZone.toMap, byRival.toList)
  }

  def getTournamentMatches(nombre: String): List[MatchLog] = {
    var l = List[MatchLog]()
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM matches WHERE LOWER(torneo_nombre)=LOWER(?) ORDER BY id ASC")
      ps.setString(1, fixEncoding(nombre))
      val rs = ps.executeQuery()
      while (rs.next()) l = l :+ MatchLog(
        rs.getInt("id"), rs.getString("rival"),
        s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}",
        rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString,
        Option(rs.getString("clima")).getOrElse(""),
        Option(rs.getString("estadio")).getOrElse(""),
        Option(rs.getString("notas_partido")).getOrElse(""),
        Option(rs.getString("video_url")).getOrElse(""),
        Option(rs.getString("reaccion_goles")).getOrElse(""),
        rs.getString("status"),
        Option(rs.getString("tipo_partido")).getOrElse("TORNEO"),
        rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"),
        Option(rs.getString("analisis_voz")).getOrElse(""),
        Option(rs.getString("torneo_nombre")).getOrElse(""),
        Option(rs.getString("fase")).getOrElse(""),
        rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"),
        rs.getInt("acciones_pie"),
        Option(rs.getString("zona_tiros")).getOrElse(""),
        Option(rs.getString("zona_goles")).getOrElse("")
      )
    } finally { conn.close() }
    l
  }

  def getTournamentNames(): List[String] = {
    var l = List[String]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT DISTINCT torneo_nombre FROM matches WHERE torneo_nombre IS NOT NULL AND torneo_nombre != '' ORDER BY torneo_nombre"
      )
      while (rs.next()) l = l :+ rs.getString("torneo_nombre")
    } finally { conn.close() }
    l
  }

  def getSeasonEvolution(): List[(String, Double, Int, Int, Int)] = {
    // (anio, mediaPartidos, pj, gc, porteriasCero)
    var l = List[(String, Double, Int, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          EXTRACT(YEAR FROM fecha)::TEXT as anio,
          AVG(nota) as media,
          COUNT(*) as pj,
          SUM(goles_contra) as gc,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as pcs
        FROM matches WHERE status='PLAYED'
        GROUP BY EXTRACT(YEAR FROM fecha)
        ORDER BY anio ASC
      """)
      while (rs.next()) l = l :+ (
        rs.getString("anio"),
        rs.getDouble("media"),
        rs.getInt("pj"),
        rs.getInt("gc"),
        rs.getInt("pcs")
      )
    } finally { conn.close() }
    l
  }

  case class InjuryRecord(id: Int, fechaInicio: String, fechaAlta: String, zona: String, tipo: String, gravedad: String, descripcion: String, diasBaja: Int, activa: Boolean)

  def logInjury(zona: String, tipo: String, gravedad: String, desc: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("INSERT INTO injuries (zona, tipo, gravedad, descripcion, activa) VALUES (?,?,?,?,true)")
      ps.setString(1, fixEncoding(zona)); ps.setString(2, fixEncoding(tipo))
      ps.setString(3, gravedad); ps.setString(4, fixEncoding(desc))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def closeInjury(id: Int, fechaAlta: String, diasBaja: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE injuries SET activa=false, fecha_alta=?, dias_baja=? WHERE id=?")
      ps.setDate(1, java.sql.Date.valueOf(fechaAlta))
      ps.setInt(2, diasBaja); ps.setInt(3, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getInjuries(): List[InjuryRecord] = {
    var l = List[InjuryRecord]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM injuries ORDER BY fecha_inicio DESC")
      while (rs.next()) l = l :+ InjuryRecord(
        rs.getInt("id"),
        rs.getDate("fecha_inicio").toString,
        Option(rs.getDate("fecha_alta")).map(_.toString).getOrElse(""),
        Option(rs.getString("zona")).getOrElse(""),
        Option(rs.getString("tipo")).getOrElse(""),
        Option(rs.getString("gravedad")).getOrElse("LEVE"),
        Option(rs.getString("descripcion")).getOrElse(""),
        rs.getInt("dias_baja"),
        rs.getBoolean("activa")
      )
    } finally { conn.close() }
    l
  }

  def getCleanSheetPredictor(rival: String): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Historial vs rival
      val psR = conn.prepareStatement("SELECT COUNT(*) as pj, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as pcs, AVG(nota) as avg_nota FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED'")
      psR.setString(1, s"%$rival%")
      val rsR = psR.executeQuery()
      val (pj, pcs, avgNota) = if (rsR.next()) (rsR.getInt("pj"), rsR.getInt("pcs"), rsR.getDouble("avg_nota")) else (0, 0, 0.0)

      // Forma reciente (ultimos 5 partidos)
      val rsF = conn.createStatement().executeQuery("SELECT AVG(nota) as avg, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as pcs, COUNT(*) as pj FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 5")
      val (formaAvg, formaPcs, formaPj) = if (rsF.next()) (rsF.getDouble("avg"), rsF.getInt("pcs"), rsF.getInt("pj")) else (0.0, 0, 5)

      // Sueno ultima noche
      val rsS = conn.createStatement().executeQuery("SELECT horas_sueno FROM wellness ORDER BY fecha DESC LIMIT 1")
      val horasSueno = if (rsS.next()) rsS.getDouble("horas_sueno") else 0.0

      // ACWR inline
      val rsAc = conn.prepareStatement("SELECT COALESCE(SUM(rpe*60),0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAc.setInt(1, 7); val rsAcR = rsAc.executeQuery()
      val acuteLoad = if (rsAcR.next()) rsAcR.getDouble(1) else 0.0
      val rsCh = conn.prepareStatement("SELECT COALESCE(SUM(rpe*60),0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCh.setInt(1, 28); val rsChR = rsCh.executeQuery()
      val chronicLoad = if (rsChR.next()) rsChR.getDouble(1) else 0.0
      val acwr = if (chronicLoad > 0) (acuteLoad / 7.0) / (chronicLoad / 28.0) else 1.0

      // Calcular probabilidad (heuristica)
      var score = 50.0
      if (pj > 0)       score += (pcs.toDouble / pj) * 20
      if (formaPj > 0)  score += (formaPcs.toDouble / formaPj) * 15
      if (formaAvg > 7) score += 10 else if (formaAvg > 5) score += 5
      if (horasSueno >= 8) score += 8 else if (horasSueno < 6 && horasSueno > 0) score -= 8
      if (acwr > 1.5)   score -= 10 else if (acwr < 0.8) score -= 5
      val prob = math.min(95, math.max(5, score.toInt))

      Map("prob" -> prob, "pj" -> pj, "pcs" -> pcs, "avgNota" -> avgNota,
          "formaAvg" -> formaAvg, "horasSueno" -> horasSueno, "acwr" -> acwr)
    } finally { conn.close() }
  }

  def getGloveThermostat(): (String, String, String) = {
    // (recomendacion, motivo, latex_type)
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT clima, temperatura FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 5"
      )
      var temps = List[Int](); var climas = List[String]()
      while (rs.next()) {
        temps = temps :+ rs.getInt("temperatura")
        climas = climas :+ Option(rs.getString("clima")).getOrElse("Sol")
      }
      val avgTemp = if (temps.nonEmpty) temps.sum / temps.size else 15
      val hasRain = climas.count(c => c.contains("Lluvia") || c.contains("lluvia")) > 0
      val (latex, motivo) = if (avgTemp <= 5) ("Latex Hibrido Frio", s"Temperatura media ${avgTemp}C — latex hibrido mantiene agarre")
                            else if (avgTemp <= 12) ("Latex Soft Grip", s"Temperatura fresca ${avgTemp}C — latex blando optimo")
                            else if (hasRain) ("Latex Aqua", "Condiciones humedas recientes — latex aqua recomendado")
                            else if (avgTemp >= 25) ("Latex Duo Soft", s"Temperatura alta ${avgTemp}C — latex suave con respiro")
                            else ("Latex Contact", s"Condiciones ideales ${avgTemp}C — latex contact estandar")
      val gloves = conn.createStatement().executeQuery("SELECT nombre FROM gear WHERE tipo='Guantes' AND activo=TRUE LIMIT 1")
      val gloveName = if (gloves.next()) gloves.getString("nombre") else "Sin guantes registrados"
      (latex, motivo, gloveName)
    } finally { conn.close() }
  }

  def getFlashCardData(rival: String): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Historial vs rival
      val ps = conn.prepareStatement("SELECT goles_favor, goles_contra, nota, fecha, zona_goles, notas_partido FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED' ORDER BY fecha DESC LIMIT 5")
      ps.setString(1, s"%$rival%")
      val rs = ps.executeQuery()
      var partidos = List[(String,Double,String,String,String)]()
      while (rs.next()) {
        val res = s"${rs.getInt(1)}-${rs.getInt(2)}"
        partidos = partidos :+ (res, rs.getDouble(3), rs.getString(4),
          Option(rs.getString(5)).getOrElse(""), Option(rs.getString(6)).getOrElse(""))
      }

      // Zona mas vulnerable vs ese rival
      val zonasCombinadas = partidos.flatMap(_._4.split(",").filter(_.nonEmpty))
      val zonaMasVulnerable = if (zonasCombinadas.nonEmpty) zonasCombinadas.groupBy(identity).maxBy(_._2.size)._1 else "—"

      // Clips de paradas recientes
      val rsV = conn.createStatement().executeQuery(
        "SELECT vt.minuto, vt.segundo, vt.tipo, m.rival, m.video_url FROM video_tags vt JOIN matches m ON vt.match_id=m.id WHERE vt.tipo='PARADA' AND m.video_url IS NOT NULL ORDER BY m.fecha DESC LIMIT 3"
      )
      var clips = List[(Int,Int,String,String,String)]()
      while (rsV.next()) clips = clips :+ (rsV.getInt(1), rsV.getInt(2), rsV.getString(3), rsV.getString(4), rsV.getString(5))

      // Rival info
      val rsRi = conn.prepareStatement("SELECT estilo_juego, jugadores_clave FROM rivals WHERE LOWER(nombre) LIKE LOWER(?)")
      rsRi.setString(1, s"%$rival%")
      val rsRiR = rsRi.executeQuery()
      val (estilo, claves) = if (rsRiR.next()) (Option(rsRiR.getString("estilo_juego")).getOrElse(""), Option(rsRiR.getString("jugadores_clave")).getOrElse("")) else ("", "")

      Map("partidos" -> partidos, "zonaMasVulnerable" -> zonaMasVulnerable,
          "clips" -> clips, "estilo" -> estilo, "claves" -> claves)
    } finally { conn.close() }
  }

  // ── GK INFLUENCE ANALYTICS ────────────────────────────────────────────────
  def getGKInfluenceStats(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Distribuciones con pie (acciones_pie) y resultado posterior
      val rs = conn.createStatement().executeQuery("""
        SELECT
          AVG(acciones_pie) as avg_pie,
          SUM(acciones_pie) as total_pie,
          AVG(CASE WHEN acciones_pie > 5 THEN nota ELSE NULL END) as nota_alta_pie,
          AVG(CASE WHEN acciones_pie <= 5 THEN nota ELSE NULL END) as nota_baja_pie,
          AVG(CASE WHEN pc_t > 0 THEN CAST(pc_ok AS FLOAT)/pc_t ELSE NULL END) as pct_centros,
          AVG(CASE WHEN pl_t > 0 THEN CAST(pl_ok AS FLOAT)/pl_t ELSE NULL END) as pct_largos,
          COUNT(*) as pj,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as pcs,
          AVG(nota) as avg_nota
        FROM matches WHERE status='PLAYED'
      """)
      if (!rs.next()) return Map.empty

      val avgPie      = rs.getDouble("avg_pie")
      val totalPie    = rs.getInt("total_pie")
      val notaAltaPie = rs.getDouble("nota_alta_pie")
      val notaBajaPie = rs.getDouble("nota_baja_pie")
      val pctCentros  = rs.getDouble("pct_centros")
      val pctLargos   = rs.getDouble("pct_largos")
      val pj          = rs.getInt("pj")
      val pcs         = rs.getInt("pcs")
      val avgNota     = rs.getDouble("avg_nota")

      // Serie temporal: acciones_pie + nota por partido (ultimos 20)
      val rsSerie = conn.createStatement().executeQuery(
        "SELECT fecha::TEXT, acciones_pie, nota, rival FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 20"
      )
      var serie = List[(String, Int, Double, String)]()
      while (rsSerie.next()) serie = serie :+ (
        rsSerie.getString(1), rsSerie.getInt(2), rsSerie.getDouble(3), rsSerie.getString(4)
      )

      // Distribucion por tipo de balon parado
      val rsTipo = conn.createStatement().executeQuery("""
        SELECT
          SUM(pc_t) as cent_total, SUM(pc_ok) as cent_ok,
          SUM(pl_t) as larg_total, SUM(pl_ok) as larg_ok
        FROM matches WHERE status='PLAYED'
      """)
      val (centTotal, centOk, largTotal, largOk) = if (rsTipo.next())
        (rsTipo.getInt(1), rsTipo.getInt(2), rsTipo.getInt(3), rsTipo.getInt(4))
      else (0, 0, 0, 0)

      Map(
        "avgPie" -> avgPie, "totalPie" -> totalPie,
        "notaAltaPie" -> notaAltaPie, "notaBajaPie" -> notaBajaPie,
        "pctCentros" -> pctCentros, "pctLargos" -> pctLargos,
        "pj" -> pj, "pcs" -> pcs, "avgNota" -> avgNota,
        "serie" -> serie,
        "centTotal" -> centTotal, "centOk" -> centOk,
        "largTotal" -> largTotal, "largOk" -> largOk
      )
    } finally { conn.close() }
  }

  // ── BIOMECANICA POSICIONAL ─────────────────────────────────────────────────
  def getBiomecPosicional(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Zonas de gol encajado vs zonas de parada (9 zonas: TL,TC,TR,ML,MC,MR,BL,BC,BR)
      val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
      val golesMap  = scala.collection.mutable.Map(zones.map(_ -> 0): _*)
      val paradasMap= scala.collection.mutable.Map(zones.map(_ -> 0): _*)
      val tirosMap  = scala.collection.mutable.Map(zones.map(_ -> 0): _*)

      val rs = conn.createStatement().executeQuery(
        "SELECT zona_goles, zona_paradas, zona_tiros FROM matches WHERE status='PLAYED'"
      )
      while (rs.next()) {
        Option(rs.getString("zona_goles")).getOrElse("").split(",").filter(_.nonEmpty).foreach { z =>
          val k = z.trim.toUpperCase; if (golesMap.contains(k)) golesMap(k) += 1
        }
        Option(rs.getString("zona_paradas")).getOrElse("").split(",").filter(_.nonEmpty).foreach { z =>
          val k = z.trim.toUpperCase; if (paradasMap.contains(k)) paradasMap(k) += 1
        }
        Option(rs.getString("zona_tiros")).getOrElse("").split(",").filter(_.nonEmpty).foreach { z =>
          val k = z.trim.toUpperCase; if (tirosMap.contains(k)) tirosMap(k) += 1
        }
      }

      // Puntos ciegos: zonas donde goles > paradas (vulnerables)
      val puntosCiegos = zones.filter(z => golesMap(z) > paradasMap(z)).sortBy(-golesMap(_))

      // Zonas fuertes: zonas donde paradas > goles
      val zonasFuertes = zones.filter(z => paradasMap(z) > golesMap(z)).sortBy(-paradasMap(_))

      // Eficiencia por zona: paradas / (paradas + goles)
      val eficiencia = zones.map { z =>
        val total = paradasMap(z) + golesMap(z)
        val pct = if (total > 0) (paradasMap(z).toDouble / total * 100).toInt else -1
        z -> pct
      }.toMap

      // Porcentaje de stop por zona de tiro
      val stopRate = zones.map { z =>
        val goles = golesMap(z); val paradas = paradasMap(z)
        val total = goles + paradas
        z -> (if (total > 0) f"${paradas.toDouble/total*100}%.0f%%" else "—")
      }.toMap

      Map(
        "goles" -> golesMap.toMap, "paradas" -> paradasMap.toMap,
        "tiros" -> tirosMap.toMap, "eficiencia" -> eficiencia,
        "puntosCiegos" -> puntosCiegos, "zonasFuertes" -> zonasFuertes,
        "stopRate" -> stopRate, "zones" -> zones
      )
    } finally { conn.close() }
  }


  // ── EMOTIONAL INTELLIGENCE ENGINE ─────────────────────────────────────────
  case class EmotionalEntry(fecha: String, animo: Int, energia: Int, notas: String,
                             notaPartido: Option[Double], reaccionGoles: String)

  def getEmotionalData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Serie de 45 dias: animo + energia + notas conducta + nota partido ese dia
      val rs = conn.createStatement().executeQuery("""
        SELECT
          w.fecha::TEXT,
          COALESCE(w.animo, 3)    as animo,
          COALESCE(w.energia, 3)  as energia,
          COALESCE(w.notas_conducta, '') as notas,
          m.nota                  as nota_partido,
          COALESCE(m.reaccion_goles, '') as reaccion
        FROM wellness w
        LEFT JOIN matches m ON m.fecha = w.fecha AND m.status = 'PLAYED'
        WHERE w.fecha >= CURRENT_DATE - 45
        ORDER BY w.fecha ASC
      """)
      var entries = List[EmotionalEntry]()
      while (rs.next()) {
        val notaOpt = { val v = rs.getDouble("nota_partido"); if (rs.wasNull()) None else Some(v) }
        entries = entries :+ EmotionalEntry(
          rs.getString("fecha"), rs.getInt("animo"), rs.getInt("energia"),
          Option(rs.getString("notas")).getOrElse(""), notaOpt,
          Option(rs.getString("reaccion")).getOrElse("")
        )
      }

      // Correlacion animo -> nota partido
      val conPartido = entries.filter(_.notaPartido.isDefined)
      val correlacion: Double = if (conPartido.size >= 3) {
        val animoAlto  = conPartido.filter(_.animo >= 4).flatMap(_.notaPartido)
        val animoBajo  = conPartido.filter(_.animo <= 2).flatMap(_.notaPartido)
        val diffCorr = (if (animoAlto.nonEmpty) animoAlto.sum / animoAlto.size else 0.0) -
                       (if (animoBajo.nonEmpty) animoBajo.sum / animoBajo.size else 0.0)
        diffCorr
      } else 0.0

      // Patron emocional: dias bajos consecutivos (riesgo burnout)
      val diasBajosConsecutivos = {
        var maxRacha = 0; var racha = 0
        entries.foreach { e =>
          if (e.animo <= 2 || e.energia <= 2) { racha += 1; maxRacha = math.max(maxRacha, racha) }
          else racha = 0
        }
        maxRacha
      }

      // Estado emocional actual (ultimos 7 dias)
      val recientes = entries.takeRight(7)
      val avgAnimoReciente  = if (recientes.nonEmpty) recientes.map(_.animo.toDouble).sum / recientes.size else 3.0
      val avgEnergiaReciente= if (recientes.nonEmpty) recientes.map(_.energia.toDouble).sum / recientes.size else 3.0

      // Notas de conducta recientes para analisis IA
      val notasParaIA = entries.takeRight(14)
        .filter(_.notas.nonEmpty)
        .map(e => s"${e.fecha}: ${e.notas}")
        .mkString(" | ")

      // Reacciones a goles encajados
      val reacciones = entries.filter(_.reaccionGoles.nonEmpty).map(_.reaccionGoles).take(5)

      // Score de resiliencia emocional (0-100)
      val resilienciaScore = {
        var s = 50.0
        if (correlacion > 1.0) s += 15 else if (correlacion > 0.5) s += 8
        if (diasBajosConsecutivos == 0) s += 15 else if (diasBajosConsecutivos <= 2) s += 5 else s -= 10
        if (avgAnimoReciente >= 4) s += 10 else if (avgAnimoReciente <= 2) s -= 10
        if (avgEnergiaReciente >= 4) s += 10 else if (avgEnergiaReciente <= 2) s -= 5
        math.min(99, math.max(1, s.toInt))
      }

      // Analisis IA de las notas emocionales (bypassCache para siempre tener fresco)
      val analisisIA: String = if (notasParaIA.nonEmpty) {
        val prompt = s"""Eres un psicopedagogo deportivo analizando el diario emocional de Hector, portero de 9 anos.
Entradas recientes (fecha: nota): $notasParaIA
Reacciones a goles encajados: ${reacciones.mkString(" | ")}
Proporciona un analisis BREVE en 3 partes:
PATRON: [patron emocional detectado en 1 frase]
FORTALEZA: [principal fortaleza mental en 1 frase]
CONSEJO: [1 consejo practico concreto para esta semana]
Responde en espanol, tono positivo y motivador para un nino."""
        AIProvider.ask(prompt, None, bypassCache = true)
      } else "Sin suficientes notas de conducta para el analisis. Registra tu estado diario para activar este modulo."

      Map(
        "entries"     -> entries,
        "correlacion" -> correlacion,
        "diasBajosConsecutivos" -> diasBajosConsecutivos,
        "avgAnimoReciente"  -> avgAnimoReciente,
        "avgEnergiaReciente"-> avgEnergiaReciente,
        "resilienciaScore"  -> resilienciaScore,
        "analisisIA"        -> analisisIA,
        "notasCount"        -> entries.count(_.notas.nonEmpty),
        "totalEntries"      -> entries.size
      )
    } finally { conn.close() }
  }


  // == DIGITAL TWIN HECTOR 2035 ==============================================

  // == MATCH GOALS: Contexto de goles encajados ============================
  def saveMatchGoal(matchId: Int, minuto: Int, origen: String, situacion: String,
                    responsabilidad: String, eraParable: String, zonaGol: String, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO match_goals (match_id, minuto, origen, situacion, responsabilidad, era_parable, zona_gol, notas) " +
        "VALUES (?,?,?,?,?,?,?,?)")
      ps.setInt(1, matchId); ps.setInt(2, minuto)
      ps.setString(3, fixEncoding(origen)); ps.setString(4, fixEncoding(situacion))
      ps.setString(5, responsabilidad); ps.setString(6, eraParable)
      ps.setString(7, zonaGol); ps.setString(8, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getLastMatchId(): Int = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT MAX(id) as id FROM matches WHERE status='PLAYED'")
      if (rs.next()) rs.getInt("id") else 0
    } finally { conn.close() }
  }

  def deleteMatchGoals(matchId: Int): Unit = {
    val conn = getConnection()
    try { conn.createStatement().executeUpdate(s"DELETE FROM match_goals WHERE match_id = $matchId") }
    finally { conn.close() }
  }

  def getGoalsAnalysis(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT mg.*, m.rival, m.fecha, m.goles_favor, m.goles_contra " +
        "FROM match_goals mg JOIN matches m ON mg.match_id = m.id ORDER BY m.fecha DESC, mg.minuto ASC")
      var rows = List[Map[String, String]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "id"              -> rs.getInt("id").toString,
          "matchId"         -> rs.getInt("match_id").toString,
          "rival"           -> Option(rs.getString("rival")).getOrElse(""),
          "fecha"           -> Option(rs.getString("fecha")).getOrElse(""),
          "minuto"          -> rs.getInt("minuto").toString,
          "origen"          -> Option(rs.getString("origen")).getOrElse(""),
          "situacion"       -> Option(rs.getString("situacion")).getOrElse(""),
          "responsabilidad" -> Option(rs.getString("responsabilidad")).getOrElse(""),
          "eraParable"      -> Option(rs.getString("era_parable")).getOrElse(""),
          "zonaGol"         -> Option(rs.getString("zona_gol")).getOrElse(""),
          "notas"           -> Option(rs.getString("notas")).getOrElse(""),
          "resultado"       -> (rs.getInt("goles_favor").toString + "-" + rs.getInt("goles_contra").toString)
        )
      }

      val total: Int = rows.size
      // Agrupaciones
      def countBy(campo: String): Map[String, Int] =
        rows.groupBy(_(campo)).map { case (k,v) => k -> v.size }

      val porOrigen: Map[String, Int]         = countBy("origen")
      val porSituacion: Map[String, Int]       = countBy("situacion")
      val porResponsabilidad: Map[String, Int] = countBy("responsabilidad")
      val porParable: Map[String, Int]         = countBy("eraParable")

      // Goles evitables = responsabilidad Alta o Media + era_parable Si
      val evitables: Int   = rows.count(r => r("responsabilidad") == "Alta" || (r("responsabilidad") == "Media" && r("eraParable") == "Si"))
      val inevitables: Int = rows.count(r => r("responsabilidad") == "Ninguna")
      val dudosos: Int     = total - evitables - inevitables

      // Nota ajustada: de cada partido, descuenta los goles inevitables
      val rsNota = conn.createStatement().executeQuery(
        "SELECT m.id, m.nota, m.goles_contra, " +
        "COUNT(CASE WHEN mg.responsabilidad='Ninguna' THEN 1 END) as goles_defensa " +
        "FROM matches m LEFT JOIN match_goals mg ON mg.match_id = m.id " +
        "WHERE m.status='PLAYED' AND m.nota > 0 " +
        "GROUP BY m.id, m.nota, m.goles_contra ORDER BY m.fecha DESC LIMIT 20")
      var notaAjustadaTotal = 0.0; var notaAjustadaCount = 0
      var notaRealTotal = 0.0
      while (rsNota.next()) {
        val nota = rsNota.getDouble("nota")
        val gc   = rsNota.getInt("goles_contra")
        val golesDefensa = rsNota.getInt("goles_defensa")
        // Cada gol de defensa suma 0.5 pts a la nota ajustada (aprox)
        val notaAdj = math.min(10.0, nota + golesDefensa * 0.5)
        notaAjustadaTotal += notaAdj
        notaRealTotal += nota
        notaAjustadaCount += 1
      }
      val notaAjustada: Double = if (notaAjustadaCount > 0) notaAjustadaTotal / notaAjustadaCount else 0.0
      val notaReal: Double     = if (notaAjustadaCount > 0) notaRealTotal / notaAjustadaCount else 0.0

      Map(
        "total"            -> total,
        "evitables"        -> evitables,
        "inevitables"      -> inevitables,
        "dudosos"          -> dudosos,
        "porOrigen"        -> porOrigen,
        "porSituacion"     -> porSituacion,
        "porResponsabilidad" -> porResponsabilidad,
        "porParable"       -> porParable,
        "notaAjustada"     -> notaAjustada,
        "notaReal"         -> notaReal,
        "rows"             -> rows
      )
    } finally { conn.close() }
  }

  // == FASE 8: PSxG DELTA (Post-Shot xG vs Goals Conceded) =====================
  def getPSxGDeltaData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Tabla de xG base por zona + situacion
      // Probabilidad de gol segun zona de porteria y tipo de disparo
      // Valores calibrados para futbol base (5-10 anos)
      def xGBase(zona: String, situacion: String): Double = {
        val xgZona: Double = zona match {
          case "MC" => 0.72  // centro medio — maxima probabilidad
          case "BC" => 0.65  // centro bajo
          case "TC" => 0.55  // centro alto
          case "ML" | "MR" => 0.42  // laterales medios
          case "BL" | "BR" => 0.38  // laterales bajos
          case "TL" | "TR" => 0.28  // esquinas
          case _           => 0.45  // sin zona — valor medio
        }
        val multSit: Double = situacion match {
          case s if s.contains("1v1")  => 1.35
          case s if s.contains("2v1")  => 1.20
          case s if s.contains("penalti") | s.contains("penalty") => 1.60
          case s if s.contains("libre") => 0.85
          case s if s.contains("cabeza") | s.contains("aereo") => 0.90
          case _ => 1.0
        }
        math.min(0.97, xgZona * multSit)
      }

      // Obtener todos los goles de match_goals con su zona y situacion
      val rsGoles = conn.createStatement().executeQuery(
        "SELECT mg.zona_gol, mg.situacion, mg.responsabilidad, mg.era_parable, " +
        "mg.minuto, m.fecha, m.rival, m.nota " +
        "FROM match_goals mg " +
        "JOIN matches m ON mg.match_id = m.id " +
        "WHERE m.status = 'PLAYED' " +
        "ORDER BY m.fecha DESC")

      case class GoalRow(zona: String, situacion: String, responsabilidad: String,
                         eraParable: String, minuto: Int, fecha: String,
                         rival: String, notaPartido: Double,
                         xg: Double)
      var goles = List[GoalRow]()
      while (rsGoles.next()) {
        val zona = Option(rsGoles.getString("zona_gol")).getOrElse("")
        val sit  = Option(rsGoles.getString("situacion")).getOrElse("").toLowerCase
        val xg   = xGBase(zona, sit)
        goles = goles :+ GoalRow(
          zona            = zona,
          situacion       = Option(rsGoles.getString("situacion")).getOrElse(""),
          responsabilidad = Option(rsGoles.getString("responsabilidad")).getOrElse(""),
          eraParable      = Option(rsGoles.getString("era_parable")).getOrElse(""),
          minuto          = rsGoles.getInt("minuto"),
          fecha           = Option(rsGoles.getString("fecha")).getOrElse("").take(10),
          rival           = Option(rsGoles.getString("rival")).getOrElse(""),
          notaPartido     = rsGoles.getDouble("nota"),
          xg              = xg
        )
      }

      val nGoles = goles.size
      // xG total acumulado (cuantos goles "debia" encajar estadisticamente)
      val xgTotal: Double = goles.map(_.xg).sum
      // Goles reales encajados
      val golesReales: Int = nGoles
      // PSxG Delta: goles reales - xG. Negativo = mejor que esperado
      val psxgDelta: Double = golesReales - xgTotal
      val psxgDeltaStr: String = (if (psxgDelta <= 0) "" else "+") + f"$psxgDelta%.2f"
      val psxgDeltaColor: String = if (psxgDelta <= -1.0) "success"
                                    else if (psxgDelta <= 0.5) "info"
                                    else if (psxgDelta <= 1.5) "warning"
                                    else "danger"
      val psxgLabel: String = if (psxgDelta <= -1.0) "BAJO LO ESPERADO"
                               else if (psxgDelta <= 0.5) "EN LO ESPERADO"
                               else if (psxgDelta <= 1.5) "ALGO POR ENCIMA"
                               else "POR ENCIMA"

      // Clasificacion de goles por dificultad del tiro
      val golesAltaDif   = goles.count(_.xg < 0.35)   // tiros muy dificiles
      val golesMediaDif  = goles.count(g => g.xg >= 0.35 && g.xg < 0.60)
      val golesBajaDif   = goles.count(_.xg >= 0.60)   // tiros faciles — los que mas duelen

      // Por zona: xG medio vs goles reales
      val zonas = List("TL","TC","TR","ML","MC","MR","BL","BC","BR")
      val porZona: List[Map[String, Any]] = zonas.map { z =>
        val golesZona = goles.filter(g => g.zona == z || (z == "" && g.zona.isEmpty))
        val nZ  = golesZona.size
        val xgZ = golesZona.map(_.xg).sum
        val deltaZ = nZ - xgZ
        Map(
          "zona"   -> z,
          "goles"  -> nZ,
          "xg"     -> xgZ,
          "delta"  -> deltaZ,
          "label"  -> (if (deltaZ <= -0.5) "Mejor" else if (deltaZ >= 0.5) "Peor" else "Normal")
        )
      }.filter(_("goles").asInstanceOf[Int] > 0)

      // Tabla individual de goles con xG
      val tablaGoles: List[Map[String, String]] = goles.take(20).map { g =>
        val deltaInd = 1.0 - g.xg  // 1 - xG = cuanto costó parar (inverso: para goles encajados, xG alto = facil)
        Map(
          "fecha"   -> g.fecha,
          "rival"   -> g.rival,
          "zona"    -> (if (g.zona.isEmpty) "—" else g.zona),
          "sit"     -> (if (g.situacion.isEmpty) "—" else g.situacion),
          "xg"      -> f"${g.xg}%.2f",
          "dific"   -> (if (g.xg < 0.35) "DIFÍCIL" else if (g.xg < 0.60) "MEDIA" else "FÁCIL"),
          "resp"    -> g.responsabilidad,
          "color"   -> (if (g.xg < 0.35) "success" else if (g.xg < 0.60) "warning" else "danger")
        )
      }

      // xG medio por partido
      val nPartidos = goles.map(_.fecha).distinct.size
      val xgPorPartido = if (nPartidos > 0) xgTotal / nPartidos else 0.0

      Map(
        "nGoles"         -> nGoles,
        "xgTotal"        -> xgTotal,
        "psxgDelta"      -> psxgDelta,
        "psxgDeltaStr"   -> psxgDeltaStr,
        "psxgDeltaColor" -> psxgDeltaColor,
        "psxgLabel"      -> psxgLabel,
        "golesAltaDif"   -> golesAltaDif,
        "golesMediaDif"  -> golesMediaDif,
        "golesBajaDif"   -> golesBajaDif,
        "xgPorPartido"   -> xgPorPartido,
        "nPartidos"      -> nPartidos,
        "porZona"        -> porZona,
        "tablaGoles"     -> tablaGoles
      )
    } finally { conn.close() }
  }

  // == FASE 8: COGNITIVE RESET RATE ============================================
  def getCognitiveResetData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Partidos con al menos un gol evitable (responsabilidad Alta o Media+parable)
      //    y la nota del partido SIGUIENTE
      val rs = conn.createStatement().executeQuery("""
        WITH partidos_con_error AS (
          SELECT DISTINCT ON (m.id)
            m.id, m.fecha, m.nota, m.rival,
            COUNT(mg.id) OVER (PARTITION BY m.id) as n_evitables
          FROM matches m
          JOIN match_goals mg ON mg.match_id = m.id
          WHERE m.status = 'PLAYED' AND m.nota > 0
            AND (mg.responsabilidad = 'Alta'
              OR (mg.responsabilidad = 'Media' AND mg.era_parable = 'Si'))
        ),
        partidos_ord AS (
          SELECT id, fecha, nota, rival,
            ROW_NUMBER() OVER (ORDER BY fecha ASC) AS rn
          FROM matches
          WHERE status = 'PLAYED' AND nota > 0
        )
        SELECT
          pe.id, pe.fecha, pe.nota AS nota_error, pe.rival,
          pe.n_evitables,
          nx.nota  AS nota_siguiente,
          nx.rival AS rival_siguiente,
          nx.fecha AS fecha_siguiente
        FROM partidos_con_error pe
        JOIN partidos_ord po ON po.id = pe.id
        JOIN partidos_ord nx ON nx.rn = po.rn + 1
        ORDER BY pe.fecha DESC
        LIMIT 20
      """)

      case class ResetRow(fecha: String, rival: String, notaError: Double,
                          nEvitables: Int, notaSig: Double, rivalSig: String,
                          fechaSig: String)
      var rows = List[ResetRow]()
      while (rs.next()) {
        rows = rows :+ ResetRow(
          fecha        = Option(rs.getString("fecha")).getOrElse(""),
          rival        = Option(rs.getString("rival")).getOrElse(""),
          notaError    = rs.getDouble("nota_error"),
          nEvitables   = rs.getInt("n_evitables"),
          notaSig      = rs.getDouble("nota_siguiente"),
          rivalSig     = Option(rs.getString("rival_siguiente")).getOrElse(""),
          fechaSig     = Option(rs.getString("fecha_siguiente")).getOrElse("")
        )
      }

      // 2. Metricas agregadas
      val n = rows.size
      val rebounds   = rows.count(r => r.notaSig >= r.notaError - 0.2)  // recuperó o mejoró
      val positivos  = rows.count(r => r.notaSig > r.notaError + 0.4)   // rebote claro
      val negativos  = rows.count(r => r.notaSig < r.notaError - 0.5)   // impacto negativo

      val resetScore: Int = if (n > 0) math.min(100, (rebounds.toDouble / n * 100).toInt) else 0
      val avgNotaError: Double = if (n > 0) rows.map(_.notaError).sum / n else 0.0
      val avgNotaSig: Double   = if (n > 0) rows.map(_.notaSig).sum / n else 0.0
      val avgDelta: Double     = avgNotaSig - avgNotaError

      // 3. Mediana de recuperacion (para ver si sube normalmente tras un error)
      val mediaGeneral: Double = {
        val rsM = conn.createStatement().executeQuery(
          "SELECT AVG(nota) FROM matches WHERE status='PLAYED' AND nota > 0")
        if (rsM.next()) rsM.getDouble(1) else 0.0
      }

      // 4. Clasificacion
      val clasificacion: String = if (resetScore >= 70) "RESILIENTE"
                                  else if (resetScore >= 45) "EN PROCESO"
                                  else "VULNERABLE"
      val clasificacionColor: String = if (resetScore >= 70) "success"
                                       else if (resetScore >= 45) "warning"
                                       else "danger"

      // 5. Series para grafico
      val fechasSerie: List[String] = rows.reverse.map(_.fechaSig.take(10))
      val notaErrorSerie: List[Double] = rows.reverse.map(_.notaError)
      val notaSigSerie: List[Double]   = rows.reverse.map(_.notaSig)

      // 6. Datos individuales para tabla
      val tablaRows: List[Map[String, String]] = rows.map { r =>
        val diff = r.notaSig - r.notaError
        val deltaStr = (if (diff >= 0) "+" else "") + f"$diff%.1f"
        val resultado = if (diff > 0.4) "REBOTE" else if (diff < -0.5) "IMPACTO" else "ESTABLE"
        Map(
          "fecha"      -> r.fecha.take(10),
          "rival"      -> r.rival,
          "notaError"  -> f"${r.notaError}%.1f",
          "nEvitables" -> r.nEvitables.toString,
          "fechaSig"   -> r.fechaSig.take(10),
          "rivalSig"   -> r.rivalSig,
          "notaSig"    -> f"${r.notaSig}%.1f",
          "delta"      -> deltaStr,
          "resultado"  -> resultado
        )
      }

      Map(
        "n"                -> n,
        "resetScore"       -> resetScore,
        "clasificacion"    -> clasificacion,
        "clasificacionColor" -> clasificacionColor,
        "rebounds"         -> rebounds,
        "positivos"        -> positivos,
        "negativos"        -> negativos,
        "avgNotaError"     -> avgNotaError,
        "avgNotaSig"       -> avgNotaSig,
        "avgDelta"         -> avgDelta,
        "mediaGeneral"     -> mediaGeneral,
        "fechasSerie"      -> fechasSerie,
        "notaErrorSerie"   -> notaErrorSerie,
        "notaSigSerie"     -> notaSigSerie,
        "tablaRows"        -> tablaRows
      )
    } finally { conn.close() }
  }


  // == FASE 8: DEVELOPMENT PATHWAY MATCHER =====================================
  def getPathwayData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Cruza el arquetipo de rivales (Striker Clustering) con métricas de Héctor
      // Responde: ¿contra qué estilo de equipo crece más como portero?

      val rs = conn.createStatement().executeQuery("""
        SELECT
          m.rival,
          m.nota,
          m.paradas,
          m.goles_contra,
          m.acciones_pie,
          m.lineas_superadas,
          m.scanning_rate,
          COUNT(mg.id)                                                       AS n_goles,
          COUNT(CASE WHEN mg.situacion ILIKE '%1v1%' THEN 1 END)            AS g_1v1,
          COUNT(CASE WHEN mg.situacion ILIKE '%aereo%'
                       OR mg.situacion ILIKE '%cabeza%' THEN 1 END)         AS g_aereo,
          COUNT(CASE WHEN mg.situacion ILIKE '%2v1%' THEN 1 END)            AS g_2v1
        FROM matches m
        LEFT JOIN match_goals mg ON mg.match_id = m.id
        WHERE m.status = 'PLAYED' AND m.nota > 0
        GROUP BY m.id, m.rival, m.nota, m.paradas, m.goles_contra,
                 m.acciones_pie, m.lineas_superadas, m.scanning_rate
        ORDER BY m.fecha DESC
      """)

      case class PRow(rival: String, nota: Double, paradas: Int, gc: Int,
                      pie: Int, lineas: Int, scan: Int,
                      nGoles: Int, g1v1: Int, gAereo: Int, g2v1: Int)
      var rows = List[PRow]()
      while (rs.next()) {
        rows = rows :+ PRow(
          rival   = Option(rs.getString("rival")).getOrElse(""),
          nota    = rs.getDouble("nota"),
          paradas = rs.getInt("paradas"),
          gc      = rs.getInt("goles_contra"),
          pie     = rs.getInt("acciones_pie"),
          lineas  = rs.getInt("lineas_superadas"),
          scan    = rs.getInt("scanning_rate"),
          nGoles  = rs.getInt("n_goles"),
          g1v1    = rs.getInt("g_1v1"),
          gAereo  = rs.getInt("g_aereo"),
          g2v1    = rs.getInt("g_2v1")
        )
      }

      // Clasificar cada partido por arquetipo de rival (misma lógica que Striker Clustering)
      def arquetipoRival(r: PRow): String = {
        val nG = r.nGoles
        if (r.g1v1 >= 2 || (nG > 0 && r.g1v1.toDouble/nG >= 0.35)) "RAPIDO"
        else if (r.gAereo >= 2 || (nG > 0 && r.gAereo.toDouble/nG >= 0.30)) "AEREO"
        else if (r.g2v1 >= 2 || (nG > 0 && r.g2v1.toDouble/nG >= 0.30)) "COLECTIVO"
        else if (r.gc >= 3) "DIRECTO"
        else "EQUILIBRADO"
      }

      // Agrupar por arquetipo y calcular métricas de crecimiento
      case class PathStats(nota: Double, paradas: Double, pie: Double,
                           lineas: Double, n: Int)

      def statsFor(arq: String): Option[PathStats] = {
        val sub = rows.filter(r => arquetipoRival(r) == arq)
        if (sub.isEmpty) None
        else Some(PathStats(
          nota    = sub.map(_.nota).sum / sub.size,
          paradas = sub.map(_.paradas.toDouble).sum / sub.size,
          pie     = sub.map(_.pie.toDouble).sum / sub.size,
          lineas  = sub.map(_.lineas.toDouble).sum / sub.size,
          n       = sub.size
        ))
      }

      val arqs = List("RAPIDO","AEREO","COLECTIVO","DIRECTO","EQUILIBRADO")
      val perArq: List[Map[String, Any]] = arqs.flatMap { arq =>
        statsFor(arq).map { s =>
          Map(
            "arquetipo" -> arq,
            "n"         -> s.n,
            "nota"      -> s.nota,
            "paradas"   -> s.paradas,
            "pie"       -> s.pie,
            "lineas"    -> s.lineas,
            "color"     -> (arq match {
              case "RAPIDO"     => "danger"
              case "AEREO"      => "info"
              case "COLECTIVO"  => "warning"
              case "DIRECTO"    => "primary"
              case _            => "secondary"
            })
          )
        }
      }

      // Mejor arquetipo para crecer (nota más alta = entorno más favorable)
      val mejorArq = if (perArq.nonEmpty)
        perArq.maxBy(_("nota").asInstanceOf[Double])
      else Map("arquetipo" -> "—", "nota" -> 0.0, "color" -> "secondary")

      // Arquetipo más desafiante (nota más baja = área de mejora)
      val peorArq = if (perArq.nonEmpty)
        perArq.minBy(_("nota").asInstanceOf[Double])
      else Map("arquetipo" -> "—", "nota" -> 0.0, "color" -> "secondary")

      // Recomendacion de entorno de desarrollo
      val recomendacion: String = mejorArq("arquetipo").asInstanceOf[String] match {
        case "RAPIDO"     => "Héctor rinde mejor contra equipos rápidos. Busca rivales con pressing alto y 1v1 frecuentes para consolidar esta fortaleza."
        case "AEREO"      => "Su mejor rendimiento es contra equipos aéreos. Los entrenamientos de salida en córner y dominio del área deben ser prioritarios."
        case "COLECTIVO"  => "Rinde bien en entornos de juego colectivo. Equipos con buen juego combinativo le sacan el máximo partido."
        case "DIRECTO"    => "Mejora contra el juego directo. Equipos que juegan largo le entrenan la salida y el despeje."
        case _            => "Perfil equilibrado. Cualquier estilo de rival le aporta crecimiento similar."
      }

      val areasMejora: String = peorArq("arquetipo").asInstanceOf[String] match {
        case "RAPIDO"     => "Trabajar salida en 1v1 y achique de ángulo con presión temporal."
        case "AEREO"      => "Reforzar dominio del área aérea — posición, grito y timing de salida."
        case "COLECTIVO"  => "Mejorar lectura de jugadas 2v1 y anticipación del pase de gol."
        case "DIRECTO"    => "Consolidar la salida a balones largos y la comunicación con la defensa."
        case _            => "Mantener la consistencia independientemente del estilo rival."
      }

      Map(
        "perArq"        -> perArq,
        "mejorArq"      -> mejorArq,
        "peorArq"       -> peorArq,
        "recomendacion" -> recomendacion,
        "areasMejora"   -> areasMejora,
        "totalPartidos" -> rows.size
      )
    } finally { conn.close() }
  }

  // == FASE 5: BIO-BANDING =====================================================
  def getBioBandingData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Edad y PHV del Digital Twin
      val rsP = conn.createStatement().executeQuery(
        "SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsP.next())
        Option(rsP.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19")
      else "2015-06-19"
      val hoy       = java.time.LocalDate.now()
      val nac       = java.time.LocalDate.parse(fechaNac)
      val edadAnios = java.time.Period.between(nac, hoy).getYears
      val edadMeses = java.time.Period.between(nac, hoy).getYears * 12 +
                      java.time.Period.between(nac, hoy).getMonths

      // 2. Altura actual y velocidad de crecimiento
      val rsG = conn.createStatement().executeQuery(
        "SELECT altura, peso, velocidad_crecimiento FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val (alturaActual, pesoActual, velCrecimiento) =
        if (rsG.next()) (rsG.getDouble("altura"), rsG.getDouble("peso"),
                         rsG.getDouble("velocidad_crecimiento"))
        else (0.0, 0.0, 0.0)

      // 3. Velocidad máxima (PHV detector)
      val rsVel = conn.createStatement().executeQuery(
        "SELECT MAX(velocidad_crecimiento) as max_vel FROM physical_growth")
      val phvVelocidad = if (rsVel.next()) rsVel.getDouble("max_vel") else 0.0
      val phvActivo    = phvVelocidad >= 6.0  // >6 cm/año = pico activo

      // 4. Fase biológica estimada
      val faseBio: String = if (edadAnios < 8) "INFANCIA TARDÍA"
                             else if (edadAnios < 10) "PRE-PUBERTAD"
                             else if (edadAnios < 12) "INICIO PUBERTAD"
                             else if (phvActivo) "PHV — PICO ACTIVO"
                             else if (edadAnios < 15) "PUBERTAD MEDIA"
                             else if (edadAnios < 17) "POST-PHV"
                             else "MADUREZ"
      val faseBioColor: String = faseBio match {
        case "PHV — PICO ACTIVO" => "danger"
        case "INICIO PUBERTAD"   => "warning"
        case "PUBERTAD MEDIA"    => "warning"
        case "POST-PHV"          => "info"
        case _                   => "secondary"
      }

      // 5. Factor de ajuste de nota según fase biológica
      // Durante el PHV el cuerpo consume energía en crecer — rendimiento esperado baja
      // Un 6.5 durante PHV activo equivale a un 7.5 en condiciones normales
      val factorAjuste: Double = faseBio match {
        case "PHV — PICO ACTIVO" => 1.15   // +15%: notas más valiosas durante el pico
        case "INICIO PUBERTAD"   => 1.08   // +8%
        case "PUBERTAD MEDIA"    => 1.05   // +5%
        case "PRE-PUBERTAD"      => 1.02   // +2%: ajuste mínimo
        case _                   => 1.0    // sin ajuste
      }

      // 6. Nota real vs nota bio-ajustada (últimos 20 partidos)
      val rsM = conn.createStatement().executeQuery(
        "SELECT fecha, rival, nota, goles_contra, paradas " +
        "FROM matches WHERE status='PLAYED' AND nota > 0 " +
        "ORDER BY fecha DESC LIMIT 20")
      var matchRows = List[Map[String, Any]]()
      while (rsM.next()) {
        val nota      = rsM.getDouble("nota")
        val notaAdj   = math.min(10.0, nota * factorAjuste)
        matchRows = matchRows :+ Map(
          "fecha"    -> rsM.getString("fecha").take(10),
          "rival"    -> Option(rsM.getString("rival")).getOrElse(""),
          "nota"     -> nota,
          "notaAdj"  -> notaAdj,
          "gc"       -> rsM.getInt("goles_contra"),
          "paradas"  -> rsM.getInt("paradas")
        )
      }

      val n = matchRows.size
      val avgNota    = if (n > 0) matchRows.map(_("nota").asInstanceOf[Double]).sum / n else 0.0
      val avgNotaAdj = if (n > 0) matchRows.map(_("notaAdj").asInstanceOf[Double]).sum / n else 0.0
      val deltaMedia = avgNotaAdj - avgNota

      // 7. Percentil de altura para la edad (estimación simplificada OMS)
      val percentilAltura: String = if (alturaActual <= 0) "Sin datos" else {
        // Medianas OMS para niños (cm) por edad
        val medianas = Map(5->109.0, 6->116.0, 7->122.0, 8->128.0, 9->133.0,
                           10->138.0, 11->143.0, 12->149.0, 13->156.0, 14->163.0,
                           15->169.0, 16->173.0, 17->175.0, 18->176.0)
        val mediana = medianas.getOrElse(edadAnios, 155.0)
        val diff = alturaActual - mediana
        if (diff > 6) "P97 — Muy alto para su edad"
        else if (diff > 3) "P75-P90 — Alto para su edad"
        else if (diff > -3) "P50 — Talla media"
        else if (diff > -6) "P25 — Algo por debajo"
        else "P10 — Por debajo de la media"
      }

      // 8. Series para gráfico
      val fechasSerie  = matchRows.reverse.map(_("fecha").asInstanceOf[String].take(5))
      val notaSerie    = matchRows.reverse.map(_("nota").asInstanceOf[Double])
      val notaAdjSerie = matchRows.reverse.map(_("notaAdj").asInstanceOf[Double])

      Map(
        "edadAnios"       -> edadAnios,
        "edadMeses"       -> edadMeses,
        "alturaActual"    -> alturaActual,
        "pesoActual"      -> pesoActual,
        "velCrecimiento"  -> velCrecimiento,
        "phvActivo"       -> phvActivo,
        "phvVelocidad"    -> phvVelocidad,
        "faseBio"         -> faseBio,
        "faseBioColor"    -> faseBioColor,
        "factorAjuste"    -> factorAjuste,
        "avgNota"         -> avgNota,
        "avgNotaAdj"      -> avgNotaAdj,
        "deltaMedia"      -> deltaMedia,
        "percentilAltura" -> percentilAltura,
        "matchRows"       -> matchRows,
        "fechasSerie"     -> fechasSerie,
        "notaSerie"       -> notaSerie,
        "notaAdjSerie"    -> notaAdjSerie,
        "n"               -> n
      )
    } finally { conn.close() }
  }

  // == FASE 7: STRIKER CLUSTERING ==============================================
  def getStrikerClusters(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      // Agrupamos rivales por perfil de ataque usando datos ya disponibles
      // Arquetipo: RAPIDO (muchos goles en contraataque/1v1), FISICO (muchos goles aereos/2v1),
      //            TECNICO (pocos goles pero alta nota rival), DIRECTO (muchos goles de tiro lejano)
      val rs = conn.createStatement().executeQuery("""
        SELECT
          m.rival,
          COUNT(DISTINCT m.id)                                              AS pj,
          SUM(m.goles_contra)                                               AS gc_total,
          AVG(m.goles_contra)                                               AS gc_media,
          AVG(m.nota)                                                       AS nota_media_hector,
          COUNT(mg.id)                                                      AS goles_analizados,
          COUNT(CASE WHEN mg.situacion ILIKE '%1v1%' THEN 1 END)           AS g_1v1,
          COUNT(CASE WHEN mg.situacion ILIKE '%2v1%' THEN 1 END)           AS g_2v1,
          COUNT(CASE WHEN mg.situacion ILIKE '%aereo%'
                       OR mg.situacion ILIKE '%cabeza%' THEN 1 END)        AS g_aereo,
          COUNT(CASE WHEN mg.origen ILIKE '%contra%'
                       OR mg.minuto <= 15
                       OR mg.minuto >= 75 THEN 1 END)                      AS g_presion,
          MAX(m.goles_contra)                                               AS gc_max
        FROM matches m
        LEFT JOIN match_goals mg ON mg.match_id = m.id
        WHERE m.status = 'PLAYED'
        GROUP BY m.rival
        HAVING COUNT(DISTINCT m.id) >= 1
        ORDER BY gc_total DESC
        LIMIT 30
      """)

      var clusters = List[Map[String, Any]]()
      while (rs.next()) {
        val rival    = Option(rs.getString("rival")).getOrElse("")
        val pj       = rs.getInt("pj")
        val gcTotal  = rs.getInt("gc_total")
        val gcMedia  = rs.getDouble("gc_media")
        val notaHec  = rs.getDouble("nota_media_hector")
        val gAnz     = rs.getInt("goles_analizados")
        val g1v1     = rs.getInt("g_1v1")
        val g2v1     = rs.getInt("g_2v1")
        val gAereo   = rs.getInt("g_aereo")
        val gPresion = rs.getInt("g_presion")
        val gcMax    = rs.getInt("gc_max")

        // Clasificacion por arquetipo
        val arquetipo: String = if (g1v1 >= 2 || (gAnz > 0 && g1v1.toDouble / gAnz >= 0.35))
          "RAPIDO"
        else if (gAereo >= 2 || (gAnz > 0 && gAereo.toDouble / gAnz >= 0.30))
          "AEREO"
        else if (g2v1 >= 2 || (gAnz > 0 && g2v1.toDouble / gAnz >= 0.30))
          "COLECTIVO"
        else if (gcMedia >= 2.5)
          "DIRECTO"
        else
          "EQUILIBRADO"

        val arquetipoColor: String = arquetipo match {
          case "RAPIDO"     => "danger"
          case "AEREO"      => "info"
          case "COLECTIVO"  => "warning"
          case "DIRECTO"    => "primary"
          case _            => "secondary"
        }

        val amenaza: String = if (gcMedia >= 3.0) "ALTA"
                               else if (gcMedia >= 1.5) "MEDIA"
                               else "BAJA"
        val amenazaColor: String = if (gcMedia >= 3.0) "danger"
                                    else if (gcMedia >= 1.5) "warning"
                                    else "success"

        clusters = clusters :+ Map(
          "rival"          -> rival,
          "pj"             -> pj,
          "gcTotal"        -> gcTotal,
          "gcMedia"        -> gcMedia,
          "notaHec"        -> notaHec,
          "g1v1"           -> g1v1,
          "g2v1"           -> g2v1,
          "gAereo"         -> gAereo,
          "gPresion"       -> gPresion,
          "gcMax"          -> gcMax,
          "arquetipo"      -> arquetipo,
          "arquetipoColor" -> arquetipoColor,
          "amenaza"        -> amenaza,
          "amenazaColor"   -> amenazaColor
        )
      }
      clusters
    } finally { conn.close() }
  }

  // == FASE 7: RED-ZONE ANALYTICS ==============================================
  def getRedZoneData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Media global de referencia
      val rsGlobal = conn.createStatement().executeQuery(
        "SELECT AVG(nota) as avg_nota, AVG(paradas) as avg_paradas, " +
        "AVG(goles_contra) as avg_gc, COUNT(*) as total " +
        "FROM matches WHERE status='PLAYED' AND nota > 0")
      val (avgNotaGlobal, avgParadasGlobal, avgGcGlobal, totalPartidos) =
        if (rsGlobal.next()) (rsGlobal.getDouble("avg_nota"), rsGlobal.getDouble("avg_paradas"),
                              rsGlobal.getDouble("avg_gc"),   rsGlobal.getInt("total"))
        else (0.0, 0.0, 0.0, 0)

      // 2. Partidos de alta presion: goles_contra >= 2 (asedio ofensivo)
      val rsAsedio = conn.createStatement().executeQuery(
        "SELECT id, fecha, rival, nota, paradas, goles_contra, goles_favor, minutos " +
        "FROM matches WHERE status='PLAYED' AND nota > 0 AND goles_contra >= 2 " +
        "ORDER BY fecha DESC LIMIT 30")
      var asedioRows = List[Map[String, Any]]()
      while (rsAsedio.next()) {
        asedioRows = asedioRows :+ Map(
          "fecha"  -> rsAsedio.getString("fecha").take(10),
          "rival"  -> Option(rsAsedio.getString("rival")).getOrElse(""),
          "nota"   -> rsAsedio.getDouble("nota"),
          "paradas"-> rsAsedio.getInt("paradas"),
          "gc"     -> rsAsedio.getInt("goles_contra"),
          "gf"     -> rsAsedio.getInt("goles_favor"),
          "minutos"-> rsAsedio.getInt("minutos")
        )
      }
      val nAsedio = asedioRows.size
      val avgNotaAsedio   = if (nAsedio > 0) asedioRows.map(_(("nota")).asInstanceOf[Double]).sum / nAsedio else 0.0
      val avgParadasAsedio= if (nAsedio > 0) asedioRows.map(_(("paradas")).asInstanceOf[Int].toDouble).sum / nAsedio else 0.0

      // 3. Partidos disputados con minutos >= 70 (final del partido - zona de fatiga)
      val rsFatiga = conn.createStatement().executeQuery(
        "SELECT id, fecha, rival, nota, paradas, goles_contra, goles_favor, minutos " +
        "FROM matches WHERE status='PLAYED' AND nota > 0 AND minutos >= 70 " +
        "ORDER BY fecha DESC LIMIT 30")
      var fatigaRows = List[Map[String, Any]]()
      while (rsFatiga.next()) {
        fatigaRows = fatigaRows :+ Map(
          "fecha"  -> rsFatiga.getString("fecha").take(10),
          "rival"  -> Option(rsFatiga.getString("rival")).getOrElse(""),
          "nota"   -> rsFatiga.getDouble("nota"),
          "paradas"-> rsFatiga.getInt("paradas"),
          "gc"     -> rsFatiga.getInt("goles_contra"),
          "gf"     -> rsFatiga.getInt("goles_favor"),
          "minutos"-> rsFatiga.getInt("minutos")
        )
      }
      val nFatiga = fatigaRows.size
      val avgNotaFatiga    = if (nFatiga > 0) fatigaRows.map(_(("nota")).asInstanceOf[Double]).sum / nFatiga else 0.0
      val avgParadasFatiga = if (nFatiga > 0) fatigaRows.map(_(("paradas")).asInstanceOf[Int].toDouble).sum / nFatiga else 0.0

      // 4. Partidos de derrota abultada (gc >= 3) — colapso total
      val rsColapso = conn.createStatement().executeQuery(
        "SELECT id, fecha, rival, nota, paradas, goles_contra, goles_favor " +
        "FROM matches WHERE status='PLAYED' AND nota > 0 AND goles_contra >= 3 " +
        "ORDER BY fecha DESC LIMIT 20")
      var colapsoRows = List[Map[String, Any]]()
      while (rsColapso.next()) {
        colapsoRows = colapsoRows :+ Map(
          "fecha"  -> rsColapso.getString("fecha").take(10),
          "rival"  -> Option(rsColapso.getString("rival")).getOrElse(""),
          "nota"   -> rsColapso.getDouble("nota"),
          "paradas"-> rsColapso.getInt("paradas"),
          "gc"     -> rsColapso.getInt("goles_contra"),
          "gf"     -> rsColapso.getInt("goles_favor")
        )
      }
      val nColapso = colapsoRows.size
      val avgNotaColapso = if (nColapso > 0) colapsoRows.map(_(("nota")).asInstanceOf[Double]).sum / nColapso else 0.0

      // 5. Resilience Index: nota en asedio vs nota global (0-100)
      val resilienceIndex: Int = if (avgNotaGlobal > 0)
        math.min(100, math.max(0, ((avgNotaAsedio / avgNotaGlobal) * 100).toInt))
        else 0
      val resilienceLabel: String = if (resilienceIndex >= 90) "ÉLITE"
                                    else if (resilienceIndex >= 75) "SOLIDO"
                                    else if (resilienceIndex >= 55) "EN PROCESO"
                                    else "VULNERABLE"
      val resilienceColor: String = if (resilienceIndex >= 90) "warning"
                                    else if (resilienceIndex >= 75) "success"
                                    else if (resilienceIndex >= 55) "info"
                                    else "danger"

      // 6. Fatigue Index: nota en partidos largos vs global
      val fatigueIndex: Int = if (avgNotaGlobal > 0)
        math.min(100, math.max(0, ((avgNotaFatiga / avgNotaGlobal) * 100).toInt))
        else 0
      val fatigueLabel: String = if (fatigueIndex >= 90) "SIN CAIDA"
                                  else if (fatigueIndex >= 75) "AGUANTA"
                                  else if (fatigueIndex >= 55) "LEVE CAIDA"
                                  else "FATIGA CLARA"
      val fatigueColor: String = if (fatigueIndex >= 90) "success"
                                  else if (fatigueIndex >= 75) "info"
                                  else if (fatigueIndex >= 55) "warning"
                                  else "danger"

      // 7. Serie temporal: nota en asedio (ultimos 15)
      val asedioSerie   = asedioRows.reverse.takeRight(15).map(_("nota").asInstanceOf[Double])
      val asedioLabels  = asedioRows.reverse.takeRight(15).map(_("fecha").asInstanceOf[String].take(5))
      val globalLine    = asedioSerie.map(_ => avgNotaGlobal)

      Map(
        "totalPartidos"     -> totalPartidos,
        "avgNotaGlobal"     -> avgNotaGlobal,
        "avgParadasGlobal"  -> avgParadasGlobal,
        "avgGcGlobal"       -> avgGcGlobal,
        // Asedio
        "nAsedio"           -> nAsedio,
        "avgNotaAsedio"     -> avgNotaAsedio,
        "avgParadasAsedio"  -> avgParadasAsedio,
        "resilienceIndex"   -> resilienceIndex,
        "resilienceLabel"   -> resilienceLabel,
        "resilienceColor"   -> resilienceColor,
        // Fatiga
        "nFatiga"           -> nFatiga,
        "avgNotaFatiga"     -> avgNotaFatiga,
        "avgParadasFatiga"  -> avgParadasFatiga,
        "fatigueIndex"      -> fatigueIndex,
        "fatigueLabel"      -> fatigueLabel,
        "fatigueColor"      -> fatigueColor,
        // Colapso
        "nColapso"          -> nColapso,
        "avgNotaColapso"    -> avgNotaColapso,
        // Series
        "asedioSerie"       -> asedioSerie,
        "asedioLabels"      -> asedioLabels,
        "globalLine"        -> globalLine,
        // Tablas detalle
        "asedioRows"        -> asedioRows,
        "fatigaRows"        -> fatigaRows
      )
    } finally { conn.close() }
  }

  // == FASE 6.5: MONEYBALL & DEEP INFLUENCE ANALYTICS =========================
  def getMoneyballData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ── 1. xT_GK: Expected Threat de Distribucion ──────────────────────────
      // Mide el peligro generado con el pie. Formula:
      // xT = (exito_pases_cortos * 0.4 + exito_pases_largos * 0.6) * volumen_relativo
      val rsXT = conn.createStatement().executeQuery(
        "SELECT pc_t, pc_ok, pl_t, pl_ok, acciones_pie, nota " +
        "FROM matches WHERE status='PLAYED' AND (pc_t+pl_t) > 0 ORDER BY fecha DESC LIMIT 30")
      var xtRows = List[(Int,Int,Int,Int,Int,Double)]()
      while (rsXT.next()) xtRows = xtRows :+ (
        rsXT.getInt("pc_t"), rsXT.getInt("pc_ok"),
        rsXT.getInt("pl_t"), rsXT.getInt("pl_ok"),
        rsXT.getInt("acciones_pie"), rsXT.getDouble("nota"))

      val xtPerMatch: List[Double] = xtRows.map { case (pct,pco,plt,plo,pie,_) =>
        val eficCorto: Double = if (pct > 0) pco.toDouble / pct else 0.0
        val eficLargo: Double = if (plt > 0) plo.toDouble / plt else 0.0
        val eficPond: Double  = eficCorto * 0.4 + eficLargo * 0.6
        val volumen: Double   = math.min(1.0, (pct + plt).toDouble / 20.0)
        eficPond * volumen * pie.toDouble * 10.0
      }
      val xtScore: Double = if (xtPerMatch.nonEmpty) xtPerMatch.sum / xtPerMatch.size else 0.0

      // Correlacion xT -> nota
      val xtNotas: List[(Double,Double)] = xtRows.zip(xtPerMatch).map { case ((a,b,c,d,e,nota),xt) => (xt,nota) }
      val xtCorr: Double = calcCorrelation(xtNotas)

      // Serie temporal para grafico (ultimos 20)
      val xtSerie: List[Double] = xtPerMatch.takeRight(20)

      // ── 2. xPoints: Expected Points Saved (Clutch Factor) ──────────────────
      // Valor de las paradas ponderado por: importancia del partido x tension del marcador
      // Importancia: TORNEO=2.0, LIGA=1.0, AMISTOSO=0.5
      // Tension: paradas en partidos ajustados (diferencia goles <= 1) valen mas
      val rsXP = conn.createStatement().executeQuery(
        "SELECT paradas, paradas_1v1, nota, goles_favor, goles_contra, tipo_partido, torneo_nombre " +
        "FROM matches WHERE status='PLAYED' AND paradas > 0 ORDER BY fecha DESC LIMIT 30")
      var xpRows = List[(Int,Int,Double,Int,Int,String,String)]()
      while (rsXP.next()) xpRows = xpRows :+ (
        rsXP.getInt("paradas"), rsXP.getInt("paradas_1v1"),
        rsXP.getDouble("nota"), rsXP.getInt("goles_favor"),
        rsXP.getInt("goles_contra"), Option(rsXP.getString("tipo_partido")).getOrElse("LIGA"),
        Option(rsXP.getString("torneo_nombre")).getOrElse(""))

      val xpPerMatch: List[Double] = xpRows.map { case (par, par1v1, nota, gf, gc, tipo, torneo) =>
        val importancia: Double = if (torneo.nonEmpty && tipo == "TORNEO") 2.0
                                  else if (tipo == "LIGA") 1.0 else 0.5
        val diferencia: Int    = math.abs(gf - gc)
        val tension: Double    = if (diferencia == 0) 1.5 else if (diferencia == 1) 1.2 else 1.0
        val valorParadas: Double = par * 1.0 + par1v1 * 0.8  // 1v1 son de alto valor
        valorParadas * importancia * tension * (nota / 100.0)
      }
      val xpTotal: Double   = if (xpPerMatch.nonEmpty) xpPerMatch.sum else 0.0
      val xpMedia: Double   = if (xpPerMatch.nonEmpty) xpTotal / xpPerMatch.size else 0.0
      val xpSerie: List[Double] = xpPerMatch.takeRight(20)

      // Clutch rating 0-100
      val maxXP: Double = if (xpPerMatch.nonEmpty) xpPerMatch.max else 1.0
      val clutchRating: Int = math.min(100, (xpMedia / math.max(maxXP, 1.0) * 100).toInt)

      // ── 3. SPV: Sweeper Keeper / Shot Prevention Value ──────────────────────
      // Cuantifica el valor del portero mas alla de la porteria
      // Paradas 1v1 (alto riesgo) + aereas (dominio espacio) + normal
      val rsSPV = conn.createStatement().executeQuery(
        "SELECT paradas, paradas_1v1, paradas_aereas, nota, fecha " +
        "FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 30")
      var spvRows = List[(Int,Int,Int,Double)]()
      while (rsSPV.next()) spvRows = spvRows :+ (
        rsSPV.getInt("paradas"), rsSPV.getInt("paradas_1v1"),
        rsSPV.getInt("paradas_aereas"), rsSPV.getDouble("nota"))

      val spvPerMatch: List[Double] = spvRows.map { case (par, p1v1, paer, nota) =>
        p1v1 * 1.5 + paer * 1.2 + math.max(0, par - p1v1 - paer) * 1.0
      }
      val spvMedia: Double   = if (spvPerMatch.nonEmpty) spvPerMatch.sum / spvPerMatch.size else 0.0
      val spvMax: Double     = if (spvPerMatch.nonEmpty) spvPerMatch.max else 1.0
      val spvScore: Int      = math.min(100, (spvMedia / math.max(spvMax, 1.0) * 100 * 1.5).toInt)
      val spvSerie: List[Double] = spvPerMatch.takeRight(20)

      // Breakdown por tipo
      val spv1v1Total: Int  = spvRows.map(_._2).sum
      val spvAerTotal: Int  = spvRows.map(_._3).sum
      val spvNorTotal: Int  = spvRows.map(r => math.max(0, r._1 - r._2 - r._3)).sum
      val spvTotalAll: Int  = spv1v1Total + spvAerTotal + spvNorTotal
      val pct1v1: Int       = if (spvTotalAll > 0) (spv1v1Total * 100 / spvTotalAll) else 0
      val pctAer: Int       = if (spvTotalAll > 0) (spvAerTotal * 100 / spvTotalAll) else 0
      val pctNor: Int       = if (spvTotalAll > 0) (spvNorTotal * 100 / spvTotalAll) else 0

      // ── 4. Bypass Rate: Lineas Superadas ────────────────────────────────────
      val rsBP = conn.createStatement().executeQuery(
        "SELECT lineas_superadas, acciones_pie, fecha FROM matches " +
        "WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 30")
      var bpRows = List[(Int,Int)]()
      while (rsBP.next()) bpRows = bpRows :+ (rsBP.getInt("lineas_superadas"), rsBP.getInt("acciones_pie"))
      val bpMedia: Double   = if (bpRows.nonEmpty) bpRows.map(_._1.toDouble).sum / bpRows.size else 0.0
      val bpConDatos: Int   = bpRows.count(_._1 > 0)
      val bpEfic: Double    = {
        val filas = bpRows.filter(_._2 > 0)
        if (filas.nonEmpty) filas.map(r => r._1.toDouble / r._2).sum / filas.size else 0.0
      }
      val bpSerie: List[Int] = bpRows.map(_._1).takeRight(20)

      // ── 5. ROI de Entrenamiento ──────────────────────────────────────────────
      // Para cada partido, busca los entrenamientos en los 7 dias previos
      // y calcula la correlacion entre calidad/atencion media y la nota del partido
      val rsROI = conn.createStatement().executeQuery(
        "SELECT m.fecha as mfecha, m.nota, " +
        "  (SELECT AVG(t.calidad) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as avg_calidad, " +
        "  (SELECT AVG(t.atencion) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as avg_atencion, " +
        "  (SELECT AVG(t.rpe) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as avg_rpe, " +
        "  (SELECT COUNT(*) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as num_sesiones " +
        "FROM matches m WHERE m.status='PLAYED' AND m.nota > 0 ORDER BY m.fecha DESC LIMIT 30")
      var roiRows = List[(Double,Double,Double,Double,Int)]()
      while (rsROI.next()) {
        val nota = rsROI.getDouble("nota")
        val cal  = Option(rsROI.getObject("avg_calidad")).map(_.toString.toDouble).getOrElse(0.0)
        val ate  = Option(rsROI.getObject("avg_atencion")).map(_.toString.toDouble).getOrElse(0.0)
        val rpe  = Option(rsROI.getObject("avg_rpe")).map(_.toString.toDouble).getOrElse(0.0)
        val ses  = rsROI.getInt("num_sesiones")
        if (ses > 0) roiRows = roiRows :+ (nota, cal, ate, rpe, ses)
      }
      val roiCorrCalidad: Double  = calcCorrelation(roiRows.map(r => (r._2, r._1)))
      val roiCorrAtencion: Double = calcCorrelation(roiRows.map(r => (r._3, r._1)))
      val roiCorrCarga: Double    = calcCorrelation(roiRows.map(r => (r._4, r._1)))
      val roiPartidosConDatos: Int = roiRows.size
      val roiSesionesMedia: Double = if (roiRows.nonEmpty) roiRows.map(_._5.toDouble).sum / roiRows.size else 0.0

      // ── 6. Analisis IA Moneyball ─────────────────────────────────────────────
      val analisisIA: String = {
        val prompt = s"""Eres un analista de datos de porteros al nivel de los mejores clubes de Europa. Analiza estas metricas avanzadas:
xT_GK (distribucion): ${f"$xtScore%.2f"} pts/partido | Correlacion con nota: ${f"$xtCorr%.2f"}
xPoints (clutch): ${f"$xpMedia%.2f"} pts/partido | Clutch Rating: $clutchRating/100
SPV (sweeper): ${f"$spvMedia%.1f"} pts/partido | Paradas 1v1: $pct1v1% | Aereas: $pctAer%
Bypass Rate: ${f"$bpMedia%.1f"} lineas/partido | Eficiencia: ${f"${bpEfic*100}%.0f"}%
ROI Entreno: correlacion calidad-nota: ${f"$roiCorrCalidad%.2f"} | atencion-nota: ${f"$roiCorrAtencion%.2f"} | carga-nota: ${f"$roiCorrCarga%.2f"}
Responde en 3 partes exactas, en espanol, conciso y directo:
PATRON: [patron principal detectado en 1-2 frases]
VENTAJA: [metrica donde destaca mas y por que es relevante]
CONSEJO: [recomendacion de mejora basada en los datos]"""
        AIProvider.ask(prompt, None, bypassCache = false)
      }

      // ── 7. Etiquetas temporales para graficos ────────────────────────────────
      val rsLabels = conn.createStatement().executeQuery(
        "SELECT TO_CHAR(fecha,'MM-DD') as f FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 20")
      var labels = List[String]()
      while (rsLabels.next()) labels = labels :+ rsLabels.getString("f")
      val labelsRev: List[String] = labels.reverse

      Map(
        "xtScore"          -> xtScore,
        "xtCorr"           -> xtCorr,
        "xtSerie"          -> xtSerie,
        "xpMedia"          -> xpMedia,
        "xpTotal"          -> xpTotal,
        "xpSerie"          -> xpSerie,
        "clutchRating"     -> clutchRating,
        "spvScore"         -> spvScore,
        "spvMedia"         -> spvMedia,
        "spvSerie"         -> spvSerie,
        "spv1v1Pct"        -> pct1v1,
        "spvAerPct"        -> pctAer,
        "spvNorPct"        -> pctNor,
        "bpMedia"          -> bpMedia,
        "bpEfic"           -> bpEfic,
        "bpSerie"          -> bpSerie,
        "bpConDatos"       -> bpConDatos,
        "roiCorrCalidad"   -> roiCorrCalidad,
        "roiCorrAtencion"  -> roiCorrAtencion,
        "roiCorrCarga"     -> roiCorrCarga,
        "roiPartidos"      -> roiPartidosConDatos,
        "roiSesiones"      -> roiSesionesMedia,
        "analisisIA"       -> analisisIA,
        "labels"           -> labelsRev
      )
    } finally { conn.close() }
  }

  // Pearson correlation helper
  private def calcCorrelation(pairs: List[(Double, Double)]): Double = {
    if (pairs.size < 3) return 0.0
    val n = pairs.size.toDouble
    val meanX = pairs.map(_._1).sum / n
    val meanY = pairs.map(_._2).sum / n
    val num   = pairs.map(p => (p._1 - meanX) * (p._2 - meanY)).sum
    val denX  = math.sqrt(pairs.map(p => math.pow(p._1 - meanX, 2)).sum)
    val denY  = math.sqrt(pairs.map(p => math.pow(p._2 - meanY, 2)).sum)
    if (denX * denY == 0) 0.0 else num / (denX * denY)
  }

  def getDigitalTwinData(hPadre: Double, hMadre: Double): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Fecha de nacimiento y edad actual
      val rsP = conn.createStatement().executeQuery(
        "SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsP.next())
        Option(rsP.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19")
      else "2015-06-19"
      val hoy = java.time.LocalDate.now()
      val nac = java.time.LocalDate.parse(fechaNac)
      val edadAnios  = java.time.Period.between(nac, hoy).getYears
      val edadMeses  = java.time.Period.between(nac, hoy).getYears * 12 +
                       java.time.Period.between(nac, hoy).getMonths

      // 2. Historial de crecimiento completo
      val rsG = conn.createStatement().executeQuery(
        "SELECT TO_CHAR(fecha,'YYYY-MM') as mes, altura, peso, velocidad_crecimiento FROM physical_growth ORDER BY fecha ASC")
      var growthRows = List[(String, Double, Double, Double)]()
      while (rsG.next()) growthRows = growthRows :+ (
        rsG.getString("mes"),
        rsG.getDouble("altura"),
        rsG.getDouble("peso"),
        rsG.getDouble("velocidad_crecimiento"))

      val alturaActual: Double = if (growthRows.nonEmpty) growthRows.last._2 else 120.0
      val pesoActual: Double   = if (growthRows.nonEmpty) growthRows.last._3 else 30.0

      // 3. Velocidad de crecimiento maxima = PHV detector
      val velocidades = growthRows.map(_._4).filter(_ > 0)
      val phvVelocidad: Double = if (velocidades.nonEmpty) velocidades.max else 0.0
      val phvDetectado: Boolean = phvVelocidad >= 6.0  // >6cm/anio = pleno pico
      val fasePhv: String = if (phvDetectado) "PICO ACTIVO" else if (edadAnios < 12) "PRE-PICO" else "POST-PICO"

      // 4. Proyeccion de altura adulta
      // Base cientifica: Metodo Tanner midparent (correlacion genetica ~0.75)
      // chico = (hPadre + hMadre + 13) / 2  => rango +/- 8.5 cm (1 SD)
      val midParent: Double = if (hPadre > 0 && hMadre > 0) (hPadre + hMadre + 13.0) / 2.0 else 183.0

      // Tabla OMS: % de altura adulta alcanzado por edad (chicos)
      val pctPorEdad = Map(5->72.0, 6->75.0, 7->77.0, 8->80.0, 9->82.0, 10->84.0,
                           11->86.5, 12->89.0, 13->93.0, 14->97.0, 15->99.0, 16->100.0)

      // Altura actual inconsistente con la edad registrada
      val alturaConsistente: Boolean = alturaActual > 80 && !(edadAnios >= 12 && alturaActual < 130)

      // Factor extrapolado desde altura actual
      val factorAlturaActual: Double =
        if (alturaConsistente && edadAnios >= 5) {
          val pct = pctPorEdad.getOrElse(edadAnios, if(edadAnios > 16) 100.0 else 75.0)
          alturaActual / (pct / 100.0)
        } else midParent

      // Ponderacion dinamica por edad:
      // A menos edad, la extrapolacion desde la altura actual es menos fiable.
      // < 8 anos: 10% altura / 90% genetica
      // 8-11 anos: 25% altura / 75% genetica
      // 12-14 anos: 40% altura / 60% genetica
      // >= 15 anos: 60% altura / 40% genetica (ya casi adulto, los datos mandan)
      val pesoAlturaActual: Double =
        if (!alturaConsistente) 0.0
        else if (edadAnios < 8)  0.10
        else if (edadAnios < 12) 0.25
        else if (edadAnios < 15) 0.40
        else                     0.60

      val alturaProyRaw: Double = factorAlturaActual * pesoAlturaActual + midParent * (1.0 - pesoAlturaActual)

      // Suelo/techo: la proyeccion no puede alejarse mas de 1 SD del midparent (8.5 cm, Tanner)
      val alturaProyectada: Double = math.max(midParent - 8.5, math.min(midParent + 8.5, alturaProyRaw))
      val alturaMin: Double = alturaProyectada - 8.5
      val alturaMax: Double = alturaProyectada + 8.5
      val advertenciaFecha: Boolean = false  // fecha real confirmada

      // 5. Metricas de portero proyectadas
      val envergaduraActual: Double  = alturaActual  * 1.065
      val envergaduraAdulta: Double  = alturaProyectada * 1.065
      val alcanceActual: Double      = alturaActual  * 1.33
      val alcanceAdulto: Double      = alturaProyectada * 1.33
      // Cobertura porteria 7.32m x 2.44m = 17.86m2
      // Portero cubre aprox (envergadura * 2.5) m2 efectivos
      val coberturaActual: Double    = math.min(100.0, (envergaduraActual * 2.5 / 17.86) * 100)
      val coberturaAdulta: Double    = math.min(100.0, (envergaduraAdulta * 2.5 / 17.86) * 100)

      // 6. Comparativa con percentiles de porteros profesionales
      val pctAltura: Int = if (alturaProyectada >= 190) 90 else if (alturaProyectada >= 185) 75
                           else if (alturaProyectada >= 180) 50 else if (alturaProyectada >= 175) 25 else 10

      // 7. Evolucion de nota media por temporada para proyeccion de rendimiento
      val rsN = conn.createStatement().executeQuery(
        "SELECT id, nombre_club, categoria, media FROM seasons ORDER BY id ASC")
      var notaTemps = List[(String, Double)]()
      while (rsN.next()) {
        val label: String = Option(rsN.getString("nombre_club")).filter(_.nonEmpty).getOrElse("T" + rsN.getInt("id"))
        notaTemps = notaTemps :+ (label, rsN.getDouble("media"))
      }
      val tendenciaNota: Double = if (notaTemps.size >= 2) {
        val mejora = notaTemps.last._2 - notaTemps.head._2
        val aniosTranscurridos = notaTemps.size.toDouble
        mejora / aniosTranscurridos
      } else 0.5
      val notaActual: Double   = if (notaTemps.nonEmpty) notaTemps.last._2 else 60.0
      val aniosHasta18: Int    = math.max(0, 18 - edadAnios)
      val notaProyectada: Double = math.min(95.0, notaActual + tendenciaNota * aniosHasta18)

      // 8. Curva de crecimiento proyectada (puntos para el grafico)
      // Genera puntos desde edad actual hasta 18 anios
      val curvaProyeccion: List[(Int, Double)] = {
        val pctPorEdad2 = Map(7->77.0, 8->80.0, 9->82.0, 10->84.0, 11->86.5, 12->89.0,
                              13->93.0, 14->97.0, 15->99.0, 16->100.0, 17->100.0, 18->100.0)
        (edadAnios to 18).toList.map { edad =>
          val pct = pctPorEdad2.getOrElse(edad, 100.0)
          edad -> (alturaProyectada * pct / 100.0)
        }
      }

      // 9. Analisis IA del Digital Twin
      val analisisIA: String = {
        val prompt = s"""Eres un ojeador de elite y analista de rendimiento. Analiza el perfil proyectado de Hector, portero de $edadAnios anos:
Altura actual: ${alturaActual.toInt} cm | Proyeccion adulta: ${alturaProyectada.toInt} cm (rango ${alturaMin.toInt}-${alturaMax.toInt} cm)
Envergadura proyectada adulta: ${envergaduraAdulta.toInt} cm | Alcance de parada: ${alcanceAdulto.toInt} cm
Fase PHV: $fasePhv | Velocidad crecimiento maxima detectada: ${phvVelocidad.toInt} cm/anio
Nota media actual: ${notaActual.toInt} | Proyeccion nota a los 18 anos: ${notaProyectada.toInt}
Trayectoria: ${notaTemps.map(t => t._1+":"+t._2.toInt).mkString(", ")}
Responde en 4 partes exactas, en espanol, conciso y directo:
BIOTIPO: [descripcion del perfil fisico proyectado en 1-2 frases]
VENTAJA: [principal ventaja competitiva de su biotipo para ser portero en 1 frase]
RIESGO: [1 riesgo o area de mejora fisica concreta]
PROYECCION: [nivel al que podria llegar segun datos actuales, en 1 frase motivadora]"""
        AIProvider.ask(prompt, None, bypassCache = false)
      }

      Map(
        "edadAnios"         -> edadAnios,
        "alturaActual"      -> alturaActual,
        "pesoActual"        -> pesoActual,
        "alturaProyectada"  -> alturaProyectada,
        "alturaMin"         -> alturaMin,
        "alturaMax"         -> alturaMax,
        "envergaduraActual" -> envergaduraActual,
        "envergaduraAdulta" -> envergaduraAdulta,
        "alcanceActual"     -> alcanceActual,
        "alcanceAdulto"     -> alcanceAdulto,
        "coberturaActual"   -> coberturaActual,
        "coberturaAdulta"   -> coberturaAdulta,
        "pctAltura"         -> pctAltura,
        "phvVelocidad"      -> phvVelocidad,
        "fasePhv"           -> fasePhv,
        "notaActual"        -> notaActual,
        "notaProyectada"    -> notaProyectada,
        "tendenciaNota"     -> tendenciaNota,
        "growthRows"        -> growthRows,
        "curvaProyeccion"   -> curvaProyeccion,
        "notaTemps"         -> notaTemps,
        "analisisIA"        -> analisisIA,
        "hPadre"            -> hPadre,
        "hMadre"            -> hMadre,
        "midParent"         -> midParent,
        "advertenciaFecha"  -> advertenciaFecha
      )
    } finally { conn.close() }
  }

  // ── EFECTO MARIPOSA ──────────────────────────────────────────────────────
  def getEfectoMariposa(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          COUNT(*) as pj,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as clean_sheets,
          SUM(CASE WHEN goles_contra = 0 AND goles_favor > goles_contra THEN 1 ELSE 0 END) as cs_wins,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) as ganados,
          SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) as empatados,
          SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) as perdidos,
          AVG(nota) as nota_media
        FROM matches WHERE status = 'PLAYED'
      """)
      if (!rs.next()) return Map("ok" -> false)
      val pj         = rs.getInt("pj")
      val cs         = rs.getInt("clean_sheets")
      val csWins     = rs.getInt("cs_wins")
      val ganados    = rs.getInt("ganados")
      val empatados  = rs.getInt("empatados")
      val perdidos   = rs.getInt("perdidos")
      val notaMedia  = rs.getDouble("nota_media")

      // Win rate con y sin clean sheet
      val csWinRate  = if (cs > 0) (csWins.toDouble / cs * 100).toInt else 0
      val nonCsWins  = ganados - csWins
      val nonCs      = pj - cs
      val nonCsWinRate = if (nonCs > 0) (nonCsWins.toDouble / nonCs * 100).toInt else 0

      // Clutch points: partidos ganados donde margen = 1 gol con nota >= 7.5
      val rsClutch = conn.createStatement().executeQuery("""
        SELECT COUNT(*) as clutch,
               SUM(goles_favor - goles_contra) as margen_total
        FROM matches
        WHERE status = 'PLAYED'
          AND goles_favor > goles_contra
          AND (goles_favor - goles_contra) = 1
          AND nota >= 7.5
      """)
      val (clutch, margenTotal) = if (rsClutch.next())
        (rsClutch.getInt("clutch"), rsClutch.getInt("margen_total")) else (0, 0)

      // Influence data: nota por resultado para gráfico
      val rsInfluence = conn.createStatement().executeQuery("""
        SELECT
          CASE WHEN goles_favor > goles_contra THEN 'G'
               WHEN goles_favor = goles_contra THEN 'E'
               ELSE 'P' END as res,
          ROUND(nota::numeric, 1) as nota,
          COUNT(*) as cnt
        FROM matches WHERE status = 'PLAYED'
        GROUP BY res, ROUND(nota::numeric, 1)
        ORDER BY nota
      """)
      var influenceData = List[Map[String, Any]]()
      while (rsInfluence.next()) {
        influenceData = influenceData :+ Map(
          "res"  -> rsInfluence.getString("res"),
          "nota" -> rsInfluence.getDouble("nota"),
          "cnt"  -> rsInfluence.getInt("cnt")
        )
      }

      Map(
        "ok"           -> true,
        "pj"           -> pj,
        "cleanSheets"  -> cs,
        "csWinRate"    -> csWinRate,
        "nonCsWinRate" -> nonCsWinRate,
        "ganados"      -> ganados,
        "empatados"    -> empatados,
        "perdidos"     -> perdidos,
        "notaMedia"    -> notaMedia,
        "clutchPoints" -> clutch,
        "influenceData"-> influenceData
      )
    } finally { conn.close() }
  }

  // ── GEAR ROI ──────────────────────────────────────────────────────────────
  def getGearROI(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      // Get all gear with their usage and price
      val rs = conn.createStatement().executeQuery("""
        SELECT g.id, g.nombre, g.tipo, g.usos_actuales, g.vida_util_estimada,
               g.activo, COALESCE(g.precio_compra, 0) as precio,
               AVG(m.nota) as nota_media,
               SUM(CASE WHEN m.clima ILIKE '%lluv%' OR m.clima ILIKE '%rain%' THEN 1 ELSE 0 END) as partidos_lluvia
        FROM gear g
        LEFT JOIN matches m ON m.status = 'PLAYED'
          AND m.fecha >= (SELECT MIN(fecha) FROM matches WHERE status='PLAYED')
        GROUP BY g.id, g.nombre, g.tipo, g.usos_actuales, g.vida_util_estimada, g.activo, g.precio_compra
        ORDER BY g.activo DESC, g.usos_actuales DESC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val usos   = rs.getInt("usos_actuales")
        val vida   = rs.getInt("vida_util_estimada")
        val precio = rs.getDouble("precio")
        val nota   = rs.getDouble("nota_media")
        val lluvia = rs.getInt("partidos_lluvia")
        val desgaste = if (vida > 0) (usos.toDouble / vida * 100).toInt else 0
        val costePorPartido = if (usos > 0 && precio > 0) precio / usos else 0.0

        // Grip alert: >60% desgaste base + lluvia multiplier
        val gripLoss = math.min(100, desgaste + (lluvia * 3))
        val gripAlert = gripLoss >= 70

        list = list :+ Map(
          "id"              -> rs.getInt("id"),
          "nombre"          -> rs.getString("nombre"),
          "tipo"            -> rs.getString("tipo"),
          "usos"            -> usos,
          "vida"            -> vida,
          "activo"          -> rs.getBoolean("activo"),
          "precio"          -> precio,
          "desgaste"        -> desgaste,
          "gripLoss"        -> gripLoss,
          "gripAlert"       -> gripAlert,
          "costePorPartido" -> costePorPartido,
          "notaMedia"       -> nota,
          "partidosLluvia"  -> lluvia
        )
      }
      list
    } finally { conn.close() }
  }

  def updateGearPrecio(gearId: Int, precio: Double): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE gear SET precio_compra = ? WHERE id = ?")
      ps.setDouble(1, precio); ps.setInt(2, gearId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getTacticalStats(): Map[String, Int] = { var stats = scala.collection.mutable.Map("g_tot"->0, "g_alt"->0, "g_med"->0, "g_ras"->0, "g_izq"->0, "g_cen"->0, "g_der"->0, "p_tot"->0, "p_alt"->0, "p_med"->0, "p_ras"->0, "p_izq"->0, "p_cen"->0, "p_der"->0); val conn = getConnection(); try { val rs = conn.createStatement().executeQuery("SELECT zona_goles, zona_paradas FROM matches WHERE status='PLAYED' ORDER BY id DESC LIMIT 20"); while(rs.next()) { val zG = Option(rs.getString("zona_goles")).getOrElse(""); val zP = Option(rs.getString("zona_paradas")).getOrElse(""); zG.split(",").filter(_.nonEmpty).foreach { z => stats("g_tot")+=1; if(z.contains("T")) stats("g_alt")+=1 else if(z.contains("M")) stats("g_med")+=1 else stats("g_ras")+=1; if(z.contains("L")) stats("g_izq")+=1 else if(z.contains("C")) stats("g_cen")+=1 else stats("g_der")+=1 }; zP.split(",").filter(_.nonEmpty).foreach { z => stats("p_tot")+=1; if(z.contains("T")) stats("p_alt")+=1 else if(z.contains("M")) stats("p_med")+=1 else stats("p_ras")+=1; if(z.contains("L")) stats("p_izq")+=1 else if(z.contains("C")) stats("p_cen")+=1 else stats("p_der")+=1 } } } finally { conn.close() }; stats.toMap }
  def updateStats(s: PlayerCardData): Unit = { val conn=getConnection(); try { val st=conn.prepareStatement("UPDATE seasons SET media=?, stat_div=?, stat_han=?, stat_kic=?, stat_ref=?, stat_spd=?, stat_pos=? WHERE id=(SELECT MAX(id) FROM seasons)"); st.setDouble(1,s.media); st.setDouble(2,s.divRaw); st.setDouble(3,s.hanRaw); st.setDouble(4,s.kicRaw); st.setDouble(5,s.refRaw); st.setDouble(6,s.spdRaw); st.setDouble(7,s.posRaw); st.executeUpdate() } finally { conn.close() } }
  def getBackupCSV(): String = { val sb=new StringBuilder(); sb.append("RIVAL,GF,GC,MIN,NOTA,PARADAS,CLIMA,ESTADIO,NOTAS,REACCION,FECHA\n"); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='PLAYED' ORDER BY fecha ASC"); while(rs.next()){ sb.append(s"${rs.getString("rival")},${rs.getInt("goles_favor")},${rs.getInt("goles_contra")},${rs.getInt("minutos")},${rs.getDouble("nota")},${rs.getInt("paradas")},${Option(rs.getString("clima")).getOrElse("Sol")},${Option(rs.getString("estadio")).getOrElse("-")},${Option(rs.getString("notas_partido")).getOrElse("")},${Option(rs.getString("reaccion_goles")).getOrElse("")},${rs.getDate("fecha")}\n") } } finally {conn.close()}; sb.toString() }
  def updateObjective(id: Int, meta: Int): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("UPDATE objectives SET meta=? WHERE id=?"); ps.setInt(1,meta); ps.setInt(2,id); ps.executeUpdate() } finally {conn.close()} }
  def startNewSeason(categoria: String): String = { val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM seasons ORDER BY id DESC LIMIT 1"); if(rs.next()){ val s=conn.prepareStatement("INSERT INTO seasons (nombre_club, foto_jugador_url, club_escudo_url, media, stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos, fecha_inicio, categoria) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"); s.setString(1,rs.getString("nombre_club")); s.setString(2,rs.getString("foto_jugador_url")); s.setString(3,rs.getString("club_escudo_url")); s.setDouble(4,rs.getDouble("media")); s.setDouble(5,rs.getDouble("stat_div")); s.setDouble(6,rs.getDouble("stat_han")); s.setDouble(7,rs.getDouble("stat_kic")); s.setDouble(8,rs.getDouble("stat_ref")); s.setDouble(9,rs.getDouble("stat_spd")); s.setDouble(10,rs.getDouble("stat_pos")); s.setDate(11,Date.valueOf(LocalDate.now())); s.setString(12,fixEncoding(categoria)); s.executeUpdate() }; "Temporada nueva creada." } finally {conn.close()} }
  def getCareerSummary(): List[SeasonSummary] = { var l=List[SeasonSummary](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT s.id, s.categoria, s.club_escudo_url, s.foto_jugador_url, s.media, (SELECT COUNT(*) FROM matches m WHERE m.season_id=s.id AND m.status='PLAYED') as pj, (SELECT SUM(goles_contra) FROM matches m WHERE m.season_id=s.id AND m.status='PLAYED') as gc FROM seasons s ORDER BY s.id DESC"); while(rs.next()){ l=l:+SeasonSummary(rs.getInt("id"), Option(rs.getString("categoria")).getOrElse("Temp"), Option(rs.getString("club_escudo_url")).getOrElse(""), Option(rs.getString("foto_jugador_url")).getOrElse(""), rs.getInt("pj"), rs.getInt("gc"), 0, rs.getDouble("media").toInt) } } finally {conn.close()}; l }
  def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String): Unit = { val conn = getConnection(); try { conn.createStatement().executeUpdate(s"DELETE FROM rivals WHERE LOWER(nombre) = LOWER('${fixEncoding(nombre)}')"); val ps = conn.prepareStatement("INSERT INTO rivals (nombre, estilo_juego, jugadores_clave, notas_scouting) VALUES (?,?,?,?)"); ps.setString(1, fixEncoding(nombre)); ps.setString(2, estilo); ps.setString(3, fixEncoding(claves)); ps.setString(4, fixEncoding(notas)); ps.executeUpdate() } finally { conn.close() } }
  def getRivalInfo(nombre: String): Option[RivalInfo] = { var r: Option[RivalInfo]=None; val conn=getConnection(); try{ val ps=conn.prepareStatement("SELECT * FROM rivals WHERE LOWER(nombre)=LOWER(?)"); ps.setString(1,fixEncoding(nombre)); val rs=ps.executeQuery(); if(rs.next()) r=Some(RivalInfo(rs.getString("nombre"), rs.getString("estilo_juego"), rs.getString("jugadores_clave"), rs.getString("notas_scouting"))) } finally {conn.close()}; r }
  def addNewDrill(nombre: String, desc: String): Unit = { val conn = getConnection(); try { val ps = conn.prepareStatement("INSERT INTO drills (nombre, descripcion, sesiones_objetivo, sesiones_actuales, activo) VALUES (?, ?, 10, 0, TRUE)"); ps.setString(1, fixEncoding(nombre)); ps.setString(2, fixEncoding(desc)); ps.executeUpdate() } finally { conn.close() } }
  def getOMSPercents(): (Double, Double, Double, Double, Double, Double) = {
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)

    // Tabla Maestra OMS 5-18 anos (Percentiles 15, 50, 85)
    // Estructura: Edad -> (H50, H15, H85, W50, W15, W85)
    val tablaOMS = Map(
      5  -> (110.0, 105.3, 114.7, 18.3, 16.2, 21.0),
      6  -> (116.0, 111.0, 121.0, 20.5, 18.0, 24.0),
      7  -> (122.1, 116.8, 127.4, 22.9, 19.8, 27.2),
      8  -> (127.7, 122.1, 133.3, 25.4, 21.9, 30.5),
      9  -> (133.3, 127.3, 139.3, 28.1, 24.1, 34.1),
      10 -> (138.4, 132.3, 144.5, 31.2, 26.7, 38.2),
      11 -> (143.5, 137.1, 149.9, 34.6, 29.5, 43.1),
      12 -> (149.1, 142.2, 156.0, 38.6, 32.7, 48.9),
      13 -> (156.0, 148.5, 163.5, 43.5, 36.5, 55.5),
      14 -> (163.2, 155.0, 171.5, 49.3, 41.0, 63.0),
      15 -> (169.0, 161.0, 177.0, 55.0, 46.5, 70.0),
      16 -> (173.0, 165.0, 181.0, 60.5, 51.5, 76.5),
      17 -> (175.2, 167.0, 183.5, 64.5, 55.0, 81.5),
      18 -> (176.0, 168.0, 184.0, 67.0, 57.0, 84.0)
    )

    tablaOMS.getOrElse(edad, (176.0, 168.0, 184.0, 67.0, 57.0, 84.0))
  }
  def getActiveDrills(): List[Drill] = { var l=List[Drill](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM drills WHERE activo=TRUE ORDER BY id DESC"); while(rs.next()) l=l:+Drill(rs.getInt("id"), rs.getString("nombre"), Option(rs.getString("descripcion")).getOrElse(""), rs.getInt("sesiones_actuales"), rs.getInt("sesiones_objetivo")) } catch {case _:Exception=>} finally {conn.close()}; l }
  def progressDrills(): Unit = { val conn=getConnection(); try{ conn.createStatement().executeUpdate("UPDATE drills SET sesiones_actuales = sesiones_actuales + 1 WHERE activo = TRUE"); conn.createStatement().executeUpdate("UPDATE drills SET activo = FALSE WHERE sesiones_actuales >= sesiones_objetivo") } finally {conn.close()} }
  def importMatchesCSV(csvData: String): String = {
    var count = 0
    val lines = csvData.split("\n").map(_.trim).filter(_.nonEmpty)
    val dataLines = if (lines.headOption.exists(_.toLowerCase.contains("rival"))) lines.tail else lines
    val today = LocalDate.now().toString

    dataLines.foreach { line =>
      try {
        val p = line.split(",").map(_.trim)
        if (p.length >= 6) {
          val rival = fixEncoding(p(0))
          val gf = p(1).toInt
          val gc = p(2).toInt
          val min = p(3).toInt
          val nota = p(4).toDouble
          val paradas = p(5).toInt
          val clima = if(p.length > 6) p(6) else "Sol"
          val notas = if(p.length > 7) fixEncoding(p(7)) else "Importado"
          val reaccion = if(p.length > 8) fixEncoding(p(8)) else ""

          val c = getLatestCardData()
          val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas, 0,0,0,0)
          updateStats(n)

          val m = (n.divRaw * 0.2 + n.hanRaw * 0.2 + n.refRaw * 0.2 + n.posRaw * 0.2 + n.spdRaw * 0.05 + n.kicRaw * 0.15)

          // AQUI ESTABA EL ERROR: Faltaba el ultimo argumento "" para mapaCampo
          logMatch(rival, gf, gc, min, nota, m, paradas, "", "", "", 0, 0, 0, clima, "-", 20, notas, "", reaccion, today, "LIGA", 0,0,0,0, "")

          count += 1
        }
      } catch {
        case e: Exception => println(s"Error import line: $line")
      }
    }
    s"Importados $count partidos"
  }
  def importCalendarCSV(csvData: String): String = { var count=0; val lines=csvData.split("\n").map(_.trim).filter(_.nonEmpty); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rs.next()){ val sId=rs.getInt("id"); val ps=conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas) VALUES (?, ?, ?, ?, 'SCHEDULED', 0, 0, 0, 0, 0)"); lines.foreach { l => try { val p=l.split(",").map(_.trim); if(p.length>=2){ ps.setInt(1,sId); ps.setDate(2,Date.valueOf(p(0))); ps.setString(3,fixEncoding(p(1))); ps.setString(4,if(p.length>2) p(2).toUpperCase else "LIGA"); ps.executeUpdate(); count+=1 } } catch {case _:Exception=>} } } } finally {conn.close()}; s"Importados $count eventos." }
  def importWellnessCSV(csvData: String): String = { var count=0; val lines=csvData.split("\n").map(_.trim).filter(_.nonEmpty); val dataLines=if(lines.headOption.exists(_.toLowerCase.contains("sueno"))) lines.tail else lines; dataLines.foreach { line => try { val p=line.split(",").map(_.trim); if(p.length>=5){ logWellness(p(0).toInt, p(1).toDouble, p(2).toInt, p(3).toInt, if(p.length>4) p(4) else "", 0, 0.0, if(p.length>5) p(5).toInt else 3, "", "DISPONIBLE"); count+=1 } } catch {case _:Exception=>} }; s"Importados $count registros bio." }
  def logGrowth(altura: Double, peso: Double): Unit = { val conn = getConnection(); try { var velocity = 0.0; val rsLast = conn.createStatement().executeQuery("SELECT altura FROM physical_growth ORDER BY fecha DESC LIMIT 1"); if(rsLast.next()) { val lastHeight = rsLast.getDouble("altura"); if(altura > lastHeight) velocity = altura - lastHeight }; val ps = conn.prepareStatement("INSERT INTO physical_growth (altura, peso, velocidad_crecimiento) VALUES (?, ?, ?)"); ps.setDouble(1, altura); ps.setDouble(2, peso); ps.setDouble(3, velocity); ps.executeUpdate() } finally { conn.close() } }
  def getGrowthHistory(): String = {
    var l=List[String](); var dAlt=List[Double](); var dPeso=List[Double]()
    val conn=getConnection()
    try {
      // Extraemos altura y peso de la tabla physical_growth
      val rs=conn.createStatement().executeQuery("SELECT TO_CHAR(fecha, 'MM-DD') as f, altura, peso FROM physical_growth ORDER BY fecha ASC LIMIT 12")
      while(rs.next()){
        l = l :+ s"'${rs.getString("f")}'"
        dAlt = dAlt :+ rs.getDouble("altura")
        dPeso = dPeso :+ rs.getDouble("peso")
      }
    } finally {conn.close()}
    // El JSON debe coincidir con lo que el JavaScript espera
    s"""{ "labels": [${l.mkString(",")}], "altura": [${dAlt.mkString(",")}], "peso": [${dPeso.mkString(",")}] }"""
  }
  def addVideoTag(matchId: Int, min: Int, sec: Int, tipo: String, desc: String): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("INSERT INTO video_tags (match_id, minuto, segundo, tipo, descripcion) VALUES (?,?,?,?,?)"); ps.setInt(1, matchId); ps.setInt(2, min); ps.setInt(3, sec); ps.setString(4, tipo); ps.setString(5, fixEncoding(desc)); ps.executeUpdate() } finally {conn.close()} }
  def getVideoTags(matchId: Int): List[VideoTag] = { var l=List[VideoTag](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery(s"SELECT * FROM video_tags WHERE match_id=$matchId ORDER BY minuto ASC, segundo ASC"); while(rs.next()) l=l:+VideoTag(rs.getInt("id"), rs.getInt("match_id"), rs.getInt("minuto"), rs.getInt("segundo"), rs.getString("tipo"), rs.getString("descripcion")) } finally {conn.close()}; l }
  def deleteVideoTag(id: Int): Unit = { val conn=getConnection(); try{ conn.createStatement().executeUpdate(s"DELETE FROM video_tags WHERE id=$id") } finally {conn.close()} }

  def getVideotecaClips(tipoFiltro: String = ""): List[VideoClip] = {
    var clips = List[VideoClip]()
    val conn = getConnection()
    try {
      val where = if (tipoFiltro.nonEmpty) s"AND vt.tipo = '$tipoFiltro'" else ""
      val sql = s"""
        SELECT vt.id, vt.match_id, m.rival, m.fecha, m.video_url AS video,
               vt.minuto, vt.segundo, vt.tipo
        FROM video_tags vt
        JOIN matches m ON vt.match_id = m.id
        WHERE m.video_url IS NOT NULL AND m.video_url != '' $where
        ORDER BY m.fecha DESC, vt.minuto ASC, vt.segundo ASC
      """
      val rs = conn.createStatement().executeQuery(sql)
      while (rs.next()) {
        clips = clips :+ VideoClip(
          rs.getInt("id"), rs.getInt("match_id"),
          rs.getString("rival"), rs.getString("fecha"),
          rs.getString("video"),
          rs.getInt("minuto"), rs.getInt("segundo"),
          rs.getString("tipo")
        )
      }
    } finally { conn.close() }
    clips
  }
  def logPenalty(rival: String, zTiro: String, zSalto: String, esGol: Boolean): Unit = { val conn = getConnection(); try { val ps = conn.prepareStatement("INSERT INTO penalties (rival, zona_tiro, zona_salto, es_gol) VALUES (?, ?, ?, ?)"); ps.setString(1, fixEncoding(rival)); ps.setString(2, zTiro); ps.setString(3, zSalto); ps.setBoolean(4, esGol); ps.executeUpdate() } finally { conn.close() } }
  def getPenaltyStats(): List[PenaltyStat] = { val l = scala.collection.mutable.ListBuffer[PenaltyStat](); val conn = getConnection(); try { val rs = conn.createStatement().executeQuery("SELECT zona_tiro, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 1 ELSE 0 END) as goles FROM penalties GROUP BY zona_tiro"); while(rs.next()) l += PenaltyStat(rs.getString("zona_tiro"), rs.getInt("total"), rs.getInt("goles")) } finally { conn.close() }; l.toList }
  def addNewGear(nombre: String, tipo: String, vida: Int, img: String): Unit = { val conn=getConnection(); try { val r=conn.prepareStatement("UPDATE gear SET activo=FALSE WHERE tipo=? AND activo=TRUE"); r.setString(1,tipo); r.executeUpdate(); val a=conn.prepareStatement("INSERT INTO gear (nombre, tipo, vida_util_estimada, usos_actuales, activo, imagen_url) VALUES (?,?,?,0,TRUE, ?)"); a.setString(1,fixEncoding(nombre)); a.setString(2,tipo); a.setInt(3,vida); a.setString(4, img); a.executeUpdate() } finally { conn.close() } }
  def getActiveGear(): List[GearItem] = { var l=List[GearItem](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT * FROM gear WHERE activo = TRUE ORDER BY tipo DESC"); while(rs.next()) { val (u,max)=(rs.getInt("usos_actuales"),rs.getInt("vida_util_estimada")); l=l:+GearItem(rs.getInt("id"),rs.getString("nombre"),rs.getString("tipo"),u,max,if(max>0 && u.toDouble/max > 0.9) "Critico" else "Optimo", Option(rs.getString("imagen_url")).getOrElse("")) } } finally { conn.close() }; l }
  def logWellness(sueno: Int, horas: Double, energia: Int, dolor: Int, zona: String, altura: Int, peso: Double, animo: Int, notas: String, estadoFisico: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("INSERT INTO wellness (sueno, horas_sueno, energia, dolor, zona_dolor, altura, peso, animo, notas_conducta, estado_fisico) VALUES (?,?,?,?,?,?,?,?,?,?)"); s.setInt(1,sueno); s.setDouble(2, horas); s.setInt(3,energia); s.setInt(4,dolor); s.setString(5,fixEncoding(zona)); s.setInt(6, altura); s.setDouble(7, peso); s.setInt(8, animo); s.setString(9, fixEncoding(notas)); s.setString(10, estadoFisico); s.executeUpdate(); if(altura > 0 && peso > 0) logGrowth(altura.toDouble, peso) } finally { conn.close() } }
  def logTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: Int, rutina: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("INSERT INTO trainings (tipo, foco, rpe, calidad, atencion, rutina_detalle) VALUES (?,?,?,?,?,?)"); s.setString(1,tipo); s.setString(2,fixEncoding(foco)); s.setInt(3,rpe); s.setInt(4,calidad); s.setInt(5, atencion); s.setString(6,fixEncoding(rutina)); s.executeUpdate(); conn.createStatement().executeUpdate("UPDATE gear SET usos_actuales = usos_actuales + 1 WHERE activo = TRUE"); if (tipo.contains("Papa")) progressDrills() } finally { conn.close() } }
  // --- EN: DatabaseManager.scala ---

  def getSmartInsights(): String = {
    val conn = getConnection()
    try {
      val sb = new StringBuilder()

      // 1. DETECTOR "TORPEZA DEL ESTIRON" (Crecimiento Rapido + Bajada Coordinacion)
      val rsGrowth = conn.createStatement().executeQuery("SELECT velocidad_crecimiento FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val growthSpeed = if(rsGrowth.next()) rsGrowth.getDouble("velocidad_crecimiento") else 0.0

      if (growthSpeed > 0.5) { // Si ha crecido mas de 0.5cm recientemente
        val rsTech = conn.createStatement().executeQuery("SELECT coordinacion FROM technical_reviews ORDER BY fecha DESC LIMIT 2")
        if (rsTech.next()) {
          val currCoord = rsTech.getInt("coordinacion")
          if (rsTech.next()) {
            val prevCoord = rsTech.getInt("coordinacion")
            if (currCoord < prevCoord) {
              sb.append(s"⚠️ ALERTA BIO-MECÁNICA: Crecimiento acelerado detectado (+${growthSpeed}cm) coincidiendo con bajada de coordinación. Riesgo de 'Torpeza del Estirón'. Recomendación: simplificar tareas técnicas y trabajar propiocepción.\n")
            }
          }
        }
      }

      // 2. DETECTOR DE PATRONES DE DOLOR (Dolor > 0 vs Tipo de Entreno)
      // Buscamos si hay correlacion entre dolor y superficie/tipo en los ultimos 10 registros
      val rsPain = conn.createStatement().executeQuery(
        """
      SELECT w.dolor, t.tipo, t.foco
      FROM wellness w
      JOIN trainings t ON w.fecha = t.fecha
      WHERE w.dolor > 1
      ORDER BY w.id DESC LIMIT 5
      """
      )

      var painCount = 0
      var lastContext = ""
      while(rsPain.next()) {
        painCount += 1
        lastContext = fixEncoding(rsPain.getString("tipo")) + " (" + fixEncoding(rsPain.getString("foco")) + ")"
      }

      if (painCount >= 2) {
        sb.append(s"🔍 PATRÓN DE DOLOR: Detectadas $painCount sesiones recientes con dolor. Contexto frecuente: $lastContext. Revisar calzado o dureza del terreno.\n")
      }

      if (sb.isEmpty) "Sin anomalías biométricas detectadas hoy." else sb.toString().trim

    } catch {
      case e: Exception => "Error calculando insights."
    } finally {
      conn.close()
    }
  }
  def getWorkloads(days: Int): Seq[Double] = {
    val conn = getConnection()
    var loads = List[Double]()
    try {
      val ps = conn.prepareStatement("""
      (SELECT (minutos * 4) as load FROM matches WHERE status='PLAYED' AND fecha >= CURRENT_DATE - ?)
      UNION ALL
      (SELECT (60 * rpe) as load FROM trainings WHERE fecha >= CURRENT_DATE - ?)
    """)
      ps.setInt(1, days); ps.setInt(2, days)
      val rs = ps.executeQuery()
      while(rs.next()) { loads = loads :+ rs.getDouble("load") }
    } finally { conn.close() }
    loads
  }

  // --- GESTION DE CINTURON DE JUDO ---
  def updateJudoBelt(nuevoCinturon: String): Unit = {
    val conn = getConnection(); try {
      // Nos aseguramos de que la columna existe en la tabla seasons
      val ps = conn.prepareStatement("UPDATE seasons SET judo_belt = ? WHERE id = (SELECT MAX(id) FROM seasons)")
      ps.setString(1, nuevoCinturon); ps.executeUpdate()
    } finally { conn.close() }
  }

  def getWeatherPerformance(): Map[String, (Double, Double)] = {
    val conn = getConnection()
    val stats = scala.collection.mutable.Map[String, (Double, Double)]()
    try {
      val query = """
      SELECT clima, AVG(nota) as media_nota, AVG(goles_contra) as media_gc
      FROM matches
      WHERE status='PLAYED'
      GROUP BY clima
    """
      val rs = conn.createStatement().executeQuery(query)
      while(rs.next()) {
        stats(rs.getString("clima")) = (rs.getDouble("media_nota"), rs.getDouble("media_gc"))
      }
    } finally { conn.close() }
    stats.toMap
  }

  def getTechnicalAlerts(): List[String] = {
    val reviews = getTechnicalReviews().takeRight(3) // Analizamos las ultimas 3
    if (reviews.size < 2) return Nil

    var alerts = List[String]()

    // Ejemplo: Logica para detectar bajada en blocaje
    val blocajes = reviews.map(_.blocaje)
    if (blocajes.last < blocajes.head) {
      alerts = alerts :+ "[!] Tendencia a la baja en BLOCAJE. Se recomienda sesion tecnica analitica."
    }

    // Ejemplo: Logica para detectar valentia baja
    if (reviews.last.valentia < 5) {
      alerts = alerts :+ "[fire] Alerta de VALENTIA: Hector necesita refuerzo en salidas 1v1."
    }

    alerts
  }

  def saveAcademicNote(asig: String, nota: Double, tipo: String, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("INSERT INTO academic_performance (asignatura, nota, tipo_evaluacion, comentarios) VALUES (?, ?, ?, ?)")
      ps.setString(1, fixEncoding(asig))
      ps.setDouble(2, nota)
      ps.setString(3, tipo)
      ps.setString(4, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ── FASE 2: FUNCIONES COGNITIVAS Y CARGA ─────────────────────────────────

  def getRPEHistory(days: Int = 60): List[(String, Int, Int, Int)] = {
    // (fecha, rpe, calidad, atencion)
    var l = List[(String, Int, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        s"SELECT fecha::TEXT, rpe, calidad, atencion FROM trainings WHERE fecha >= CURRENT_DATE - $days ORDER BY fecha ASC"
      )
      while (rs.next()) l = l :+ (rs.getString(1), rs.getInt(2), rs.getInt(3), rs.getInt(4))
    } finally { conn.close() }
    l
  }

  def getWeeklyLoad(weeks: Int = 12): List[(String, Int, Int)] = {
    // (semana, carga_total=minutos*rpe, num_sesiones)
    var l = List[(String, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          TO_CHAR(DATE_TRUNC('week', fecha), 'YYYY-MM-DD') as semana,
          SUM(rpe * 60) as carga,
          COUNT(*) as sesiones
        FROM trainings
        WHERE fecha >= CURRENT_DATE - ${weeks * 7}
        GROUP BY DATE_TRUNC('week', fecha)
        ORDER BY semana ASC
      """)
      while (rs.next()) l = l :+ (rs.getString(1), rs.getInt(2), rs.getInt(3))
    } finally { conn.close() }
    l
  }

  def getSleepMatchCorrelation(): List[(String, Double, Double, String)] = {
    // Para cada partido: (fecha, horas_sueno_noche_anterior, nota_partido, rival)
    var l = List[(String, Double, Double, String)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          m.fecha::TEXT,
          COALESCE(w.horas_sueno, 0) as horas,
          m.nota,
          m.rival
        FROM matches m
        LEFT JOIN wellness w ON w.fecha = m.fecha - INTERVAL '1 day'
        WHERE m.status = 'PLAYED' AND m.nota > 0
        ORDER BY m.fecha DESC
        LIMIT 30
      """)
      while (rs.next()) l = l :+ (rs.getString(1), rs.getDouble(2), rs.getDouble(3), rs.getString(4))
    } finally { conn.close() }
    l
  }

  def getSleepHistory(days: Int = 60): List[(String, Double, Int, Int)] = {
    // (fecha, horas_sueno, calidad_sueno(1-5), animo)
    var l = List[(String, Double, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        s"SELECT fecha::TEXT, horas_sueno, sueno, animo FROM wellness WHERE fecha >= CURRENT_DATE - $days ORDER BY fecha ASC"
      )
      while (rs.next()) l = l :+ (rs.getString(1), rs.getDouble(2), rs.getInt(3), rs.getInt(4))
    } finally { conn.close() }
    l
  }

  def getFatigaDetector(days: Int = 30): List[(String, Int, Double, Int)] = {
    // (fecha, atencion_entreno, nota_academica, animo_wellness)
    var l = List[(String, Int, Double, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          t.fecha::TEXT,
          t.atencion,
          COALESCE(a.nota, 0) as nota_acad,
          COALESCE(w.animo, 3) as animo
        FROM trainings t
        LEFT JOIN academic_performance a ON a.fecha = t.fecha
        LEFT JOIN wellness w ON w.fecha = t.fecha
        WHERE t.fecha >= CURRENT_DATE - $days
        ORDER BY t.fecha ASC
      """)
      while (rs.next()) l = l :+ (rs.getString(1), rs.getInt(2), rs.getDouble(3), rs.getInt(4))
    } finally { conn.close() }
    l
  }

  def getNotificationAlerts(): List[(String, String, String)] = {
    val alerts = scala.collection.mutable.ListBuffer[(String, String, String)]()
    val conn = getConnection()
    try {
      // 1. ACWR riesgo — calculado directo sin sub-conexion
      val rsAcute = conn.prepareStatement(
        "SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?"
      )
      rsAcute.setInt(1, 7); val rsA = rsAcute.executeQuery()
      val acuteLoad = if (rsA.next()) rsA.getDouble(1) else 0.0

      val rsChronic = conn.prepareStatement(
        "SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?"
      )
      rsChronic.setInt(1, 28); val rsC = rsChronic.executeQuery()
      val chronicLoad = if (rsC.next()) rsC.getDouble(1) else 0.0

      val acwr = if (chronicLoad > 0) (acuteLoad / 7.0) / (chronicLoad / 28.0) else 0.0
      if (acwr > 1.5) alerts += (("danger", "ACWR ALTO", s"Ratio carga: ${f"$acwr%.2f"} — Riesgo de lesion"))

      // 2. Sin registrar wellness hoy
      val rsW = conn.createStatement().executeQuery("SELECT COUNT(*) FROM wellness WHERE fecha = CURRENT_DATE")
      if (rsW.next() && rsW.getInt(1) == 0)
        alerts += (("warning", "SIN WELLNESS", "No has registrado tu estado fisico hoy"))

      // 3. Proximo partido en menos de 3 dias
      val rsM = conn.createStatement().executeQuery(
        "SELECT rival, fecha FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC LIMIT 1"
      )
      if (rsM.next()) {
        val fechaPartido = rsM.getDate("fecha").toLocalDate
        val diasRestantes = java.time.temporal.ChronoUnit.DAYS.between(java.time.LocalDate.now(), fechaPartido)
        if (diasRestantes <= 3 && diasRestantes >= 0)
          alerts += (("info", s"PARTIDO EN ${diasRestantes}D", s"vs ${fixEncoding(rsM.getString("rival"))} — ${rsM.getDate("fecha")}"))
      }

      // 4. Baja atencion sostenida
      val rsAtt = conn.createStatement().executeQuery(
        "SELECT COUNT(*) FROM trainings WHERE fecha >= CURRENT_DATE - 7 AND atencion < 6"
      )
      if (rsAtt.next() && rsAtt.getInt(1) >= 3)
        alerts += (("warning", "FATIGA MENTAL", "3+ sesiones con baja atencion esta semana"))

    } catch { case _: Exception => } finally { conn.close() }
    alerts.toList
  }

   def getCognitiveInsight(): String = {
    val conn = getConnection()
    try {
      // Buscamos la media de notas de los ultimos 30 dias
      val rsAcad = conn.createStatement().executeQuery("SELECT AVG(nota) FROM academic_performance WHERE fecha > CURRENT_DATE - 30")
      val avgAcad = if(rsAcad.next()) rsAcad.getDouble(1) else 0.0

      // Buscamos la media de 'atencion' en los entrenamientos de los ultimos 30 dias
      val rsTrain = conn.createStatement().executeQuery("SELECT AVG(atencion) FROM trainings WHERE fecha > CURRENT_DATE - 30")
      val avgAtt = if(rsTrain.next()) rsTrain.getDouble(1) else 0.0

      val baseMsg = if (avgAcad > 0 && avgAtt > 0) {
        if (avgAcad < 6.0 && avgAtt < 7.0)
          "[IA] **ALERTA COGNITIVA**: Baja concentracion detectada en ambos entornos. Posible fatiga mental general."
        else if (avgAcad > 8.0 && avgAtt < 6.0)
          "[futbol] **DESCONEXION**: Alto rendimiento academico pero baja atencion en campo. ?Falta de motivacion deportiva?"
        else
          "[OK] **SINERGIA OPTIMA**: Equilibrio detectado entre estudios y deporte."
      } else "Faltan datos para analisis cognitivo."

      // Footbar: contexto de carga fisica de los ultimos 5 partidos (senal de fatiga)
      val rsFb = conn.createStatement().executeQuery("""
        SELECT AVG(f.distancia_km) AS dist_media, AVG(f.sprint_max_kmh) AS sprint_media,
               AVG(m.nota) AS nota_media, COUNT(*) AS n
        FROM (SELECT id, nota FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 5) m
        JOIN footbar_sessions f ON f.match_id = m.id
      """)
      val fatigaMsg =
        if (rsFb.next() && rsFb.getInt("n") >= 2) {
          val distMedia = rsFb.getDouble("dist_media")
          val sprintMedia = rsFb.getDouble("sprint_media")
          val notaMediaFb = rsFb.getDouble("nota_media")
          if (notaMediaFb < 6.0 && (distMedia > 3.0 || sprintMedia > 24.0))
            " [Footbar] **CARGA FISICA ALTA**: Los ultimos partidos con datos Footbar muestran distancia/sprint elevados junto a notas bajas — posible senal de fatiga."
          else ""
        } else ""

      baseMsg + fatigaMsg
    } finally { conn.close() }
  }

  def saveMedicalReport(fecha: String, tipo: String, fileBase64: String, esPrevio: Boolean): String = {
    val conn = getConnection()
    try {
      val prompt = """
      Analiza este informe medico de un nino deportista.
      Extrae: 1) Diagnostico claro. 2) Impacto en el deporte (ej: limitar saltos, reposo).
      3) Si es una analitica, destaca valores fuera de rango.
      Responde en formato: DIAGNOSTICO: ... | RECOMENDACION: ...
    """

      // Detectamos el formato para el AIProvider
      val mime = if (fileBase64.contains("pdf")) "application/pdf" else "image/jpeg"

      // La magia: AIProvider.ask devolvera el analisis de la cache si ya se subio este mismo archivo
      val analisisIA = AIProvider.ask(prompt, Some((mime, fileBase64)))

      val partes = analisisIA.split("\\|")
      val diag = partes.headOption.getOrElse("No detectado").replace("DIAGNOSTICO:", "").trim
      val rec = partes.lastOption.getOrElse("No detectado").replace("RECOMENDACION:", "").trim

      val ps = conn.prepareStatement("INSERT INTO medical_vault (fecha_informe, tipo_informe, diagnostico_ia, recomendaciones_ia, es_previo_futbol) VALUES (?, ?, ?, ?, ?)")
      ps.setDate(1, java.sql.Date.valueOf(fecha))
      ps.setString(2, tipo)
      ps.setString(3, fixEncoding(diag))
      ps.setString(4, fixEncoding(rec))
      ps.setBoolean(5, esPrevio)
      ps.executeUpdate()

      s"Informe procesado: $diag"
    } catch { case e: Exception => s"Error medico: ${e.getMessage}" } finally { conn.close() }
  }

  def getLatestMedicalInsight(): String = {
    val conn = getConnection()
    try {
      // Buscamos el ultimo informe que no sea un simple 'Baseline' previo
      val query = """
      SELECT diagnostico_ia, recomendaciones_ia
      FROM medical_vault
      WHERE es_previo_futbol = FALSE
      ORDER BY fecha_informe DESC LIMIT 1
    """
      val rs = conn.createStatement().executeQuery(query)
      if (rs.next()) {
        val diag = rs.getString("diagnostico_ia")
        val rec = rs.getString("recomendaciones_ia")
        // Retornamos un string combinado para el widget
        s"$diag. RECOMENDACION: $rec"
      } else ""
    } catch {
      case _: Exception => ""
    } finally {
      conn.close()
    }
  }

  def saveMedicalRecordFull(fecha: String, tipo: String, diag: String, rec: String, esPrevio: Boolean): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO medical_vault (fecha_informe, tipo_informe, diagnostico_ia, recomendaciones_ia, es_previo_futbol) VALUES (?, ?, ?, ?, ?)"
      )
      ps.setDate(1, java.sql.Date.valueOf(fecha))
      ps.setString(2, tipo)
      ps.setString(3, fixEncoding(diag))
      ps.setString(4, fixEncoding(rec))
      ps.setBoolean(5, esPrevio)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getMedicalReports(): List[MedicalReport] = {
    val conn = getConnection()
    val reports = scala.collection.mutable.ListBuffer[MedicalReport]()
    try {
      // Consultamos los informes ordenados por fecha, los mas recientes primero
      val query = """
      SELECT id, fecha_informe, tipo_informe, diagnostico_ia, recomendaciones_ia, es_previo_futbol
      FROM medical_vault
      ORDER BY fecha_informe DESC
    """
      val rs = conn.createStatement().executeQuery(query)
      while (rs.next()) {
        reports += MedicalReport(
          id = rs.getInt("id"),
          fecha = rs.getString("fecha_informe"),
          tipo = rs.getString("tipo_informe"),
          diagnostico = rs.getString("diagnostico_ia"),
          recomendaciones = rs.getString("recomendaciones_ia"),
          esPrevio = rs.getBoolean("es_previo_futbol")
        )
      }
    } catch {
      case e: Exception => println(s"Error recuperando informes medicos: ${e.getMessage}")
    } finally {
      conn.close()
    }
    reports.toList
  }
  def getCoachAdvice(): String = {
    val card = getLatestCardData()
    val matches = getMatchesList().take(3) // Ultimos 3 partidos
    val cog = getCognitiveInsight() // Datos de estudios/atencion
    val wellness = getOracleInsights() // Datos fisicos/crecimiento

    val prompt = s"""
    Actua como un Coach de Elite para un portero de ${calcularEdadExacta(card.fechaNacimiento)} anos.
    CONTEXTO TECNICO: Media ${card.media}, Reflejos ${card.ref}.
    ESTADO FISICO: $wellness
    ESTADO COGNITIVO: $cog
    ULTIMOS PARTIDOS: ${matches.map(m => m.rival + " nota:" + m.nota).mkString(", ")}

    Dame 3 consejos breves y motivadores. Si detectas fatiga o baja atencion academica, prioriza el descanso psicologico.
  """

    // Usamos el AIProvider centralizado con cache (se refresca una vez al dia o tras cambios)
    AIProvider.ask(prompt)
  }
  def analyzeAudioLog(matchId: Int, audioBase64: String): String = {
    val prompt = """Eres el psicólogo deportivo de Héctor, portero de élite. Analiza este audio post-partido con el Protocolo de 4 Anclas. Sé directo y conciso.

ANCLA 1 — ESTADO BIO-EMOCIONAL:
Detecta nivel de energía, fatiga o frustración por tono de voz y mensaje.
Formato: [Motivado/Neutro/Bajón/Frustrado/Eufórico] + 1 frase explicativa

ANCLA 2 — HITO CRÍTICO:
La acción técnica más relevante mencionada (parada clave, fallo en salida, etc.)
Formato: ACIERTO o ERROR: descripción breve

ANCLA 3 — FACTOR EXTERNO:
Menciona clima, campo, árbitro u otros factores externos no numéricos.
Formato: [detectado/no mencionado] + detalle si existe

ANCLA 4 — ENFOQUE DE MEJORA:
Qué aspecto específico quiere trabajar para el próximo partido.
Formato: OBJETIVO: descripción + 1 consejo técnico concreto

Responde en texto plano. Si el audio no cubre un ancla, escribe "No mencionado"."""

    // Llamada al motor unificado con soporte para audio
    val res = AIProvider.ask(prompt, Some(("audio/webm", audioBase64)))

    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE matches SET analisis_voz = ? WHERE id = ?")
      ps.setString(1, fixEncoding(res))
      ps.setInt(2, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
    res
  }
  def testAIConnection(): String = {
    try {
      AIProvider.ask("Responde solo con la palabra OK si me escuchas.", None, bypassCache = true)
    } catch {
      case e: Exception => s"Fallo total: ${e.getMessage}"
    }
  }

  // ── MATCH CONTEXT ANALYTICS ─────────────────────────────────────────────
  def getMatchContextData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. POR TIPO DE PARTIDO (LIGA / TORNEO / AMISTOSO)
      val rsTipo = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(tipo_partido, 'LIGA') as tipo,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias,
          AVG(paradas) as paradas_media
        FROM matches WHERE status='PLAYED'
        GROUP BY COALESCE(tipo_partido, 'LIGA')
        ORDER BY nota_media DESC
      """)
      var porTipo = List[Map[String, Any]]()
      while (rsTipo.next()) {
        porTipo = porTipo :+ Map(
          "tipo"    -> rsTipo.getString("tipo"),
          "pj"      -> rsTipo.getInt("pj"),
          "nota"    -> rsTipo.getDouble("nota_media"),
          "gc"      -> rsTipo.getDouble("gc_media"),
          "limpias" -> rsTipo.getInt("limpias"),
          "paradas" -> rsTipo.getDouble("paradas_media")
        )
      }

      // 2. POR CLIMA
      val rsClima = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(clima, 'Sin datos') as clima,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches WHERE status='PLAYED' AND clima IS NOT NULL AND clima != ''
        GROUP BY COALESCE(clima, 'Sin datos')
        ORDER BY nota_media DESC
      """)
      var porClima = List[Map[String, Any]]()
      while (rsClima.next()) {
        porClima = porClima :+ Map(
          "clima"   -> rsClima.getString("clima"),
          "pj"      -> rsClima.getInt("pj"),
          "nota"    -> rsClima.getDouble("nota_media"),
          "gc"      -> rsClima.getDouble("gc_media"),
          "limpias" -> rsClima.getInt("limpias")
        )
      }

      // 3. LOCAL vs VISITANTE — campo es_local de la tabla matches
      val rsLV = conn.createStatement().executeQuery("""
        SELECT
          es_local,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches
        WHERE status='PLAYED' AND es_local IS NOT NULL
        GROUP BY es_local
      """)
      var localNota = 0.0; var localGC = 0.0; var localPJ = 0; var localLimpias = 0
      var visitNota = 0.0; var visitGC  = 0.0; var visitPJ = 0; var visitLimpias = 0
      while (rsLV.next()) {
        val esLocal = rsLV.getBoolean("es_local")
        val nota = rsLV.getDouble("nota_media")
        val gc   = rsLV.getDouble("gc_media")
        val pj   = rsLV.getInt("pj")
        val cs   = rsLV.getInt("limpias")
        if (esLocal) {
          localNota = nota; localGC = gc; localPJ = pj; localLimpias = cs
        } else {
          visitNota = nota; visitGC = gc; visitPJ = pj; visitLimpias = cs
        }
      }
      val localNotaFinal = localNota
      val visitNotaFinal = visitNota
      val localGCFinal   = localGC
      val visitGCFinal   = visitGC

      // 4. POR DURACIÓN (franjas de minutos)
      val rsDur = conn.createStatement().executeQuery("""
        SELECT
          CASE
            WHEN minutos < 40 THEN 'Partido corto (<40 min)'
            WHEN minutos < 60 THEN 'Media parte (40-59 min)'
            WHEN minutos < 80 THEN 'Normal (60-79 min)'
            ELSE 'Partido completo (80+ min)'
          END as franja,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches WHERE status='PLAYED' AND minutos > 0
        GROUP BY 1
        ORDER BY nota_media DESC
      """)
      var porDuracion = List[Map[String, Any]]()
      while (rsDur.next()) {
        porDuracion = porDuracion :+ Map(
          "franja"  -> rsDur.getString("franja"),
          "pj"      -> rsDur.getInt("pj"),
          "nota"    -> rsDur.getDouble("nota_media"),
          "gc"      -> rsDur.getDouble("gc_media"),
          "limpias" -> rsDur.getInt("limpias")
        )
      }

      // 5. TENDENCIA MENSUAL (últimos 12 meses, para el gráfico de línea)
      val rsTrend = conn.createStatement().executeQuery("""
        SELECT
          TO_CHAR(DATE_TRUNC('month', fecha), 'MM/YY') as mes,
          AVG(nota) as nota_media,
          COUNT(*) as pj,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches
        WHERE status='PLAYED' AND fecha >= CURRENT_DATE - INTERVAL '12 months'
        GROUP BY DATE_TRUNC('month', fecha)
        ORDER BY DATE_TRUNC('month', fecha) ASC
      """)
      var trendLabels = List[String]()
      var trendNotas  = List[Double]()
      var trendPJs    = List[Int]()
      while (rsTrend.next()) {
        trendLabels = trendLabels :+ rsTrend.getString("mes")
        trendNotas  = trendNotas  :+ rsTrend.getDouble("nota_media")
        trendPJs    = trendPJs    :+ rsTrend.getInt("pj")
      }

      // 6. MEJOR Y PEOR CONTEXTO (resumen ejecutivo)
      val allContexts: List[(String, Double, Int)] = (
        porTipo.map(m => (m("tipo").toString, m("nota").asInstanceOf[Double], m("pj").asInstanceOf[Int])) ++
        porClima.map(m => (m("clima").toString, m("nota").asInstanceOf[Double], m("pj").asInstanceOf[Int]))
      ).filter(_._3 >= 2) // mínimo 2 partidos para ser significativo

      val mejorCtx = allContexts.sortBy(-_._2).headOption
      val peorCtx  = allContexts.sortBy(_._2).headOption

      // 7. Totales globales para referencia
      val rsGlobal = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as pj, AVG(nota) as nota, AVG(goles_contra) as gc FROM matches WHERE status='PLAYED'"
      )
      val (totalPJ, notaGlobal, gcGlobal) = if (rsGlobal.next())
        (rsGlobal.getInt("pj"), rsGlobal.getDouble("nota"), rsGlobal.getDouble("gc"))
      else (0, 0.0, 0.0)

      Map(
        "porTipo"        -> porTipo,
        "porClima"       -> porClima,
        "porDuracion"    -> porDuracion,
        "localNota"      -> localNotaFinal,
        "localGC"        -> localGCFinal,
        "localPJ"        -> localPJ,
        "localLimpias"   -> localLimpias,
        "visitNota"      -> visitNotaFinal,
        "visitGC"        -> visitGCFinal,
        "visitPJ"        -> visitPJ,
        "visitLimpias"   -> visitLimpias,
        "trendLabels"    -> trendLabels,
        "trendNotas"     -> trendNotas,
        "trendPJs"       -> trendPJs,
        "mejorCtx"       -> mejorCtx,
        "peorCtx"        -> peorCtx,
        "totalPJ"        -> totalPJ,
        "notaGlobal"     -> notaGlobal,
        "gcGlobal"       -> gcGlobal
      )
    } finally { conn.close() }
  }

  // ── BYPASS RATE POR TEMPORADA (Fase 6.5 completion) ─────────────────────
  def getBypassRateEvolution(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          EXTRACT(YEAR FROM fecha)::TEXT as anio,
          AVG(lineas_superadas) as bp_media,
          AVG(CASE WHEN acciones_pie > 0 THEN lineas_superadas::FLOAT / acciones_pie ELSE NULL END) as bp_efic,
          COUNT(*) as pj,
          SUM(lineas_superadas) as bp_total
        FROM matches
        WHERE status='PLAYED' AND lineas_superadas > 0
        GROUP BY EXTRACT(YEAR FROM fecha)
        ORDER BY anio ASC
      """)
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "anio"     -> rs.getString("anio"),
          "bpMedia"  -> rs.getDouble("bp_media"),
          "bpEfic"   -> rs.getDouble("bp_efic"),
          "pj"       -> rs.getInt("pj"),
          "bpTotal"  -> rs.getInt("bp_total")
        )
      }
      rows
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — MARKET ESTIMATOR
  // Regresion lineal multivariable sobre metricas existentes.
  // Variables: nota_media, psxg_delta, spv_score, bypass_efic, bio_factor, edad
  // Output: valor de mercado formativo estimado (€) + percentil vs academias
  // ─────────────────────────────────────────────────────────────────────────────
  def getMarketEstimatorData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ── 1. Metricas base ────────────────────────────────────────────────────
      val rsBase = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(AVG(nota), 0.0)                                             AS nota_media,
          COALESCE(AVG(paradas), 0.0)                                          AS par_media,
          COALESCE(AVG(paradas_1v1), 0.0)                                      AS par1v1_media,
          COALESCE(AVG(paradas_aereas), 0.0)                                   AS paer_media,
          COALESCE(AVG(CASE WHEN acciones_pie > 0
                    THEN lineas_superadas::FLOAT / acciones_pie END), 0.0)     AS bypass_efic,
          COALESCE(AVG(lineas_superadas), 0.0)                                 AS bypass_vol,
          COUNT(*) FILTER (WHERE goles_contra = 0)                            AS limpias,
          COUNT(*)                                                             AS pj,
          COALESCE(
            SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END)::FLOAT
            / NULLIF(COUNT(*), 0), 0.0)                                       AS win_rate
        FROM matches WHERE status='PLAYED'
      """)

      var notaMedia = 0.0; var par1v1 = 0.0; var parAer = 0.0
      var bypassEfic = 0.0; var bypassVol = 0.0; var limpias = 0
      var pj = 0; var winRate = 0.0; var parMedia = 0.0
      if (rsBase.next()) {
        notaMedia  = rsBase.getDouble("nota_media")
        parMedia   = rsBase.getDouble("par_media")
        par1v1     = rsBase.getDouble("par1v1_media")
        parAer     = rsBase.getDouble("paer_media")
        bypassEfic = rsBase.getDouble("bypass_efic")
        bypassVol  = rsBase.getDouble("bypass_vol")
        limpias    = rsBase.getInt("limpias")
        pj         = rsBase.getInt("pj")
        winRate    = rsBase.getDouble("win_rate")
      }

      // ── 2. PSxG delta (goles esperados - reales) ────────────────────────────
      val rsPsxg = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(AVG(goles_contra), 0) as gc_media,
          COALESCE(AVG(
            CASE zona_goles
              WHEN '5' THEN 0.85 WHEN '4' THEN 0.65 WHEN '6' THEN 0.65
              WHEN '2' THEN 0.45 WHEN '8' THEN 0.45
              ELSE 0.30
            END
          ), 0) as xg_media
        FROM matches
        WHERE status='PLAYED' AND goles_contra > 0
      """)
      var psxgDelta = 0.0
      if (rsPsxg.next()) {
        val gcReal = rsPsxg.getDouble("gc_media")
        val xgEsperado = rsPsxg.getDouble("xg_media")
        psxgDelta = xgEsperado - gcReal  // positivo = mejor que esperado
      }

      // ── 3. Edad y bio-banding ───────────────────────────────────────────────
      val rsEdad = conn.createStatement().executeQuery(
        "SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsEdad.next())
        Option(rsEdad.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19")
      else "2015-06-19"
      val hoy    = java.time.LocalDate.now()
      val nac    = java.time.LocalDate.parse(fechaNac)
      val edad   = java.time.Period.between(nac, hoy).getYears
      // Factor bio-banding: mayor = más avanzado en madurez (ventaja para clubes)
      val bioFactor: Double = if (edad <= 10) 0.70 else if (edad <= 12) 0.80
                               else if (edad <= 14) 0.90 else if (edad <= 16) 1.00
                               else 1.10

      // ── 4. SPV calculado inline ─────────────────────────────────────────────
      val spvEfic: Double = {
        val total = par1v1 * 1.5 + parAer * 1.2 + (parMedia - par1v1 - parAer)
        if (total > 0) math.min(100.0, total * 10.0) else 0.0
      }

      // ── 5. REGRESION LINEAL MULTIVARIABLE ───────────────────────────────────
      // Formula derivada de la literatura de scouting formativo (pesos calibrados):
      //   V = nota_normalizada * 35
      //     + spv_normalizado   * 20
      //     + bypass_efic       * 15
      //     + psxg_delta_norm   * 15
      //     + win_rate          * 10
      //     + bio_factor_boost  *  5
      // Resultado en unidades de "puntuación de valor" (0-100) → mapeado a €
      // notaMedia esta en escala 0-10 (campo "nota" del Match Center) — se escala a
      // 0-100 antes de normalizar contra el rango de referencia 40-100.
      val notaNorm:   Double = math.max(0, math.min(1.0, (notaMedia * 10.0 - 40.0) / 60.0))
      val spvNorm:    Double = math.max(0, math.min(1.0, spvEfic / 100.0))
      val bypassNorm: Double = math.max(0, math.min(1.0, bypassEfic))
      val psxgNorm:   Double = math.max(0, math.min(1.0, (psxgDelta + 2.0) / 4.0))
      val bioBoost:   Double = (bioFactor - 0.7) / 0.4  // 0-1 range

      val rawScore: Double =
        notaNorm   * 35.0 +
        spvNorm    * 20.0 +
        bypassNorm * 15.0 +
        psxgNorm   * 15.0 +
        winRate    * 10.0 +
        bioBoost   *  5.0

      // Escala de valor formativo: 0-100 pts → 0 a 150.000 €
      // (referencia: porteros de academia sub-16 elite: 30k-80k; sub-14: 10k-40k)
      val valorEstimado: Int = (rawScore * 1500).toInt

      // ── 6. Percentil vs academias españolas (tabla de referencia calibrada) ──
      // Percentiles empiricos por edad para porteros de academia regional/nacional
      val percentilesRef: Map[Int, List[Int]] = Map(
        // edad -> [p10, p25, p50, p75, p90] en puntos rawScore
        9  -> List(15, 22, 35, 48, 62),
        10 -> List(18, 26, 38, 51, 65),
        11 -> List(20, 29, 42, 55, 68),
        12 -> List(22, 32, 45, 58, 71),
        13 -> List(25, 35, 48, 62, 74),
        14 -> List(28, 38, 52, 65, 77),
        15 -> List(30, 42, 55, 68, 80),
        16 -> List(32, 45, 58, 71, 83),
        17 -> List(35, 48, 62, 74, 86)
      )
      val edadRef = math.max(9, math.min(17, edad))
      val refs    = percentilesRef.getOrElse(edadRef, List(20, 35, 50, 65, 80))
      val percentil: Int =
        if (rawScore <= refs(0)) 5
        else if (rawScore <= refs(1)) 15
        else if (rawScore <= refs(2)) 35
        else if (rawScore <= refs(3)) 60
        else if (rawScore <= refs(4)) 80
        else 95

      // ── 7. Label de nivel formativo ─────────────────────────────────────────
      val nivelLabel: String =
        if (percentil >= 90) "ELITE NACIONAL"
        else if (percentil >= 75) "ACADEMIA PRIMERA"
        else if (percentil >= 50) "ACADEMIA REGIONAL"
        else if (percentil >= 25) "FORMATIVO MEDIO"
        else "EN DESARROLLO"

      val nivelColor: String =
        if (percentil >= 90) "success"
        else if (percentil >= 75) "info"
        else if (percentil >= 50) "warning"
        else "secondary"

      // ── 8. Análisis IA ──────────────────────────────────────────────────────
      val analisisIA: String = {
        val euroStr = if (valorEstimado >= 1000) s"${valorEstimado/1000}K€" else s"${valorEstimado}€"
        val prompt = s"""Eres un director de captacion de un club de LaLiga evaluando el perfil formativo de un portero.
Datos del portero (${edad} años):
- Nota media actuaciones: ${f"$notaMedia%.1f"}/100
- SPV (Sweeper Value): ${f"$spvEfic%.1f"}/100
- Bypass Rate eficiencia: ${f"${bypassEfic*100}%.0f"}%
- PSxG Delta: ${if(psxgDelta >= 0) "+" else ""}${f"$psxgDelta%.2f"} (positivo = mejor que xG esperado)
- Win Rate: ${f"${winRate*100}%.0f"}%
- Factor madurez bio-banding: $bioFactor
- Puntuacion modelo: ${f"$rawScore%.1f"}/100
- Valor estimado: $euroStr
- Percentil academias nacionales: $percentil%

Responde en espanol en exactamente 3 bloques HTML:
<h4>DIAGNOSTICO</h4><p>[perfil general en 2 frases]</p>
<h4>PALANCAS DE VALOR</h4><p>[las 2 metricas que mas elevan su valor y por que]</p>
<h4>RUTA AL SIGUIENTE NIVEL</h4><p>[que tiene que mejorar para subir un tier en los proximos 12 meses]</p>
Solo HTML limpio, sin markdown ni backticks."""
        AIProvider.ask(prompt, None, bypassCache = false)
      }

      // ── 9. Evolucion del rawScore por temporada ─────────────────────────────
      val rsEvo = conn.createStatement().executeQuery("""
        SELECT
          s.nombre_club as club, s.id as tid,
          COALESCE(AVG(m.nota), 0) as nota_t,
          COALESCE(AVG(m.paradas_1v1), 0) as p1v1_t,
          COALESCE(AVG(m.paradas_aereas), 0) as paer_t,
          COALESCE(AVG(m.paradas), 0) as par_t
        FROM seasons s
        LEFT JOIN matches m ON m.status='PLAYED'
          AND m.fecha BETWEEN COALESCE(s.fecha_inicio, '2000-01-01') AND COALESCE(s.fecha_fin, CURRENT_DATE)
        GROUP BY s.id, s.nombre_club ORDER BY s.id ASC
      """)
      var evoSeries = List[(String, Double)]()
      while (rsEvo.next()) {
        val nT   = rsEvo.getDouble("nota_t")
        val p1T  = rsEvo.getDouble("p1v1_t")
        val pAT  = rsEvo.getDouble("paer_t")
        val pT   = rsEvo.getDouble("par_t")
        val spvT = math.min(100.0, (p1T * 1.5 + pAT * 1.2 + (pT - p1T - pAT)) * 10.0)
        val nNorm = math.max(0, math.min(1.0, (nT - 40.0) / 60.0))
        val sNorm = math.max(0, math.min(1.0, spvT / 100.0))
        val scoreT = nNorm * 35.0 + sNorm * 20.0 + 50.0 * 0.45  // otras metricas sin historico
        val label  = Option(rsEvo.getString("club")).filter(_.nonEmpty).getOrElse(s"T${rsEvo.getInt("tid")}")
        evoSeries = evoSeries :+ (label, math.min(100.0, scoreT))
      }

      Map(
        "rawScore"      -> rawScore,
        "valorEstimado" -> valorEstimado,
        "percentil"     -> percentil,
        "nivelLabel"    -> nivelLabel,
        "nivelColor"    -> nivelColor,
        "notaMedia"     -> notaMedia,
        "spvEfic"       -> spvEfic,
        "bypassEfic"    -> bypassEfic,
        "psxgDelta"     -> psxgDelta,
        "winRate"       -> winRate,
        "bioFactor"     -> bioFactor,
        "edad"          -> edad,
        "pj"            -> pj,
        "limpias"       -> limpias,
        "analisisIA"    -> analisisIA,
        "evoLabels"     -> evoSeries.map(_._1),
        "evoScores"     -> evoSeries.map(_._2),
        "refs"          -> refs
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — NLP SCOUTING AGGREGATOR
  // ─────────────────────────────────────────────────────────────────────────────
  def processScoutReport(
    textoRaw: String, ojeador: String, clubOrigen: String, fecha: String
  ): Map[String, Any] = {
    // Llamada a Gemini con prompt estructurado
    val prompt = s"""Eres un analista de captacion experto en porteros de formacion.
Has recibido el siguiente informe de un ojeador sobre un portero:

---
$textoRaw
---

Extrae y estructura la informacion en JSON PURO (sin markdown, sin backticks, sin explicaciones):
{
  "nivel_tecnico": <0-10>,
  "nivel_tactico": <0-10>,
  "nivel_fisico": <0-10>,
  "nivel_mental": <0-10>,
  "nivel_distribucion": <0-10>,
  "nivel_global": <0-10>,
  "proyeccion": "<ELITE|PRIMERA|SEGUNDA|REGIONAL|FORMATIVO>",
  "recomendacion": "<FICHAR_YA|SEGUIMIENTO_6M|SEGUIMIENTO_12M|DESCARTAR>",
  "fortalezas": "<lista de 2-3 puntos fuertes concretos en 1 linea>",
  "areas_mejora": "<lista de 2-3 areas de mejora concretas en 1 linea>",
  "resumen_ia": "<parrafo de 3-4 frases con el veredicto final del informe>"
}
SOLO el JSON, nada mas."""

    val respuesta = AIProvider.ask(prompt, None, bypassCache = true)

    // Parsear JSON de Gemini
    val cleaned = respuesta.replace("```json","").replace("```","").trim
    val parsed: ujson.Value = try { ujson.read(cleaned) }
                              catch { case _: Exception => ujson.Obj() }

    def jInt(k: String): Int    = try { parsed(k).num.toInt } catch { case _: Exception => 0 }
    def jStr(k: String): String = try { parsed(k).str }       catch { case _: Exception => "" }

    val nivTec = jInt("nivel_tecnico")
    val nivTac = jInt("nivel_tactico")
    val nivFis = jInt("nivel_fisico")
    val nivMen = jInt("nivel_mental")
    val nivDis = jInt("nivel_distribucion")
    val nivGlb = jInt("nivel_global")
    val proy   = jStr("proyeccion")
    val rec    = jStr("recomendacion")
    val fort   = jStr("fortalezas")
    val areas  = jStr("areas_mejora")
    val res    = jStr("resumen_ia")

    // Guardar en DB
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO scouting_reports
          (fecha, ojeador, club_origen, texto_raw, nivel_tecnico, nivel_tactico,
           nivel_fisico, nivel_mental, nivel_distribucion, nivel_global,
           proyeccion, recomendacion, fortalezas, areas_mejora, resumen_ia)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        RETURNING id
      """)
      val fechaDate = try { java.sql.Date.valueOf(fecha) }
                      catch { case _: Exception => java.sql.Date.valueOf(java.time.LocalDate.now().toString) }
      ps.setDate(1, fechaDate)
      ps.setString(2, fixEncoding(ojeador))
      ps.setString(3, fixEncoding(clubOrigen))
      ps.setString(4, fixEncoding(textoRaw))
      ps.setInt(5, nivTec); ps.setInt(6, nivTac); ps.setInt(7, nivFis)
      ps.setInt(8, nivMen); ps.setInt(9, nivDis); ps.setInt(10, nivGlb)
      ps.setString(11, proy); ps.setString(12, rec)
      ps.setString(13, fixEncoding(fort)); ps.setString(14, fixEncoding(areas))
      ps.setString(15, fixEncoding(res))
      val rs = ps.executeQuery()
      val newId = if (rs.next()) rs.getInt(1) else -1
      Map(
        "id"              -> newId,
        "nivel_tecnico"   -> nivTec, "nivel_tactico"     -> nivTac,
        "nivel_fisico"    -> nivFis, "nivel_mental"       -> nivMen,
        "nivel_distribucion" -> nivDis, "nivel_global"   -> nivGlb,
        "proyeccion"      -> proy,  "recomendacion"      -> rec,
        "fortalezas"      -> fort,  "areas_mejora"       -> areas,
        "resumen_ia"      -> res
      )
    } finally { conn.close() }
  }

  def getScoutReports(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT id, fecha, ojeador, club_origen, nivel_global, proyeccion,
               recomendacion, resumen_ia, fortalezas, areas_mejora,
               nivel_tecnico, nivel_tactico, nivel_fisico, nivel_mental, nivel_distribucion
        FROM scouting_reports ORDER BY fecha DESC, id DESC
      """)
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "id"           -> rs.getInt("id"),
          "fecha"        -> rs.getDate("fecha").toString,
          "ojeador"      -> Option(rs.getString("ojeador")).getOrElse(""),
          "club"         -> Option(rs.getString("club_origen")).getOrElse(""),
          "global"       -> rs.getInt("nivel_global"),
          "proyeccion"   -> Option(rs.getString("proyeccion")).getOrElse(""),
          "recomendacion"-> Option(rs.getString("recomendacion")).getOrElse(""),
          "resumen"      -> Option(rs.getString("resumen_ia")).getOrElse(""),
          "fortalezas"   -> Option(rs.getString("fortalezas")).getOrElse(""),
          "areas"        -> Option(rs.getString("areas_mejora")).getOrElse(""),
          "tec"          -> rs.getInt("nivel_tecnico"),
          "tac"          -> rs.getInt("nivel_tactico"),
          "fis"          -> rs.getInt("nivel_fisico"),
          "men"          -> rs.getInt("nivel_mental"),
          "dis"          -> rs.getInt("nivel_distribucion")
        )
      }
      rows
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — PERIODIZACION NUTRICIONAL REACTIVA
  // ─────────────────────────────────────────────────────────────────────────────
  def getNutritionPlan(forceRefresh: Boolean = false): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ── 1. ACWR actual ──────────────────────────────────────────────────────
      val rsAg = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAg.setInt(1, 7); val ag = rsAg.executeQuery(); val cargaAguda = if (ag.next()) ag.getDouble(1) else 0.0
      val rsCr = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) / 4.0 FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCr.setInt(1, 28); val cr = rsCr.executeQuery(); val cargaCronica = if (cr.next() && cr.getDouble(1) > 0) cr.getDouble(1) else 1.0
      val acwr: Double = cargaAguda / cargaCronica

      // ── 2. RPE media últimos 7 días ─────────────────────────────────────────
      val rsRpe = conn.createStatement().executeQuery(
        "SELECT COALESCE(AVG(rpe), 5.0) as rpe_m FROM trainings WHERE fecha >= CURRENT_DATE - 7")
      val rpeMedia: Double = if (rsRpe.next()) rsRpe.getDouble("rpe_m") else 5.0

      // ── 3. Nota último partido ──────────────────────────────────────────────
      val rsUlt = conn.createStatement().executeQuery(
        "SELECT nota, rival FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 1")
      val (notaUlt, rivalUlt) = if (rsUlt.next()) (rsUlt.getDouble("nota"), rsUlt.getString("rival")) else (60.0, "rival")

      // ── 4. Próximo partido ──────────────────────────────────────────────────
      val rsProx = conn.createStatement().executeQuery(
        "SELECT rival, fecha, tipo_partido FROM matches WHERE status='SCHEDULED' AND fecha >= CURRENT_DATE ORDER BY fecha ASC LIMIT 1")
      val (proximoRival, proximoFecha, proximoTipo) =
        if (rsProx.next()) (rsProx.getString("rival"),
          rsProx.getDate("fecha").toString,
          Option(rsProx.getString("tipo_partido")).getOrElse("LIGA"))
        else ("", "", "LIGA")

      // ── 5. Datos físicos actuales ───────────────────────────────────────────
      val rsFis = conn.createStatement().executeQuery(
        "SELECT altura, peso FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val (altura, peso) = if (rsFis.next()) (rsFis.getDouble("altura"), rsFis.getDouble("peso")) else (140.0, 35.0)

      // ── 6. Fase de carga: determinar contexto semanal ──────────────────────
      val faseStr: String =
        if (acwr > 1.5) "CARGA ALTA — semana de mucho trabajo o partido reciente"
        else if (acwr > 1.2) "CARGA MODERADA-ALTA — semana exigente"
        else if (acwr > 0.8) "CARGA NORMAL — semana standard"
        else "DESCARGA — semana de poco trabajo"

      val rendimientoStr: String =
        if (notaUlt >= 75) "buen rendimiento reciente (nota alta)"
        else if (notaUlt >= 55) "rendimiento normal"
        else "rendimiento bajo — posible fatiga o situacion de mejora"

      // ── 7. Verificar si hay plan reciente (< 6 días) en cache ─────────────
      if (!forceRefresh) {
        val rsCache = conn.createStatement().executeQuery(
          "SELECT plan_ia FROM nutrition_plans WHERE semana >= CURRENT_DATE - 6 ORDER BY created_at DESC LIMIT 1")
        if (rsCache.next()) {
          val cached = rsCache.getString("plan_ia")
          if (cached.nonEmpty && !cached.startsWith("Error")) {
            return Map("plan" -> cached, "acwr" -> acwr, "rpe" -> rpeMedia, "nota" -> notaUlt,
              "faseStr" -> faseStr, "altura" -> altura, "peso" -> peso, "cached" -> true)
          }
        }
      }

      // ── 8. Prompt a Gemini ──────────────────────────────────────────────────
      val proximoStr = if (proximoRival.nonEmpty) s"Tiene partido $proximoTipo contra $proximoRival el $proximoFecha."
                       else "No tiene partido programado esta semana."
      val prompt = s"""Eres un nutricionista deportivo especializado en porteros de formacion (academias de futbol).

Perfil del portero:
- Edad estimada: ${java.time.Period.between(
        try { val rs2 = conn.createStatement().executeQuery("SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
              if (rs2.next()) java.time.LocalDate.parse(Option(rs2.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19"))
              else java.time.LocalDate.of(2015,6,19) }
        catch { case _:Exception => java.time.LocalDate.of(2015,6,19) },
        java.time.LocalDate.now()).getYears} años
- Altura: ${altura.toInt} cm | Peso: ${f"$peso%.1f"} kg
- Contexto de carga: ACWR = ${f"$acwr%.2f"} ($faseStr)
- RPE media ultimos 7 dias: ${f"$rpeMedia%.1f"}/10
- Ultimo partido: ${f"$notaUlt%.0f"}/100 ($rendimientoStr)
- $proximoStr

Genera un PLAN NUTRICIONAL SEMANAL REACTIVO en HTML limpio (sin markdown, sin backticks).
Estructura exacta:
<h4>DIAGNOSTICO DE CARGA</h4>
<p>[1 parrafo evaluando el estado energetico actual]</p>

<h4>MACROS RECOMENDADOS (diarios)</h4>
<div class="row g-2 mb-3">
  <div class="col-6 col-md-3"><div class="card bg-dark border-primary text-center p-2">
    <div class="h3 text-primary fw-bold">[X]g</div><div class="small text-muted">Proteina</div>
    <div class="xx-small text-secondary">[razon]</div>
  </div></div>
  [repite para Carbohidratos, Grasas Saludables, Hidratacion en litros]
</div>

<h4>DISTRIBUCION POR DIA</h4>
<p>[descripcion de los 3 tipos de dias de la semana: dia pre-partido, dia partido, dia recuperacion]</p>

<h4>ALIMENTOS CLAVE ESTA SEMANA</h4>
<ul>[3-5 alimentos especificos con su razon nutricional]</ul>

<h4>ALERTA NUTRICIONAL</h4>
<p>[1 aviso especifico basado en ACWR o estado actual]</p>

Adapta TODO al contexto real: si ACWR > 1.3 prioriza recuperacion; si ACWR < 0.8 prioriza carga. Si hay partido proximos dias, reajusta los carbohidratos.
Solo HTML limpio."""

      val planIA = AIProvider.ask(prompt, None, bypassCache = true)

      // ── 9. Guardar en cache ─────────────────────────────────────────────────
      val psSave = conn.prepareStatement(
        "INSERT INTO nutrition_plans (semana, acwr, rpe_media, nota_ultimo, plan_ia) VALUES (CURRENT_DATE, ?, ?, ?, ?)")
      psSave.setDouble(1, acwr); psSave.setDouble(2, rpeMedia)
      psSave.setDouble(3, notaUlt); psSave.setString(4, planIA)
      psSave.executeUpdate()

      Map("plan" -> planIA, "acwr" -> acwr, "rpe" -> rpeMedia, "nota" -> notaUlt,
          "faseStr" -> faseStr, "altura" -> altura, "peso" -> peso, "cached" -> false)
    } finally { conn.close() }
  }

}