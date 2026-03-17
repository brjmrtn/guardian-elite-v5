import java.sql.{Connection, Date}
import java.time.LocalDate
import java.security.MessageDigest

// ─────────────────────────────────────────────────────────────────────────────
// AMATEUR DATA MODELS
// Tablas propias, sin compartir nada con el sistema Elite.
// ─────────────────────────────────────────────────────────────────────────────

case class AmUser(id: Int, username: String, nombre: String)

case class AmMatch(
                    id: Int, rival: String, gf: Int, gc: Int,
                    nota: Double, clima: String, estadio: String,
                    esLocal: Option[Boolean], fecha: String,
                    videoUrl: String, notas: String, analisisVoz: String,
                    posicionPartido: String,   // "portero" | "jugador"
                    posicionCampo: String,     // "Delantero", "Centrocampista", "Defensa", "" si portero
                    golesMarcados: Int,        // solo relevante si jugó de jugador de campo
                    asistencias: Int           // solo relevante si jugó de jugador de campo
                  )

case class AmGoal(
                   id: Int, matchId: Int, zona: String, situacion: String,
                   errorDefensivo: Boolean, minuto: Int, notas: String
                 )

case class AmPenalty(
                      id: Int, userId: Int, fecha: String, rival: String,
                      direccionTiro: String, direccionEstirada: String, parada: Boolean,
                      matchId: Option[Int], notas: String
                    )

case class AmGearItem(
                       id: Int, userId: Int, nombre: String, marca: String,
                       tipoLatex: String, corte: String, partidosUsados: Int,
                       activo: Boolean, notas: String
                     )

case class AmSchedule(
                       id: Int, userId: Int, rival: String, fecha: String,
                       hora: String, lugar: String, tipo: String, notas: String,
                       matchId: Option[Int]
                     )

// ─────────────────────────────────────────────────────────────────────────────
object AmateurDatabaseManager {

  // Reutilizamos el pool de conexiones existente — misma DB, tablas distintas
  private def getConn(): Connection = DatabaseManager.getConnection()

  private def md5(s: String): String =
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map("%02x".format(_)).mkString

  private def fix(s: String): String =
    if (s == null) "" else s
      .replace("\u00e1","á").replace("\u00e9","é").replace("\u00ed","í")
      .replace("\u00f3","ó").replace("\u00fa","ú").replace("\u00f1","ñ")
      .replace("\u00c1","Á").replace("\u00c9","É").replace("\u00cd","Í")
      .replace("\u00d3","Ó").replace("\u00da","Ú").replace("\u00d1","Ñ")

  private def md5am(s: String): String =
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map("%02x".format(_)).mkString

  // Cache-aware Gemini call — solo llama a la API si el hash no existe
  def askCached(prompt: String, invalidateCache: Boolean = false): String = {
    val hash = md5am(prompt)
    val conn = getConn()
    try {
      if (!invalidateCache) {
        val ps = conn.prepareStatement("SELECT respuesta FROM am_ai_cache WHERE prompt_hash = ?")
        ps.setString(1, hash)
        val rs = ps.executeQuery()
        if (rs.next()) return rs.getString("respuesta")
      }

      val apiKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim
      if (apiKey.isEmpty) return ""

      val payload = ujson.Obj("contents" -> ujson.Arr(ujson.Obj(
        "parts" -> ujson.Arr(ujson.Obj("text" -> prompt)))))
      val r = requests.post(
        s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=$apiKey",
        data = ujson.write(payload),
        headers = Map("Content-Type" -> "application/json"),
        readTimeout = 45000
      )
      val response = if (r.statusCode == 200)
        ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str.trim
      else ""

      if (response.nonEmpty && !response.startsWith("Error")) {
        val save = conn.prepareStatement(
          "INSERT INTO am_ai_cache (prompt_hash, respuesta) VALUES (?, ?) ON CONFLICT (prompt_hash) DO UPDATE SET respuesta = EXCLUDED.respuesta, creado_en = NOW()")
        save.setString(1, hash); save.setString(2, response)
        save.executeUpdate()
      }
      response
    } catch { case _: Exception => "" }
    finally { conn.close() }
  }

  // Invalida toda la cache IA de un usuario (llamar tras guardar partido)
  def invalidateAiCache(userId: Int): Unit = {
    val conn = getConn()
    try {
      // Borramos entradas que contengan el userId en el hash source
      // (borramos toda la cache del usuario — se recalcula con los nuevos datos)
      conn.prepareStatement(s"DELETE FROM am_ai_cache WHERE prompt_hash IN (SELECT prompt_hash FROM am_ai_cache WHERE creado_en < NOW() - INTERVAL '5 minutes')").executeUpdate()
      // Invalidación específica por usuario: guardamos un flag de "dirty"
      val ps = conn.prepareStatement(
        "INSERT INTO am_ai_cache (prompt_hash, respuesta) VALUES (?, 'INVALIDATED') ON CONFLICT (prompt_hash) DO UPDATE SET respuesta = 'INVALIDATED', creado_en = NOW()")
      ps.setString(1, s"dirty_user_$userId")
      ps.executeUpdate()
    } finally { conn.close() }
  }

  private def isUserCacheDirty(userId: Int): Boolean = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("SELECT respuesta FROM am_ai_cache WHERE prompt_hash = ?")
      ps.setString(1, s"dirty_user_$userId")
      val rs = ps.executeQuery()
      rs.next() && rs.getString("respuesta") == "INVALIDATED"
    } finally { conn.close() }
  }

  private def clearDirtyFlag(userId: Int): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("DELETE FROM am_ai_cache WHERE prompt_hash = ?")
      ps.setString(1, s"dirty_user_$userId")
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ── INIT TABLES ────────────────────────────────────────────────────────────
  def initTables(): Unit = {
    val conn = getConn()
    try {
      val s = conn.createStatement()

      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_users (
        id            SERIAL PRIMARY KEY,
        username      TEXT UNIQUE NOT NULL,
        password_hash TEXT NOT NULL,
        nombre        TEXT DEFAULT '',
        created_at    TIMESTAMP DEFAULT NOW()
      )""")

      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_matches (
        id               SERIAL PRIMARY KEY,
        user_id          INT REFERENCES am_users(id) ON DELETE CASCADE,
        rival            TEXT NOT NULL,
        goles_favor      INT DEFAULT 0,
        goles_contra     INT DEFAULT 0,
        nota             DOUBLE PRECISION DEFAULT 5.0,
        clima            TEXT DEFAULT 'Sol',
        estadio          TEXT DEFAULT '',
        es_local         BOOLEAN DEFAULT NULL,
        fecha            DATE DEFAULT CURRENT_DATE,
        video_url        TEXT DEFAULT '',
        notas            TEXT DEFAULT '',
        analisis_voz     TEXT DEFAULT '',
        posicion_partido TEXT DEFAULT 'portero',
        posicion_campo   TEXT DEFAULT '',
        goles_marcados   INT DEFAULT 0,
        asistencias      INT DEFAULT 0,
        current_season_num INT DEFAULT 1,
        created_at       TIMESTAMP DEFAULT NOW()
      )""")

      // Columnas añadidas en v7.2 — idempotentes
      s.executeUpdate("ALTER TABLE am_matches ADD COLUMN IF NOT EXISTS posicion_partido TEXT DEFAULT 'portero'")
      s.executeUpdate("ALTER TABLE am_matches ADD COLUMN IF NOT EXISTS posicion_campo TEXT DEFAULT ''")
      s.executeUpdate("ALTER TABLE am_matches ADD COLUMN IF NOT EXISTS goles_marcados INT DEFAULT 0")
      s.executeUpdate("ALTER TABLE am_matches ADD COLUMN IF NOT EXISTS asistencias INT DEFAULT 0")
      s.executeUpdate("ALTER TABLE am_matches ADD COLUMN IF NOT EXISTS current_season_num INT DEFAULT 1")

      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_match_goals (
        id               SERIAL PRIMARY KEY,
        match_id         INT REFERENCES am_matches(id) ON DELETE CASCADE,
        zona             TEXT DEFAULT 'MC',
        situacion        TEXT DEFAULT 'Remate',
        error_defensivo  BOOLEAN DEFAULT FALSE,
        minuto           INT DEFAULT 0,
        notas            TEXT DEFAULT ''
      )""")

      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_penalties (
        id                  SERIAL PRIMARY KEY,
        user_id             INT REFERENCES am_users(id) ON DELETE CASCADE,
        fecha               DATE DEFAULT CURRENT_DATE,
        rival               TEXT DEFAULT '',
        direccion_tiro      TEXT NOT NULL,
        direccion_estirada  TEXT NOT NULL,
        parada              BOOLEAN DEFAULT FALSE,
        match_id            INT REFERENCES am_matches(id) ON DELETE SET NULL,
        notas               TEXT DEFAULT ''
      )""")

      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_gear (
        id              SERIAL PRIMARY KEY,
        user_id         INT REFERENCES am_users(id) ON DELETE CASCADE,
        nombre          TEXT NOT NULL,
        marca           TEXT DEFAULT '',
        tipo_latex      TEXT DEFAULT '',
        corte           TEXT DEFAULT '',
        partidos_usados INT DEFAULT 0,
        activo          BOOLEAN DEFAULT TRUE,
        notas           TEXT DEFAULT ''
      )""")

      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_schedule (
        id          SERIAL PRIMARY KEY,
        user_id     INT REFERENCES am_users(id) ON DELETE CASCADE,
        rival       TEXT NOT NULL,
        fecha       DATE NOT NULL,
        hora        TEXT DEFAULT '',
        lugar       TEXT DEFAULT '',
        tipo        TEXT DEFAULT 'LIGA',
        notas       TEXT DEFAULT '',
        match_id    INT REFERENCES am_matches(id) ON DELETE SET NULL,
        created_at  TIMESTAMP DEFAULT NOW()
      )""")

      // IA cache Amateur — añadido en v7.4
      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_ai_cache (
        prompt_hash TEXT PRIMARY KEY,
        respuesta   TEXT,
        creado_en   TIMESTAMP DEFAULT NOW()
      )""")
      // Limpiar errores cacheados al arrancar
      s.executeUpdate("""DELETE FROM am_ai_cache WHERE
        respuesta LIKE 'Error:%' OR respuesta LIKE '%status code%' OR respuesta = ''
      """)

      // Wellness — añadido en v7.4
      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_wellness (
        id        SERIAL PRIMARY KEY,
        user_id   INT REFERENCES am_users(id) ON DELETE CASCADE,
        fecha     DATE NOT NULL,
        sueno     INT DEFAULT 0,
        energia   INT DEFAULT 0,
        animo     INT DEFAULT 0,
        notas     TEXT DEFAULT '',
        created_at TIMESTAMP DEFAULT NOW(),
        UNIQUE(user_id, fecha)
      )""")

      // Temporadas — añadidas en v7.3
      s.executeUpdate("ALTER TABLE am_users ADD COLUMN IF NOT EXISTS current_season INT DEFAULT 1")
      s.executeUpdate("""CREATE TABLE IF NOT EXISTS am_seasons (
        id          SERIAL PRIMARY KEY,
        user_id     INT REFERENCES am_users(id) ON DELETE CASCADE,
        season_num  INT NOT NULL,
        ended_at    TIMESTAMP DEFAULT NOW(),
        pj          INT DEFAULT 0,
        ganados     INT DEFAULT 0,
        empatados   INT DEFAULT 0,
        perdidos    INT DEFAULT 0,
        nota_media  DOUBLE PRECISION DEFAULT 0.0,
        gc_media    DOUBLE PRECISION DEFAULT 0.0,
        limpias     INT DEFAULT 0
      )""")

    } finally { conn.close() }
  }

  // ── USERS ──────────────────────────────────────────────────────────────────
  def registerUser(username: String, password: String, nombre: String): Either[String, Int] = {
    val conn = getConn()
    try {
      // Check if username exists
      val check = conn.prepareStatement("SELECT id FROM am_users WHERE LOWER(username) = LOWER(?)")
      check.setString(1, username)
      val rs = check.executeQuery()
      if (rs.next()) return Left("El nombre de usuario ya existe")

      val ps = conn.prepareStatement(
        "INSERT INTO am_users (username, password_hash, nombre) VALUES (?, ?, ?) RETURNING id"
      )
      ps.setString(1, username.trim.toLowerCase)
      ps.setString(2, md5(password))
      ps.setString(3, fix(nombre))
      val rs2 = ps.executeQuery()
      if (rs2.next()) Right(rs2.getInt(1)) else Left("Error al crear usuario")
    } catch {
      case e: Exception => Left(s"Error: ${e.getMessage}")
    } finally { conn.close() }
  }

  def authenticate(username: String, password: String): Option[AmUser] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT id, username, nombre FROM am_users WHERE LOWER(username) = LOWER(?) AND password_hash = ?"
      )
      ps.setString(1, username.trim)
      ps.setString(2, md5(password))
      val rs = ps.executeQuery()
      if (rs.next()) Some(AmUser(rs.getInt("id"), rs.getString("username"), rs.getString("nombre")))
      else None
    } finally { conn.close() }
  }

  def getUserById(id: Int): Option[AmUser] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("SELECT id, username, nombre FROM am_users WHERE id = ?")
      ps.setInt(1, id)
      val rs = ps.executeQuery()
      if (rs.next()) Some(AmUser(rs.getInt("id"), rs.getString("username"), rs.getString("nombre")))
      else None
    } finally { conn.close() }
  }

  def listUsers(): List[AmUser] = {
    val conn = getConn()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, username, nombre FROM am_users ORDER BY nombre ASC"
      )
      var list = List[AmUser]()
      while (rs.next())
        list = list :+ AmUser(rs.getInt("id"), rs.getString("username"), rs.getString("nombre"))
      list
    } finally { conn.close() }
  }

  def checkPassword(userId: Int, password: String): Boolean = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT 1 FROM am_users WHERE id = ? AND password_hash = ?"
      )
      ps.setInt(1, userId)
      ps.setString(2, md5(password))
      ps.executeQuery().next()
    } finally { conn.close() }
  }

  // ── MATCHES ────────────────────────────────────────────────────────────────
  def logMatch(
                userId: Int, rival: String, gf: Int, gc: Int,
                nota: Double, clima: String, estadio: String,
                esLocal: Option[Boolean], fecha: String,
                videoUrl: String, notas: String,
                posicionPartido: String = "portero", posicionCampo: String = "",
                golesMarcados: Int = 0, asistencias: Int = 0
              ): Int = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_matches
          (user_id, rival, goles_favor, goles_contra, nota, clima, estadio,
           es_local, fecha, video_url, notas,
           posicion_partido, posicion_campo, goles_marcados, asistencias,
           current_season_num)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
          COALESCE((SELECT current_season FROM am_users WHERE id = ?), 1))
        RETURNING id
      """)
      ps.setInt(1, userId)
      ps.setString(2, fix(rival))
      ps.setInt(3, gf)
      ps.setInt(4, gc)
      ps.setDouble(5, nota)
      ps.setString(6, clima)
      ps.setString(7, fix(estadio))
      esLocal match {
        case Some(v) => ps.setBoolean(8, v)
        case None    => ps.setNull(8, java.sql.Types.BOOLEAN)
      }
      ps.setDate(9, Date.valueOf(if (fecha.nonEmpty) fecha else LocalDate.now().toString))
      ps.setString(10, videoUrl)
      ps.setString(11, fix(notas))
      ps.setString(12, posicionPartido)
      ps.setString(13, posicionCampo)
      ps.setInt(14, golesMarcados)
      ps.setInt(15, asistencias)
      ps.setInt(16, userId)
      val rs = ps.executeQuery()
      val newId = if (rs.next()) rs.getInt(1) else -1
      // Invalida la caché IA para que los insights se recalculen con el nuevo partido
      if (newId > 0) invalidateAiCache(userId)
      newId
    } finally { conn.close() }
  }

  def getMatches(userId: Int): List[AmMatch] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM am_matches WHERE user_id = ? ORDER BY fecha DESC, created_at DESC"
      )  // posicion_partido, posicion_campo, goles_marcados, asistencias leídos vía mapRow
      ps.setInt(1, userId)
      val rs = ps.executeQuery()
      var list = List[AmMatch]()
      while (rs.next()) {
        val esLocalRaw = rs.getBoolean("es_local")
        val esLocalOpt = if (rs.wasNull()) None else Some(esLocalRaw)
        list = list :+ AmMatch(
          rs.getInt("id"), rs.getString("rival"),
          rs.getInt("goles_favor"), rs.getInt("goles_contra"),
          rs.getDouble("nota"),
          Option(rs.getString("clima")).getOrElse("Sol"),
          Option(rs.getString("estadio")).getOrElse(""),
          esLocalOpt,
          rs.getDate("fecha").toString,
          Option(rs.getString("video_url")).getOrElse(""),
          Option(rs.getString("notas")).getOrElse(""),
          Option(rs.getString("analisis_voz")).getOrElse(""),
          Option(rs.getString("posicion_partido")).getOrElse("portero"),
          Option(rs.getString("posicion_campo")).getOrElse(""),
          rs.getInt("goles_marcados"),
          rs.getInt("asistencias")
        )
      }
      list
    } finally { conn.close() }
  }

  def getMatch(userId: Int, matchId: Int): Option[AmMatch] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM am_matches WHERE id = ? AND user_id = ?"
      )
      ps.setInt(1, matchId)
      ps.setInt(2, userId)
      val rs = ps.executeQuery()
      if (rs.next()) {
        val esLocalRaw = rs.getBoolean("es_local")
        val esLocalOpt = if (rs.wasNull()) None else Some(esLocalRaw)
        Some(AmMatch(
          rs.getInt("id"), rs.getString("rival"),
          rs.getInt("goles_favor"), rs.getInt("goles_contra"),
          rs.getDouble("nota"),
          Option(rs.getString("clima")).getOrElse("Sol"),
          Option(rs.getString("estadio")).getOrElse(""),
          esLocalOpt,
          rs.getDate("fecha").toString,
          Option(rs.getString("video_url")).getOrElse(""),
          Option(rs.getString("notas")).getOrElse(""),
          Option(rs.getString("analisis_voz")).getOrElse(""),
          Option(rs.getString("posicion_partido")).getOrElse("portero"),
          Option(rs.getString("posicion_campo")).getOrElse(""),
          rs.getInt("goles_marcados"),
          rs.getInt("asistencias")
        ))
      } else None
    } finally { conn.close() }
  }

  def saveGoal(matchId: Int, zona: String, situacion: String,
               errorDefensivo: Boolean, minuto: Int, notas: String): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_match_goals (match_id, zona, situacion, error_defensivo, minuto, notas)
        VALUES (?, ?, ?, ?, ?, ?)
      """)
      ps.setInt(1, matchId)
      ps.setString(2, zona)
      ps.setString(3, situacion)
      ps.setBoolean(4, errorDefensivo)
      ps.setInt(5, minuto)
      ps.setString(6, fix(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def deleteGoal(goalId: Int, userId: Int): Unit = {
    val conn = getConn()
    try {
      // Ensure goal belongs to a match owned by this user
      val ps = conn.prepareStatement("""
        DELETE FROM am_match_goals
        WHERE id = ?
        AND match_id IN (SELECT id FROM am_matches WHERE user_id = ?)
      """)
      ps.setInt(1, goalId)
      ps.setInt(2, userId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getGoals(matchId: Int): List[AmGoal] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM am_match_goals WHERE match_id = ? ORDER BY minuto ASC"
      )
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      var list = List[AmGoal]()
      while (rs.next()) {
        list = list :+ AmGoal(
          rs.getInt("id"), rs.getInt("match_id"),
          Option(rs.getString("zona")).getOrElse("MC"),
          Option(rs.getString("situacion")).getOrElse("Remate"),
          rs.getBoolean("error_defensivo"),
          rs.getInt("minuto"),
          Option(rs.getString("notas")).getOrElse("")
        )
      }
      list
    } finally { conn.close() }
  }

  def saveVoiceAnalysis(matchId: Int, analysis: String): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("UPDATE am_matches SET analisis_voz = ? WHERE id = ?")
      ps.setString(1, fix(analysis))
      ps.setInt(2, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ── PENALTIES ──────────────────────────────────────────────────────────────
  def savePenalty(
                   userId: Int, fecha: String, rival: String,
                   dirTiro: String, dirEstirada: String, parada: Boolean,
                   matchId: Option[Int], notas: String
                 ): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_penalties
          (user_id, fecha, rival, direccion_tiro, direccion_estirada, parada, match_id, notas)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      """)
      ps.setInt(1, userId)
      ps.setDate(2, Date.valueOf(if (fecha.nonEmpty) fecha else LocalDate.now().toString))
      ps.setString(3, fix(rival))
      ps.setString(4, dirTiro)
      ps.setString(5, dirEstirada)
      ps.setBoolean(6, parada)
      matchId match {
        case Some(id) => ps.setInt(7, id)
        case None     => ps.setNull(7, java.sql.Types.INTEGER)
      }
      ps.setString(8, fix(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def deletePenalty(penaltyId: Int, userId: Int): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("DELETE FROM am_penalties WHERE id = ? AND user_id = ?")
      ps.setInt(1, penaltyId)
      ps.setInt(2, userId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getPenalties(userId: Int): List[AmPenalty] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM am_penalties WHERE user_id = ? ORDER BY fecha DESC, id DESC"
      )
      ps.setInt(1, userId)
      val rs = ps.executeQuery()
      var list = List[AmPenalty]()
      while (rs.next()) {
        val mId = rs.getInt("match_id")
        val mIdOpt = if (rs.wasNull()) None else Some(mId)
        list = list :+ AmPenalty(
          rs.getInt("id"), rs.getInt("user_id"),
          rs.getDate("fecha").toString,
          Option(rs.getString("rival")).getOrElse(""),
          rs.getString("direccion_tiro"),
          rs.getString("direccion_estirada"),
          rs.getBoolean("parada"),
          mIdOpt,
          Option(rs.getString("notas")).getOrElse("")
        )
      }
      list
    } finally { conn.close() }
  }

  // Devuelve Map con todas las estadísticas de penaltis
  def getPenaltyStats(userId: Int): Map[String, Any] = {
    val conn = getConn()
    try {
      val rs = conn.prepareStatement("""
        SELECT
          COUNT(*) as total,
          SUM(CASE WHEN parada THEN 1 ELSE 0 END) as paradas,
          SUM(CASE WHEN direccion_tiro = direccion_estirada THEN 1 ELSE 0 END) as adivinados,
          SUM(CASE WHEN direccion_tiro = 'Izquierda' THEN 1 ELSE 0 END) as tiros_izq,
          SUM(CASE WHEN direccion_tiro = 'Centro'    THEN 1 ELSE 0 END) as tiros_cen,
          SUM(CASE WHEN direccion_tiro = 'Derecha'   THEN 1 ELSE 0 END) as tiros_der,
          SUM(CASE WHEN direccion_estirada = 'Izquierda' THEN 1 ELSE 0 END) as est_izq,
          SUM(CASE WHEN direccion_estirada = 'Centro'    THEN 1 ELSE 0 END) as est_cen,
          SUM(CASE WHEN direccion_estirada = 'Derecha'   THEN 1 ELSE 0 END) as est_der,
          -- paradas con intuición correcta
          SUM(CASE WHEN parada AND direccion_tiro = direccion_estirada THEN 1 ELSE 0 END) as paradas_con_intuicion
        FROM am_penalties WHERE user_id = ?
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }

      if (rs.next()) {
        val total  = rs.getInt("total")
        val paradas = rs.getInt("paradas")
        val adiv   = rs.getInt("adivinados")
        Map(
          "total"              -> total,
          "paradas"            -> paradas,
          "adivinados"         -> adiv,
          "pctParada"          -> (if (total > 0) paradas * 100 / total else 0),
          "pctIntuicion"       -> (if (total > 0) adiv * 100 / total else 0),
          "paradasConIntuicion"-> rs.getInt("paradas_con_intuicion"),
          "tirIzq"             -> rs.getInt("tiros_izq"),
          "tirCen"             -> rs.getInt("tiros_cen"),
          "tirDer"             -> rs.getInt("tiros_der"),
          "estIzq"             -> rs.getInt("est_izq"),
          "estCen"             -> rs.getInt("est_cen"),
          "estDer"             -> rs.getInt("est_der")
        )
      } else Map("total" -> 0, "paradas" -> 0, "adivinados" -> 0, "pctParada" -> 0,
        "pctIntuicion" -> 0, "paradasConIntuicion" -> 0,
        "tirIzq" -> 0, "tirCen" -> 0, "tirDer" -> 0,
        "estIzq" -> 0, "estCen" -> 0, "estDer" -> 0)
    } finally { conn.close() }
  }

  // ── GEAR ───────────────────────────────────────────────────────────────────
  def saveGear(userId: Int, nombre: String, marca: String,
               tipoLatex: String, corte: String, notas: String): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_gear (user_id, nombre, marca, tipo_latex, corte, notas)
        VALUES (?, ?, ?, ?, ?, ?)
      """)
      ps.setInt(1, userId)
      ps.setString(2, fix(nombre))
      ps.setString(3, fix(marca))
      ps.setString(4, tipoLatex)
      ps.setString(5, corte)
      ps.setString(6, fix(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getGear(userId: Int): List[AmGearItem] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM am_gear WHERE user_id = ? ORDER BY activo DESC, id DESC"
      )
      ps.setInt(1, userId)
      val rs = ps.executeQuery()
      var list = List[AmGearItem]()
      while (rs.next()) {
        list = list :+ AmGearItem(
          rs.getInt("id"), rs.getInt("user_id"),
          rs.getString("nombre"),
          Option(rs.getString("marca")).getOrElse(""),
          Option(rs.getString("tipo_latex")).getOrElse(""),
          Option(rs.getString("corte")).getOrElse(""),
          rs.getInt("partidos_usados"),
          rs.getBoolean("activo"),
          Option(rs.getString("notas")).getOrElse("")
        )
      }
      list
    } finally { conn.close() }
  }

  def incrementGearUsage(gearId: Int, userId: Int): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "UPDATE am_gear SET partidos_usados = partidos_usados + 1 WHERE id = ? AND user_id = ?"
      )
      ps.setInt(1, gearId)
      ps.setInt(2, userId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def toggleGearActive(gearId: Int, userId: Int): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "UPDATE am_gear SET activo = NOT activo WHERE id = ? AND user_id = ?"
      )
      ps.setInt(1, gearId)
      ps.setInt(2, userId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ── DASHBOARD STATS ────────────────────────────────────────────────────────
  def getDashboardStats(userId: Int): Map[String, Any] = {
    val conn = getConn()
    try {
      val rs = conn.prepareStatement("""
        SELECT
          COUNT(*) as pj,
          COALESCE(AVG(nota), 0) as nota_media,
          COALESCE(AVG(goles_contra), 0) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) as ganados,
          SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) as empatados,
          SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) as perdidos,
          COALESCE(SUM(goles_contra), 0) as gc_total
        FROM am_matches WHERE user_id = ?
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }

      // Nota ajustada: penaltis marcados como error defensivo no penalizan
      val rsAdj = conn.prepareStatement("""
        SELECT
          m.id,
          m.nota,
          COUNT(g.id) FILTER (WHERE g.error_defensivo = TRUE) as errores_defensivos
        FROM am_matches m
        LEFT JOIN am_match_goals g ON g.match_id = m.id
        WHERE m.user_id = ?
        GROUP BY m.id, m.nota
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }

      var notaAjustadaTotal = 0.0
      var matchCount = 0
      while (rsAdj.next()) {
        val notaPartido = rsAdj.getDouble("nota")
        val errDef = rsAdj.getInt("errores_defensivos")
        // Cada gol de error defensivo suma +0.3 a la nota del partido (máx 10)
        val notaAdj = math.min(10.0, notaPartido + errDef * 0.3)
        notaAjustadaTotal += notaAdj
        matchCount += 1
      }
      val notaAjustada = if (matchCount > 0) notaAjustadaTotal / matchCount else 0.0

      // Racha actual
      val rsRacha = conn.prepareStatement("""
        SELECT goles_contra FROM am_matches WHERE user_id = ? ORDER BY fecha DESC, id DESC LIMIT 10
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      var rachaLimpias = 0
      var rachaRota = false
      while (rsRacha.next() && !rachaRota) {
        if (rsRacha.getInt("goles_contra") == 0) rachaLimpias += 1
        else rachaRota = true
      }

      // Últimos 5 partidos
      val rsLast = conn.prepareStatement("""
        SELECT rival, goles_favor, goles_contra, nota, fecha, posicion_partido
        FROM am_matches WHERE user_id = ? ORDER BY fecha DESC, id DESC LIMIT 5
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      var ultimos = List[Map[String, String]]()
      while (rsLast.next()) {
        ultimos = ultimos :+ Map(
          "rival"    -> rsLast.getString("rival"),
          "res"      -> s"${rsLast.getInt("goles_favor")}-${rsLast.getInt("goles_contra")}",
          "nota"     -> f"${rsLast.getDouble("nota")}%.1f",
          "fecha"    -> rsLast.getDate("fecha").toString,
          "posicion" -> Option(rsLast.getString("posicion_partido")).getOrElse("portero")
        )
      }

      if (rs.next()) Map(
        "pj"           -> rs.getInt("pj"),
        "notaMedia"    -> rs.getDouble("nota_media"),
        "notaAjustada" -> notaAjustada,
        "gcMedia"      -> rs.getDouble("gc_media"),
        "gcTotal"      -> rs.getInt("gc_total"),
        "limpias"      -> rs.getInt("limpias"),
        "ganados"      -> rs.getInt("ganados"),
        "empatados"    -> rs.getInt("empatados"),
        "perdidos"     -> rs.getInt("perdidos"),
        "rachaLimpias" -> rachaLimpias,
        "ultimos"      -> ultimos
      ) else Map(
        "pj" -> 0, "notaMedia" -> 0.0, "notaAjustada" -> 0.0,
        "gcMedia" -> 0.0, "gcTotal" -> 0, "limpias" -> 0,
        "ganados" -> 0, "empatados" -> 0, "perdidos" -> 0,
        "rachaLimpias" -> 0, "ultimos" -> List.empty
      )
    } finally { conn.close() }
  }


  // ── SCHEDULE / CALENDARIO ──────────────────────────────────────────────────

  def saveSchedule(userId: Int, rival: String, fecha: String, hora: String,
                   lugar: String, tipo: String, notas: String): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_schedule (user_id, rival, fecha, hora, lugar, tipo, notas)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      """)
      ps.setInt(1, userId)
      ps.setString(2, fix(rival))
      ps.setDate(3, Date.valueOf(fecha))
      ps.setString(4, hora)
      ps.setString(5, fix(lugar))
      ps.setString(6, tipo)
      ps.setString(7, fix(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def deleteSchedule(scheduleId: Int, userId: Int): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("DELETE FROM am_schedule WHERE id = ? AND user_id = ?")
      ps.setInt(1, scheduleId)
      ps.setInt(2, userId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getScheduleRange(userId: Int, fromDate: String, toDate: String): List[AmSchedule] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        SELECT * FROM am_schedule
        WHERE user_id = ? AND fecha BETWEEN ? AND ?
        ORDER BY fecha ASC, hora ASC
      """)
      ps.setInt(1, userId)
      ps.setDate(2, Date.valueOf(fromDate))
      ps.setDate(3, Date.valueOf(toDate))
      val rs = ps.executeQuery()
      var list = List[AmSchedule]()
      while (rs.next()) {
        val mId = rs.getInt("match_id")
        list = list :+ AmSchedule(
          rs.getInt("id"), rs.getInt("user_id"),
          rs.getString("rival"),
          rs.getDate("fecha").toString,
          Option(rs.getString("hora")).getOrElse(""),
          Option(rs.getString("lugar")).getOrElse(""),
          Option(rs.getString("tipo")).getOrElse("LIGA"),
          Option(rs.getString("notas")).getOrElse(""),
          if (rs.wasNull()) None else Some(mId)
        )
      }
      list
    } finally { conn.close() }
  }

  def getUpcomingSchedule(userId: Int, limit: Int = 3): List[AmSchedule] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        SELECT * FROM am_schedule
        WHERE user_id = ? AND fecha >= CURRENT_DATE AND match_id IS NULL
        ORDER BY fecha ASC, hora ASC
        LIMIT ?
      """)
      ps.setInt(1, userId)
      ps.setInt(2, limit)
      val rs = ps.executeQuery()
      var list = List[AmSchedule]()
      while (rs.next()) {
        list = list :+ AmSchedule(
          rs.getInt("id"), rs.getInt("user_id"),
          rs.getString("rival"),
          rs.getDate("fecha").toString,
          Option(rs.getString("hora")).getOrElse(""),
          Option(rs.getString("lugar")).getOrElse(""),
          Option(rs.getString("tipo")).getOrElse("LIGA"),
          Option(rs.getString("notas")).getOrElse(""),
          None
        )
      }
      list
    } finally { conn.close() }
  }

  // ── DATOS PARA INFORME PDF ────────────────────────────────────────────────
  def getSeasonInfo(userId: Int): Map[String, Any] = {
    val conn = getConn()
    try {
      // Current season number
      val ps1 = conn.prepareStatement("SELECT COALESCE(current_season, 1) AS cs FROM am_users WHERE id = ?")
      ps1.setInt(1, userId)
      val rs1 = ps1.executeQuery()
      val currentSeason = if (rs1.next()) rs1.getInt("cs") else 1

      // Stats for current season matches
      val ps2 = conn.prepareStatement("""
        SELECT COUNT(*) AS pj,
               SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) AS ganados,
               SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) AS empatados,
               SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) AS perdidos,
               ROUND(AVG(nota)::numeric, 1) AS nota_media
        FROM am_matches
        WHERE user_id = ? AND current_season_num = ?
      """)
      ps2.setInt(1, userId)
      ps2.setInt(2, currentSeason)
      val rs2 = ps2.executeQuery()
      val (pj, g, e, p, nm) = if (rs2.next())
        (rs2.getInt("pj"), rs2.getInt("ganados"), rs2.getInt("empatados"),
          rs2.getInt("perdidos"), rs2.getDouble("nota_media"))
      else (0, 0, 0, 0, 0.0)

      // Past seasons
      val ps3 = conn.prepareStatement(
        "SELECT * FROM am_seasons WHERE user_id = ? ORDER BY season_num DESC LIMIT 5")
      ps3.setInt(1, userId)
      val rs3 = ps3.executeQuery()
      var seasons = List[Map[String, String]]()
      while (rs3.next()) {
        seasons = seasons :+ Map(
          "num"   -> rs3.getInt("season_num").toString,
          "pj"    -> rs3.getInt("pj").toString,
          "g"     -> rs3.getInt("ganados").toString,
          "e"     -> rs3.getInt("empatados").toString,
          "p"     -> rs3.getInt("perdidos").toString,
          "nota"  -> f"${rs3.getDouble("nota_media")}%.1f",
          "ended" -> rs3.getString("ended_at").take(10)
        )
      }

      Map(
        "currentSeason" -> currentSeason,
        "pj"            -> pj,
        "ganados"       -> g,
        "empatados"     -> e,
        "perdidos"      -> p,
        "notaMedia"     -> nm,
        "pastSeasons"   -> seasons
      )
    } finally { conn.close() }
  }

  def endSeason(userId: Int): Int = {
    val conn = getConn()
    try {
      // Get current season number
      val ps1 = conn.prepareStatement("SELECT COALESCE(current_season, 1) AS cs FROM am_users WHERE id = ?")
      ps1.setInt(1, userId)
      val rs1 = ps1.executeQuery()
      val currentSeason = if (rs1.next()) rs1.getInt("cs") else 1

      // Compute season stats
      val ps2 = conn.prepareStatement("""
        SELECT COUNT(*) AS pj,
               SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) AS g,
               SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) AS e,
               SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) AS p,
               COALESCE(ROUND(AVG(nota)::numeric,1), 0) AS nm,
               COALESCE(ROUND(AVG(goles_contra)::numeric,2), 0) AS gcm,
               SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) AS lim
        FROM am_matches WHERE user_id = ? AND current_season_num = ?
      """)
      ps2.setInt(1, userId)
      ps2.setInt(2, currentSeason)
      val rs2 = ps2.executeQuery()
      val (pj, g, e, p, nm, gcm, lim) = if (rs2.next())
        (rs2.getInt("pj"), rs2.getInt("g"), rs2.getInt("e"), rs2.getInt("p"),
          rs2.getDouble("nm"), rs2.getDouble("gcm"), rs2.getInt("lim"))
      else (0, 0, 0, 0, 0.0, 0.0, 0)

      // Archive season
      val ps3 = conn.prepareStatement("""
        INSERT INTO am_seasons (user_id, season_num, pj, ganados, empatados, perdidos, nota_media, gc_media, limpias)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      """)
      ps3.setInt(1, userId); ps3.setInt(2, currentSeason)
      ps3.setInt(3, pj); ps3.setInt(4, g); ps3.setInt(5, e); ps3.setInt(6, p)
      ps3.setDouble(7, nm); ps3.setDouble(8, gcm); ps3.setInt(9, lim)
      ps3.executeUpdate()

      // Bump season counter on user
      val newSeason = currentSeason + 1
      val ps4 = conn.prepareStatement("UPDATE am_users SET current_season = ? WHERE id = ?")
      ps4.setInt(1, newSeason)
      ps4.setInt(2, userId)
      ps4.executeUpdate()

      newSeason
    } finally { conn.close() }
  }

  def getGoalHeatmap(userId: Int, tipo: String = ""): Map[String, Int] = {
    val conn = getConn()
    try {
      val sql = if (tipo.nonEmpty) """
        SELECT g.zona, COUNT(*) as cnt
        FROM am_match_goals g
        JOIN am_matches m ON m.id = g.match_id
        JOIN am_schedule s ON s.match_id = m.id
        WHERE m.user_id = ? AND LOWER(s.tipo) = LOWER(?)
          AND m.posicion_partido = 'portero'
        GROUP BY g.zona
      """ else """
        SELECT g.zona, COUNT(*) as cnt
        FROM am_match_goals g
        JOIN am_matches m ON m.id = g.match_id
        WHERE m.user_id = ? AND m.posicion_partido = 'portero'
        GROUP BY g.zona
      """
      val ps = conn.prepareStatement(sql)
      ps.setInt(1, userId)
      if (tipo.nonEmpty) ps.setString(2, tipo)
      val rs = ps.executeQuery()
      var map = Map[String, Int]()
      while (rs.next()) {
        map = map + (rs.getString("zona") -> rs.getInt("cnt"))
      }
      // Ensure all 9 zones present
      val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
      zones.foreach(z => if (!map.contains(z)) map = map + (z -> 0))
      map
    } finally { conn.close() }
  }

  def getGoalHeatmapByRival(userId: Int, rival: String): Map[String, Int] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        SELECT g.zona, COUNT(*) as cnt
        FROM am_match_goals g
        JOIN am_matches m ON m.id = g.match_id
        WHERE m.user_id = ? AND LOWER(m.rival) LIKE LOWER(?)
          AND m.posicion_partido = 'portero'
        GROUP BY g.zona
      """)
      ps.setInt(1, userId)
      ps.setString(2, s"%$rival%")
      val rs = ps.executeQuery()
      var map = Map[String, Int]()
      while (rs.next()) {
        map = map + (rs.getString("zona") -> rs.getInt("cnt"))
      }
      val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
      zones.foreach(z => if (!map.contains(z)) map = map + (z -> 0))
      map
    } finally { conn.close() }
  }

  def getRivalesConGoles(userId: Int): List[String] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        SELECT DISTINCT m.rival
        FROM am_match_goals g
        JOIN am_matches m ON m.id = g.match_id
        WHERE m.user_id = ? AND m.posicion_partido = 'portero'
        ORDER BY m.rival
      """)
      ps.setInt(1, userId)
      val rs = ps.executeQuery()
      var list = List[String]()
      while (rs.next()) list = list :+ rs.getString("rival")
      list
    } finally { conn.close() }
  }

  // ── WELLNESS ───────────────────────────────────────────────────────────────
  def saveWellness(userId: Int, fecha: String, sueno: Int, energia: Int, animo: Int, notas: String): Unit = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_wellness (user_id, fecha, sueno, energia, animo, notas)
        VALUES (?, ?, ?, ?, ?, ?)
        ON CONFLICT (user_id, fecha) DO UPDATE
          SET sueno = EXCLUDED.sueno, energia = EXCLUDED.energia,
              animo = EXCLUDED.animo, notas = EXCLUDED.notas
      """)
      ps.setInt(1, userId); ps.setString(2, fecha)
      ps.setInt(3, sueno); ps.setInt(4, energia); ps.setInt(5, animo)
      ps.setString(6, fix(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getWellnessCorrelation(userId: Int): Map[String, Any] = {
    val conn = getConn()
    try {
      // Join wellness with match of same date
      val ps = conn.prepareStatement("""
        SELECT w.fecha, w.sueno, w.energia, w.animo,
               m.nota, m.goles_contra, m.goles_favor
        FROM am_wellness w
        JOIN am_matches m ON m.user_id = w.user_id AND m.fecha = w.fecha::date
        WHERE w.user_id = ?
        ORDER BY w.fecha DESC
        LIMIT 20
      """)
      ps.setInt(1, userId)
      val rs = ps.executeQuery()
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "fecha"   -> rs.getString("fecha").take(10),
          "sueno"   -> rs.getInt("sueno"),
          "energia" -> rs.getInt("energia"),
          "animo"   -> rs.getInt("animo"),
          "nota"    -> rs.getDouble("nota"),
          "gc"      -> rs.getInt("goles_contra")
        )
      }

      // Last wellness entry (for the check-in status indicator)
      val ps2 = conn.prepareStatement(
        "SELECT * FROM am_wellness WHERE user_id = ? ORDER BY fecha DESC LIMIT 1")
      ps2.setInt(1, userId)
      val rs2 = ps2.executeQuery()
      val lastWellness: Option[Map[String, Any]] = if (rs2.next()) Some(Map(
        "fecha"   -> rs2.getString("fecha").take(10),
        "sueno"   -> rs2.getInt("sueno"),
        "energia" -> rs2.getInt("energia"),
        "animo"   -> rs2.getInt("animo"),
        "notas"   -> Option(rs2.getString("notas")).getOrElse("")
      )) else None

      // Averages by sleep bucket
      val highSleep = rows.filter(_("sueno").asInstanceOf[Int] >= 7)
      val lowSleep  = rows.filter(_("sueno").asInstanceOf[Int] < 7)
      val avgNotaHigh = if (highSleep.nonEmpty) highSleep.map(_("nota").asInstanceOf[Double]).sum / highSleep.size else 0.0
      val avgNotaLow  = if (lowSleep.nonEmpty)  lowSleep.map(_("nota").asInstanceOf[Double]).sum  / lowSleep.size  else 0.0

      Map(
        "rows"         -> rows,
        "lastWellness" -> lastWellness,
        "avgNotaHigh"  -> avgNotaHigh,
        "avgNotaLow"   -> avgNotaLow,
        "nHighSleep"   -> highSleep.size,
        "nLowSleep"    -> lowSleep.size
      )
    } finally { conn.close() }
  }

  def callGeminiWellness(data: List[Map[String, Any]]): String = {
    if (data.isEmpty) return ""
    val rows = data.take(15).map { r =>
      s"Fecha:${r("fecha")} Sueño:${r("sueno")}h Energía:${r("energia")}/5 Ánimo:${r("animo")}/5 Nota:${r("nota")} GC:${r("gc")}"
    }.mkString("\n")
    val prompt = s"""Eres un analista de rendimiento deportivo amateur. Analiza estos datos de bienestar y rendimiento de un portero:

$rows

Responde en español con exactamente 3 insights cortos (máximo 15 palabras cada uno) sobre patrones detectados entre el bienestar (sueño, energía, ánimo) y el rendimiento (nota, goles encajados). Formato: una línea por insight, sin numeración, sin guiones."""

    // Cache: el prompt incluye los datos reales → hash cambia automáticamente cuando hay nuevos partidos
    askCached(prompt)
  }

  def processCalendarNLP(userId: Int, texto: String, teamName: String): Map[String, Any] = {
    val apiKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim
    if (apiKey.isEmpty) return Map("ok" -> false, "error" -> "GEMINI_API_KEY no configurada")

    val prompt = s"""Actúa como un analista de datos deportivo para el equipo $teamName. Tu misión es procesar el siguiente texto pegado de una web de liga y extraer información exclusiva para el perfil de portero de $teamName.

Tu equipo: $teamName.

Tareas de extracción:
1. Filtro de Partidos: Busca únicamente las líneas que mencionen a '$teamName'. Extrae el Rival, la Fecha, la Hora y el Marcador (si existe).
2. Lógica Local/Visitante: Si $teamName aparece a la izquierda del marcador o primero en la línea, marca es_local: true. Si aparece a la derecha, marca es_local: false.
3. Inteligencia de Rivales: Identifica el próximo rival de $teamName en el calendario. Si encuentras tabla de goleadores o estadísticas de jugadores en el texto, extrae los nombres de los delanteros más peligrosos de ese rival.

Texto a procesar:
$texto

Salida obligatoria: Devuelve ÚNICAMENTE un JSON válido, sin texto adicional, sin bloques de código, sin explicaciones. El formato exacto debe ser:
{"partidos":[{"rival":"nombre","fecha":"YYYY-MM-DD","hora":"HH:MM","es_local":true,"marcador_favor":0,"marcador_contra":0,"tipo":"LIGA"}],"amenazas_rival":["nombre1","nombre2"],"proximo_rival":"nombre"}

Si no encuentras fecha en formato claro, usa null. Si no hay marcador, usa 0. Si no hay hora, usa "". Devuelve solo el JSON."""

    try {
      // NLP calendar — bypass cache (texto siempre distinto, no tiene sentido cachear)
      val payload = ujson.Obj("contents" -> ujson.Arr(ujson.Obj(
        "parts" -> ujson.Arr(ujson.Obj("text" -> prompt)))))
      val r = requests.post(
        s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=$apiKey",
        data = ujson.write(payload),
        headers = Map("Content-Type" -> "application/json"),
        readTimeout = 45000
      )
      if (r.statusCode != 200) return Map("ok" -> false, "error" -> s"Gemini error ${r.statusCode}")

      val raw = ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str.trim
      // Strip possible markdown code blocks
      val clean = raw
        .replaceAll("(?s)```json\\s*", "")
        .replaceAll("(?s)```\\s*", "")
        .trim

      val json     = ujson.read(clean)
      val partidos = json("partidos").arr.toList
      var inserted = 0
      var skipped  = 0

      partidos.foreach { p =>
        try {
          val rival    = p("rival").str
          val fecha    = if (p("fecha").isNull) null else p("fecha").str
          val hora     = try p("hora").str catch { case _: Exception => "" }
          val esLocal  = try p("es_local").bool.toString catch { case _: Exception => "" }
          val mf       = try p("marcador_favor").num.toInt catch { case _: Exception => 0 }
          val mc       = try p("marcador_contra").num.toInt catch { case _: Exception => 0 }
          val tipo     = try p("tipo").str catch { case _: Exception => "LIGA" }

          if (rival.nonEmpty && fecha != null) {
            val conn = getConn()
            try {
              // Check if already exists
              val check = conn.prepareStatement(
                "SELECT id FROM am_schedule WHERE user_id = ? AND rival = ? AND fecha = ?::date")
              check.setInt(1, userId); check.setString(2, rival); check.setString(3, fecha)
              val rs = check.executeQuery()
              if (!rs.next()) {
                val ps = conn.prepareStatement("""
                  INSERT INTO am_schedule (user_id, rival, fecha, hora, tipo, notas)
                  VALUES (?, ?, ?::date, ?, ?, ?)
                """)
                ps.setInt(1, userId); ps.setString(2, rival); ps.setString(3, fecha)
                ps.setString(4, hora); ps.setString(5, tipo)
                val nota = if (esLocal == "true") "Local" else if (esLocal == "false") "Visitante" else ""
                ps.setString(6, nota)
                ps.executeUpdate()
                inserted += 1
              } else skipped += 1
            } finally { conn.close() }
          }
        } catch { case e: Exception => println(s"Skip partido: ${e.getMessage}") }
      }

      val amenazas = try json("amenazas_rival").arr.map(_.str).toList
      catch { case _: Exception => List.empty[String] }
      val proximo  = try json("proximo_rival").str catch { case _: Exception => "" }

      Map(
        "ok"        -> true,
        "inserted"  -> inserted,
        "skipped"   -> skipped,
        "total"     -> partidos.size,
        "amenazas"  -> amenazas,
        "proximo"   -> proximo
      )
    } catch { case e: Exception =>
      Map("ok" -> false, "error" -> e.getMessage)
    }
  }

  def getRivalesList(userId: Int): List[Map[String, String]] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        SELECT
          rival,
          COUNT(*) AS pj,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) AS g,
          SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) AS e,
          SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) AS p,
          ROUND(AVG(nota)::numeric, 1) AS nota_media,
          SUM(goles_contra) AS gc_total,
          MAX(fecha) AS ultimo
        FROM am_matches
        WHERE user_id = ?
        GROUP BY rival
        ORDER BY pj DESC, rival ASC
      """)
      ps.setInt(1, userId)
      val rs = ps.executeQuery()
      var list = List[Map[String, String]]()
      while (rs.next()) {
        val pj = rs.getInt("pj")
        val g  = rs.getInt("g")
        val e  = rs.getInt("e")
        val p  = rs.getInt("p")
        val resultado = if (g > p) "W" else if (g < p) "L" else "D"
        list = list :+ Map(
          "rival"      -> rs.getString("rival"),
          "pj"         -> pj.toString,
          "g"          -> g.toString,
          "e"          -> e.toString,
          "p"          -> p.toString,
          "nota"       -> f"${rs.getDouble("nota_media")}%.1f",
          "gc"         -> rs.getInt("gc_total").toString,
          "ultimo"     -> Option(rs.getDate("ultimo")).map(_.toString).getOrElse(""),
          "resultado"  -> resultado
        )
      }
      list
    } finally { conn.close() }
  }

  def getRivalDetail(userId: Int, rival: String): Map[String, Any] = {
    val conn = getConn()
    try {
      // Aggregate KPIs
      val ps1 = conn.prepareStatement("""
        SELECT
          COUNT(*) AS pj,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) AS g,
          SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) AS e,
          SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) AS p,
          ROUND(AVG(nota)::numeric, 1) AS nota_media,
          ROUND(AVG(goles_contra)::numeric, 2) AS gc_media,
          SUM(goles_marcados) AS goles_marcados_total,
          SUM(asistencias) AS asistencias_total,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) AS limpias
        FROM am_matches
        WHERE user_id = ? AND LOWER(rival) = LOWER(?)
      """)
      ps1.setInt(1, userId); ps1.setString(2, rival)
      val rs1 = ps1.executeQuery()
      val (pj, g, e, p, nota, gcMedia, gmTotal, aTotal, limpias) =
        if (rs1.next()) (
          rs1.getInt("pj"), rs1.getInt("g"), rs1.getInt("e"), rs1.getInt("p"),
          rs1.getDouble("nota_media"), rs1.getDouble("gc_media"),
          rs1.getInt("goles_marcados_total"), rs1.getInt("asistencias_total"),
          rs1.getInt("limpias")
        ) else (0, 0, 0, 0, 0.0, 0.0, 0, 0, 0)

      // Match history with notes
      val ps2 = conn.prepareStatement("""
        SELECT fecha, goles_favor, goles_contra, nota, posicion_partido,
               posicion_campo, goles_marcados, asistencias, notas, es_local
        FROM am_matches
        WHERE user_id = ? AND LOWER(rival) = LOWER(?)
        ORDER BY fecha DESC
      """)
      ps2.setInt(1, userId); ps2.setString(2, rival)
      val rs2 = ps2.executeQuery()
      var partidos = List[Map[String, String]]()
      while (rs2.next()) {
        val gf  = rs2.getInt("goles_favor")
        val gc  = rs2.getInt("goles_contra")
        val res = if (gf > gc) "G" else if (gf < gc) "P" else "E"
        val esLocalRaw = rs2.getBoolean("es_local")
        val loc = if (rs2.wasNull()) "—" else if (esLocalRaw) "Local" else "Visitante"
        partidos = partidos :+ Map(
          "fecha"    -> rs2.getDate("fecha").toString,
          "res"      -> res,
          "score"    -> s"$gf-$gc",
          "nota"     -> f"${rs2.getDouble("nota")}%.1f",
          "posicion" -> Option(rs2.getString("posicion_partido")).getOrElse("portero"),
          "campo"    -> Option(rs2.getString("posicion_campo")).getOrElse(""),
          "gm"       -> rs2.getInt("goles_marcados").toString,
          "ast"      -> rs2.getInt("asistencias").toString,
          "notas"    -> Option(rs2.getString("notas")).getOrElse(""),
          "local"    -> loc
        )
      }

      // Collect all tactical notes (non-empty)
      val notasTacticas = partidos
        .filter(_("notas").trim.nonEmpty)
        .map(m => s"${m("fecha").take(7)}: ${m("notas")}")

      Map(
        "rival"         -> rival,
        "pj"            -> pj,
        "g"             -> g,
        "e"             -> e,
        "p"             -> p,
        "notaMedia"     -> nota,
        "gcMedia"       -> gcMedia,
        "gmTotal"       -> gmTotal,
        "aTotal"        -> aTotal,
        "limpias"       -> limpias,
        "partidos"      -> partidos,
        "notasTacticas" -> notasTacticas
      )
    } finally { conn.close() }
  }

  def getReportData(userId: Int): Map[String, Any] = {
    val conn = getConn()
    try {
      // Stats globales separadas por rol
      val rsGlobal = conn.prepareStatement("""
        SELECT
          COUNT(*) as pj,
          COUNT(*) FILTER (WHERE posicion_partido = 'portero') as pj_portero,
          COUNT(*) FILTER (WHERE posicion_partido = 'jugador') as pj_jugador,
          COALESCE(AVG(nota), 0) as nota_media,
          COALESCE(AVG(nota) FILTER (WHERE posicion_partido = 'portero'), 0) as nota_portero,
          COALESCE(AVG(nota) FILTER (WHERE posicion_partido = 'jugador'), 0) as nota_jugador,
          COALESCE(AVG(goles_contra), 0) as gc_media,
          SUM(CASE WHEN goles_contra = 0 AND posicion_partido = 'portero' THEN 1 ELSE 0 END) as limpias,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) as ganados,
          SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) as empatados,
          SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) as perdidos,
          COALESCE(SUM(goles_marcados), 0) as goles_marcados_total,
          COALESCE(SUM(asistencias), 0) as asistencias_total
        FROM am_matches WHERE user_id = ?
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }

      val statsMap = if (rsGlobal.next()) Map(
        "pj"           -> rsGlobal.getInt("pj"),
        "pjPortero"    -> rsGlobal.getInt("pj_portero"),
        "pjJugador"    -> rsGlobal.getInt("pj_jugador"),
        "notaMedia"    -> rsGlobal.getDouble("nota_media"),
        "notaPortero"  -> rsGlobal.getDouble("nota_portero"),
        "notaJugador"  -> rsGlobal.getDouble("nota_jugador"),
        "gcMedia"      -> rsGlobal.getDouble("gc_media"),
        "limpias"      -> rsGlobal.getInt("limpias"),
        "ganados"      -> rsGlobal.getInt("ganados"),
        "empatados"    -> rsGlobal.getInt("empatados"),
        "perdidos"     -> rsGlobal.getInt("perdidos"),
        "golesMarcados"-> rsGlobal.getInt("goles_marcados_total"),
        "asistencias"  -> rsGlobal.getInt("asistencias_total")
      ) else Map.empty[String, Any]

      // Historial últimos 20 partidos
      val rsH = conn.prepareStatement("""
        SELECT rival, goles_favor, goles_contra, nota, fecha,
               posicion_partido, posicion_campo, goles_marcados, asistencias
        FROM am_matches WHERE user_id = ?
        ORDER BY fecha DESC, id DESC LIMIT 20
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      var historial = List[Map[String, String]]()
      while (rsH.next()) {
        val pos = Option(rsH.getString("posicion_partido")).getOrElse("portero")
        val posCampo = Option(rsH.getString("posicion_campo")).getOrElse("")
        val posLabel = if (pos == "jugador") s"Jugador${if (posCampo.nonEmpty) s" ($posCampo)" else ""}" else "Portero"
        historial = historial :+ Map(
          "rival"    -> rsH.getString("rival"),
          "res"      -> s"${rsH.getInt("goles_favor")}-${rsH.getInt("goles_contra")}",
          "nota"     -> f"${rsH.getDouble("nota")}%.1f",
          "fecha"    -> rsH.getDate("fecha").toString,
          "posicion" -> posLabel,
          "goles"    -> rsH.getInt("goles_marcados").toString,
          "asist"    -> rsH.getInt("asistencias").toString
        )
      }

      // Penaltis
      val rsPen = conn.prepareStatement("""
        SELECT COUNT(*) as total,
               SUM(CASE WHEN parada = TRUE THEN 1 ELSE 0 END) as paradas
        FROM am_penalties WHERE user_id = ?
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      val (totalPen, paradasPen) = if (rsPen.next()) {
        (rsPen.getInt("total"), rsPen.getInt("paradas"))
      } else (0, 0)

      // Próximo partido
      val rsNext = conn.prepareStatement("""
        SELECT rival, fecha, hora, lugar, tipo FROM am_schedule
        WHERE user_id = ? AND fecha >= CURRENT_DATE AND match_id IS NULL
        ORDER BY fecha ASC LIMIT 1
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      val nextMatch = if (rsNext.next()) Some(Map(
        "rival" -> rsNext.getString("rival"),
        "fecha" -> rsNext.getDate("fecha").toString,
        "hora"  -> Option(rsNext.getString("hora")).getOrElse(""),
        "lugar" -> Option(rsNext.getString("lugar")).getOrElse(""),
        "tipo"  -> Option(rsNext.getString("tipo")).getOrElse("LIGA")
      )) else None

      statsMap ++ Map(
        "historial"   -> historial,
        "totalPen"    -> totalPen,
        "paradasPen"  -> paradasPen,
        "nextMatch"   -> nextMatch
      )
    } finally { conn.close() }
  }

  // ── PROGRESION Y TENDENCIAS ───────────────────────────────────────────────
  def getProgressionData(userId: Int): Map[String, Any] = {
    val conn = getConn()
    try {
      // Todos los partidos cronológicos para gráfico de evolución
      val rsAll = conn.prepareStatement("""
        SELECT fecha, nota, goles_contra, goles_favor,
               CASE WHEN goles_favor > goles_contra THEN 'W'
                    WHEN goles_favor = goles_contra THEN 'D'
                    ELSE 'L' END as resultado
        FROM am_matches WHERE user_id = ?
        ORDER BY fecha ASC, id ASC
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }

      var labels    = List[String]()
      var notas     = List[Double]()
      var gcList    = List[Int]()
      var resultados = List[String]()
      var counter   = 1

      while (rsAll.next()) {
        labels     = labels     :+ s"P$counter"
        notas      = notas      :+ rsAll.getDouble("nota")
        gcList     = gcList     :+ rsAll.getInt("goles_contra")
        resultados = resultados :+ rsAll.getString("resultado")
        counter += 1
      }

      // Tendencia: comparar últimos 5 vs 5 anteriores (nota media)
      val tendencia = if (notas.size >= 6) {
        val last5 = notas.takeRight(5)
        val prev5 = notas.dropRight(5).takeRight(5)
        val avgLast = last5.sum / last5.size
        val avgPrev = prev5.sum / prev5.size
        val delta = avgLast - avgPrev
        if (delta > 0.3) "MEJORANDO"
        else if (delta < -0.3) "BAJANDO"
        else "ESTABLE"
      } else "POCOS_DATOS"

      val tendenciaDelta = if (notas.size >= 6) {
        val last5 = notas.takeRight(5)
        val prev5 = notas.dropRight(5).takeRight(5)
        last5.sum / last5.size - prev5.sum / prev5.size
      } else 0.0

      // Stats por mes
      val rsMes = conn.prepareStatement("""
        SELECT
          TO_CHAR(fecha, 'YYYY-MM') as mes,
          COUNT(*) as pj,
          ROUND(AVG(nota)::numeric, 1) as nota_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) as ganados
        FROM am_matches WHERE user_id = ?
        GROUP BY TO_CHAR(fecha, 'YYYY-MM')
        ORDER BY mes DESC LIMIT 6
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }

      var mesList = List[Map[String, Any]]()
      while (rsMes.next()) {
        mesList = mesList :+ Map(
          "mes"       -> rsMes.getString("mes"),
          "pj"        -> rsMes.getInt("pj"),
          "notaMedia" -> rsMes.getDouble("nota_media"),
          "limpias"   -> rsMes.getInt("limpias"),
          "ganados"   -> rsMes.getInt("ganados")
        )
      }

      // Mejor y peor actuación
      val rsBest = conn.prepareStatement("""
        SELECT rival, nota, fecha, goles_favor, goles_contra
        FROM am_matches WHERE user_id = ?
        ORDER BY nota DESC, id DESC LIMIT 1
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      val mejorPartido = if (rsBest.next()) Some(Map(
        "rival" -> rsBest.getString("rival"),
        "nota"  -> f"${rsBest.getDouble("nota")}%.1f",
        "fecha" -> rsBest.getDate("fecha").toString,
        "res"   -> s"${rsBest.getInt("goles_favor")}-${rsBest.getInt("goles_contra")}"
      )) else None

      val rsWorst = conn.prepareStatement("""
        SELECT rival, nota, fecha, goles_favor, goles_contra
        FROM am_matches WHERE user_id = ?
        ORDER BY nota ASC, id DESC LIMIT 1
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      val peorPartido = if (rsWorst.next()) Some(Map(
        "rival" -> rsWorst.getString("rival"),
        "nota"  -> f"${rsWorst.getDouble("nota")}%.1f",
        "fecha" -> rsWorst.getDate("fecha").toString,
        "res"   -> s"${rsWorst.getInt("goles_favor")}-${rsWorst.getInt("goles_contra")}"
      )) else None

      // Racha actual (W/D/L)
      val rsRacha = conn.prepareStatement("""
        SELECT goles_favor, goles_contra FROM am_matches
        WHERE user_id = ? ORDER BY fecha DESC, id DESC LIMIT 10
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      var racha = List[String]()
      while (rsRacha.next()) {
        val gf = rsRacha.getInt("goles_favor")
        val gc = rsRacha.getInt("goles_contra")
        racha = racha :+ (if (gf > gc) "W" else if (gf == gc) "D" else "L")
      }

      Map(
        "labels"         -> labels,
        "notas"          -> notas,
        "gcList"         -> gcList,
        "resultados"     -> resultados,
        "tendencia"      -> tendencia,
        "tendenciaDelta" -> tendenciaDelta,
        "mesList"        -> mesList,
        "mejorPartido"   -> mejorPartido,
        "peorPartido"    -> peorPartido,
        "racha"          -> racha,
        "totalPartidos"  -> notas.size
      )
    } finally { conn.close() }
  }

  // Extensión para PreparedStatement (sintaxis .also)
  implicit class PSExt(ps: java.sql.PreparedStatement) {
    def also(f: java.sql.PreparedStatement => java.sql.ResultSet): java.sql.ResultSet = f(ps)
  }
}