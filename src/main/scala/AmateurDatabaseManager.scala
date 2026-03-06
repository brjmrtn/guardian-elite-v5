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
  videoUrl: String, notas: String, analisisVoz: String
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
        id            SERIAL PRIMARY KEY,
        user_id       INT REFERENCES am_users(id) ON DELETE CASCADE,
        rival         TEXT NOT NULL,
        goles_favor   INT DEFAULT 0,
        goles_contra  INT DEFAULT 0,
        nota          DOUBLE PRECISION DEFAULT 5.0,
        clima         TEXT DEFAULT 'Sol',
        estadio       TEXT DEFAULT '',
        es_local      BOOLEAN DEFAULT NULL,
        fecha         DATE DEFAULT CURRENT_DATE,
        video_url     TEXT DEFAULT '',
        notas         TEXT DEFAULT '',
        analisis_voz  TEXT DEFAULT '',
        created_at    TIMESTAMP DEFAULT NOW()
      )""")

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
    videoUrl: String, notas: String
  ): Int = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO am_matches
          (user_id, rival, goles_favor, goles_contra, nota, clima, estadio,
           es_local, fecha, video_url, notas)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
      val rs = ps.executeQuery()
      if (rs.next()) rs.getInt(1) else -1
    } finally { conn.close() }
  }

  def getMatches(userId: Int): List[AmMatch] = {
    val conn = getConn()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM am_matches WHERE user_id = ? ORDER BY fecha DESC, created_at DESC"
      )
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
          Option(rs.getString("analisis_voz")).getOrElse("")
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
          Option(rs.getString("analisis_voz")).getOrElse("")
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
        SELECT rival, goles_favor, goles_contra, nota, fecha
        FROM am_matches WHERE user_id = ? ORDER BY fecha DESC, id DESC LIMIT 5
      """).also { ps => ps.setInt(1, userId); ps.executeQuery() }
      var ultimos = List[Map[String, String]]()
      while (rsLast.next()) {
        ultimos = ultimos :+ Map(
          "rival"  -> rsLast.getString("rival"),
          "res"    -> s"${rsLast.getInt("goles_favor")}-${rsLast.getInt("goles_contra")}",
          "nota"   -> f"${rsLast.getDouble("nota")}%.1f",
          "fecha"  -> rsLast.getDate("fecha").toString
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

  // Extensión para PreparedStatement (sintaxis .also)
  implicit class PSExt(ps: java.sql.PreparedStatement) {
    def also(f: java.sql.PreparedStatement => java.sql.ResultSet): java.sql.ResultSet = f(ps)
  }
}
