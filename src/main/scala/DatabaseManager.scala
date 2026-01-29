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
case class RivalInfo(nombre: String, estilo: String, claves: String, notas: String)
case class PenaltyStat(zona: String, total: Int, goles: Int)
case class RPGStatus(nivel: Int, xp: Int, nextLevelXp: Int, titulo: String, cinturonJudo: String)
case class TechReview(id: Int, fecha: String, blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String)

object DatabaseManager {
  val url = "jdbc:postgresql://ep-fancy-cherry-abkfneqp-pooler.eu-west-2.aws.neon.tech/neondb?user=neondb_owner&password=npg_5VxYysTm8vQa&sslmode=require&options=-c%20client_encoding=UTF8"

  def getConnection(): Connection = {
    val props = new Properties(); props.setProperty("user","neondb_owner"); props.setProperty("password","npg_5VxYysTm8vQa"); props.setProperty("ssl","true"); DriverManager.getConnection(url, props)
  }

  def fixEncoding(s: String): String = { try { if (s == null) "" else if (s.contains("Ã")) new String(s.getBytes("ISO-8859-1"), "UTF-8") else s } catch { case e: Exception => s } }
  def calcularEdadExacta(fechaStr: String): Int = { try { Period.between(LocalDate.parse(fechaStr), LocalDate.now()).getYears } catch { case _: Exception => 5 } }

  // --- NUEVO: SISTEMA DE AUDITORÍA TÉCNICA ---
  def saveTechnicalReview(blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String): Unit = {
    val conn = getConnection(); try {
      conn.createStatement().executeUpdate("CREATE TABLE IF NOT EXISTS technical_reviews (id SERIAL PRIMARY KEY, fecha DATE, blocaje INT, pies INT, aereo INT, valentia INT, concentracion INT, coordinacion INT, notas TEXT)")
      val ps = conn.prepareStatement("INSERT INTO technical_reviews (fecha, blocaje, pies, aereo, valentia, concentracion, coordinacion, notas) VALUES (?,?,?,?,?,?,?,?)")
      ps.setDate(1, Date.valueOf(LocalDate.now()))
      ps.setInt(2, blocaje); ps.setInt(3, pies); ps.setInt(4, aereo); ps.setInt(5, valentia); ps.setInt(6, concentracion); ps.setInt(7, coordinacion)
      ps.setString(8, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getTechnicalReviews(): List[TechReview] = {
    var l = List[TechReview](); val conn = getConnection(); try {
      conn.createStatement().executeUpdate("CREATE TABLE IF NOT EXISTS technical_reviews (id SERIAL PRIMARY KEY, fecha DATE, blocaje INT, pies INT, aereo INT, valentia INT, concentracion INT, coordinacion INT, notas TEXT)")
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
        { "label": "Valentía", "data": [$d2], "borderColor": "#dc3545", "tension": 0.3, "fill": false },
        { "label": "Concentración", "data": [$d3], "borderColor": "#ffc107", "tension": 0.3, "fill": false }
      ]
    }"""
  }

  // --- LEYENDAS Y COMPARATIVA (Mantenido) ---
  def initLegendsTable(): String = {
    val conn = getConnection(); try {
      val stmt = conn.createStatement(); stmt.executeUpdate("CREATE TABLE IF NOT EXISTS legends_milestones (id SERIAL PRIMARY KEY, nombre TEXT, edad INT, hito TEXT)")
      stmt.executeUpdate("DELETE FROM legends_milestones")
      val ps = conn.prepareStatement("INSERT INTO legends_milestones (nombre, edad, hito) VALUES (?,?,?)")
      val data = Seq(("Marc-André ter Stegen", 5, "Jugaba de DELANTERO. No se puso de portero hasta los 10 años."), ("Thibaut Courtois", 5, "Su deporte principal era el VOLEIBOL."), ("Iker Casillas", 6, "Jugaba en el patio del colegio El Recuerdo sobre cemento."), ("Gianluigi Buffon", 6, "Jugaba de centrocampista. Le gustaba correr y marcar goles."), ("Manuel Neuer", 5, "Llevaba un osito de peluche a la portería."))
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
      val legendHtml = if (rsLegend.next()) s"<div class='mb-3'><h6 class='text-warning text-uppercase mb-1'>A TU EDAD ($edad AÑOS)...</h6><h4 class='text-white fw-bold mb-1'>${rsLegend.getString("nombre")}</h4><p class='text-light small fst-italic'>\"${rsLegend.getString("hito")}\"</p></div>" else ""
      val diff = mediaLiga - miMedia; val color = if(diff >= 0) "text-success" else "text-danger"
      f"""<div class="card bg-secondary bg-opacity-10 border-warning shadow mb-4"><div class="card-header bg-dark text-warning fw-bold text-center small">CONTEXTO & LEYENDAS</div><div class="card-body">$legendHtml<hr class="border-secondary"><h6 class="text-info text-uppercase text-center mb-2 small fw-bold">COMPARATIVA RFFM</h6><div class="row text-center align-items-center"><div class="col-6 border-end border-secondary"><div class="small text-muted fw-bold">TU MEDIA</div><div class="display-6 fw-bold $color">${f"$miMedia%1.1f"}</div></div><div class="col-6"><div class="small text-muted fw-bold">MEDIA LIGA</div><div class="display-6 fw-bold text-white">${f"$mediaLiga%1.1f"}</div></div></div></div></div>"""
    } catch { case e: Exception => "" } finally { conn.close() }
  }

  // --- IA CONFIG ---
  val modelList = Seq("gemini-1.5-flash", "gemini-1.5-pro", "gemini-flash-latest")
  def callGeminiAI(prompt: String): String = { val envKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim; if (envKey.isEmpty) return "⚠️ Error Config: Falta GEMINI_API_KEY"; attemptNextModel(prompt, envKey, 0) }
  def attemptNextModel(prompt: String, apiKey: String, index: Int): String = { if (index >= modelList.length) return "❌ Error IA"; try { val r = requests.post(s"https://generativelanguage.googleapis.com/v1beta/models/${modelList(index)}:generateContent?key=$apiKey", data = ujson.Obj("contents" -> ujson.Arr(ujson.Obj("parts" -> ujson.Arr(ujson.Obj("text" -> prompt))))).toString(), headers = Map("Content-Type" -> "application/json"), check = false, readTimeout = 15000); if (r.statusCode == 200) ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str else attemptNextModel(prompt, apiKey, index + 1) } catch { case _: Exception => attemptNextModel(prompt, apiKey, index + 1) } }
  def analyzeAudioLog(matchId: Int, audioBase64: String): String = {
    val prompt = "Eres un Psicólogo Deportivo experto en niños. Analiza este audio post-partido de un portero de 5 años. 1) Transcribe. 2) Estado emocional. 3) Consejo breve para el padre. Texto plano."
    val cleanBase64 = if(audioBase64.contains(",")) audioBase64.split(",")(1) else audioBase64
    val envKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim
    try {
      val r = requests.post(s"https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=$envKey", data = ujson.Obj("contents" -> ujson.Arr(ujson.Obj("parts" -> ujson.Arr(ujson.Obj("text" -> prompt), ujson.Obj("inlineData" -> ujson.Obj("mimeType" -> "audio/webm", "data" -> cleanBase64)))))).toString(), headers = Map("Content-Type" -> "application/json"), check = false, readTimeout = 30000)
      if (r.statusCode == 200) { val an = ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str; val conn = getConnection(); try { val ps = conn.prepareStatement("UPDATE matches SET analisis_voz = ? WHERE id = ?"); ps.setString(1, fixEncoding(an)); ps.setInt(2, matchId); ps.executeUpdate() } finally { conn.close() }; an } else s"Error IA (${r.statusCode})"
    } catch { case e: Exception => s"Error: ${e.getMessage}" }
  }
  // --- EN: DatabaseManager.scala ---

  def generateTrainingSession(mode: String, focus: String): String = {
    var ctx = ""
    val conn = getConnection()
    try {
      // 1. Contexto del último PARTIDO (Ya lo tenías)
      val rsMatch = conn.createStatement().executeQuery("SELECT notas_partido, reaccion_goles FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 1")
      if(rsMatch.next()) ctx += s"Último Partido: ${rsMatch.getString(1)}. Fallos/Reacción: ${rsMatch.getString(2)}. "

      // 2. NUEVO: Contexto del último ENTRENO (Carga Crónica)
      val rsTrain = conn.createStatement().executeQuery("SELECT tipo, rpe, foco, fecha FROM trainings ORDER BY id DESC LIMIT 1")
      if (rsTrain.next()) {
        val (tipo, rpe, foco) = (rsTrain.getString("tipo"), rsTrain.getInt("rpe"), rsTrain.getString("foco"))
        ctx += s"AYER entrenó: Tipo $tipo, Foco $foco, Carga RPE $rpe/10. "

        // Lógica de "Director Deportivo" inyectada en el prompt
        if (rpe >= 8) ctx += "IMPORTANTE: Ayer fue carga muy alta. La sesión de HOY debe ser RECUPERACIÓN o LÚDICA, baja carga física. "
        else if (tipo == "Academia") ctx += "IMPORTANTE: Viene de Academia (técnica analítica). HOY priorizar TOMA DE DECISIÓN o JUEGO REAL. "
      }
    } finally {
      conn.close()
    }

    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)
    val role = if(mode.contains("Jugador")) s"JUGADOR ($edad años)" else s"PORTERO ($edad años)"

    // Prompt enriquecido
    callGeminiAI(s"Eres Entrenador Elite de Fútbol Base. Crea sesión 45min (Padre/Hijo). ROL: $role. OBJETIVO USUARIO: $focus. CONTEXTO REAL: $ctx. Estructura: 1. Calentamiento (Ludico), 2. Bloque Principal (Adaptado al contexto), 3. Reto Final. SOLO TEXTO PLANO.")
      .replace("```html","").replace("```","").trim
  }
  // --- NUEVO: CENTRO DE PREDICCIÓN BIOMÉTRICA (EL ORÁCULO) ---
  def getOracleInsights(): String = {
    val conn = getConnection()
    try {
      val sb = new StringBuilder()

      // 1. Obtener datos de crecimiento (últimos 2 para comparar)
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

      // 2. Análisis de Biotipo y Composición
      val imc = if(currentH > 0) currentW / Math.pow(currentH/100, 2) else 0.0
      sb.append(s"<div class='mb-3 text-white'><b>📊 COMPOSICIÓN:</b> ${currentH}cm / ${currentW}kg</div>")

      val (perfilNombre, perfilDesc) = if (imc < 15) {
        ("<span class='text-info fw-bold'>VELOCISTA</span>", "Peso ligero que favorece la <b>agilidad pura</b> y velocidad de desplazamiento.")
      } else if (imc >= 15 && imc <= 17) {
        ("<span class='text-success fw-bold'>EQUILIBRADO</span>", "Relación potencia-peso óptima. Buen equilibrio entre <b>salto y velocidad</b>.")
      } else {
        ("<span class='text-warning fw-bold'>TANQUE</span>", "Mayor masa corporal. Ventaja en <b>protección de balón</b> y duelos 1v1.")
      }
      sb.append(s"<div class='mb-3 small text-light'><b>🕵️ Perfil Físico:</b> $perfilNombre. $perfilDesc</div>")

      // 3. Alerta de Estirón (Solo si hay historial)
      if (hasPrev && currentH > prevH && currentW <= prevW) {
        sb.append("<div class='alert alert-warning p-2 small mb-3'>")
        sb.append("<b>🦴 ESTIRÓN DETECTADO:</b> Ha crecido en altura sin aumentar masa. ")
        sb.append("Es probable que esté algo más impreciso. Trabajar <b>propiocepción</b>.</div>")
      }

      // 4. Cálculo de Cargas (ACWR)
      val acuteLoads = getWorkloads(7)
      val chronicLoads = getWorkloads(28)
      val acuteAvg = if (acuteLoads.nonEmpty) acuteLoads.sum / 7.0 else 0.0
      val chronicAvg = if (chronicLoads.nonEmpty) chronicLoads.sum / 28.0 else 1.0
      val acwr = if (chronicAvg > 0) acuteAvg / chronicAvg else 0.0

      // 5. Gráfico de Barras ACWR
      val maxVal = Math.max(acuteAvg, chronicAvg).max(100.0)
      val acuteWidth = (acuteAvg / maxVal * 100).toInt
      val chronicWidth = (chronicAvg / maxVal * 100).toInt
      val barColor = if(acwr > 1.5) "bg-danger" else if(acwr < 0.8) "bg-info" else "bg-success"

      sb.append("<div class='mb-4 p-3 bg-black bg-opacity-25 rounded border border-secondary'>")
      sb.append("<h6 class='text-uppercase x-small fw-bold text-muted mb-3'>Estado de Carga (ACWR)</h6>")
      sb.append(s"<div class='mb-2'><div class='progress' style='height: 6px; background:#111;'><div class='progress-bar bg-secondary' style='width: $chronicWidth%'></div></div><div class='x-small text-muted'>Carga Crónica</div></div>")
      sb.append(s"<div class='mb-2'><div class='progress' style='height: 12px; background:#111;'><div class='progress-bar $barColor progress-bar-striped progress-bar-animated' style='width: $acuteWidth%'></div></div><div class='x-small text-muted'>Carga Aguda (Semana)</div></div>")
      sb.append(f"<div class='text-center mt-2'><span class='badge bg-dark border border-secondary'>Ratio: $acwr%.2f</span></div>")
      sb.append("</div>")

      // 6. Plan de Trabajo Dinámico
      sb.append("<div class='card bg-primary bg-opacity-10 border-primary p-3 mb-2'>")
      sb.append("<h6 class='text-primary fw-bold'><i class='fas fa-clipboard-list'></i> Plan Recomendado:</h6>")
      if (acwr > 1.5) {
        sb.append("<p class='small text-warning mb-0'><b>⚠️ FATIGA DETECTADA:</b> Sesión teórica o técnica manual sentado.</p>")
      } else {
        sb.append("<p class='small text-light mb-0'><b>✅ LISTO:</b> Coordinación de pies y blocajes en movimiento.</p>")
      }
      sb.append("</div>")

      sb.toString()
    } catch {
      case e: Exception => s"Analizando datos bioptométricos... (${e.getMessage})"
    } finally {
      conn.close()
    }
  }

  def getRPGStatus(): RPGStatus = {
    var xp = 0
    var belt = "Blanco"
    val conn = getConnection()

    try {
      // 1. Cálculo de XP basado en rendimiento real
      val rs = conn.createStatement().executeQuery("SELECT COUNT(*) as pj, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as cs, SUM(paradas) as sv, AVG(nota) as avg_n FROM matches WHERE status='PLAYED'")
      if(rs.next()){
        val pj = rs.getInt("pj")
        val cs = rs.getInt("cs")
        val sv = rs.getInt("sv")
        val avg = rs.getDouble("avg_n")
        xp = (pj * 50) + (cs * 100) + (sv * 5) + (if(avg > 7.0) ((avg - 7.0) * 100).toInt else 0)
      }

      // 2. Obtención del cinturón de Judo
      val rsBelt = conn.createStatement().executeQuery("SELECT judo_belt FROM seasons ORDER BY id DESC LIMIT 1")
      if(rsBelt.next()) {
        belt = Option(rsBelt.getString("judo_belt")).getOrElse("Blanco")
      }
    } finally { conn.close() }

    // 3. Lógica de progresión (Se calcula una sola vez aquí)
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

  def getOraclePrediction(hDad: Double, hMom: Double): String = { val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT altura FROM physical_growth ORDER BY fecha DESC LIMIT 1"); val currentHeight=if(rs.next()) rs.getDouble("altura") else 115.0; val midParent=(hDad+hMom+13)/2.0; val projected=(currentHeight*(180.0/110.0)+midParent)/2.0+5.0; val minH=projected-4; val maxH=projected+4; f"<div class='text-center'><h1 class='display-1 text-warning fw-bold'>${projected.toInt} cm</h1><p class='text-muted'>Proyección Adulta Estimada</p><div class='progress mb-2' style='height:10px;'><div class='progress-bar bg-success' style='width:${(projected/200.0)*100}%%'></div></div><p class='small'>Rango probable: <b>${minH.toInt}cm - ${maxH.toInt}cm</b></p><hr><p class='small text-info'>Comparativa Élite: <b>189 cm</b> (Media Pro)</p></div>" } catch { case _:Exception => "Error calculando." } finally { conn.close() } }

  // --- CORE MATCH LOGIC ---
  def logMatch(
                riv: String, gf: Int, gc: Int, min: Int, n: Double, med: Double, par: Int,
                zG: String, zT: String, zP: String, p1v1: Int, pAir: Int, pPie: Int,
                clima: String, estadio: String, temp: Int, notas: String, video: String,
                reaccion: String, fechaStr: String, tipo: String,
                pcTot: Int, pcOk: Int, plTot: Int, plOk: Int,
                mapaCampo: String // <--- NUEVO PARÁMETRO
              ): Unit = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons")
      if(rs.next()){
        // Consulta SQL actualizada con 'mapa_campo' al final
        val s = conn.prepareStatement("""
        INSERT INTO matches (
          season_id, rival, goles_favor, goles_contra, minutos, nota, media_historica,
          paradas, zona_goles, zona_tiros, zona_paradas, paradas_1v1, paradas_aereas,
          acciones_pie, clima, estadio, temperatura, notas_partido, video_url,
          reaccion_goles, fecha, status, tipo_partido, pc_t, pc_ok, pl_t, pl_ok,
          torneo_nombre, fase, mapa_campo
        ) VALUES (
          ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
          'PLAYED', ?, ?, ?, ?, ?, '', '', ?
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
        s.setString(27, mapaCampo) // <--- ASIGNACIÓN DEL NUEVO VALOR

        s.executeUpdate()
      }
    } finally {
      conn.close()
    }
  }
  def playScheduledMatch(
                          id: Int, gf: Int, gc: Int, min: Int, nota: Double, paradas: Int,
                          notas: String, video: String, reaccion: String, clima: String, estadio: String,
                          zonaGoles: String, zonaTiros: String, zonaParadas: String,
                          p1v1: Int, pAir: Int, pPie: Int, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int,
                          mapaCampo: String // <--- NUEVO PARÁMETRO
                        ): Unit = {
    val conn = getConnection()
    try {
      val c = getLatestCardData()
      val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas, pcTot, pcOk, plTot, plOk)
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
  def deleteMatch(id: Int): Unit = { val conn=getConnection(); try { conn.createStatement().executeUpdate(s"DELETE FROM matches WHERE id=$id") } finally { conn.close() } }
  def updateRFFMSettings(url: String, teamName: String): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("UPDATE seasons SET rffm_url=?, rffm_team_name=? WHERE id=(SELECT MAX(id) FROM seasons)"); ps.setString(1,url); ps.setString(2,teamName); ps.executeUpdate() } finally { conn.close() } }
  def updateSeasonSettings(f: String, c: String, n: String, fecha: String): String = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE seasons SET foto_jugador_url=COALESCE(NULLIF(?,''), foto_jugador_url), club_escudo_url=COALESCE(NULLIF(?,''), club_escudo_url), nombre_club=COALESCE(NULLIF(?,''), nombre_club), fecha_nacimiento=? WHERE id=(SELECT MAX(id) FROM seasons)"); s.setString(1,f); s.setString(2,c); s.setString(3,fixEncoding(n)); s.setDate(4, Date.valueOf(fecha)); s.executeUpdate(); "DATOS ACTUALIZADOS" } finally { conn.close() } }
  def getLatestCardData(): PlayerCardData = { var conn: Connection=null; try { conn=getConnection(); val rs=conn.createStatement().executeQuery("SELECT * FROM seasons ORDER BY id DESC LIMIT 1"); if(rs.next()){ val fecha=Option(rs.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2020-06-19"); PlayerCardData("HECTOR", rs.getDouble("media").toInt, "GK", Option(rs.getString("foto_jugador_url")).getOrElse(""), Option(rs.getString("club_escudo_url")).getOrElse(""), "", Option(rs.getString("nombre_club")).getOrElse(""), rs.getDouble("stat_div").toInt, rs.getDouble("stat_han").toInt, rs.getDouble("stat_kic").toInt, rs.getDouble("stat_ref").toInt, rs.getDouble("stat_spd").toInt, rs.getDouble("stat_pos").toInt, rs.getDouble("stat_div"), rs.getDouble("stat_han"), rs.getDouble("stat_kic"), rs.getDouble("stat_ref"), rs.getDouble("stat_spd"), rs.getDouble("stat_pos"), fecha, Option(rs.getString("rffm_url")).getOrElse(""), Option(rs.getString("rffm_team_name")).getOrElse("")) } else { PlayerCardData("HECTOR", 59, "GK", "", "", "", "", 80, 60, 55, 60, 62, 58, 80, 60, 55, 60, 62, 58, "2020-06-19", "", "") } } finally { if(conn!=null) conn.close() } }
  def getDeepAnalysis(): String = { var conn:Connection=null; try { conn=getConnection(); val sb=new StringBuilder(); val card=getLatestCardData(); val edad=calcularEdadExacta(card.fechaNacimiento); sb.append(s"Analista Elite ($edad años). Tendencias:\n"); val rs=conn.createStatement().executeQuery("SELECT fecha, rival, nota FROM matches WHERE status='PLAYED' ORDER BY fecha ASC"); var c=0; while(rs.next()){ c+=1; sb.append(s"${rs.getString(1)}|${rs.getString(2)}|${rs.getDouble(3)}\n") }; if(c<2) return "Pocos datos."; callGeminiAI(sb.toString()+"\nDame HTML limpio: <h4>ANALISIS</h4>...").replace("```html","").replace("```","").trim } catch { case e:Exception=>"Error" } finally { if(conn!=null) conn.close() } }
  def getChartData(): String = { var l=List[String](); var d=List[Double](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT rival, media_historica FROM matches WHERE status='PLAYED' ORDER BY fecha ASC LIMIT 15"); while(rs.next()){ l=l:+s"'${rs.getString("rival")}'"; d=d:+rs.getDouble("media_historica") } } finally {conn.close()}; s"""{ "labels": [${l.mkString(",")}], "data": [${d.mkString(",")}] }""" }
  def getAchievements(): List[Achievement] = { var l=List[Achievement](); val conn=getConnection(); try { val s=conn.createStatement(); val r1=s.executeQuery("SELECT COUNT(*) FROM matches WHERE goles_contra=0 AND status='PLAYED'"); if(r1.next()&&r1.getInt(1)>=5) l=l:+Achievement("(M)","El Muro",r1.getInt(1)/5,""); val r2=s.executeQuery("SELECT COUNT(*) FROM matches WHERE nota>=9 AND status='PLAYED'"); if(r2.next()&&r2.getInt(1)>0) l=l:+Achievement("(E)","MVP",r2.getInt(1),"") } finally { conn.close() }; l }
  def getSeasonObjectives(): List[Objective] = { var l=List[Objective](); val conn=getConnection(); try { val rsObj=conn.createStatement().executeQuery("SELECT id, tipo, objetivo, descripcion FROM objectives"); val objs=new scala.collection.mutable.ListBuffer[(Int,String,Int,String)](); while(rsObj.next()) objs+=((rsObj.getInt("id"),rsObj.getString("tipo"),rsObj.getInt("objetivo"),rsObj.getString("descripcion"))); val rsStats=conn.createStatement().executeQuery("SELECT COUNT(*) as pj, COUNT(CASE WHEN goles_contra=0 THEN 1 END) as cs, AVG(nota) as media FROM matches WHERE status='PLAYED'"); var (cs,pj,md)=(0,0,0.0); if(rsStats.next()){cs=rsStats.getInt("cs");pj=rsStats.getInt("pj");md=rsStats.getDouble("media")}; objs.foreach { case (id,t,m,d) => val act=t match { case "CleanSheets"=>cs.toDouble case "MediaNota"=>md case "PartidosJugados"=>pj.toDouble case _=>0.0 }; l=l:+Objective(id,t,act,m,d) } } finally { conn.close() }; l }
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

    // Tabla Maestra OMS 5-18 años (Percentiles 15, 50, 85)
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

          // AQUI ESTABA EL ERROR: Faltaba el último argumento "" para mapaCampo
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

      // 1. DETECTOR "TORPEZA DEL ESTIRÓN" (Crecimiento Rápido + Bajada Coordinación)
      val rsGrowth = conn.createStatement().executeQuery("SELECT velocidad_crecimiento FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val growthSpeed = if(rsGrowth.next()) rsGrowth.getDouble("velocidad_crecimiento") else 0.0

      if (growthSpeed > 0.5) { // Si ha crecido más de 0.5cm recientemente
        val rsTech = conn.createStatement().executeQuery("SELECT coordinacion FROM technical_reviews ORDER BY fecha DESC LIMIT 2")
        if (rsTech.next()) {
          val currCoord = rsTech.getInt("coordinacion")
          if (rsTech.next()) {
            val prevCoord = rsTech.getInt("coordinacion")
            if (currCoord < prevCoord) {
              sb.append("<div class='alert alert-danger p-2 small mb-2'><strong>⚠️ ALERTA BIO-MECÁNICA:</strong> Crecimiento acelerado detectado (+"+growthSpeed+"cm) coincidiendo con bajada de coordinación. Riesgo de 'Torpeza del Estirón'. <br>Recomendación: <em>Simplificar tareas técnicas y trabajar propiocepción.</em></div>")
            }
          }
        }
      }

      // 2. DETECTOR DE PATRONES DE DOLOR (Dolor > 0 vs Tipo de Entreno)
      // Buscamos si hay correlación entre dolor y superficie/tipo en los últimos 10 registros
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
        lastContext = rsPain.getString("tipo") + " (" + rsPain.getString("foco") + ")"
      }

      if (painCount >= 2) {
        sb.append(s"<div class='alert alert-warning p-2 small mb-0'><strong>🔍 PATRÓN DE DOLOR:</strong> Detectadas $painCount sesiones recientes con dolor. Contexto frecuente: $lastContext. <br>Revisar calzado o dureza del terreno.</div>")
      }

      if (sb.isEmpty) "<div class='text-muted small text-center fst-italic'>Sin anomalías biométricas detectadas hoy.</div>" else sb.toString()

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

  // --- GESTIÓN DE CINTURÓN DE JUDO ---
  def updateJudoBelt(nuevoCinturon: String): Unit = {
    val conn = getConnection(); try {
      // Nos aseguramos de que la columna existe en la tabla seasons
      conn.createStatement().executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS judo_belt TEXT DEFAULT 'Blanco'")
      val ps = conn.prepareStatement("UPDATE seasons SET judo_belt = ? WHERE id = (SELECT MAX(id) FROM seasons)")
      ps.setString(1, nuevoCinturon); ps.executeUpdate()
    } finally { conn.close() }
  }
}