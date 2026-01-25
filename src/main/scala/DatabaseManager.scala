import java.sql.{Connection, DriverManager, Date}
import java.util.Properties
import java.time.LocalDate
import requests._
import ujson._

// DATA MODELS
case class PlayerCardData(nombre: String, media: Int, posicion: String, fotoUrl: String, clubUrl: String, flagUrl: String, clubNombre: String, div: Int, han: Int, kic: Int, ref: Int, spd: Int, pos: Int, divRaw: Double, hanRaw: Double, kicRaw: Double, refRaw: Double, spdRaw: Double, posRaw: Double)
case class MatchLog(id: Int, rival: String, resultado: String, minutos: Int, nota: Double, fecha: String, clima: String, notas: String, video: String, reaccion: String, status: String, tipo: String, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int)
case class SeasonSummary(id: Int, categoria: String, clubUrl: String, fotoUrl: String, partidosJugados: Int, golesContra: Int, porteriasCero: Int, mediaFinal: Int)
case class Achievement(icono: String, nombre: String, cantidad: Int, descripcion: String)
case class GearItem(id: Int, nombre: String, tipo: String, usos: Int, maxUsos: Int, estado: String, img: String)
case class Objective(id: Int, tipo: String, actual: Double, meta: Int, descripcion: String)
case class Drill(id: Int, nombre: String, desc: String, actual: Int, objetivo: Int)
case class VideoTag(id: Int, matchId: Int, minuto: Int, segundo: Int, tipo: String, desc: String)
case class RivalInfo(nombre: String, estilo: String, claves: String, notas: String)
case class PenaltyStat(zona: String, total: Int, goles: Int)

object DatabaseManager {
  val url = "jdbc:postgresql://ep-fancy-cherry-abkfneqp-pooler.eu-west-2.aws.neon.tech/neondb?user=neondb_owner&password=npg_5VxYysTm8vQa&sslmode=require&options=-c%20client_encoding=UTF8"

  def getConnection(): Connection = {
    val props = new Properties(); props.setProperty("user","neondb_owner"); props.setProperty("password","npg_5VxYysTm8vQa"); props.setProperty("ssl","true"); DriverManager.getConnection(url, props)
  }

  def fixEncoding(s: String): String = { try { if (s == null) "" else if (s.contains("Ã")) new String(s.getBytes("ISO-8859-1"), "UTF-8") else s } catch { case e: Exception => s } }

  // --- IA CONFIG ---
  // CORRECCIÓN: Usar modelos estables primero
  val modelList = Seq("gemini-1.5-flash", "gemini-1.5-pro", "gemini-flash-latest")

  def callGeminiAI(prompt: String): String = {
    // IMPORTANTE: Asegúrate de que esta variable de entorno existe en Render
    val envKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim
    if (envKey.isEmpty) return "⚠️ Error Config: Falta GEMINI_API_KEY en las variables de entorno."
    attemptNextModel(prompt, envKey, 0)
  }

  def attemptNextModel(prompt: String, apiKey: String, index: Int): String = {
    if (index >= modelList.length) return "❌ Error IA: Modelos saturados o API Key inválida."
    val modelName = modelList(index)
    val url = s"https://generativelanguage.googleapis.com/v1beta/models/$modelName:generateContent?key=$apiKey"
    try {
      val r = requests.post(url, data = ujson.Obj("contents" -> ujson.Arr(ujson.Obj("parts" -> ujson.Arr(ujson.Obj("text" -> prompt))))).toString(), headers = Map("Content-Type" -> "application/json"), check = false, readTimeout = 15000)
      if (r.statusCode == 200) {
        try {
          ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str
        } catch {
          case e: Exception => s"Error parseando respuesta IA: ${e.getMessage}"
        }
      } else {
        println(s"IA Fail ($modelName): ${r.statusCode} - ${r.text()}")
        attemptNextModel(prompt, apiKey, index + 1)
      }
    } catch { case e: Exception =>
      println(s"IA Exception ($modelName): ${e.getMessage}")
      attemptNextModel(prompt, apiKey, index + 1)
    }
  }

  // --- IA GENERADOR DE ENTRENOS ---
  def generateTrainingSession(mode: String, focus: String): String = {
    var context = ""
    val conn = getConnection(); try {
      val rs = conn.createStatement().executeQuery("SELECT notas_partido, reaccion_goles FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 1")
      if(rs.next()) context = s"Último partido: ${rs.getString("notas_partido")}. Fallos recientes: ${rs.getString("reaccion_goles")}."
    } finally { conn.close() }

    val role = if(mode.contains("Jugador")) "JUGADOR DE CAMPO (Fútbol 7, niño 8 años)" else "PORTERO (Fútbol 7, niño 8 años)"
    val prompt = s"""
      Eres un Entrenador de Fútbol experto en cantera. Diseña una sesión de entrenamiento individual (Padre e Hijo) de 45 min.
      ROL: $role.
      OBJETIVO PRINCIPAL: $focus.
      CONTEXTO RECIENTE: $context

      Estructura la respuesta SOLO con este formato HTML (sin markdown ```html):
      <p><b>1. Calentamiento (10'):</b> [Ejercicio divertido]</p>
      <p><b>2. Técnica (20'):</b> [2 ejercicios clave para $role usando el objetivo]</p>
      <p><b>3. Reto Final (15'):</b> [Juego competitivo]</p>
      Sé breve, motivador y directo.
    """
    callGeminiAI(prompt).replace("```html", "").replace("```", "").trim
  }

  // --- PENALTY LAB ---
  def logPenalty(rival: String, zTiro: String, zSalto: String, esGol: Boolean): Unit = {
    val conn = getConnection(); try {
      val ps = conn.prepareStatement("INSERT INTO penalties (rival, zona_tiro, zona_salto, es_gol) VALUES (?, ?, ?, ?)")
      ps.setString(1, fixEncoding(rival)); ps.setString(2, zTiro); ps.setString(3, zSalto); ps.setBoolean(4, esGol)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getPenaltyStats(): List[PenaltyStat] = {
    val l = scala.collection.mutable.ListBuffer[PenaltyStat]()
    val conn = getConnection(); try {
      val rs = conn.createStatement().executeQuery("SELECT zona_tiro, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 1 ELSE 0 END) as goles FROM penalties GROUP BY zona_tiro")
      while(rs.next()) l += PenaltyStat(rs.getString("zona_tiro"), rs.getInt("total"), rs.getInt("goles"))
    } finally { conn.close() }
    l.toList
  }

  // --- GARAGE CON FOTOS ---
  def addNewGear(nombre: String, tipo: String, vida: Int, img: String): Unit = {
    val conn=getConnection(); try {
      val r=conn.prepareStatement("UPDATE gear SET activo=FALSE WHERE tipo=? AND activo=TRUE"); r.setString(1,tipo); r.executeUpdate();
      val a=conn.prepareStatement("INSERT INTO gear (nombre, tipo, vida_util_estimada, usos_actuales, activo, imagen_url) VALUES (?,?,?,0,TRUE, ?)")
      a.setString(1,fixEncoding(nombre)); a.setString(2,tipo); a.setInt(3,vida); a.setString(4, img)
      a.executeUpdate()
    } finally { conn.close() }
  }
  def getActiveGear(): List[GearItem] = { var l=List[GearItem](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT * FROM gear WHERE activo = TRUE ORDER BY tipo DESC"); while(rs.next()) { val (u,max)=(rs.getInt("usos_actuales"),rs.getInt("vida_util_estimada")); l=l:+GearItem(rs.getInt("id"),rs.getString("nombre"),rs.getString("tipo"),u,max,if(max>0 && u.toDouble/max > 0.9) "Critico" else "Optimo", Option(rs.getString("imagen_url")).getOrElse("")) } } finally { conn.close() }; l }

  // --- FUNCIONES CLÁSICAS (DEBUGGEADAS) ---
  def getDeepAnalysis(): String = {
    var conn: Connection = null
    try {
      conn = getConnection(); val stmt = conn.createStatement()
      val sb = new StringBuilder()
      var nextRival = "Ninguno"; var nextDate = ""

      // 1. Agenda
      val rsNext = stmt.executeQuery("SELECT rival, fecha FROM matches WHERE status = 'SCHEDULED' ORDER BY fecha ASC LIMIT 1")
      if(rsNext.next()) { nextRival = rsNext.getString("rival"); nextDate = rsNext.getDate("fecha").toString }

      // 2. Wellness
      var estadoFisico = "Desconocido"; var bioData = ""
      // Usamos nueva consulta para evitar error si tabla vacía
      val rsW = conn.createStatement().executeQuery("SELECT estado_fisico, sueno, horas_sueno, animo, dolor FROM wellness ORDER BY id DESC LIMIT 1")
      if(rsW.next()) { estadoFisico = rsW.getString("estado_fisico"); bioData = s"Sueño: ${rsW.getDouble("horas_sueno")}h, Ánimo: ${rsW.getInt("animo")}/5." }

      // 3. Crecimiento
      var growthAlert = ""
      val rsG = conn.createStatement().executeQuery("SELECT velocidad_crecimiento FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      if(rsG.next() && rsG.getDouble("velocidad_crecimiento") > 0.5) growthAlert = s"⚠️ ALERTA: Crecimiento rápido (${rsG.getDouble("velocidad_crecimiento")}cm). Posible descoordinación."

      // 4. Pases
      var passStats = ""
      val rsPass = conn.createStatement().executeQuery("SELECT SUM(pc_t) as st, SUM(pc_ok) as so, SUM(pl_t) as lt, SUM(pl_ok) as lo FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 3")
      if(rsPass.next()) { val (st, so, lt, lo) = (rsPass.getInt("st"), rsPass.getInt("so"), rsPass.getInt("lt"), rsPass.getInt("lo")); if(st+lt > 0) passStats = s"Pases (Últimos 3): Corto ${so}/${st}, Largo ${lo}/${lt}." }

      sb.append("Eres el Director Deportivo. Analiza:\n"); sb.append(s"1. AGENDA: Próximo vs $nextRival ($nextDate).\n"); sb.append(s"2. FÍSICO: $estadoFisico. $bioData. $growthAlert\n"); sb.append(s"3. JUEGO DE PIES: $passStats\n")
      sb.append("Responde en HTML bonito (fondo oscuro, letra 16px). Título '🧠 ANÁLISIS DEL MÍSTER'.")
      sb.append("<div style='background: rgba(13, 202, 240, 0.1); border-left: 6px solid #0dcaf0; padding: 15px; margin: 10px 0; border-radius: 4px;'>"); sb.append("<h4 style='color: #0dcaf0; margin: 0 0 10px 0; font-size: 18px; font-family: sans-serif; letter-spacing: 1px; text-transform: uppercase;'>🧠 ANÁLISIS DEL MÍSTER</h4>"); sb.append("<p style='color: #ffffff; font-size: 16px; line-height: 1.6; font-family: sans-serif; margin: 0; font-weight: 400;'>[Tu consejo]</p>"); sb.append("</div>")

      val raw = callGeminiAI(sb.toString());
      raw.replace("```html", "").replace("```", "").trim

    } catch {
      case e: Exception =>
        println(s"ERROR DEEP ANALYSIS: ${e.getMessage}")
        e.printStackTrace()
        s"<div style='color: #ff6b6b; font-size: 12px;'>⚠️ DEBUG IA: ${e.getMessage}</div>"
    } finally { if(conn!=null) conn.close() }
  }

  def logMatch(riv: String, gf: Int, gc: Int, min: Int, n: Double, med: Double, par: Int, zG: String, zT: String, zP: String, p1v1: Int, pAir: Int, pPie: Int, clima: String, temp: Int, notas: String, video: String, reaccion: String, fechaStr: String, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int): Unit = { val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rs.next()){ val s=conn.prepareStatement("INSERT INTO matches (season_id, rival, goles_favor, goles_contra, minutos, nota, media_historica, paradas, zona_goles, zona_tiros, zona_paradas, paradas_1v1, paradas_aereas, acciones_pie, clima, temperatura, notas_partido, video_url, reaccion_goles, fecha, status, pc_t, pc_ok, pl_t, pl_ok) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'PLAYED',?,?,?,?)"); s.setInt(1,rs.getInt("id")); s.setString(2,fixEncoding(riv)); s.setInt(3,gf); s.setInt(4,gc); s.setInt(5,min); s.setDouble(6,n); s.setDouble(7,med); s.setInt(8,par); s.setString(9,zG); s.setString(10,zT); s.setString(11,zP); s.setInt(12,p1v1); s.setInt(13,pAir); s.setInt(14,pPie); s.setString(15,clima); s.setInt(16,temp); s.setString(17,fixEncoding(notas)); s.setString(18,video); s.setString(19,fixEncoding(reaccion)); s.setDate(20,Date.valueOf(fechaStr)); s.setInt(21,pcTot); s.setInt(22,pcOk); s.setInt(23,plTot); s.setInt(24,plOk); s.executeUpdate() } } finally {conn.close()} }
  def updateMatch(id: Int, rival: String, gf: Int, gc: Int, min: Int, nota: Double, clima: String, temp: Int, notas: String, video: String, reaccion: String, fechaStr: String): Unit = { val conn = getConnection(); try { val s = conn.prepareStatement("UPDATE matches SET rival=?, goles_favor=?, goles_contra=?, minutos=?, nota=?, clima=?, temperatura=?, notas_partido=?, video_url=?, reaccion_goles=?, fecha=? WHERE id=?"); s.setString(1, fixEncoding(rival)); s.setInt(2, gf); s.setInt(3, gc); s.setInt(4, min); s.setDouble(5, nota); s.setString(6, clima); s.setInt(7, temp); s.setString(8, fixEncoding(notas)); s.setString(9, video); s.setString(10, fixEncoding(reaccion)); s.setDate(11, Date.valueOf(fechaStr)); s.setInt(12, id); s.executeUpdate() } finally { conn.close() } }
  def addNewDrill(nombre: String, desc: String): Unit = { val conn = getConnection(); try { val ps = conn.prepareStatement("INSERT INTO drills (nombre, descripcion, sesiones_objetivo, sesiones_actuales, activo) VALUES (?, ?, 10, 0, TRUE)"); ps.setString(1, fixEncoding(nombre)); ps.setString(2, fixEncoding(desc)); ps.executeUpdate() } finally { conn.close() } }
  def logWellness(sueno: Int, horas: Double, energia: Int, dolor: Int, zona: String, altura: Int, peso: Double, animo: Int, notas: String, estadoFisico: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("INSERT INTO wellness (sueno, horas_sueno, energia, dolor, zona_dolor, altura, peso, animo, notas_conducta, estado_fisico) VALUES (?,?,?,?,?,?,?,?,?,?)"); s.setInt(1,sueno); s.setDouble(2, horas); s.setInt(3,energia); s.setInt(4,dolor); s.setString(5,fixEncoding(zona)); s.setInt(6, altura); s.setDouble(7, peso); s.setInt(8, animo); s.setString(9, fixEncoding(notas)); s.setString(10, estadoFisico); s.executeUpdate(); if(altura > 0 && peso > 0) logGrowth(altura.toDouble, peso) } finally { conn.close() } }
  def logTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: Int, rutina: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("INSERT INTO trainings (tipo, foco, rpe, calidad, atencion, rutina_detalle) VALUES (?,?,?,?,?,?)"); s.setString(1,tipo); s.setString(2,fixEncoding(foco)); s.setInt(3,rpe); s.setInt(4,calidad); s.setInt(5, atencion); s.setString(6,fixEncoding(rutina)); s.executeUpdate(); conn.createStatement().executeUpdate("UPDATE gear SET usos_actuales = usos_actuales + 1 WHERE activo = TRUE"); if (tipo.contains("Papa")) progressDrills() } finally { conn.close() } }
  def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String): Unit = { val conn = getConnection(); try { conn.createStatement().executeUpdate(s"DELETE FROM rivals WHERE LOWER(nombre) = LOWER('${fixEncoding(nombre)}')"); val ps = conn.prepareStatement("INSERT INTO rivals (nombre, estilo_juego, jugadores_clave, notas_scouting) VALUES (?,?,?,?)"); ps.setString(1, fixEncoding(nombre)); ps.setString(2, estilo); ps.setString(3, fixEncoding(claves)); ps.setString(4, fixEncoding(notas)); ps.executeUpdate() } finally { conn.close() } }
  def importMatchesCSV(csvData: String): String = { var count = 0; val lines = csvData.split("\n").map(_.trim).filter(_.nonEmpty); val dataLines = if (lines.headOption.exists(_.toLowerCase.contains("rival"))) lines.tail else lines; val today = LocalDate.now().toString; dataLines.foreach { line => try { val p = line.split(",").map(_.trim); if (p.length >= 6) { val rival = fixEncoding(p(0)); val gf = p(1).toInt; val gc = p(2).toInt; val min = p(3).toInt; val nota = p(4).toDouble; val paradas = p(5).toInt; val clima = if(p.length > 6) p(6) else "Sol"; val notas = if(p.length > 7) fixEncoding(p(7)) else "Importado"; val reaccion = if(p.length > 8) fixEncoding(p(8)) else ""; val c = getLatestCardData(); val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas, 0,0,0,0); updateStats(n); val m = (n.divRaw * 0.2 + n.hanRaw * 0.2 + n.refRaw * 0.2 + n.posRaw * 0.2 + n.spdRaw * 0.05 + n.kicRaw * 0.15); logMatch(rival, gf, gc, min, nota, m, paradas, "", "", "", 0, 0, 0, clima, 20, notas, "", reaccion, today, 0,0,0,0); count += 1 } } catch { case e: Exception => println(s"Error import line: $line") } }; s"Importados $count partidos" }
  def importCalendarCSV(csvData: String): String = { var count=0; val lines=csvData.split("\n").map(_.trim).filter(_.nonEmpty); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rs.next()){ val sId=rs.getInt("id"); val ps=conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas) VALUES (?, ?, ?, ?, 'SCHEDULED', 0, 0, 0, 0, 0)"); lines.foreach { l => try { val p=l.split(",").map(_.trim); if(p.length>=2){ ps.setInt(1,sId); ps.setDate(2,Date.valueOf(p(0))); ps.setString(3,fixEncoding(p(1))); ps.setString(4,if(p.length>2) p(2).toUpperCase else "LIGA"); ps.executeUpdate(); count+=1 } } catch {case _:Exception=>} } } } finally {conn.close()}; s"Importados $count eventos." }
  def logGrowth(altura: Double, peso: Double): Unit = { val conn = getConnection(); try { var velocity = 0.0; val rsLast = conn.createStatement().executeQuery("SELECT altura FROM physical_growth ORDER BY fecha DESC LIMIT 1"); if(rsLast.next()) { val lastHeight = rsLast.getDouble("altura"); if(altura > lastHeight) velocity = altura - lastHeight }; val ps = conn.prepareStatement("INSERT INTO physical_growth (altura, peso, velocidad_crecimiento) VALUES (?, ?, ?)"); ps.setDouble(1, altura); ps.setDouble(2, peso); ps.setDouble(3, velocity); ps.executeUpdate() } finally { conn.close() } }
  def getGrowthHistory(): String = { var l=List[String](); var d=List[Double](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT TO_CHAR(fecha, 'MM-DD') as f, altura FROM physical_growth ORDER BY fecha ASC LIMIT 12"); while(rs.next()){ l=l:+s"'${rs.getString("f")}'"; d=d:+rs.getDouble("altura") } } finally {conn.close()}; s"""{ "labels": [${l.mkString(",")}], "data": [${d.mkString(",")}] }""" }
  def addVideoTag(matchId: Int, min: Int, sec: Int, tipo: String, desc: String): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("INSERT INTO video_tags (match_id, minuto, segundo, tipo, descripcion) VALUES (?,?,?,?,?)"); ps.setInt(1, matchId); ps.setInt(2, min); ps.setInt(3, sec); ps.setString(4, tipo); ps.setString(5, fixEncoding(desc)); ps.executeUpdate() } finally {conn.close()} }
  def getVideoTags(matchId: Int): List[VideoTag] = { var l=List[VideoTag](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery(s"SELECT * FROM video_tags WHERE match_id=$matchId ORDER BY minuto ASC, segundo ASC"); while(rs.next()) l=l:+VideoTag(rs.getInt("id"), rs.getInt("match_id"), rs.getInt("minuto"), rs.getInt("segundo"), rs.getString("tipo"), rs.getString("descripcion")) } finally {conn.close()}; l }
  def deleteVideoTag(id: Int): Unit = { val conn=getConnection(); try{ conn.createStatement().executeUpdate(s"DELETE FROM video_tags WHERE id=$id") } finally {conn.close()} }
  def getRivalInfo(nombre: String): Option[RivalInfo] = { var r: Option[RivalInfo]=None; val conn=getConnection(); try{ val ps=conn.prepareStatement("SELECT * FROM rivals WHERE LOWER(nombre)=LOWER(?)"); ps.setString(1,fixEncoding(nombre)); val rs=ps.executeQuery(); if(rs.next()) r=Some(RivalInfo(rs.getString("nombre"), rs.getString("estilo_juego"), rs.getString("jugadores_clave"), rs.getString("notas_scouting"))) } finally {conn.close()}; r }
  def getActiveDrills(): List[Drill] = { var l=List[Drill](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM drills WHERE activo=TRUE ORDER BY id DESC"); while(rs.next()) l=l:+Drill(rs.getInt("id"), rs.getString("nombre"), Option(rs.getString("descripcion")).getOrElse(""), rs.getInt("sesiones_actuales"), rs.getInt("sesiones_objetivo")) } catch {case _:Exception=>} finally {conn.close()}; l }
  def progressDrills(): Unit = { val conn=getConnection(); try{ conn.createStatement().executeUpdate("UPDATE drills SET sesiones_actuales = sesiones_actuales + 1 WHERE activo = TRUE"); conn.createStatement().executeUpdate("UPDATE drills SET activo = FALSE WHERE sesiones_actuales >= sesiones_objetivo") } finally {conn.close()} }
  def getLatestCardData(): PlayerCardData = { var conn:Connection=null; try{conn=getConnection(); val rs=conn.createStatement().executeQuery("SELECT * FROM seasons ORDER BY id DESC LIMIT 1"); if(rs.next()){ PlayerCardData("HECTOR", rs.getDouble("media").toInt, "GK", Option(rs.getString("foto_jugador_url")).getOrElse(""), Option(rs.getString("club_escudo_url")).getOrElse(""), "", Option(rs.getString("nombre_club")).getOrElse(""), rs.getDouble("stat_div").toInt, rs.getDouble("stat_han").toInt, rs.getDouble("stat_kic").toInt, rs.getDouble("stat_ref").toInt, rs.getDouble("stat_spd").toInt, rs.getDouble("stat_pos").toInt, rs.getDouble("stat_div"), rs.getDouble("stat_han"), rs.getDouble("stat_kic"), rs.getDouble("stat_ref"), rs.getDouble("stat_spd"), rs.getDouble("stat_pos")) } else PlayerCardData("HECTOR",59,"GK","","","","",80,60,55,60,62,58,80,60,55,60,62,58) } finally {if(conn!=null) conn.close()} }
  def getMatchesList(): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='PLAYED' ORDER BY fecha DESC"); while(rs.next()){ l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString, Option(rs.getString("clima")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok")) } } finally {conn.close()}; l }
  def getUpcomingMatches(): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC"); while(rs.next()){ l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), "-", 0, 0, rs.getDate("fecha").toString, "", "", "", "", rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"),0,0,0,0) } } finally {conn.close()}; l }
  def getChartData(): String = { var l=List[String](); var d=List[Double](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT rival, media_historica FROM matches WHERE status='PLAYED' ORDER BY fecha ASC LIMIT 15"); while(rs.next()){ l=l:+s"'${rs.getString("rival")}'"; d=d:+rs.getDouble("media_historica") } } finally {conn.close()}; s"""{ "labels": [${l.mkString(",")}], "data": [${d.mkString(",")}] }""" }
  def getAchievements(): List[Achievement] = { var l=List[Achievement](); val conn=getConnection(); try { val s=conn.createStatement(); val r1=s.executeQuery("SELECT COUNT(*) FROM matches WHERE goles_contra=0 AND status='PLAYED'"); if(r1.next() && r1.getInt(1)>=5) l=l:+Achievement("(M)","El Muro",r1.getInt(1)/5,""); val r2=s.executeQuery("SELECT COUNT(*) FROM matches WHERE nota>=9 AND status='PLAYED'"); if(r2.next() && r2.getInt(1)>0) l=l:+Achievement("(E)","MVP",r2.getInt(1),"") } finally { conn.close() }; l }
  def getSeasonObjectives(): List[Objective] = { var l=List[Objective](); val conn=getConnection(); try { val rsObj=conn.createStatement().executeQuery("SELECT id, tipo, objetivo, descripcion FROM objectives"); val objs=new scala.collection.mutable.ListBuffer[(Int,String,Int,String)](); while(rsObj.next()) objs+=((rsObj.getInt("id"),rsObj.getString("tipo"),rsObj.getInt("objetivo"),rsObj.getString("descripcion"))); val rsStats=conn.createStatement().executeQuery("SELECT COUNT(*) as pj, COUNT(CASE WHEN goles_contra=0 THEN 1 END) as cs, AVG(nota) as media FROM matches WHERE status='PLAYED'"); var (cs,pj,md)=(0,0,0.0); if(rsStats.next()){cs=rsStats.getInt("cs");pj=rsStats.getInt("pj");md=rsStats.getDouble("media")}; objs.foreach { case (id,t,m,d) => val act=t match { case "CleanSheets"=>cs.toDouble case "MediaNota"=>md case "PartidosJugados"=>pj.toDouble case _=>0.0 }; l=l:+Objective(id,t,act,m,d) } } finally { conn.close() }; l }
  def getTacticalStats(): Map[String, Int] = { var stats = scala.collection.mutable.Map("g_tot"->0, "g_alt"->0, "g_med"->0, "g_ras"->0, "g_izq"->0, "g_cen"->0, "g_der"->0, "p_tot"->0, "p_alt"->0, "p_med"->0, "p_ras"->0, "p_izq"->0, "p_cen"->0, "p_der"->0); val conn = getConnection(); try { val rs = conn.createStatement().executeQuery("SELECT zona_goles, zona_paradas FROM matches WHERE status='PLAYED' ORDER BY id DESC LIMIT 20"); while(rs.next()) { val zG = Option(rs.getString("zona_goles")).getOrElse(""); val zP = Option(rs.getString("zona_paradas")).getOrElse(""); zG.split(",").filter(_.nonEmpty).foreach { z => stats("g_tot")+=1; if(z.contains("T")) stats("g_alt")+=1 else if(z.contains("M")) stats("g_med")+=1 else stats("g_ras")+=1; if(z.contains("L")) stats("g_izq")+=1 else if(z.contains("C")) stats("g_cen")+=1 else stats("g_der")+=1 }; zP.split(",").filter(_.nonEmpty).foreach { z => stats("p_tot")+=1; if(z.contains("T")) stats("p_alt")+=1 else if(z.contains("M")) stats("p_med")+=1 else stats("p_ras")+=1; if(z.contains("L")) stats("p_izq")+=1 else if(z.contains("C")) stats("p_cen")+=1 else stats("p_der")+=1 } } } finally { conn.close() }; stats.toMap }
  def updateStats(s: PlayerCardData): Unit = { val conn=getConnection(); try { val st=conn.prepareStatement("UPDATE seasons SET media=?, stat_div=?, stat_han=?, stat_kic=?, stat_ref=?, stat_spd=?, stat_pos=? WHERE id=(SELECT MAX(id) FROM seasons)"); st.setDouble(1,s.media); st.setDouble(2,s.divRaw); st.setDouble(3,s.hanRaw); st.setDouble(4,s.kicRaw); st.setDouble(5,s.refRaw); st.setDouble(6,s.spdRaw); st.setDouble(7,s.posRaw); st.executeUpdate() } finally { conn.close() } }
  def playScheduledMatch(id: Int, gf: Int, gc: Int, min: Int, nota: Double, paradas: Int, notas: String, video: String, reaccion: String, clima: String, zonaGoles: String, zonaTiros: String, zonaParadas: String, p1v1: Int, pAir: Int, pPie: Int, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("UPDATE matches SET status='PLAYED', goles_favor=?, goles_contra=?, minutos=?, nota=?, paradas=?, notas_partido=?, video_url=?, reaccion_goles=?, clima=?, zona_goles=?, zona_tiros=?, zona_paradas=?, paradas_1v1=?, paradas_aereas=?, acciones_pie=?, pc_t=?, pc_ok=?, pl_t=?, pl_ok=? WHERE id=?"); ps.setInt(1,gf); ps.setInt(2,gc); ps.setInt(3,min); ps.setDouble(4,nota); ps.setInt(5,paradas); ps.setString(6,fixEncoding(notas)); ps.setString(7,video); ps.setString(8,fixEncoding(reaccion)); ps.setString(9,clima); ps.setString(10,zonaGoles); ps.setString(11,zonaTiros); ps.setString(12,zonaParadas); ps.setInt(13,p1v1); ps.setInt(14,pAir); ps.setInt(15,pPie); ps.setInt(16,pcTot); ps.setInt(17,pcOk); ps.setInt(18,plTot); ps.setInt(19,plOk); ps.setInt(20,id); ps.executeUpdate() } finally {conn.close()} }
  def deleteMatch(id: Int): Unit = { val conn = getConnection(); try { val s = conn.prepareStatement("DELETE FROM matches WHERE id=?"); s.setInt(1, id); s.executeUpdate() } finally { conn.close() } }
  def getMatchById(id: Int): Option[MatchLog] = { var m: Option[MatchLog] = None; val conn = getConnection(); try { val s = conn.prepareStatement("SELECT * FROM matches WHERE id = ?"); s.setInt(1, id); val rs = s.executeQuery(); if (rs.next()) { m = Some(MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString, Option(rs.getString("clima")).getOrElse("Sol"), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"))) } } finally { conn.close() }; m }
  def importWellnessCSV(csvData: String): String = { var count = 0; val lines = csvData.split("\n").map(_.trim).filter(_.nonEmpty); val dataLines = if (lines.headOption.exists(_.toLowerCase.contains("sueno"))) lines.tail else lines; dataLines.foreach { line => try { val p = line.split(",").map(_.trim); if (p.length >= 3) { val sueno = p(0).toInt; val energia = p(1).toInt; val dolor = p(2).toInt; val zona = if(p.length > 3) fixEncoding(p(3)) else ""; logWellness(sueno, 0.0, energia, dolor, zona, 0, 0.0, 3, "", "DISPONIBLE"); count += 1 } } catch { case e: Exception => println(s"Error import wellness: $line") } }; s"Importados $count registros wellness" }
  def getCareerSummary(): List[SeasonSummary] = { var l=List[SeasonSummary](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT s.id, s.categoria, s.club_escudo_url, s.foto_jugador_url, s.media, COUNT(m.id) as pj, COALESCE(SUM(m.goles_contra),0) as gc, COUNT(CASE WHEN m.goles_contra=0 THEN 1 END) as cs FROM seasons s LEFT JOIN matches m ON s.id=m.season_id AND m.status='PLAYED' GROUP BY s.id, s.categoria, s.club_escudo_url, s.foto_jugador_url, s.media ORDER BY s.id DESC"); while(rs.next()) l=l:+SeasonSummary(rs.getInt("id"), Option(rs.getString("categoria")).getOrElse("Temp"), Option(rs.getString("club_escudo_url")).getOrElse(""), Option(rs.getString("foto_jugador_url")).getOrElse(""), rs.getInt("pj"), rs.getInt("gc"), rs.getInt("cs"), rs.getInt("media")) } finally { conn.close() }; l }
  def getBackupCSV(): String = { val sb=new StringBuilder(); sb.append("Fecha,Rival,GF,GC,Min,Nota,Clima,Notas,Reaccion\n"); getMatchesList().foreach(m => sb.append(s"${m.fecha},${m.rival},${m.resultado},${m.minutos},${m.nota},${m.clima},${m.notas.replace(",",";")},${m.reaccion.replace(",",";")}\n")); sb.toString() }
  def updateObjective(id: Int, nuevo: Int): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE objectives SET objetivo=? WHERE id=?"); s.setInt(1,nuevo); s.setInt(2,id); s.executeUpdate() } finally { conn.close() } }
  def startNewSeason(cat: String): String = { val conn=getConnection(); try { val l=getLatestCardData(); val s=conn.prepareStatement("INSERT INTO seasons (categoria, foto_jugador_url, club_escudo_url, media, stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos, nombre_club) VALUES (?,?,?,?,?,?,?,?,?,?,?)"); s.setString(1,fixEncoding(cat)); s.setString(2,l.fotoUrl); s.setString(3,l.clubUrl); s.setDouble(4,l.media.toDouble); s.setDouble(5,l.divRaw); s.setDouble(6,l.hanRaw); s.setDouble(7,l.kicRaw); s.setDouble(8,l.refRaw); s.setDouble(9,l.spdRaw); s.setDouble(10,l.posRaw); s.setString(11, l.clubNombre); s.executeUpdate(); s"Etapa $cat iniciada" } finally { conn.close() } }
  def updateSeasonSettings(f: String, c: String, n: String): String = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE seasons SET foto_jugador_url=COALESCE(NULLIF(?,''), foto_jugador_url), club_escudo_url=COALESCE(NULLIF(?,''), club_escudo_url), nombre_club=COALESCE(NULLIF(?,''), nombre_club) WHERE id=(SELECT MAX(id) FROM seasons)"); s.setString(1,f); s.setString(2,c); s.setString(3,fixEncoding(n)); s.executeUpdate(); "DATOS ACTUALIZADOS" } finally { conn.close() } }
  def getRivalScouting(rivalBusqueda: String): (List[MatchLog], Map[String, Int]) = { var matches = List[MatchLog](); var stats = scala.collection.mutable.Map("pj"->0, "gf"->0, "gc"->0, "ganados"->0, "empatados"->0, "perdidos"->0); val conn = getConnection(); try { val query = s"SELECT * FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED' ORDER BY fecha DESC"; val stmt = conn.prepareStatement(query); stmt.setString(1, s"%$rivalBusqueda%"); val rs = stmt.executeQuery(); while(rs.next()) { val (gf, gc) = (rs.getInt("goles_favor"), rs.getInt("goles_contra")); matches = matches :+ MatchLog(rs.getInt("id"), rs.getString("rival"), s"$gf-$gc", rs.getInt("minutos"), rs.getDouble("nota"), rs.getString("fecha"), Option(rs.getString("clima")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok")); stats("pj")+=1; stats("gf")+=gf; stats("gc")+=gc; if(gf>gc) stats("ganados")+=1 else if(gf==gc) stats("empatados")+=1 else stats("perdidos")+=1 } } finally { conn.close() }; (matches, stats.toMap) }
}