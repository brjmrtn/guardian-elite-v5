import java.sql.{Connection, DriverManager, Date}
import java.util.Properties
import java.time.LocalDate
import requests._
import ujson._


case class PlayerCardData(
                           nombre: String, media: Int, posicion: String, fotoUrl: String, clubUrl: String, flagUrl: String, clubNombre: String,
                           div: Int, han: Int, kic: Int, ref: Int, spd: Int, pos: Int,
                           divRaw: Double, hanRaw: Double, kicRaw: Double, refRaw: Double, spdRaw: Double, posRaw: Double
                         )
case class MatchLog(id: Int, rival: String, resultado: String, minutos: Int, nota: Double, fecha: String, clima: String, notas: String, video: String, reaccion: String)
case class SeasonSummary(id: Int, categoria: String, clubUrl: String, fotoUrl: String, partidosJugados: Int, golesContra: Int, porteriasCero: Int, mediaFinal: Int)
case class Achievement(icono: String, nombre: String, cantidad: Int, descripcion: String)
case class GearItem(id: Int, nombre: String, tipo: String, usos: Int, maxUsos: Int, estado: String)
case class Objective(id: Int, tipo: String, actual: Double, meta: Int, descripcion: String)

object DatabaseManager {
  val url = "jdbc:postgresql://ep-fancy-cherry-abkfneqp-pooler.eu-west-2.aws.neon.tech/neondb?user=neondb_owner&password=npg_5VxYysTm8vQa&sslmode=require&options=-c%20client_encoding=UTF8"

  def getConnection(): Connection = {
    val props = new Properties(); props.setProperty("user","neondb_owner"); props.setProperty("password","npg_5VxYysTm8vQa"); props.setProperty("ssl","true"); DriverManager.getConnection(url, props)
  }

  // --- IA GEMINI INTEGRATION ---
  def callGeminiAI(prompt: String): String = {
    val apiKey = "AIzaSyCk11VUA0Fbop3GsWgruTbo1QLt38mZO6A"
    if (apiKey.isEmpty) return "⚠️ <b>Falta API Key:</b> Configura GEMINI_API_KEY en Render para activar mi cerebro."

    try {
      val url = s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=$apiKey"
      val payload = ujson.Obj(
        "contents" -> ujson.Arr(
          ujson.Obj("parts" -> ujson.Arr(ujson.Obj("text" -> prompt)))
        )
      )

      val r = requests.post(url, data = payload.toString(), headers = Map("Content-Type" -> "application/json"))
      val json = ujson.read(r.text())
      // Extraemos el texto de la respuesta de Google
      json("candidates")(0)("content")("parts")(0)("text").str
    } catch {
      case e: Exception => s"Error IA: ${e.getMessage}"
    }
  }

  def getDeepAnalysis(): String = {
    var conn: Connection = null
    try {
      conn = getConnection(); val stmt = conn.createStatement()

      // 1. RECOLECTAR DATOS (Contexto para la IA)
      val sb = new StringBuilder()
      sb.append("Eres el entrenador de porteros y psicólogo deportivo de Héctor (niño con posible TDA). Analiza estos datos recientes y dame un consejo breve (max 40 palabras) y motivador, usando negritas HTML <b> para resaltar lo clave. Cruza datos de sueño/foco con rendimiento.\n\n")

      // Partidos recientes
      sb.append("ÚLTIMOS PARTIDOS:\n")
      val rsM = stmt.executeQuery("SELECT rival, nota, reaccion_goles, zona_goles FROM (SELECT * FROM matches ORDER BY fecha DESC, id DESC LIMIT 3) as sub")
      while(rsM.next()) {
        sb.append(s"- Vs ${rsM.getString("rival")}: Nota ${rsM.getDouble("nota")}. Reacción: ${Option(rsM.getString("reaccion_goles")).getOrElse("-")}. Goles por: ${Option(rsM.getString("zona_goles")).getOrElse("-")}\n")
      }

      // Wellness reciente
      sb.append("\nESTADO FISICO/MENTAL (Últimos 3 días):\n")
      val rsW = stmt.executeQuery("SELECT sueno, animo, notas_conducta FROM (SELECT * FROM wellness ORDER BY id DESC LIMIT 3) as sub")
      while(rsW.next()) {
        sb.append(s"- Sueño: ${rsW.getInt("sueno")}/5. Ánimo: ${rsW.getInt("animo")}/5. Notas: ${Option(rsW.getString("notas_conducta")).getOrElse("-")}\n")
      }

      // Entrenos
      sb.append("\nENTRENAMIENTOS (Foco/Atención):\n")
      val rsT = stmt.executeQuery("SELECT foco, atencion FROM (SELECT * FROM trainings ORDER BY id DESC LIMIT 3) as sub")
      while(rsT.next()) {
        sb.append(s"- Foco en: ${rsT.getString("foco")}. Nivel Atención: ${rsT.getInt("atencion")}/5\n")
      }

      // 2. LLAMAR A LA IA
      callGeminiAI(sb.toString())

    } catch {
      case e: Exception => "Analizando datos tácticos..."
    } finally {
      if(conn!=null) conn.close()
    }
  }

  // --- RESTO DEL CÓDIGO (Igual que v5.8) ---
  def getLatestCardData(): PlayerCardData = {
    var conn: Connection = null; try { conn = getConnection(); val rs = conn.createStatement().executeQuery("SELECT club_escudo_url, foto_jugador_url, nombre_club, stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos FROM seasons ORDER BY id DESC LIMIT 1"); if (rs.next()) {
      val (f, c, n) = (Option(rs.getString("foto_jugador_url")).getOrElse(""), Option(rs.getString("club_escudo_url")).getOrElse(""), Option(rs.getString("nombre_club")).getOrElse("Club"))
      val (rD, rH, rK, rR, rS, rP) = (rs.getDouble("stat_div"), rs.getDouble("stat_han"), rs.getDouble("stat_kic"), rs.getDouble("stat_ref"), rs.getDouble("stat_spd"), rs.getDouble("stat_pos"))
      val (d,h,k,r,s,p) = (if(rD>0) rD else 80.0, if(rH>0) rH else 60.0, if(rK>0) rK else 55.0, if(rR>0) rR else 60.0, if(rS>0) rS else 62.0, if(rP>0) rP else 58.0)
      val m = StatsCalculator.calcularMediaGlobal(d,h,k,r,s,p)
      PlayerCardData("HECTOR", m, "GK", if(f.nonEmpty) f else "https://cdn-icons-png.flaticon.com/512/4825/4825038.png", if(c.nonEmpty) c else "https://cdn-icons-png.flaticon.com/512/1077/1077186.png", "https://upload.wikimedia.org/wikipedia/en/9/9a/Flag_of_Spain.svg", n, d.toInt, h.toInt, k.toInt, r.toInt, s.toInt, p.toInt, d, h, k, r, s, p)
    } else getDefaultData() } catch { case e: Exception => println("Err Card"); getDefaultData() } finally { if(conn!=null) conn.close() }
  }

  def updateSeasonSettings(f: String, c: String, n: String): String = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE seasons SET foto_jugador_url=COALESCE(NULLIF(?,''), foto_jugador_url), club_escudo_url=COALESCE(NULLIF(?,''), club_escudo_url), nombre_club=COALESCE(NULLIF(?,''), nombre_club) WHERE id=(SELECT MAX(id) FROM seasons)"); s.setString(1,f); s.setString(2,c); s.setString(3,n); s.executeUpdate(); "DATOS ACTUALIZADOS" } finally { conn.close() } }
  def updateStats(s: PlayerCardData): Unit = { val conn=getConnection(); try { val st=conn.prepareStatement("UPDATE seasons SET media=?, stat_div=?, stat_han=?, stat_kic=?, stat_ref=?, stat_spd=?, stat_pos=? WHERE id=(SELECT MAX(id) FROM seasons)"); st.setDouble(1,s.media.toDouble); st.setDouble(2,s.divRaw); st.setDouble(3,s.hanRaw); st.setDouble(4,s.kicRaw); st.setDouble(5,s.refRaw); st.setDouble(6,s.spdRaw); st.setDouble(7,s.posRaw); st.executeUpdate() } finally { conn.close() } }
  def logMatch(riv: String, gf: Int, gc: Int, min: Int, n: Double, med: Double, par: Int, zG: String, zT: String, zP: String, p1v1: Int, pAir: Int, pPie: Int, clima: String, temp: Int, notas: String, video: String, reaccion: String, fechaStr: String): Unit = { val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rs.next()){ val s=conn.prepareStatement("INSERT INTO matches (season_id, rival, goles_favor, goles_contra, minutos, nota, media_historica, paradas, zona_goles, zona_tiros, zona_paradas, paradas_1v1, paradas_aereas, acciones_pie, clima, temperatura, notas_partido, video_url, reaccion_goles, fecha) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"); s.setInt(1,rs.getInt("id")); s.setString(2,riv); s.setInt(3,gf); s.setInt(4,gc); s.setInt(5,min); s.setDouble(6,n); s.setDouble(7,med); s.setInt(8,par); s.setString(9,zG); s.setString(10,zT); s.setString(11,zP); s.setInt(12,p1v1); s.setInt(13,pAir); s.setInt(14,pPie); s.setString(15, clima); s.setInt(16, temp); s.setString(17, notas); s.setString(18, video); s.setString(19, reaccion); s.setDate(20, Date.valueOf(fechaStr)); s.executeUpdate() }; conn.createStatement().executeUpdate("UPDATE gear SET usos_actuales = usos_actuales + 1 WHERE activo = TRUE") } finally { conn.close() } }
  def getMatchesList(): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT id, rival, goles_favor, goles_contra, minutos, nota, TO_CHAR(fecha, 'YYYY-MM-DD') as f, clima, notas_partido, video_url, reaccion_goles FROM matches ORDER BY fecha DESC, id DESC"); while(rs.next()) { l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getString("f"), Option(rs.getString("clima")).getOrElse("Sol"), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse("")) } } finally { conn.close() }; l }
  def getMatchById(id: Int): Option[MatchLog] = { var m: Option[MatchLog] = None; val conn = getConnection(); try { val s = conn.prepareStatement("SELECT id, rival, goles_favor, goles_contra, minutos, nota, TO_CHAR(fecha, 'YYYY-MM-DD') as f, clima, notas_partido, video_url, reaccion_goles FROM matches WHERE id = ?"); s.setInt(1, id); val rs = s.executeQuery(); if (rs.next()) { m = Some(MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getString("f"), Option(rs.getString("clima")).getOrElse("Sol"), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""))) } } finally { conn.close() }; m }
  def updateMatch(id: Int, rival: String, gf: Int, gc: Int, min: Int, nota: Double, clima: String, temp: Int, notas: String, video: String, reaccion: String, fechaStr: String): Unit = { val conn = getConnection(); try { val s = conn.prepareStatement("UPDATE matches SET rival=?, goles_favor=?, goles_contra=?, minutos=?, nota=?, clima=?, temperatura=?, notas_partido=?, video_url=?, reaccion_goles=?, fecha=? WHERE id=?"); s.setString(1, rival); s.setInt(2, gf); s.setInt(3, gc); s.setInt(4, min); s.setDouble(5, nota); s.setString(6, clima); s.setInt(7, temp); s.setString(8, notas); s.setString(9, video); s.setString(10, reaccion); s.setDate(11, Date.valueOf(fechaStr)); s.setInt(12, id); s.executeUpdate() } finally { conn.close() } }
  def deleteMatch(id: Int): Unit = { val conn = getConnection(); try { val s = conn.prepareStatement("DELETE FROM matches WHERE id=?"); s.setInt(1, id); s.executeUpdate() } finally { conn.close() } }
  def getTacticalStats(): Map[String, Int] = { var stats = scala.collection.mutable.Map("g_tot"->0, "g_alt"->0, "g_med"->0, "g_ras"->0, "g_izq"->0, "g_cen"->0, "g_der"->0, "p_tot"->0, "p_alt"->0, "p_med"->0, "p_ras"->0, "p_izq"->0, "p_cen"->0, "p_der"->0); val conn = getConnection(); try { val rs = conn.createStatement().executeQuery("SELECT zona_goles, zona_paradas FROM matches ORDER BY id DESC LIMIT 20"); while(rs.next()) { val zG = Option(rs.getString("zona_goles")).getOrElse(""); val zP = Option(rs.getString("zona_paradas")).getOrElse(""); zG.split(",").filter(_.nonEmpty).foreach { z => stats("g_tot")+=1; if(z.contains("T")) stats("g_alt")+=1 else if(z.contains("M")) stats("g_med")+=1 else stats("g_ras")+=1; if(z.contains("L")) stats("g_izq")+=1 else if(z.contains("C")) stats("g_cen")+=1 else stats("g_der")+=1 }; zP.split(",").filter(_.nonEmpty).foreach { z => stats("p_tot")+=1; if(z.contains("T")) stats("p_alt")+=1 else if(z.contains("M")) stats("p_med")+=1 else stats("p_ras")+=1; if(z.contains("L")) stats("p_izq")+=1 else if(z.contains("C")) stats("p_cen")+=1 else stats("p_der")+=1 } } } finally { conn.close() }; stats.toMap }
  def getChartData(): String = { var l=List[String](); var d=List[Double](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT rival, media_historica FROM matches ORDER BY fecha ASC, id ASC LIMIT 15"); while(rs.next()) { l=l:+s"'${rs.getString("rival")}'"; d=d:+rs.getDouble("media_historica") } } finally { conn.close() }; s"""{ "labels": [${l.mkString(",")}], "data": [${d.mkString(",")}] }""" }
  def getAchievements(): List[Achievement] = { var l=List[Achievement](); val conn=getConnection(); try { val s=conn.createStatement(); val r1=s.executeQuery("SELECT COUNT(*) FROM matches WHERE goles_contra=0"); if(r1.next() && r1.getInt(1)>=5) l=l:+Achievement("(M)","El Muro",r1.getInt(1)/5,""); val r2=s.executeQuery("SELECT COUNT(*) FROM matches WHERE nota>=9"); if(r2.next() && r2.getInt(1)>0) l=l:+Achievement("(E)","MVP",r2.getInt(1),"") } finally { conn.close() }; l }
  def logWellness(sueno: Int, energia: Int, dolor: Int, zona: String, altura: Int, peso: Double, animo: Int, notas: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("INSERT INTO wellness (sueno, energia, dolor, zona_dolor, altura, peso, animo, notas_conducta) VALUES (?,?,?,?,?,?,?,?)"); s.setInt(1,sueno); s.setInt(2,energia); s.setInt(3,dolor); s.setString(4,zona); s.setInt(5, altura); s.setDouble(6, peso); s.setInt(7, animo); s.setString(8, notas); s.executeUpdate() } finally { conn.close() } }
  def logTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: Int): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("INSERT INTO trainings (tipo, foco, rpe, calidad, atencion) VALUES (?,?,?,?,?)"); s.setString(1,tipo); s.setString(2,foco); s.setInt(3,rpe); s.setInt(4,calidad); s.setInt(5, atencion); s.executeUpdate(); conn.createStatement().executeUpdate("UPDATE gear SET usos_actuales = usos_actuales + 1 WHERE activo = TRUE") } finally { conn.close() } }
  def getActiveGear(): List[GearItem] = { var l=List[GearItem](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT id, nombre, tipo, usos_actuales, vida_util_estimada FROM gear WHERE activo = TRUE ORDER BY tipo DESC"); while(rs.next()) { val (u,max)=(rs.getInt("usos_actuales"),rs.getInt("vida_util_estimada")); val pct=if(max>0)(u.toDouble/max.toDouble)*100 else 0; val est=if(pct>=90)"Critico" else if(pct>=75)"Desgastado" else "Optimo"; l=l:+GearItem(rs.getInt("id"),rs.getString("nombre"),rs.getString("tipo"),u,max,est) } } finally { conn.close() }; l }
  def addNewGear(nombre: String, tipo: String, vida: Int): Unit = { val conn=getConnection(); try { val r=conn.prepareStatement("UPDATE gear SET activo=FALSE WHERE tipo=? AND activo=TRUE"); r.setString(1,tipo); r.executeUpdate(); val a=conn.prepareStatement("INSERT INTO gear (nombre, tipo, vida_util_estimada, usos_actuales, activo) VALUES (?,?,?,0,TRUE)"); a.setString(1,nombre); a.setString(2,tipo); a.setInt(3,vida); a.executeUpdate() } finally { conn.close() } }
  def getRivalScouting(rivalBusqueda: String): (List[MatchLog], Map[String, Int]) = { var matches = List[MatchLog](); var stats = scala.collection.mutable.Map("pj"->0, "gf"->0, "gc"->0, "ganados"->0, "empatados"->0, "perdidos"->0); val conn = getConnection(); try { val query = s"SELECT id, rival, goles_favor, goles_contra, minutos, nota, TO_CHAR(fecha, 'DD/MM/YYYY') as f, clima, notas_partido, video_url, reaccion_goles FROM matches WHERE LOWER(rival) LIKE LOWER(?) ORDER BY fecha DESC"; val stmt = conn.prepareStatement(query); stmt.setString(1, s"%$rivalBusqueda%"); val rs = stmt.executeQuery(); while(rs.next()) { val (gf, gc) = (rs.getInt("goles_favor"), rs.getInt("goles_contra")); val cl=Option(rs.getString("clima")).getOrElse(""); val nt=Option(rs.getString("notas_partido")).getOrElse(""); val vd=Option(rs.getString("video_url")).getOrElse(""); val re=Option(rs.getString("reaccion_goles")).getOrElse(""); matches = matches :+ MatchLog(rs.getInt("id"), rs.getString("rival"), s"$gf-$gc", rs.getInt("minutos"), rs.getDouble("nota"), rs.getString("f"), cl, nt, vd, re); stats("pj")+=1; stats("gf")+=gf; stats("gc")+=gc; if(gf>gc) stats("ganados")+=1 else if(gf==gc) stats("empatados")+=1 else stats("perdidos")+=1 } } finally { conn.close() }; (matches, stats.toMap) }
  def getSeasonObjectives(): List[Objective] = { var l=List[Objective](); val conn=getConnection(); try { val rsObj=conn.createStatement().executeQuery("SELECT id, tipo, objetivo, descripcion FROM objectives"); val objs=new scala.collection.mutable.ListBuffer[(Int,String,Int,String)](); while(rsObj.next()) objs+=((rsObj.getInt("id"),rsObj.getString("tipo"),rsObj.getInt("objetivo"),rsObj.getString("descripcion"))); val rsStats=conn.createStatement().executeQuery("SELECT COUNT(*) as pj, COUNT(CASE WHEN goles_contra=0 THEN 1 END) as cs, AVG(nota) as media FROM matches"); var (cs,pj,md)=(0,0,0.0); if(rsStats.next()){cs=rsStats.getInt("cs");pj=rsStats.getInt("pj");md=rsStats.getDouble("media")}; objs.foreach { case (id,t,m,d) => val act=t match { case "CleanSheets"=>cs.toDouble case "MediaNota"=>md case "PartidosJugados"=>pj.toDouble case _=>0.0 }; l=l:+Objective(id,t,act,m,d) } } finally { conn.close() }; l }
  def updateObjective(id: Int, nuevo: Int): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE objectives SET objetivo=? WHERE id=?"); s.setInt(1,nuevo); s.setInt(2,id); s.executeUpdate() } finally { conn.close() } }
  def getBackupCSV(): String = { val sb=new StringBuilder(); sb.append("Fecha,Rival,GF,GC,Min,Nota,Clima,Notas,Reaccion\n"); getMatchesList().foreach(m => sb.append(s"${m.fecha},${m.rival},${m.resultado},${m.minutos},${m.nota},${m.clima},${m.notas.replace(",",";")},${m.reaccion.replace(",",";")}\n")); sb.toString() }
  def getCareerSummary(): List[SeasonSummary] = { var l=List[SeasonSummary](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT s.id, s.categoria, s.club_escudo_url, s.foto_jugador_url, s.media, COUNT(m.id) as pj, COALESCE(SUM(m.goles_contra),0) as gc, COUNT(CASE WHEN m.goles_contra=0 THEN 1 END) as cs FROM seasons s LEFT JOIN matches m ON s.id=m.season_id GROUP BY s.id, s.categoria, s.club_escudo_url, s.foto_jugador_url, s.media ORDER BY s.id DESC"); while(rs.next()) l=l:+SeasonSummary(rs.getInt("id"), Option(rs.getString("categoria")).getOrElse("Temp"), Option(rs.getString("club_escudo_url")).getOrElse(""), Option(rs.getString("foto_jugador_url")).getOrElse(""), rs.getInt("pj"), rs.getInt("gc"), rs.getInt("cs"), rs.getInt("media")) } finally { conn.close() }; l }
  def startNewSeason(cat: String): String = { val conn=getConnection(); try { val l=getLatestCardData(); val s=conn.prepareStatement("INSERT INTO seasons (categoria, foto_jugador_url, club_escudo_url, media, stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos, nombre_club) VALUES (?,?,?,?,?,?,?,?,?,?,?)"); s.setString(1,cat); s.setString(2,l.fotoUrl); s.setString(3,l.clubUrl); s.setDouble(4,l.media.toDouble); s.setDouble(5,l.divRaw); s.setDouble(6,l.hanRaw); s.setDouble(7,l.kicRaw); s.setDouble(8,l.refRaw); s.setDouble(9,l.spdRaw); s.setDouble(10,l.posRaw); s.setString(11, l.clubNombre); s.executeUpdate(); s"Etapa $cat iniciada" } finally { conn.close() } }
  private def getDefaultData() = PlayerCardData("HECTOR", 59, "GK", "", "", "", "Club", 80, 60, 55, 60, 62, 58, 80.0, 60.0, 55.0, 60.0, 62.0, 58.0)
  def importMatchesCSV(csvData: String): String = { var count = 0; val lines = csvData.split("\n").map(_.trim).filter(_.nonEmpty); val dataLines = if (lines.headOption.exists(_.toLowerCase.contains("rival"))) lines.tail else lines; val today = LocalDate.now().toString; dataLines.foreach { line => try { val p = line.split(",").map(_.trim); if (p.length >= 6) { val rival = p(0); val gf = p(1).toInt; val gc = p(2).toInt; val min = p(3).toInt; val nota = p(4).toDouble; val paradas = p(5).toInt; val clima = if(p.length > 6) p(6) else "Sol"; val notas = if(p.length > 7) p(7) else "Importado"; val reaccion = if(p.length > 8) p(8) else ""; val c = getLatestCardData(); val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas); updateStats(n); val m = (n.divRaw * 0.2 + n.hanRaw * 0.2 + n.refRaw * 0.2 + n.posRaw * 0.2 + n.spdRaw * 0.05 + n.kicRaw * 0.15); logMatch(rival, gf, gc, min, nota, m, paradas, "", "", "", 0, 0, 0, clima, 20, notas, "", reaccion, today); count += 1 } } catch { case e: Exception => println(s"Error import line: $line") } }; s"Importados $count partidos" }
  def importWellnessCSV(csvData: String): String = { var count = 0; val lines = csvData.split("\n").map(_.trim).filter(_.nonEmpty); val dataLines = if (lines.headOption.exists(_.toLowerCase.contains("sueno"))) lines.tail else lines; dataLines.foreach { line => try { val p = line.split(",").map(_.trim); if (p.length >= 3) { val sueno = p(0).toInt; val energia = p(1).toInt; val dolor = p(2).toInt; val zona = if(p.length > 3) p(3) else ""; logWellness(sueno, energia, dolor, zona, 0, 0.0, 3, ""); count += 1 } } catch { case e: Exception => println(s"Error import wellness: $line") } }; s"Importados $count registros wellness" }

}
