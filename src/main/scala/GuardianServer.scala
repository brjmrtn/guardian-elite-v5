import cask._
import upickle.default._
import scalatags.Text.all._
import scalatags.Text.tags2
import java.net.URLEncoder
import cask.model.FormValue


object GuardianServer extends cask.MainRoutes {
  override def host: String = "0.0.0.0"
  override def port: Int = sys.env.getOrElse("PORT", "8081").toInt

  // --- CONFIGURACIÓN DE SEGURIDAD (Render Environment Ready) ---
  val authUser = sys.env.getOrElse("GUARDIAN_USER", "admin")
  val authPass = sys.env.getOrElse("GUARDIAN_PASS", "hector2026")
  val sessionCookieName = "guardian_session"

  // --- EL PORTERO (MIDDLEWARE) ---
  // Bloquea el acceso a cualquier ruta si no existe la cookie de sesión activa.
  // Cambiamos Response[String] por Response[_]
  def withAuth(request: cask.Request)(block: => cask.Response[Array[Byte]]): cask.Response[Array[Byte]] = {
    val isAuthenticated = request.cookies.get(sessionCookieName).exists(_.value == "active")
    if (isAuthenticated) {
      block
    } else {
      // Guardamos la ruta actual para volver después del login
      val currentPath = request.exchange.getRequestPath
      val red = cask.Redirect(s"/login?next=$currentPath")
      cask.Response(Array.empty[Byte], red.statusCode, red.headers ++ Seq("Cache-Control" -> "no-store, no-cache, must-revalidate"), red.cookies)
    }
  }

  def fixEncoding(s: String): String = { try { if (s.contains("Ã")) new String(s.getBytes("ISO-8859-1"), "UTF-8") else s } catch { case e: Exception => s } }

  // ==========================================
  // RUTAS DE ACCESO (ABIERTAS)
  // ==========================================

  @cask.get("/login")
  def loginPage(error: Boolean = false, next: String = "/") = { // 1. Aceptamos el parámetro 'next'
    val content = basePage("login", div(cls:="container d-flex justify-content-center align-items-center", style:="height: 80vh",
      div(cls:="card bg-dark border-info p-4 shadow", style:="width: 350px",
        h3(cls:="text-info text-center mb-4", "🛡️ GUARDIAN LOGIN"),
        if(error) div(cls:="alert alert-danger small p-2", "Credenciales incorrectas") else "",
        form(action:="/login", method:="post",
          // 2. AÑADIMOS ESTA LÍNEA (Campo oculto para no perder la ruta):
          input(tpe:="hidden", name:="next", value:=next),

          div(cls:="mb-3",
            label(cls:="text-muted small fw-bold", "USUARIO"),
            input(tpe:="text", name:="user", cls:="form-control bg-dark text-white border-secondary", required:=true)
          ),
          div(cls:="mb-3",
            label(cls:="text-muted small fw-bold", "CONTRASEÑA"),
            input(tpe:="password", name:="pass", cls:="form-control bg-dark text-white border-secondary", required:=true)
          ),
          button(tpe:="submit", cls:="btn btn-info w-100 fw-bold", "ENTRAR AL SISTEMA")
        )
      )
    ))
    renderHtml(content)
  }

  @cask.postForm("/login")
  def doPostLogin(user: String, pass: String, next: String ="/") = {
    if (user == authUser && pass == authPass) {
      cask.Response(
        data = "Redirigiendo...",
        statusCode = 302,
        headers = Seq(
          "Location" -> next,
          // Aumentamos a 24h (86400) y añadimos SameSite=Lax para evitar bloqueos del navegador
          "Set-Cookie" -> s"$sessionCookieName=active; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
        )
      )
    } else {
      cask.Redirect("/login?error=true")
    }
  }

  @cask.get("/logout")
  def doLogout() = {
    cask.Response(
      data = "",
      statusCode = 302,
      headers = Seq(
        "Location" -> "/login",
        "Set-Cookie" -> s"$sessionCookieName=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
      )
    )
  }
  // --- RENDERIZADO DE FILA DE PARTIDO ---
  def renderMatchRow(m: MatchLog) = {
    val pParts = m.resultado.split("-").map(s => try s.trim.toInt catch { case _:Exception => 0 })
    val colorClass = if(pParts.length >= 2) { if (pParts(0) > pParts(1)) "text-success" else if (pParts(0) == pParts(1)) "text-warning" else "text-danger" } else "text-muted"
    val extraBtn = if(m.video.nonEmpty) a(href:=m.video, target:="_blank", cls:="btn btn-sm btn-danger py-0 ms-1", style:="font-size:10px", "V") else span("")
    val waText = URLEncoder.encode(s"MATCH: ${m.rival} ${m.resultado}", "UTF-8").replace("+", "%20")
    val tipoBadge = if(m.tipo == "LIGA") span(cls:="badge bg-primary me-1", style:="font-size:9px", "LIGA") else if(m.tipo=="AMISTOSO") span(cls:="badge bg-secondary me-1", style:="font-size:9px", "AMIST") else span(cls:="badge bg-warning text-dark me-1", style:="font-size:9px", "TORNEO")
    val estadioInfo = if(m.estadio.nonEmpty && m.estadio != "-") span(cls:="d-block text-muted fst-italic", style:="font-size:10px", s"🏟️ ${fixEncoding(m.estadio)}") else span("")

    tr(
      td(cls:="fw-bold small", tipoBadge, fixEncoding(m.rival), extraBtn, br, span(cls:="text-muted xx-small", style:="font-size:11px", s"${m.fecha} ${m.clima}"), estadioInfo),
      td(cls:=s"text-center fw-bold $colorClass", style:="font-size:16px", m.resultado),
      td(cls:="text-center", span(cls:="badge bg-dark text-warning border border-warning", m.nota)),
      td(cls:="text-end",
        a(href:=s"https://wa.me/?text=$waText", target:="_blank", cls:="btn btn-sm btn-success me-1", style:="padding:2px 6px;", "W"),
        a(href:=s"/match/edit/${m.id}", cls:="btn btn-sm btn-outline-primary me-1", style:="padding:2px 6px;", "E"),
        a(href:=s"/match/delete/${m.id}", onclick:="return confirm('Borrar?');", cls:="btn btn-sm btn-outline-danger", style:="padding:2px 6px;", "X")
      )
    )
  }

  // ==========================================
  // 1. DASHBOARD (INICIO)
  // ==========================================
  @cask.get("/")
  def dashboard(request: cask.Request) = withAuth(request) {
    // 1. OBTENCIÓN DE DATOS Y NUEVOS MOTORES (FASE 1)
    val techAlerts = DatabaseManager.getTechnicalAlerts() // Auditoría técnica recurrente
    val weatherStats = DatabaseManager.getWeatherPerformance() // Correlación nota vs clima
    val smartInsights = DatabaseManager.getSmartInsights()
    val card = DatabaseManager.getLatestCardData()
    val matches = DatabaseManager.getMatchesList()
    val chartData = DatabaseManager.getChartData()
    val aiMessage = DatabaseManager.getDeepAnalysis()
    val cognitiveInsight = DatabaseManager.getCognitiveInsight()

    val cognitiveWidget = div(cls:="card bg-dark border-info shadow mb-3",
      div(cls:="card-header border-info text-info fw-bold py-1 text-center small", "🧠 ANALISTA COGNITIVO"),
      div(cls:="card-body p-2",
        p(cls:="text-light small mb-0 text-center fw-bold", raw(cognitiveInsight))
      )
    )
    val tac = DatabaseManager.getTacticalStats()
    val objs = DatabaseManager.getSeasonObjectives()
    val upcoming = DatabaseManager.getUpcomingMatches().headOption

    // 2. CÁLCULOS DE TENDENCIAS Y XP
    val last5 = matches.take(5)
    val avgLast5 = if (last5.nonEmpty) last5.map(_.nota).sum / last5.length else 0.0
    val avgSeason = if (matches.nonEmpty) matches.map(_.nota).sum / matches.length else 0.0
    val trendDiff = avgLast5 - avgSeason
    val trendColor = if (trendDiff > 0) "text-success" else if (trendDiff < 0) "text-danger" else "text-muted"

    val radarData = s"""[${card.div}, ${card.han}, ${card.kic}, ${card.ref}, ${card.spd}, ${card.pos}]"""
    val rawMedia = (card.divRaw * 0.20) + (card.hanRaw * 0.20) + (card.kicRaw * 0.15) + (card.refRaw * 0.20) + (card.spdRaw * 0.05) + (card.posRaw * 0.20)
    val xpPercent = ((rawMedia - rawMedia.floor) * 100).toInt

    // 3. ESTADO FÍSICO (ACWR)
    val acute = DatabaseManager.getWorkloads(7)
    val chronic = DatabaseManager.getWorkloads(28)
    val acwr = StatsCalculator.calculateACWR(acute, chronic)
    val (acwrColor, acwrText) = if(acwr > 2.0) ("text-danger", "RIESGO ALTO")
    else if(acwr > 1.5) ("text-warning", "SOBRECARGA")
    else ("text-success", "ÓPTIMO")

    // 4. MÉTODOS AUXILIARES Y CÁLCULOS TÁCTICOS
    // Definimos pct una sola vez como valor interno para evitar "ambiguous reference"
    val calculatePct = (n: Double, d: Double) => if(d > 0) ((n/d)*100).toInt else 0
    def tactCell(label: String, valPct: Int, colorBg: String) = div(cls:=s"flex-fill text-center p-1 border border-secondary $colorBg", style:="font-size: 10px; color: black; font-weight: 800;", div(label), div(s"$valPct%"))

    val totG = if(tac("g_tot") > 0) tac("g_tot").toDouble else 1.0
    val (ga, gm, gr) = (calculatePct(tac("g_alt"), totG), calculatePct(tac("g_med"), totG), calculatePct(tac("g_ras"), totG))
    val (gl, gc_tact, gd) = (calculatePct(tac("g_izq"), totG), calculatePct(tac("g_cen"), totG), calculatePct(tac("g_der"), totG))

    val totP = if(tac("p_tot") > 0) tac("p_tot").toDouble else 1.0
    val (pa, pm, pr) = (calculatePct(tac("p_alt"), totP), calculatePct(tac("p_med"), totP), calculatePct(tac("p_ras"), totP))
    val (pl, pc_tact, pd) = (calculatePct(tac("p_izq"), totP), calculatePct(tac("p_cen"), totP), calculatePct(tac("p_der"), totP))

    // --- WIDGETS DINÁMICOS ---

    val nextMatchWidget = upcoming match {
      case Some(m) => div(cls:="alert alert-dark border-warning shadow p-3 mb-3", div(cls:="d-flex justify-content-between align-items-center", div(h6(cls:="text-muted mb-0 small", "PROXIMO PARTIDO"), h4(cls:="text-white fw-bold mb-0", fixEncoding(m.rival))), div(cls:="text-end", span(cls:="badge bg-warning text-dark", m.fecha), a(href:=s"/match-center?scheduleId=${m.id}", cls:="btn btn-sm btn-outline-light ms-2", "JUGAR"))))
      case None => div(cls:="alert alert-dark border-secondary p-2 mb-3 text-center text-muted small", "Sin partidos programados.")
    }

    val techAuditorWidget = div(cls:="card bg-dark border-warning mb-3 shadow",
      div(cls:="card-header bg-warning text-dark small fw-bold text-center", "📋 PLAN DE MEJORA (AUDITOR)"),
      div(cls:="card-body p-2",
        if(techAlerts.isEmpty) div(cls:="text-center p-2", span(cls:="text-success", "✅ Técnica estable"), br, span(cls:="xx-small text-muted", "Sin fallos recurrentes detectados"))
        else ul(cls:="list-unstyled mb-0", for(alert <- techAlerts) yield li(cls:="border-bottom border-secondary py-1 small text-white", span(cls:="text-warning me-2", "⚡"), alert))
      )
    )

    val weatherPerformanceWidget = if(weatherStats.nonEmpty) {
      div(cls:="card bg-dark border-info shadow mb-3",
        div(cls:="card-header border-info text-info fw-bold py-1 text-uppercase text-center small", "🌤️ RENDIMIENTO POR CLIMA"),
        div(cls:="card-body p-0",
          table(cls:="table table-dark table-sm mb-0 xx-small text-center",
            thead(tr(th("Clima"), th("Nota"), th("GC"))),
            tbody(for((clima, (nota, gc)) <- weatherStats.toSeq.take(3)) yield tr(td(clima), td(cls:="text-warning", f"$nota%1.1f"), td(cls:="text-danger", f"$gc%1.1f")))
          )
        )
      )
    } else div()

    // En GuardianServer.scala, dentro del dashboard:
    val cognitiveStatus = DatabaseManager.getCognitiveInsight()
    // Vault Medico
    val lastMedical = DatabaseManager.getLatestMedicalInsight() // Implementar consulta en DB

    val medicalAlertWidget = if(lastMedical.nonEmpty) {
      div(cls:="alert alert-danger border-danger shadow p-3 mb-3",
        div(cls:="d-flex align-items-center",
          span(style:="font-size: 24px; margin-right: 10px;", "🏥"),
          div(
            strong(cls:="text-danger", "ALERTA MÉDICA"),
            div(cls:="small fw-bold", lastMedical)
          )
        )
      )
    } else div()


    // --- RENDERIZADO FINAL ---

    val content = basePage("home", div(cls := "row justify-content-center",
      div(cls := "col-md-5 mb-4",
        div(cls := "d-flex justify-content-center mobile-scale",
          div(cls := "fut-card",
            div(cls := "left-info", div(cls := "rating", card.media), div(cls := "position", card.posicion), img(src := card.flagUrl, cls := "nation")),
            img(src := card.clubUrl, cls := "club-badge"),
            div(cls := "player-circle-container", img(src := card.fotoUrl, cls := "player-img")),
            div(cls := "name-container", div(cls := "player-name", card.nombre), div(style:="font-size:12px; margin-top:-5px; opacity:0.9; font-weight:bold;", card.clubNombre)),
            div(cls := "stats-container", div(cls := "stats-grid",
              div(cls := "stat-item", span(cls:="stat-val", card.div), span(cls:="stat-label", "DIV")),
              div(cls := "stat-item", span(cls:="stat-val", card.kic), span(cls:="stat-label", "KIC")),
              div(cls := "stat-item", span(cls:="stat-val", card.spd), span(cls:="stat-label", "SPD")),
              div(cls := "stat-item", span(cls:="stat-val", card.han), span(cls:="stat-label", "HAN")),
              div(cls := "stat-item", span(cls:="stat-val", card.ref), span(cls:="stat-label", "REF")),
              div(cls := "stat-item", span(cls:="stat-val", card.pos), span(cls:="stat-label", "POS")))))),

        div(cls:="mt-3 mb-4 text-center",
          div(cls:="d-flex justify-content-between text-white xx-small px-4", span(s"Nivel ${card.media}"), span(s"${xpPercent}% XP"), span(s"Nivel ${card.media+1}")),
          div(cls:="progress mx-4", style:="height: 8px; background-color: #333;", div(cls:="progress-bar bg-warning", style:=s"width: $xpPercent%"))),

        techAuditorWidget,

        div(cls:="row g-2 mb-3",
          div(cls:="col-6", a(href:="/scouting", cls:="btn btn-outline-info w-100 shadow fw-bold d-flex flex-column align-items-center py-2", span(style:="font-size:20px;", "🔍"), span(style:="font-size:10px;", "SCOUTING"))),
          div(cls:="col-6", a(href:="/penalties", cls:="btn btn-outline-danger w-100 shadow fw-bold d-flex flex-column align-items-center py-2", span(style:="font-size:20px;", "🥅"), span(style:="font-size:10px;", "PENALTIS")))
        ),

        div(cls:="card bg-dark border-secondary shadow p-3 mb-3",
          div(cls:="d-flex justify-content-between align-items-center",
            div(h6(cls:="text-muted mb-0 small fw-bold", "RACHA (5)"), h3(cls:=s"mb-0 $trendColor fw-bold", f"$avgLast5%2.2f")),
            div(cls:="text-end", span(cls:="small text-muted fw-bold", "CARGA ACWR"), div(cls:=s"fw-bold $acwrColor", f"$acwr%1.2f")))
        )
      ),

      div(cls := "col-md-5",
        nextMatchWidget,
        cognitiveWidget,
        div(cls := "alert alert-dark border-info shadow p-3 mb-3", div(cls:="d-flex align-items-center mb-2", span(style:="font-size: 24px; margin-right: 10px;", "🧠"), strong(cls:="text-info", "IA NEURO-SCOUT")), div(cls:="text-light small fst-italic lh-sm fw-bold", raw(aiMessage))),

        weatherPerformanceWidget,

        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header border-secondary text-warning fw-bold py-1 text-center small", "SCOUTING RADAR"), div(cls := "card-body p-1 d-flex justify-content-center", div(style:="width: 200px; height: 200px;", canvas(id := "radarChart")))),

        div(cls := "card bg-dark text-white border-danger shadow mb-3", div(cls := "card-header border-danger text-danger fw-bold py-1 text-center small", "🕵️ INTELIGENCIA DE DATOS"), div(cls := "card-body p-2", raw(smartInsights))),

        div(cls:="row mt-3",
          div(cls:="col-6 pe-1", div(cls:="card bg-dark border-danger shadow p-1", h6(cls:="text-center text-danger mb-1 xx-small fw-bold", "GOLES RECIBIDOS"), div(cls:="d-flex mb-1", tactCell("A", ga, "bg-danger bg-opacity-75"), tactCell("M", gm, "bg-warning bg-opacity-75"), tactCell("B", gr, "bg-light bg-opacity-75")), div(cls:="d-flex", tactCell("I", gl, "bg-danger bg-opacity-75"), tactCell("C", gc_tact, "bg-warning bg-opacity-75"), tactCell("D", gd, "bg-danger bg-opacity-75")))),
          div(cls:="col-6 ps-1", div(cls:="card bg-dark border-success shadow p-1", h6(cls:="text-center text-success mb-1 xx-small fw-bold", "PARADAS"), div(cls:="d-flex mb-1", tactCell("A", pa, "bg-success bg-opacity-75"), tactCell("M", pm, "bg-info bg-opacity-75"), tactCell("B", pr, "bg-light bg-opacity-75")), div(cls:="d-flex", tactCell("I", pl, "bg-success bg-opacity-75"), tactCell("C", pc_tact, "bg-info bg-opacity-75"), tactCell("D", pd, "bg-success bg-opacity-75"))))))
    )
    , script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""const ctxRadar=document.getElementById('radarChart');if(ctxRadar){new Chart(ctxRadar,{type:'radar',data:{labels:['DIV','HAN','KIC','REF','SPD','POS'],datasets:[{data:$radarData,backgroundColor:'rgba(212,175,55,0.4)',borderColor:'#d4af37',borderWidth:2}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{r:{angleLines:{color:'#444'},grid:{color:'#444'},pointLabels:{color:'#fff',font:{size:10}},ticks:{display:false},suggestedMin:40,suggestedMax:90}}}});""")))
    renderHtml(content)
  }

  // --- 2. MATCH CENTER (JUGAR) - VERSIÓN CORREGIDA 4.1 ---
  @cask.get("/match-center")
  def matchCenterPage(request: cask.Request, scheduleId: Int = 0) = withAuth(request) {
    val today = java.time.LocalDate.now().toString
    var preRival = ""; var preFecha = today; var isScheduled = false; var preEstadio = ""

    // Si venimos de un partido programado, cargamos datos
    if(scheduleId > 0) {
      val matches = DatabaseManager.getUpcomingMatches()
      matches.find(_.id == scheduleId).foreach { m =>
        preRival = m.rival
        preFecha = m.fecha
        preEstadio = m.estadio
        isScheduled = true
      }
    }

    // Celdas de la portería
    val gridCells = for(r <- Seq("T","M","B"); c <- Seq("L","C","R")) yield {
      val zoneId = r + c
      div(cls:=s"goal-cell zone-$zoneId", onclick:=s"registerAction('$zoneId')", span(cls:="action-marker", ""))
    }

    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-6 col-12",
          div(cls := "card bg-dark text-white border-warning shadow",
            div(cls := "card-header bg-warning text-dark fw-bold text-center", "MATCH TRACKER PRO"),
            div(cls := "card-body p-3",
              form(action := "/match-center/save", method := "post", attr("accept-charset") := "UTF-8",

                // 1. DATOS GENERALES
                input(tpe:="hidden", name:="scheduleId", value:=scheduleId.toString),
                div(cls:="mb-3", label(cls:="form-label text-white fw-bold small", "TIPO DE PARTIDO"),
                  if(isScheduled) {
                    div(input(tpe:="hidden", name:="tipo", value:="LIGA"), input(tpe:="text", cls:="form-control bg-dark text-white border-primary fw-bold", value:="🏆 LIGA (OFICIAL RFFM)", readonly:=true))
                  } else {
                    div(cls:="d-flex", select(name:="tipo", cls:="form-select bg-dark text-white fw-bold flex-grow-1", option(value:="AMISTOSO", "🤝 AMISTOSO"), option(value:="TORNEO", "🏅 TORNEO"), option(value:="LIGA", "🏆 LIGA (Manual)")), a(href:="/tournament/new", cls:="btn btn-sm btn-outline-warning ms-2 d-flex align-items-center fw-bold", "➕ CREAR TORNEO"))
                  }
                ),
                div(cls := "mb-3",
                  label(cls := "form-label text-warning fw-bold small", "RIVAL"),
                  input(
                    tpe := "text",
                    name := "rival",
                    id := "rivalInput",
                    cls := "form-control form-control-lg fw-bold text-white",
                    value := (if (preRival.nonEmpty) fixEncoding(preRival) else ""),
                    placeholder := "Ej: Rayo Vallecano",
                    required := true,
                    // CAMBIO CLAVE: Si scheduleId es 0, no se renderiza ningún atributo readonly
                    if (scheduleId > 0) readonly := true else ()
                  )
                ),
                div(cls := "mb-3", label(cls := "form-label text-white fw-bold small", "FECHA"), input(tpe := "date", name := "fecha", cls := "form-control", value := preFecha)),
                div(cls:="mb-3", label(cls:="form-label text-white fw-bold small", "ESTADIO / CAMPO"), input(tpe:="text", name:="estadio", cls:="form-control bg-dark text-white", value:=fixEncoding(preEstadio), placeholder:="Ej: Valdebebas Campo 3")),

                // 2. MARCADOR Y PARADAS
                div(cls := "row mb-3 bg-secondary bg-opacity-25 p-2 rounded mx-0",
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "GOLES (GC)"), input(tpe := "number", name := "gc", id:="gcInput", cls := "form-control text-center bg-danger text-white border-0 fw-bold fs-4", value := "0", readonly:=true)),
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "PARADAS"), input(tpe := "number", name := "paradas", id:="parInput", cls := "form-control text-center bg-success text-white border-0 fw-bold fs-4", value := "0", readonly:=true)),
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "A FAVOR (GF)"), input(tpe := "number", name := "gf", cls := "form-control text-center", value := "0", attr("inputmode"):="numeric"))
                ),

                // 3. DISTRIBUCIÓN (EDERSON)
                div(cls:="mb-4 p-2 border border-info rounded bg-info bg-opacity-10", label(cls:="form-label text-info small fw-bold w-100 text-center", "DISTRIBUCIÓN"),
                  div(cls:="row mb-2 align-items-center", div(cls:="col-4 text-end small fw-bold", "CORTO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pc', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pc', false)", "❌"), input(tpe:="text", id:="display_pc", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true)))),
                  div(cls:="row align-items-center", div(cls:="col-4 text-end small fw-bold", "LARGO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pl', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pl', false)", "❌"), input(tpe:="text", id:="display_pl", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true))))
                ),
                input(tpe:="hidden", name:="passData", id:="passData", value:="0,0,0,0"), input(tpe:="hidden", id:="pcTot", value:="0"), input(tpe:="hidden", id:="pcOk", value:="0"), input(tpe:="hidden", id:="plTot", value:="0"), input(tpe:="hidden", id:="plOk", value:="0"),

                // 4. PORTERÍA (REJILLA 3x3)
                div(cls:="tactical-section mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  div(cls:="d-flex justify-content-center mb-2", div(cls:="btn-group w-100", role:="group", input(tpe:="radio", cls:="btn-check", name:="mode", id:="modeSave", autocomplete:="off", checked:=true, onclick:="setMode('save')"), label(cls:="btn btn-outline-success fw-bold", attr("for"):="modeSave", "MODO PARADA"), input(tpe:="radio", cls:="btn-check", name:="mode", id:="modeGoal", autocomplete:="off", onclick:="setMode('goal')"), label(cls:="btn btn-outline-danger fw-bold", attr("for"):="modeGoal", "MODO GOL"))),
                  div(cls:="goal-grid-3x3", gridCells),
                  input(tpe:="hidden", name:="zonaGoles", id:="hiddenGoles"), input(tpe:="hidden", name:="zonaParadas", id:="hiddenParadas"),
                  div(cls:="text-center mt-2 small text-muted", "Toca la zona para registrar la accion"),

                  label(cls:="form-label text-white small fw-bold w-100 text-center mt-3 border-top pt-2", "ACCIONES"),
                  div(cls:="row g-2",
                    div(cls:="col-4", div(cls:="d-grid", button(tpe:="button", cls:="btn btn-outline-info btn-sm", onclick:="incCounter('p1v1')", "1vs1"), input(tpe:="text", id:="disp_p1v1", value:="0", cls:="form-control form-control-sm text-center mt-1 bg-dark text-white border-0", readonly:=true))),
                    div(cls:="col-4", div(cls:="d-grid", button(tpe:="button", cls:="btn btn-outline-warning btn-sm", onclick:="incCounter('pAir')", "Aereo"), input(tpe:="text", id:="disp_pAir", value:="0", cls:="form-control form-control-sm text-center mt-1 bg-dark text-white border-0", readonly:=true))),
                    div(cls:="col-4", div(cls:="d-grid", button(tpe:="button", cls:="btn btn-outline-light btn-sm", onclick:="incCounter('pPie')", "Pie"), input(tpe:="text", id:="disp_pPie", value:="0", cls:="form-control form-control-sm text-center mt-1 bg-dark text-white border-0", readonly:=true)))
                  ),
                  input(tpe:="hidden", name:="actionData", id:="actionData", value:="0,0,0"), input(tpe:="hidden", id:="cnt_p1v1", value:="0"), input(tpe:="hidden", id:="cnt_pAir", value:="0"), input(tpe:="hidden", id:="cnt_pPie", value:="0"),

                  label(cls:="form-label text-white small fw-bold w-100 text-center mt-3", "ZONAS DE ATAQUE (Tiros)"),
                  div(cls:="shot-origin d-flex gap-2 justify-content-center", div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Left')", "Izquierda"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Center')", "Centro"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Right')", "Derecha"), input(tpe:="hidden", name:="zonaTiros", id:="hiddenOrigin"))
                ),

                // 5. NUEVO: MAPA DE CALOR DE CAMPO (AQUÍ ESTÁ LA INTEGRACIÓN)
                div(cls:="mb-4 p-2 border border-success rounded bg-success bg-opacity-10",
                  label(cls:="form-label text-success small fw-bold w-100 text-center", "MAPA DE CALOR (INTERVENCIONES)"),
                  div(cls:="position-relative mx-auto shadow", style:="width: 280px; height: 380px; background-color: #2e7d32; border: 2px solid white; border-radius: 4px;",
                    div(style:="position:absolute; top:0; left:50%; transform:translateX(-50%); width:60%; height:15%; border:2px solid rgba(255,255,255,0.6); border-top:none;"),
                    div(style:="position:absolute; bottom:0; left:50%; transform:translateX(-50%); width:60%; height:15%; border:2px solid rgba(255,255,255,0.6); border-bottom:none;"),
                    div(style:="position:absolute; top:50%; width:100%; height:2px; background:rgba(255,255,255,0.4);"),
                    div(style:="position:absolute; top:50%; left:50%; transform:translate(-50%,-50%); width:60px; height:60px; border:2px solid rgba(255,255,255,0.4); border-radius:50%;"),
                    div(id:="fieldMap", style:="width:100%; height:100%; cursor:crosshair; z-index:10;", onclick:="regFieldPos(event)")
                  ),
                  div(cls:="text-center mt-1 small text-muted", "Toca donde intervino (Parada/Corte/Pase)"),
                  input(tpe:="hidden", name:="mapaCampo", id:="hiddenFieldMap", value:="")
                ),

                // 6. ENTORNO Y NOTAS
                div(cls:="mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  label(cls:="form-label text-white small fw-bold w-100 text-center", "ENTORNO"),
                  div(cls:="row mb-2",
                    div(cls:="col-6", label(cls:="small text-muted fw-bold", "Clima"), select(name:="clima", cls:="form-select form-select-sm bg-dark text-white fw-bold", option(value:="Sol", "Sol"), option(value:="Nubes", "Nubes"), option(value:="Lluvia", "Lluvia"), option(value:="Nublado", "Nublado"), option(value:="Frio", "Frio"), option(value:="Viento", "Viento"))),
                    div(cls:="col-6", label(cls:="small text-muted fw-bold", "Temp (C)"), input(tpe:="number", name:="temp", cls:="form-control form-control-sm bg-dark text-white fw-bold", value:="20"))
                  )
                ),
                div(cls:="mb-3 p-2 border border-danger rounded bg-danger bg-opacity-10", label(cls:="form-label text-danger small fw-bold w-100 text-center", "SALA DE VIDEO"), input(tpe:="url", name:="video", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="Link Video (Youtube/Drive)")),
                div(cls:="mb-3", label(cls:="form-label text-white small fw-bold", "ANOTACIONES DEL ENTRENADOR"), textarea(name:="notas", cls:="form-control form-control-sm bg-dark text-white fw-bold", rows:="3", placeholder:="Notas generales: Saques, posicionamiento, lectura del juego, voz de mando...")),
                div(cls:="mb-4", label(cls:="form-label text-danger small fw-bold", "ANALISIS GOLES / REACCION"), textarea(name:="reaccion", cls:="form-control form-control-sm bg-dark text-white border-danger fw-bold", rows:="3", placeholder:="Descripcion goles encajados y reaccion mental posterior.")),
                div(cls := "mb-3", label(cls := "form-label small fw-bold", "MINUTOS"), input(tpe := "number", name := "minutos", cls := "form-control fw-bold", value := "40", attr("inputmode") := "numeric")),
                div(cls := "mb-4", label(cls := "form-label text-warning fw-bold small", "NOTA (0-10)"), input(tpe := "number", step := "0.1", name := "nota", cls := "form-control form-control-lg text-center fw-bold", placeholder := "Ej: 7.5", required := true, attr("inputmode") := "decimal")),

                div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg py-3 fw-bold", "GUARDAR PARTIDO"))
              ) // fin form
            ),

            // SCRIPTS
            script(raw("""
              var currentMode='save';var goals=[];var saves=[];var origins=[];
              function setMode(mode){currentMode=mode;}
              function registerAction(zone){const cell=document.querySelector('.zone-'+zone);const marker=cell.querySelector('.action-marker');if(currentMode==='save'){saves.push(zone);marker.innerHTML+='<span style="color:#198754; font-weight:bold;">●</span>';document.getElementById('parInput').value=parseInt(document.getElementById('parInput').value||0)+1;document.getElementById('hiddenParadas').value=saves.join(',');}else{goals.push(zone);marker.innerHTML+='<span style="color:#dc3545; font-weight:bold;">●</span>';document.getElementById('gcInput').value=parseInt(document.getElementById('gcInput').value||0)+1;document.getElementById('hiddenGoles').value=goals.join(',');}}
              function incCounter(key){var el=document.getElementById('cnt_'+key); var val=parseInt(el.value||0)+1; el.value=val; document.getElementById('disp_'+key).value=val; updateActionData();}
              function updateActionData(){var d = [document.getElementById('cnt_p1v1').value, document.getElementById('cnt_pAir').value, document.getElementById('cnt_pPie').value]; document.getElementById('actionData').value = d.join(',');}
              function toggleOrigin(el,origin){el.classList.toggle('active');el.classList.toggle('btn-warning');if(origins.includes(origin)){origins=origins.filter(o=>o!==origin);}else{origins.push(origin);}document.getElementById('hiddenOrigin').value=origins.join(',');}
              function pass(type, success) { var totEl = document.getElementById(type+'Tot'); var okEl = document.getElementById(type+'Ok'); var dispEl = document.getElementById('display_'+type); var t = parseInt(totEl.value)+1; var o = parseInt(okEl.value) + (success ? 1 : 0); totEl.value=t; okEl.value=o; dispEl.value = o + '/' + t; updatePassData(); }
              function updatePassData(){var d = [document.getElementById('pcTot').value, document.getElementById('pcOk').value, document.getElementById('plTot').value, document.getElementById('plOk').value]; document.getElementById('passData').value = d.join(',');}

              // SCRIPT NUEVO MAPA CAMPO
              var fieldPoints = [];
              function regFieldPos(e) {
                var rect = e.target.getBoundingClientRect();
                var x = e.clientX - rect.left;
                var y = e.clientY - rect.top;
                var pctX = Math.round((x / rect.width) * 100);
                var pctY = Math.round((y / rect.height) * 100);
                fieldPoints.push(pctX + ":" + pctY);
                document.getElementById('hiddenFieldMap').value = fieldPoints.join(',');
                var dot = document.createElement('div');
                dot.style.cssText = 'position:absolute; width:10px; height:10px; background:orange; border:1px solid white; border-radius:50%; transform:translate(-50%,-50%); pointer-events:none; left:'+x+'px; top:'+y+'px;';
                e.target.appendChild(dot);
              }
            """))
          )
        )
      )
    );
    renderHtml(content)
  }

  @cask.post("/match-center/save")
  def saveMatch(request: cask.Request) = withAuth(request) {
    // CORRECCIÓN: Usamos request.data para leer los bytes del cuerpo
    val bodyBytes = request.data.readAllBytes()
    val bodyString = new String(bodyBytes, "UTF-8")

    // Parseamos el string clave=valor&clave2=valor2
    val formData = bodyString.split("&").map { part =>
      val pair = part.split("=", 2)
      val key = java.net.URLDecoder.decode(pair(0), "UTF-8")
      val value = if (pair.length > 1) java.net.URLDecoder.decode(pair(1), "UTF-8") else ""
      key -> value
    }.toMap

    // Funciones auxiliares de extracción
    def getStr(key: String): String = formData.getOrElse(key, "")
    def getInt(key: String): Int = try { getStr(key).toInt } catch { case _: Exception => 0 }
    def getDouble(key: String): Double = try { getStr(key).toDouble } catch { case _: Exception => 0.0 }

    // Los 23 parámetros (Extraídos manualmente del mapa)
    val scheduleId = getInt("scheduleId")
    val rival = getStr("rival")
    val gf = getInt("gf")
    val gc = getInt("gc")
    val minutos = getInt("minutos")
    val nota = getDouble("nota")
    val paradas = getInt("paradas")
    val zonaGoles = getStr("zonaGoles")
    val zonaTiros = getStr("zonaTiros")
    val zonaParadas = getStr("zonaParadas")
    val clima = getStr("clima")
    val estadio = getStr("estadio")
    val temp = getInt("temp")
    val notas = getStr("notas")
    val video = getStr("video")
    val reaccion = getStr("reaccion")
    val fecha = getStr("fecha")
    val mode = getStr("mode")
    val passData = getStr("passData")
    val actionData = getStr("actionData")
    val tipo = getStr("tipo")
    val mapaCampo = getStr("mapaCampo")

    // --- LÓGICA DE PROCESAMIENTO (Base de datos y cálculos) ---
    val pArr = passData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (pcTot, pcOk, plTot, plOk) = if(pArr.length >= 4) (pArr(0), pArr(1), pArr(2), pArr(3)) else (0,0,0,0)
    val aArr = actionData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (p1v1, pAir, pPie) = if(aArr.length >= 3) (aArr(0), aArr(1), aArr(2)) else (0,0,0)

    val cleanRival = fixEncoding(rival)
    val cleanNotas = fixEncoding(notas)
    val cleanReaccion = fixEncoding(reaccion)

    val c = DatabaseManager.getLatestCardData()
    val n = StatsCalculator.calculateGrowth(c, minutos, gc, nota, paradas, pcTot, pcOk, plTot, plOk)
    DatabaseManager.updateStats(n)

    if (scheduleId > 0) {
      DatabaseManager.playScheduledMatch(scheduleId, gf, gc, minutos, nota, paradas, cleanNotas, video, cleanReaccion, clima, estadio, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, pcTot, pcOk, plTot, plOk, mapaCampo)
    } else {
      DatabaseManager.logMatch(cleanRival, gf, gc, minutos, nota, n.media, paradas, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, clima, estadio, temp, cleanNotas, video, cleanReaccion, fecha, tipo, pcTot, pcOk, plTot, plOk, mapaCampo)
    }

    // Respuesta visual renderizada como Array[Byte] para cumplir con withAuth
    val d = n.media - c.media
    val msg = if(d > 0) s"SUBIDA DE NIVEL! +$d" else "Experiencia acumulada..."

    renderHtml(doctype("html")(
      html(
        head(tags2.style(raw(getCss()))),
        body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';",
          h1("OK"),
          h2(style := "color: #d4af37;", "ANÁLISIS GUARDADO"),
          div(style := "margin: 30px auto; width: 300px; background: #333; padding: 20px; border-radius: 10px;",
            h3("Media Global"),
            div(style := "font-size: 50px; font-weight: bold;", s"${c.media.toInt} -> ${n.media}"),
            p(style := "color: #ffc107;", msg)
          ),
          a(href := "/", style := "color: white; text-decoration: none; border: 1px solid white; padding: 10px 20px;", "Volver a Inicio")
        )
      )
    ).render)
  }
  @cask.get("/tournament/new") def newTournamentPage() = { val content = basePage("match-center", div(cls:="row justify-content-center", div(cls:="col-md-8", div(cls:="card bg-dark text-white border-warning shadow", div(cls:="card-header bg-warning text-dark fw-bold text-center", "🏆 NUEVO TORNEO"), div(cls:="card-body", form(action:="/tournament/create", method:="post", div(cls:="mb-3", label(cls:="form-label fw-bold", "Nombre del Torneo"), input(tpe:="text", name:="nombre", cls:="form-control fw-bold", placeholder:="Ej: Mundialito Algarve", required:=true)), div(cls:="mb-3", label(cls:="form-label fw-bold", "Cuadro / Estructura"), div(cls:="alert alert-secondary small p-2 fw-bold", "Formato por línea: FASE | RIVAL | FECHA (AAAA-MM-DD)"), textarea(name:="estructura", cls:="form-control bg-secondary text-white fw-bold", rows:="6", placeholder:="Fase Grupos | Betis | 2026-04-12\nFase Grupos | Benfica | 2026-04-12\nCuartos | ? | 2026-04-13\nFinal | ? | 2026-04-14")), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-warning fw-bold", "GENERAR CUADRO"))))), div(cls:="text-center mt-3", a(href:="/match-center", cls:="text-muted", "Cancelar"))))); cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.postForm("/tournament/create") def createTournamentAction(nombre: String, estructura: String) = { val res = DatabaseManager.createTournament(nombre, estructura); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("TORNEO CREADO"), h3(res), div(style:="margin-top:20px;", a(href:="/match-center", cls:="btn btn-warning fw-bold", "Ir a Jugar"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }

  @cask.get("/scouting") def scoutingPage(request: cask.Request, query: String = "") = withAuth(request) { val (matches, stats) = if(query.nonEmpty) DatabaseManager.getRivalScouting(query) else (List[MatchLog](), Map[String,Int]()); val rivalInfo = if(query.nonEmpty) DatabaseManager.getRivalInfo(query) else None; val rivalCardWidget = if(query.nonEmpty) { val estiloVal = rivalInfo.map(_.estilo).getOrElse("Desconocido"); val clavesVal = rivalInfo.map(_.claves).getOrElse(""); val notasVal = rivalInfo.map(_.notas).getOrElse(""); div(cls:="card bg-dark border-secondary shadow mb-4", div(cls:="card-header bg-secondary text-white fw-bold", "FICHA RIVAL"), div(cls:="card-body", form(action:="/scouting/save_rival", method:="post", input(tpe:="hidden", name:="nombre", value:=query), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Estilo"), select(name:="estilo", cls:="form-select form-select-sm bg-dark text-white fw-bold", option(value:="Desconocido","?"), option(value:="Directo","Balon Largo"), option(value:="Combinativo","Toque"), option(value:="Contra","Contraataque"), attr("value"):=estiloVal)), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Claves"), textarea(name:="claves", cls:="form-control form-control-sm bg-dark text-white fw-bold", rows:="2", fixEncoding(clavesVal))), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Notas"), textarea(name:="notas", cls:="form-control form-control-sm bg-dark text-white fw-bold", rows:="2", fixEncoding(notasVal))), button(tpe:="submit", cls:="btn btn-sm btn-outline-warning w-100 fw-bold", "Guardar Ficha")))) } else div(); val resultsWidget = if(query.nonEmpty && matches.isEmpty) div(cls:="alert alert-warning text-center", s"Sin datos vs '$query'") else if(matches.nonEmpty) { val rows = for(m <- matches) yield { val extra = if(m.video.nonEmpty) a(href:=m.video, target:="_blank", cls:="btn btn-sm btn-outline-danger w-100", "Video") else span(""); div(cls:="card bg-dark border-secondary shadow mb-3", div(cls:="card-body", div(cls:="d-flex justify-content-between align-items-center mb-2", div(strong(cls:="text-warning", m.fecha), span(cls:="ms-2 badge bg-secondary", m.clima)), div(cls:="fs-5 fw-bold text-white", m.resultado)), if(m.estadio.nonEmpty) div(cls:="small text-muted mb-2 fw-bold", s"📍 ${m.estadio}"), if(m.notas.nonEmpty) div(cls:="alert alert-dark border-secondary p-2 small text-light fst-italic mb-2 fw-bold", s"Nota: ${fixEncoding(m.notas)}"), extra)) }; div(div(cls:="card bg-secondary bg-opacity-25 border-info mb-4 p-3", h5(cls:="text-center text-white mb-3", s"Vs ${matches.head.rival}"), div(cls:="d-flex justify-content-around text-center text-white", div(h3(stats("pj")), span(cls:="small text-muted", "PJ")), div(h3(cls:="text-success", stats("ganados")), span(cls:="small text-muted", "G")), div(h3(cls:="text-danger", stats("gc")), span(cls:="small text-muted", "GC")))), h6(cls:="text-white border-bottom border-secondary pb-2 mb-3", "Partidos"), div(rows)) } else div(cls:="text-center text-muted mt-5", "Busca un rival..."); val content = basePage("scouting", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", h2(cls := "text-info mb-4 text-center", "SCOUTING"), form(action:="/scouting", method:="get", cls:="mb-4", div(cls:="input-group", input(tpe:="text", name:="query", cls:="form-control form-control-lg bg-dark text-white border-secondary fw-bold", placeholder:="Nombre equipo", value:=query), button(tpe:="submit", cls:="btn btn-info fw-bold", "Buscar"))), rivalCardWidget, resultsWidget))); renderHtml(content) }
  @cask.postForm("/scouting/save_rival") def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String) = { DatabaseManager.saveRivalInfo(fixEncoding(nombre), estilo, fixEncoding(claves), fixEncoding(notas)); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> s"/scouting?query=$nombre")) }

  @cask.get("/gear") def gearPage(request: cask.Request) = withAuth(request) { val items = DatabaseManager.getActiveGear(); val gearList = if(items.isEmpty) div(cls:="alert alert-secondary text-center", "Sin material.") else { val rows = for(i <- items) yield { val pct = if(i.maxUsos>0) (i.usos.toDouble/i.maxUsos.toDouble*100).toInt else 0; val color = if(pct > 90) "bg-danger" else if(pct > 75) "bg-warning" else "bg-success"; val imgTag = if(i.img.length > 50) img(src:=i.img, style:="width:50px; height:50px; object-fit:cover; border-radius:50%; margin-right:10px;") else div(cls:="me-3", style:="font-size: 30px;", if(i.tipo=="Guantes") "🧤" else "👟"); div(cls:="col-12 mb-3", div(cls:="card bg-dark border-secondary shadow", div(cls:="card-body d-flex align-items-center", imgTag, div(cls:="flex-grow-1", h5(cls:="text-white mb-0", i.nombre), div(cls:="small text-muted mb-1 fw-bold", i.tipo), div(cls:="progress", style:="height: 10px;", div(cls:=s"progress-bar $color", style:=s"width: $pct%"))), div(cls:="ms-3 text-end", div(cls:="fw-bold text-white", s"${i.usos}/${i.maxUsos}"), div(style:="font-size:10px", "USOS"))))) }; div(cls:="row", rows) }; val content = basePage("gear", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", h2(cls := "text-warning mb-4 text-center", "MATERIAL"), gearList, div(cls:="card bg-secondary bg-opacity-10 border-secondary mt-4", div(cls:="card-body", h5(cls:="text-white mb-3", "Nuevo"), form(action:="/gear/add", method:="post", div(cls:="row", div(cls:="col-6 mb-2", input(tpe:="text", name:="nombre", cls:="form-control", placeholder:="Nombre", required:=true)), div(cls:="col-6 mb-2", select(name:="tipo", cls:="form-select", option(value:="Guantes", "Guantes"), option(value:="Botas", "Botas"))), div(cls:="col-12 mb-2", input(tpe:="number", name:="vida", cls:="form-control", value:="30", placeholder:="Vida util")), div(cls:="col-12 mb-2", label("Foto"), input(tpe:="file", cls:="form-control", onchange:="convertToBase64(this, 'gearImg')")), input(tpe:="hidden", name:="img", id:="gearImg"), div(cls:="col-12", button(tpe:="submit", cls:="btn btn-warning w-100", "Anadir"))))), script(raw("""function convertToBase64(i,t){if(i.files&&i.files[0]){var r=new FileReader();r.onload=function(e){document.getElementById(t).value=e.target.result;};r.readAsDataURL(i.files[0]);}}""")))))); renderHtml(content) }
  @cask.postForm("/gear/add")
  def addGear(request: cask.Request, nombre: String, tipo: String, vida: Int, img: String) = withAuth(request) {
    // 1. Guardamos el nuevo material
    DatabaseManager.addNewGear(nombre, tipo, vida, if(img != null) img else "")

    // 2. Redireccionamos a la página de material (Gear) de forma limpia
    renderRedirect("/gear")
  }

  def medicalSection(reports: List[MedicalReport]) = {
    div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "🏥 MEDICAL VAULT & PASAPORTE BIOLÓGICO"),
      div(cls := "card-body p-3",
        // Formulario de Subida
        form(action := "/bio/medical/upload", method := "post", enctype := "multipart/form-data",
          div(cls:="row g-2 mb-3",
            div(cls:="col-7",
              label(cls:="xx-small text-muted text-uppercase", "Tipo de Informe"),
              select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
                option(value:="Pediatría", "Pediatría (Crecimiento)"),
                option(value:="Analítica", "Analítica de Sangre"),
                option(value:="Traumatología", "Traumatología / Fisio"),
                option(value:="Otros", "Otros")
              )
            ),
            div(cls:="col-5",
              label(cls:="xx-small text-muted text-uppercase", "Fecha"),
              input(tpe:="date", name:="fecha", cls:="form-control form-control-sm bg-dark text-white border-secondary", required:=true)
            )
          ),
          div(cls:="mb-3",
            label(cls:="xx-small text-muted text-uppercase", "Archivo (PDF/Imagen)"),
            input(tpe:="file", name:="archivo", cls:="form-control form-control-sm bg-dark text-white", required:=true)
          ),
          div(cls:="form-check form-switch mb-3",
            input(cls:="form-check-input", tpe:="checkbox", name:="esPrevio", id:="checkPrevio"),
            label(cls:="form-check-label small text-muted", `for`:="checkPrevio", "Informe previo al inicio en fútbol")
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-danger fw-bold", "Subir y Analizar con IA"))
        ),
        hr(cls:="border-secondary"),
        // Lista de Informes Procesados
        div(cls:="medical-history",
          if(reports.isEmpty) p(cls:="text-center text-muted small", "No hay registros médicos aún.")
          else for(r <- reports) yield div(cls:="border-start border-danger border-3 ps-2 mb-3",
            div(cls:="d-flex justify-content-between",
              span(cls:="fw-bold small text-danger", r.tipo),
              span(cls:="xx-small text-muted", r.fecha)
            ),
            div(cls:="xx-small text-light fst-italic", strong("Diagnóstico: "), r.diagnostico),
            div(cls:="xx-small text-info", strong("Rec. IA: "), r.recomendaciones)
          )
        )
      )
    )
  }

  // --- 5. BIO & EVALUACIÓN (CORREGIDO MODO OSCURO) ---
  @cask.get("/bio")
  def bioPage(request: cask.Request) = withAuth(request) {
    val activeDrills = DatabaseManager.getActiveDrills()
    val growthData = DatabaseManager.getGrowthHistory()
    val techChart = DatabaseManager.getTechEvolutionChart()
    val rpg = DatabaseManager.getRPGStatus()
    val cognitiveInsight = DatabaseManager.getCognitiveInsight()
    val medicalReports = DatabaseManager.getMedicalReports()
    // --- WIDGET 1: ANÁLISIS COGNITIVO ---
    val cognitiveWidget = div(cls:="card bg-dark border-info shadow mb-3",
      div(cls:="card-header border-info text-info fw-bold py-1 text-center small", "🧠 ANALISTA COGNITIVO"),
      div(cls:="card-body p-2",
        p(cls:="text-light small mb-0 text-center fw-bold", raw(cognitiveInsight))
      )
    )

    // --- FASE 4: MEDICAL VAULT SECTION ---
    val medicalVault = div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "🏥 MEDICAL VAULT & PASAPORTE BIOLÓGICO"),
      div(cls := "card-body p-3",
        form(action := "/bio/medical/upload", method := "post", enctype := "multipart/form-data",
          div(cls:="row g-2 mb-3",
            div(cls:="col-7",
              label(cls:="xx-small text-muted text-uppercase", "Tipo de Informe"),
              select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
                option(value:="Pediatría", "Pediatría"), option(value:="Analítica", "Analítica"), option(value:="Traumatología", "Traumatología"), option(value:="Otros", "Otros")
              )
            ),
            div(cls:="col-5",
              label(cls:="xx-small text-muted text-uppercase", "Fecha"),
              input(tpe:="date", name:="fecha", cls:="form-control form-control-sm bg-dark text-white border-secondary", required:=true)
            )
          ),
          div(cls:="mb-3",
            label(cls:="xx-small text-muted text-uppercase", "Archivo (PDF/Imagen)"),
            input(tpe:="file", name:="archivo", cls:="form-control form-control-sm bg-dark text-white", required:=true)
          ),
          div(cls:="form-check form-switch mb-3",
            input(cls:="form-check-input", tpe:="checkbox", name:="esPrevio", id:="checkPrevio"),
            label(cls:="form-check-label small text-muted", `for`:="checkPrevio", "Informe previo al fútbol")
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-danger fw-bold", "SUBIR Y ANALIZAR"))
        ),
        hr(cls:="border-secondary"),
        div(cls:="medical-history", style:="max-height: 150px; overflow-y: auto;",
          if(medicalReports.isEmpty) p(cls:="text-center text-muted small", "Sin registros médicos.")
          else for(r <- medicalReports) yield div(cls:="border-start border-danger border-2 ps-2 mb-2",
            div(cls:="d-flex justify-content-between xx-small", span(cls:="fw-bold text-danger", r.tipo), span(cls:="text-muted", r.fecha)),
            div(cls:="xx-small text-light", r.diagnostico)
          )
        )
      )
    )

    // --- WIDGET 2: FORMULARIO ACADÉMICO ---
    val academicForm = div(cls := "card bg-dark text-white border-warning shadow mb-3",
      div(cls := "card-header bg-warning text-dark fw-bold text-center small", "📚 REGISTRO ACADÉMICO"),
      div(cls := "card-body p-3",
        form(action := "/bio/save_academic", method := "post",
          div(cls:="mb-2",
            input(tpe:="text", name:="asignatura", cls:="form-control form-control-sm bg-dark text-white border-secondary", placeholder:="Asignatura", required:=true)
          ),
          div(cls:="row g-2 mb-2",
            div(cls:="col-6", input(tpe:="number", step:="0.1", name:="nota", cls:="form-control form-control-sm text-center bg-dark text-white border-warning fw-bold", placeholder:="Nota 0-10", required:=true)),
            div(cls:="col-6", select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
              option(value:="Examen", "Examen"), option(value:="Trabajo", "Trabajo"), option(value:="Trimestral", "Trimestral")
            ))
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-outline-warning fw-bold", "Guardar Nota"))
        )
      )
    )

    val drillList = if (activeDrills.nonEmpty) { val dItems = for(d <- activeDrills) yield div(cls:="mb-2", div(cls:="d-flex justify-content-between small", span(fixEncoding(d.nombre)), span(s"${d.actual}/${d.objetivo}")), div(cls:="progress", style:="height: 6px;", div(cls:="progress-bar bg-warning", style:=s"width:${(d.actual.toDouble/d.objetivo.toDouble*100).toInt}%"))); div(id:="drillsContainer", style:="display:none;", cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", h6(cls:="text-warning small fw-bold mb-2", "🎯 MISIONES ACTIVAS"), dItems) } else div(id:="drillsContainer", style:="display:none;", cls:="alert alert-dark p-2 small text-center", "Sin misiones activas.")

    val content = basePage("bio", div(cls := "row justify-content-center",
      div(cls := "col-md-6 mb-4",
        // LABORATORIO
        div(cls:="card bg-secondary bg-opacity-10 border-info shadow mb-4", div(cls:="card-header bg-dark text-info fw-bold text-center", "🔬 LABORATORIO DE DATOS"), div(cls:="card-body p-2 d-flex justify-content-around", a(href:="/gear", cls:="btn btn-outline-light flex-fill me-1", div(style:="font-size:20px", "👟"), span(cls:="small", "Material")), a(href:="/oracle", cls:="btn btn-outline-info flex-fill me-1", div(style:="font-size:20px", "🔮"), span(cls:="small", "Oráculo")), a(href:="/distribution", cls:="btn btn-outline-warning flex-fill", div(style:="font-size:20px", "📊"), span(cls:="small", "Moneyball")))),
        // --- NUEVO: MÓDULO JUDO (Insertar aquí) ---
        div(cls:="card bg-dark border-warning shadow mb-4",
          div(cls:="card-header bg-warning text-dark fw-bold text-center", "🥋 ESTADO DOJO (JUDO)"),
          div(cls:="card-body p-3",
            div(cls:="d-flex align-items-center justify-content-between mb-3",
              div(
                div(cls:="small text-muted fw-bold", "Cinturón Actual"),
                h4(cls:="mb-0 text-white", rpg.cinturonJudo) // Asegúrate de que 'rpg' esté cargado arriba
              ),
              div(cls:="px-3 py-2 rounded border border-light",
                style:=s"background-color: ${rpg.cinturonJudo.toLowerCase match {
                  case "blanco" => "#fff"
                  case "amarillo" => "#ff0"
                  case "naranja" => "#f80"
                  case "verde" => "#080"
                  case _ => "#333"
                }}; color: black; font-weight: bold;", "GRADO")
            ),
            form(action:="/bio/update_belt", method:="post", cls:="d-flex gap-2",
              select(name:="belt", cls:="form-select form-select-sm bg-dark text-white border-warning fw-bold",
                option(value:="Blanco", "Blanco"),
                option(value:="Blanco-Amarillo", "Blanco-Amarillo"),
                option(value:="Amarillo", "Amarillo"),
                option(value:="Naranja", "Naranja")
              ),
              button(tpe:="submit", cls:="btn btn-sm btn-warning fw-bold", "ACTUALIZAR")
            )
          )
        ),
        medicalVault,
        academicForm, // Entrada de datos escolares


        // WELLNESS
        div(cls := "card bg-dark text-white border-info shadow mb-3", div(cls := "card-header bg-info text-dark fw-bold text-center", "DIARIO DE CARGA Y SUEÑO"), div(cls := "card-body p-3", form(action := "/bio/save_wellness", method := "post", div(cls:="mb-3", label(cls:="small text-danger fw-bold", "Estado Fisico"), select(name:="estadoFisico", cls:="form-select bg-dark text-white border-secondary fw-bold", option(value:="DISPONIBLE", "✅ Disponible"), option(value:="MOLESTIAS", "⚠️ Molestias"), option(value:="LESION", "❌ Lesionado"), option(value:="ENFERMO", "🤒 Enfermo"))), div(cls:="row mb-3 align-items-end", div(cls:="col-6 text-center", label(cls:="small fw-bold", "Calidad Sueño (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="sueno")), div(cls:="col-6", label(cls:="small text-warning fw-bold", "Horas Dormidas"), input(tpe:="number", step:="0.5", name:="horas", cls:="form-control text-center bg-dark text-white border-warning fw-bold", value:="9.0"))), div(cls:="mb-3 border-top pt-2", label(cls:="small fw-bold", "Energia (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="energia")), div(cls:="mb-3", label(cls:="small text-info fw-bold", "Estado Animico (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="animo"), div(cls:="d-flex justify-content-between xx-small text-muted fw-bold", span("Crisis"), span("Top"))), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Notas conducta"), input(tpe:="text", name:="notas_conducta", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="... ")), div(cls:="mb-3 row", div(cls:="col-6", select(name:="dolor", cls:="form-select fw-bold", option(value:="1","Nada"), option(value:="2","Molestia"), option(value:="3","Dolor"), option(value:="5","Lesion"))), div(cls:="col-6", input(tpe:="text", name:="zona", cls:="form-control fw-bold", placeholder:="Zona?"))), div(cls:="row mb-3 border-top pt-3", div(cls:="col-6", label(cls:="small text-info fw-bold", "Altura (cm)"), input(tpe:="number", name:="altura", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar")), div(cls:="col-6", label(cls:="small text-info fw-bold", "Peso (kg)"), input(tpe:="number", step:="0.1", name:="peso", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar"))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-info fw-bold", "Guardar Bio"))))),

        // NUEVO: EVALUACIÓN TÉCNICA (LABELS BLANCOS FORZADOS)
        div(cls:="card bg-secondary bg-opacity-25 border-warning shadow", div(cls:="card-header bg-warning text-dark fw-bold text-center", "EVALUACIÓN TÉCNICA (MENSUAL)"), div(cls:="card-body p-3",
          form(action:="/bio/save_eval", method:="post",
            div(cls:="row mb-2", div(cls:="col-6", label(cls:="small fw-bold text-white", "Blocaje Manos"), input(tpe:="number", name:="blocaje", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5")), div(cls:="col-6", label(cls:="small fw-bold text-white", "Juego Pies"), input(tpe:="number", name:="pies", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5"))),
            div(cls:="row mb-2", div(cls:="col-6", label(cls:="small fw-bold text-white", "Juego Aéreo"), input(tpe:="number", name:="aereo", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5")), div(cls:="col-6", label(cls:="small fw-bold text-danger", "Valentía"), input(tpe:="number", name:="valentia", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5"))),
            div(cls:="row mb-3", div(cls:="col-6", label(cls:="small fw-bold text-white", "Concentración"), input(tpe:="number", name:="concentracion", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5")), div(cls:="col-6", label(cls:="small fw-bold text-white", "Coordinación"), input(tpe:="number", name:="coordinacion", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5"))),
            div(cls:="mb-3", textarea(name:="notas", cls:="form-control fw-bold", rows:="2", placeholder:="Observaciones del mes...")),
            button(tpe:="submit", cls:="btn btn-warning w-100 fw-bold", "Registrar Evolución")
          )
        ))
      ),
      div(cls := "col-md-6",
        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header text-secondary fw-bold text-center small", "PROGRESO TÉCNICO"), div(cls := "card-body p-2", canvas(id:="techChart", style:="max-height:200px;"))),
        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header text-secondary fw-bold text-center small", "CURVA DE CRECIMIENTO"), div(cls := "card-body p-2", canvas(id:="growthChart", style:="max-height:150px;"))),
        div(cls := "card bg-dark text-white border-success shadow mb-3", div(cls := "card-header bg-success text-dark fw-bold text-center", "REGISTRO ENTRENO"), div(cls := "card-body p-3", form(action := "/bio/save_training", method := "post", div(cls:="mb-3", label(cls:="small fw-bold", "Tipo"), select(name:="tipo", id:="trainingType", onchange:="toggleDrills()", cls:="form-select bg-dark text-white fw-bold", option(value:="Club", "Club"), option(value:="Academia", "Academia"), option(value:="Judo", "🥋 Judo"), option(value:="Papa", "Papa (Portero)"), option(value:="Papa (Jugador)", "Papa (Jugador)"))), drillList, div(cls:="mb-3", label(cls:="small fw-bold", "Foco / Actividad"), div(cls:="d-flex gap-2", input(tpe:="text", name:="foco", id:="drillFocus", cls:="form-control fw-bold", placeholder:="Ej: Tiros, Resistencia...", required:=true), button(tpe:="button", id:="aiBtn", cls:="btn btn-warning fw-bold", onclick:="generateAI()", style:="display:none;", "🤖 IA"))), div(id:="manualDesign", style:="display:none;", textarea(name:="rutina", id:="rutinaText", cls:="form-control mb-3 fw-bold", rows:="4", placeholder:="Detalle de la sesión...")), div(cls:="row mb-3", div(cls:="col-4 text-center", label(cls:="small fw-bold", "RPE"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="rpe", value:="7", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Calidad"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="calidad", value:="8", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Atención"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="atencion", value:="8", min:="1", max:="10"))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-success fw-bold", "Guardar Sesión")))),
          div(cls:="card bg-secondary bg-opacity-10 border-secondary", div(cls:="card-body p-2", h6(cls:="text-muted small mb-2", "+ Añadir Misión Técnica (10 Sesiones)"), form(action:="/bio/add_drill", method:="post", cls:="d-flex gap-2", input(tpe:="text", name:="nombre", cls:="form-control form-control-sm fw-bold", placeholder:="Ej: Control Orientado", required:=true), button(tpe:="submit", cls:="btn btn-sm btn-secondary fw-bold", "Crear"))))
        )
      ), script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""
      const gCtx=document.getElementById('growthChart');const gData=$growthData;if(gCtx){new Chart(gCtx,{type:'line',data:{labels:gData.labels,datasets:[{label:'Altura (cm)',data:gData.data,borderColor:'#0dcaf0',borderWidth:3,tension:0.3,pointBackgroundColor:'#fff',pointRadius:4}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{y:{ticks:{color:'#eee',font:{weight:'bold'}},grid:{color:'#444'},pointLabels:{color:'#fff'}},x:{display:false}}}});}
      const tCtx=document.getElementById('techChart');const tData=$techChart;if(tCtx){new Chart(tCtx,{type:'line',data:tData,options:{responsive:true,maintainAspectRatio:false,scales:{y:{min:0,max:10,ticks:{color:'#eee'},grid:{color:'#444'}},x:{ticks:{color:'#eee'}}}}});}
      function toggleDrills(){ var type=document.getElementById('trainingType').value; var container=document.getElementById('drillsContainer'); var manual=document.getElementById('manualDesign'); var aiBtn = document.getElementById('aiBtn'); if(container){if(type.includes('Papa') && !type.includes('Jugador')) container.style.display='block'; else container.style.display='none';} if(manual){if(type.includes('Papa')) manual.style.display='block'; else manual.style.display='none';} if(aiBtn){if(type.includes('Papa')) aiBtn.style.display='block'; else aiBtn.style.display='none';} }
      function generateAI(){ var focus = document.getElementById('drillFocus').value; var type = document.getElementById('trainingType').value; if(!focus) { alert('Pon un objetivo primero (ej: Velocidad)'); return; } document.getElementById('rutinaText').value = "Generando..."; fetch('/bio/ai_gen?focus='+encodeURIComponent(focus)+'&mode='+encodeURIComponent(type)).then(r=>r.text()).then(t => document.getElementById('rutinaText').value = t); }
      window.addEventListener('DOMContentLoaded', toggleDrills);
    """))))
    renderHtml(content)
  }

  @cask.postForm("/bio/save_academic")
  def saveAcademic(asignatura: String, nota: Double, tipo: String, comentarios: String = "") = {
    DatabaseManager.saveAcademicNote(asignatura, nota, tipo, comentarios)
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  @cask.postForm("/bio/save_wellness") def saveWellness(sueno: Int, horas: String, energia: Int, dolor: Int, zona: String, altura: String, peso: String, animo: Int, notas_conducta: String, estadoFisico: String) = { val h = if(horas.nonEmpty) horas.toDouble else 0.0; val alt = if(altura.nonEmpty) altura.toInt else 0; val pes = if(peso.nonEmpty) peso.toDouble else 0.0; DatabaseManager.logWellness(sueno, h, energia, dolor, zona, alt, pes, animo, notas_conducta, estadoFisico); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio")) }
  @cask.postForm("/bio/save_training") def saveTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: String, rutina: String) = { val att = if(atencion != null && atencion.nonEmpty) atencion.toInt else 3; DatabaseManager.logTraining(tipo, foco, rpe, calidad, att, rutina); val htmlStr = doctype("html")(html(head(meta(charset := "utf-8"), tags2.title("Entreno Guardado"), tags2.style(raw(getCss()))), body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';", h1(style := "color: #28a745; font-size: 60px; margin-bottom: 0;", "✔"), h2(style := "color: #d4af37; letter-spacing: 2px;", "SESIÓN COMPLETADA"), div(style := "margin: 30px auto; width: 300px; background: #333; padding: 20px; border-radius: 10px; border: 1px solid #444;", h4(style := "color: #0dcaf0; margin-bottom: 5px;", tipo.toUpperCase), div(style := "font-style: italic; color: #ccc; margin-bottom: 15px;", if(foco.nonEmpty) foco else "Entrenamiento General"), div(style := "display: flex; justify-content: space-around; margin-top: 15px; border-top: 1px solid #555; padding-top: 10px;", div(div(style:="font-size:12px; color:#aaa;", "RPE"), div(style:="font-weight:bold; font-size:20px;", rpe)), div(div(style:="font-size:12px; color:#aaa;", "CALIDAD"), div(style:="font-weight:bold; font-size:20px; color:#ffc107;", calidad)), div(div(style:="font-size:12px; color:#aaa;", "ATENCIÓN"), div(style:="font-weight:bold; font-size:20px;", att)))), p(style := "color: #999; font-size: 14px;", "Datos registrados en el historial."), div(style := "margin-top: 40px;", a(href := "/bio", cls := "btn btn-outline-light btn-lg", "Continuar"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.postForm("/bio/update_belt")
  def updateBelt(belt: String) = {
    DatabaseManager.updateJudoBelt(belt)
    cask.Response("", statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  @cask.postForm("/bio/save_eval") def saveEval(blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String) = { DatabaseManager.saveTechnicalReview(blocaje, pies, aereo, valentia, concentracion, coordinacion, notas); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio")) }
  @cask.get("/bio/ai_gen") def aiGenDrill(focus: String, mode: String) = { cask.Response(DatabaseManager.generateTrainingSession(mode, focus)) }
  @cask.postForm("/bio/add_drill") def addDrill(nombre: String) = { DatabaseManager.addNewDrill(fixEncoding(nombre), ""); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio")) }
  @cask.get("/history")
  def historyPage(request: cask.Request) = withAuth(request) {
    val matches = DatabaseManager.getMatchesList()

    // 1. Generamos las filas de la tabla
    val tableRows = if (matches.isEmpty) {
      Seq(tr(td(colspan := 4, cls := "text-center p-4", "Sin partidos")))
    } else {
      matches.map(m => renderMatchRow(m))
    }

    // 2. Definimos el contenido central (SIN llamar a basePage aquí)
    val mainContent = div(cls := "row justify-content-center",
      div(cls := "col-md-10 col-12",
        h2(cls := "text-warning mb-3 text-center", "HISTORIAL"),
        div(cls := "card shadow-sm border-0",
          div(cls := "card-body p-0",
            table(cls := "table table-hover tm-table mb-0",
              thead(tr(
                th("Rival"),
                th(cls:="text-center", "Res"),
                th(cls:="text-center", "Nota"),
                th(cls:="text-end", "Acción")
              )),
              tbody(tableRows)
            )
          )
        )
      )
    )

    // 3. Renderizamos llamando a basePage UNA SOLA VEZ
    renderHtml(basePage("history", mainContent))
  }
  @cask.get("/match/delete/:id") def deleteMatchAction(id: Int) = { DatabaseManager.deleteMatch(id); cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history")) }
  @cask.get("/match/edit/:matchId") def editMatchPage(matchId: Int) = { val m = DatabaseManager.getMatchById(matchId); if (m.isEmpty) { cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history")) } else { val matchData = m.get; val (gf, gc) = if(matchData.resultado.contains("-")) (matchData.resultado.split("-")(0), matchData.resultado.split("-")(1)) else ("0", "0"); val tags = DatabaseManager.getVideoTags(matchId); val tagList = if(matchData.video.nonEmpty) { val tItems = for(t <- tags) yield { val link = if(matchData.video.contains("?")) s"${matchData.video}&t=${t.minuto*60 + t.segundo}" else s"${matchData.video}?t=${t.minuto*60 + t.segundo}"; a(href:=link, target:="_blank", cls:="list-group-item list-group-item-action bg-dark text-white border-secondary d-flex justify-content-between align-items-center", div(span(cls:="badge bg-danger me-2", s"${t.minuto}:${t.segundo}"), span(t.tipo)), a(href:=s"/video/delete_tag/${t.id}/${matchId}", cls:="text-danger fw-bold text-decoration-none", "X")) }; div(form(action:="/video/add_tag", method:="post", cls:="row g-2 mb-3", input(tpe:="hidden", name:="matchId", value:=matchId.toString), div(cls:="col-3", input(tpe:="number", name:="min", cls:="form-control form-control-sm", placeholder:="Min", required:=true)), div(cls:="col-3", input(tpe:="number", name:="sec", cls:="form-control form-control-sm", placeholder:="Sec", required:=true)), div(cls:="col-6", div(cls:="input-group input-group-sm", select(name:="tipo", cls:="form-select", option("PARADA"), option("ERROR"), option("GOL"), option("PASE")), button(tpe:="submit", cls:="btn btn-warning", "+")))), div(cls:="list-group", tItems)) } else div(cls:="alert alert-secondary small", "Añade URL de video para usar tags."); val content = basePage("history", div(cls := "row justify-content-center", div(cls := "col-md-6 col-12", div(cls := "card bg-dark text-white border-primary shadow", div(cls := "card-header bg-primary text-white fw-bold text-center", "EDITAR PARTIDO & VIDEO"), div(cls := "card-body p-3", form(action := "/match/update", method := "post", attr("accept-charset") := "UTF-8", input(tpe:="hidden", name:="id", value:=matchId.toString), div(cls:="mb-3", label("Rival"), input(tpe:="text", name:="rival", value:=matchData.rival, cls:="form-control")), div(cls:="mb-3", label("Fecha"), input(tpe:="date", name:="fecha", value:=matchData.fecha, cls:="form-control")), div(cls:="row mb-3", div(cls:="col-6", label("GF"), input(tpe:="number", name:="gf", value:=gf, cls:="form-control")), div(cls:="col-6", label("GC"), input(tpe:="number", name:="gc", value:=gc, cls:="form-control"))), div(cls:="mb-3", label("Estadio"), input(tpe:="text", name:="estadio", value:=matchData.estadio, cls:="form-control")), div(cls:="mb-3", label("Nota"), input(tpe:="number", step:="0.1", name:="nota", value:=matchData.nota.toString, cls:="form-control")), div(cls:="mb-3", label("Notas Texto"), textarea(name:="notas", cls:="form-control", rows:="3", matchData.notas)), div(cls:="mb-3", label("Reaccion/Goles"), textarea(name:="reaccion", cls:="form-control", rows:="3", matchData.reaccion)), div(cls:="mb-3", label("Video URL (Youtube)"), input(tpe:="text", name:="video", value:=matchData.video, cls:="form-control")), div(cls:="d-grid gap-2 mb-4", button(tpe:="submit", cls:="btn btn-success", "Guardar Cambios"), a(href:="/history", cls:="btn btn-outline-secondary", "Cancelar")))), div(cls:="card-footer bg-secondary bg-opacity-10 border-top border-secondary mt-3", h6(cls:="text-info small fw-bold mb-2", "🎙️ DIARIO DE VOZ (POST-PARTIDO)"), div(cls:="mb-2 small text-muted", "Graba a Héctor contando cómo se sintió o sube un audio."), div(cls:="d-flex gap-2 mb-3", button(id:="btnRecord", cls:="btn btn-sm btn-outline-danger", onclick:="toggleRecording()", "⏺ Grabar"), button(id:="btnStop", cls:="btn btn-sm btn-danger", style:="display:none;", onclick:="stopRecording()", "⏹ Parar"), input(tpe:="file", id:="fileUpload", accept:="audio/*", cls:="form-control form-control-sm bg-dark text-white", onchange:="handleFileUpload(this)")), audio(id:="audioPreview", attr("controls"):="true", style:="width: 100%; display:none;", cls:="mb-2"), form(action:="/match/analyze_audio", method:="post", id:="audioForm", input(tpe:="hidden", name:="matchId", value:=matchId.toString), input(tpe:="hidden", name:="audioData", id:="hiddenAudioData"), button(tpe:="button", id:="btnAnalyze", cls:="btn btn-info w-100", onclick:="submitAudio()", disabled:=true, "🧠 Analizar Emociones con IA")), if(matchData.analisisVoz.nonEmpty) div(cls:="mt-3 p-2 border border-info rounded bg-dark text-light small", style:="white-space: pre-wrap;", b(cls:="text-info", "Psicólogo IA: "), br, fixEncoding(matchData.analisisVoz)) else div()), div(cls:="card-footer bg-secondary bg-opacity-25", h6(cls:="text-white small fw-bold", "CORTES DE VIDEO (TAGS)"), tagList))), script(raw(""" let mediaRecorder; let audioChunks = []; async function toggleRecording() { try { const stream = await navigator.mediaDevices.getUserMedia({ audio: true }); mediaRecorder = new MediaRecorder(stream); mediaRecorder.start(); document.getElementById('btnRecord').style.display='none'; document.getElementById('btnStop').style.display='inline-block'; document.getElementById('btnAnalyze').disabled = true; mediaRecorder.ondataavailable = event => { audioChunks.push(event.data); }; mediaRecorder.onstop = () => { const audioBlob = new Blob(audioChunks, { type: 'audio/webm' }); const audioUrl = URL.createObjectURL(audioBlob); const audioEl = document.getElementById('audioPreview'); audioEl.src = audioUrl; audioEl.style.display = 'block'; const reader = new FileReader(); reader.readAsDataURL(audioBlob); reader.onloadend = () => { document.getElementById('hiddenAudioData').value = reader.result; document.getElementById('btnAnalyze').disabled = false; document.getElementById('btnAnalyze').innerHTML = "🧠 Analizar Grabación"; }; audioChunks = []; }; } catch(err) { alert('Error microfono: ' + err); } } function stopRecording() { mediaRecorder.stop(); document.getElementById('btnRecord').style.display='inline-block'; document.getElementById('btnStop').style.display='none'; } function handleFileUpload(input) { if (input.files && input.files[0]) { const reader = new FileReader(); reader.onload = function (e) { document.getElementById('hiddenAudioData').value = e.target.result; document.getElementById('audioPreview').src = e.target.result; document.getElementById('audioPreview').style.display = 'block'; document.getElementById('btnAnalyze').disabled = false; document.getElementById('btnAnalyze').innerHTML = "🧠 Analizar Archivo"; }; reader.readAsDataURL(input.files[0]); } } function submitAudio() { document.getElementById('btnAnalyze').innerHTML = "⏳ Procesando... (puede tardar 10s)"; document.getElementById('btnAnalyze').disabled = true; document.getElementById('audioForm').submit(); } """)))); renderHtml(content.render) } }
  @cask.postForm("/match/analyze_audio")
  def analyzeAudioAction(matchId: Int, audioData: String) = {
    // La lógica de IA y la actualización de la DB ahora ocurren dentro de analyzeAudioLog
    // audioData ya viene como Base64 desde el script del navegador
    DatabaseManager.analyzeAudioLog(matchId, audioData)

    cask.Response(
      "".getBytes("UTF-8"),
      statusCode = 302,
      headers = Seq("Location" -> s"/match/edit/$matchId")
    )
  }
  @cask.postForm("/video/add_tag") def addVideoTag(matchId: Int, min: Int, sec: Int, tipo: String) = { DatabaseManager.addVideoTag(matchId, min, sec, tipo, ""); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> s"/match/edit/$matchId")) }
  @cask.get("/video/delete_tag/:id/:matchId") def deleteVideoTag(id: Int, matchId: Int) = { DatabaseManager.deleteVideoTag(id); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> s"/match/edit/$matchId")) }
  @cask.postForm("/match/update") def updateMatchAction(id: Int, rival: String, gf: Int, gc: Int, nota: Double, notas: String, video: String, reaccion: String, fecha: String, estadio: String) = { val cleanRival = fixEncoding(rival); val cleanNotas = fixEncoding(notas); val cleanReaccion = fixEncoding(reaccion); DatabaseManager.updateMatch(id, cleanRival, gf, gc, 60, nota, "Sol", estadio, 20, cleanNotas, video, cleanReaccion, fecha); cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history")) }

  @cask.get("/settings") def settingsPage() = {
    val card = DatabaseManager.getLatestCardData()
    val content = div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", div(cls := "card bg-dark text-white border-secondary shadow p-4 mb-3", h2(cls := "text-warning mb-4", "Configuración General"),
      form(action := "/settings/save_base64", method := "post",
        div(cls := "mb-4", label(cls := "form-label text-info fw-bold", "Nombre Visual (Carta)"), input(tpe := "text", name := "nombreClub", cls := "form-control fw-bold", value:=card.clubNombre, placeholder := "Ej: Rayo (Corto)")),
        div(cls := "mb-4", label(cls := "form-label text-success fw-bold", "Fecha de Nacimiento"), input(tpe := "date", name := "fechaNac", cls := "form-control fw-bold", value:=card.fechaNacimiento)),
        div(cls := "mb-4 border-top border-secondary pt-3", h5(cls:="text-info", "Scouting 2.0 (RFFM)"), div(cls:="mb-3", label(cls:="small text-muted fw-bold", "URL Grupo RFFM"), input(tpe:="text", name:="rffmUrl", cls:="form-control fw-bold", value:=Option(card.rffmUrl).getOrElse(""), placeholder:="https://www.rffm.es/competicion/...")), div(cls:="mb-3", label(cls:="small text-muted fw-bold", "Nombre Oficial (Federación)"), input(tpe:="text", name:="rffmName", cls:="form-control fw-bold", value:=Option(card.rffmName).getOrElse(""), placeholder:="Ej: RAYO VALLECANO DE MADRID 'B'"))),
        div(cls := "mb-4", label(cls := "form-label text-info fw-bold", "Foto Jugador"), input(tpe := "file", cls := "form-control fw-bold", accept := "image/*", onchange := "convertToBase64(this, 'hidden_foto')"), input(tpe := "hidden", name := "fotoBase64", id := "hidden_foto")),
        div(cls := "mb-4", label(cls := "form-label text-warning fw-bold", "Escudo Club"), input(tpe := "file", cls := "form-control fw-bold", accept := "image/*", onchange := "convertToBase64(this, 'hidden_club')"), input(tpe := "hidden", name := "clubBase64", id := "hidden_club")),
        div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg fw-bold", "Guardar"))),
      script(raw("""function convertToBase64(i,t){if(i.files&&i.files[0]){var r=new FileReader();r.onload=function(e){document.getElementById(t).value=e.target.result;};r.readAsDataURL(i.files[0]);}}"""))), div(cls:="d-grid", a(href:="/admin", cls:="btn btn-outline-danger fw-bold", "ZONA ADMIN"))));
    renderHtml(basePage("settings", content))
  }
  @cask.postForm("/settings/save_base64") def saveSettingsBase64(fotoBase64: String, clubBase64: String, nombreClub: String, fechaNac: String, rffmUrl: String, rffmName: String) = { val fechaFinal = if(fechaNac != null && fechaNac.nonEmpty) fechaNac else "2020-06-19"; DatabaseManager.updateRFFMSettings(if(rffmUrl!=null) rffmUrl else "", if(rffmName!=null) rffmName else ""); val res = DatabaseManager.updateSeasonSettings(if(fotoBase64!=null) fotoBase64 else "", if(clubBase64!=null) clubBase64 else "", if(nombreClub!=null) nombreClub else "", fechaFinal); val htmlStr = doctype("html")(html(head(meta(charset := "utf-8"), tags2.title("Exito"), tags2.style(raw(getCss()))), body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';", h1("OK"), h2(res), div(style := "margin-top: 20px;", a(href := "/", cls := "btn btn-warning fw-bold", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }

  @cask.get("/career")
  def careerPage(request: cask.Request) = withAuth(request) { val c = DatabaseManager.getCareerSummary(); val listRows = for (s <- c) yield tr(td(cls:="fw-bold text-primary small", s.categoria), td(img(src := s.fotoUrl, style := "height: 35px; width: 35px; border-radius: 50%; object-fit: cover; border: 2px solid #ddd;")), td(cls:="text-center fw-bold small", s.partidosJugados), td(cls:="text-center text-danger small", s.golesContra), td(cls:="text-center", span(cls:="badge bg-dark text-warning border border-warning", s.mediaFinal))); val content=div(cls := "row justify-content-center", div(cls := "col-md-10 col-12", div(cls := "d-flex flex-column justify-content-center align-items-center mb-4 text-center", h2(cls := "text-warning m-0 mb-2", "Trayectoria"), div(cls:="mb-3 w-100", a(href:="/career/legacy", cls:="btn btn-warning w-100 fw-bold", "⭐ MODO LEGADO (RPG)")),
    // AQUÍ VA LA COMPARATIVA DE LEYENDAS
    raw(DatabaseManager.getLegendComparison()),
    div(cls:="card bg-secondary p-2 w-100 mt-3", form(action := "/career/new-season", method := "post", cls:="d-flex flex-column gap-2", div(label(cls:="form-label text-white small m-0 fw-bold", "Nueva Categoria:"), input(tpe := "text", name := "categoria", cls := "form-control form-control-sm fw-bold", placeholder := "Ej: Benjamin A", required := true)), button(tpe := "submit", cls := "btn btn-danger btn-sm fw-bold", onclick := "return confirm('Seguro?');", "Cerrar & Empezar")))), div(cls := "card shadow-sm border-0", div(cls := "card-body p-0 table-responsive", table(cls := "table table-hover tm-table mb-0", thead(tr(th("Cat"), th("Ficha"), th("PJ"), th("GC"), th("Media"))), tbody(listRows)))))); renderHtml(basePage("career", content)) }
  @cask.postForm("/career/new-season") def newSeasonAction(categoria: String) = { val msg = DatabaseManager.startNewSeason(categoria); val htmlStr = doctype("html")(html(head(meta(charset := "utf-8"), tags2.title("Nueva Temp"), tags2.style(raw(getCss()))), body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';", h1("OK"), h2(msg), p(s"Etapa iniciada: $categoria"), div(style := "margin-top: 20px;", a(href := "/", cls := "btn btn-warning fw-bold", "Ir a Inicio"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }

  @cask.get("/admin") def adminPage() = { val objs = DatabaseManager.getSeasonObjectives(); val content = basePage("settings", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", h2(cls:="text-danger text-center mb-4", "ADMINISTRACION"), div(cls:="card bg-dark border-warning shadow mb-4 p-3", h5(cls:="text-warning", "Base de Datos Leyendas"), p(cls:="small text-muted fw-bold", "Si no ves la comparación en Trayectoria, pulsa aquí."), a(href:="/admin/init_legends", cls:="btn btn-outline-warning w-100 fw-bold", "Inicializar BBDD Leyendas")), div(cls:="card bg-secondary bg-opacity-25 border-secondary mb-4 p-3", h5(cls:="text-white", "Copia de Seguridad"), p(cls:="small text-muted fw-bold", "Descarga todos los partidos en formato Excel/CSV."), a(href:="/admin/download_csv", cls:="btn btn-primary w-100 fw-bold", "Descargar CSV")), div(cls:="card bg-secondary bg-opacity-25 border-secondary mb-4 p-3", h5(cls:="text-white", "Informe PDF"), p(cls:="small text-muted fw-bold", "Genera un informe limpio para imprimir o guardar como PDF."), a(href:="/admin/print_report", target:="_blank", cls:="btn btn-info w-100 fw-bold", "Generar Informe")), div(cls:="card bg-dark border-info shadow p-3", h5(cls:="text-info", "Gestionar Objetivos"), if(objs.isEmpty) div("Sin objetivos.") else div((for(o <- objs) yield form(action:="/admin/update_obj", method:="post", cls:="row align-items-center mb-2", div(cls:="col-7 small text-white fw-bold", o.descripcion), div(cls:="col-3", input(tpe:="number", name:="meta", value:=o.meta.toString, cls:="form-control form-control-sm text-center fw-bold")), input(tpe:="hidden", name:="id", value:=o.id.toString), div(cls:="col-2", button(tpe:="submit", cls:="btn btn-sm btn-outline-success fw-bold", "S")))).toSeq)), div(cls:="d-grid mt-4", a(href:="/admin/importer", cls:="btn btn-warning fw-bold", "IMPORTAR DATOS MASIVOS (CSV)"))))); renderHtml(content) }
  @cask.get("/admin/init_legends") def initLegendsAction() = { val msg = DatabaseManager.initLegendsTable(); cask.Response(msg.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/plain")) }
  @cask.postForm("/admin/update_obj") def updateObj(id: Int, meta: Int) = { DatabaseManager.updateObjective(id, meta); cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/admin")) }
  @cask.get("/admin/download_csv") def downloadCsv() = { cask.Response(DatabaseManager.getBackupCSV().getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/csv; charset=utf-8", "Content-Disposition" -> "attachment; filename=guardian_backup.csv")) }
  @cask.get("/admin/print_report") def printReport() = { val card = DatabaseManager.getLatestCardData(); val matches = DatabaseManager.getMatchesList(); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.title("Informe Temporada"), tags2.style("""body{font-family:sans-serif;color:black;background:white;padding:20px;}h1,h2{text-align:center;color:#333;}table{width:100%;border-collapse:collapse;margin-top:20px;font-size:12px;}th,td{border:1px solid #ddd;padding:8px;text-align:left;}th{background-color:#f2f2f2;}.header{text-align:center;margin-bottom:30px;border-bottom:2px solid #333;padding-bottom:20px;}@media print{.no-print{display:none;}}""")), body(div(cls:="no-print", style:="text-align:center;margin-bottom:20px;", button(onclick:="window.print()", style:="padding:10px 20px;font-size:16px;cursor:pointer;", "IMPRIMIR / GUARDAR PDF")), div(cls:="header", h1(s"INFORME DE TEMPORADA - ${card.nombre}"), div(s"Media: ${card.media} | Posicion: ${card.posicion}"), div(s"Estirada: ${card.div} | Manos: ${card.han} | Saque: ${card.kic}"), div(s"Reflejos: ${card.ref} | Velocidad: ${card.spd} | Posicion: ${card.pos}")), h2("Historial de Partidos"), table(thead(tr(th("Fecha"), th("Rival"), th("Resultado"), th("Min"), th("Nota"), th("Clima"))), tbody((for(m <- matches) yield tr(td(m.fecha), td(m.rival), td(m.resultado), td(m.minutos), td(m.nota), td(m.clima))).toSeq))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.get("/admin/importer") def importerPage() = { val content = basePage("settings", div(cls:="row justify-content-center", div(cls:="col-md-8", h2(cls:="text-info text-center mb-4", "IMPORTADOR DE DATOS"), div(cls:="card bg-dark text-white border-success shadow p-4 mb-4", h4("🌍 Conexión RFFM"), p(cls:="small text-muted fw-bold", "Descarga calendario y rivales directamente de la Federación."), form(action:="/admin/sync_rffm", method:="post", button(tpe:="submit", cls:="btn btn-success w-100 fw-bold", "🔄 Sincronizar Calendario"))), div(cls:="card bg-dark text-white border-primary shadow p-4 mb-4", h4("📅 Importar Calendario Manual"), p(cls:="small text-muted fw-bold", "Formato: FECHA, RIVAL, TIPO"), form(action:="/admin/upload_calendar", method:="post", textarea(name:="csvContent", cls:="form-control mb-3 fw-bold", rows:="3"), button(tpe:="submit", cls:="btn btn-primary w-100 fw-bold", "Cargar"))), div(cls:="card bg-dark text-white border-warning shadow p-4 mb-4", h4("Importar Historial"), form(action:="/admin/upload_matches", method:="post", textarea(name:="csvContent", cls:="form-control mb-3 fw-bold", rows:="3"), button(tpe:="submit", cls:="btn btn-warning w-100 fw-bold", "Procesar"))), div(cls:="card bg-dark text-white border-info shadow p-4", h4("Importar Wellness"), form(action:="/admin/upload_wellness", method:="post", textarea(name:="csvContent", cls:="form-control mb-3 fw-bold", rows:="3"), button(tpe:="submit", cls:="btn btn-info w-100 fw-bold", "Procesar"))), div(cls:="mt-3 text-center", a(href:="/admin", cls:="btn btn-outline-light fw-bold", "Volver"))))); cask.Response(content.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/upload_calendar") def uploadCalendar(csvContent: String) = { val res = DatabaseManager.importCalendarCSV(fixEncoding(csvContent)); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("CALENDARIO"), h3(res), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-primary fw-bold", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/upload_matches") def uploadMatches(csvContent: String) = { val cleanCsv = fixEncoding(csvContent); val res = DatabaseManager.importMatchesCSV(cleanCsv); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("IMPORTACION"), h3(res), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-warning fw-bold", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/upload_wellness") def uploadWellness(csvContent: String) = { val cleanCsv = fixEncoding(csvContent); val res = DatabaseManager.importWellnessCSV(cleanCsv); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("IMPORTACION"), h3(res), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-info fw-bold", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/sync_rffm") def syncRffmAction() = { val log = DatabaseManager.syncRFFMCalendar(); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("SCOUTING 2.0"), pre(style:="text-align:left; background:#333; padding:20px; margin:20px; fw-bold", log), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-primary fw-bold", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.get("/admin/test-ai")
  def testAI(request: cask.Request) = withAuth(request) {
    val resultado = DatabaseManager.testAIConnection()
    renderHtml(basePage("settings", div(cls:="container mt-5 text-center",
      h2("Diagnóstico de IA"),
      div(cls:=s"alert ${if(resultado.contains("OK")) "alert-success" else "alert-danger"}", resultado),
      a(href:="/admin", cls:="btn btn-primary", "Volver")
    )))
  }
  @cask.get("/penalties") def penaltiesPage(request: cask.Request) = withAuth(request) { val stats = DatabaseManager.getPenaltyStats(); val content = basePage("match-center", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", h2(cls := "text-center text-danger mb-4", "LABORATORIO PENALTIS"), div(cls:="card bg-dark border-danger shadow mb-4", div(cls:="card-header bg-danger text-white fw-bold", "REGISTRAR LANZAMIENTO"), div(cls:="card-body", form(action:="/penalties/save", method:="post", div(cls:="mb-3", input(tpe:="text", name:="rival", cls:="form-control fw-bold", placeholder:="Nombre Rival (Opcional)")), div(cls:="row mb-3", div(cls:="col-6", label(cls:="small text-white fw-bold", "Zona Tiro"), select(name:="zTiro", cls:="form-select fw-bold", option(value:="TL", "Arriba Izq"), option(value:="TM", "Arriba Cen"), option(value:="TR", "Arriba Der"), option(value:="ML", "Media Izq"), option(value:="MM", "Media Cen"), option(value:="MR", "Media Der"), option(value:="BL", "Baja Izq"), option(value:="BM", "Baja Cen"), option(value:="BR", "Baja Der"))), div(cls:="col-6", label(cls:="small text-warning fw-bold", "Salto Hector"), select(name:="zSalto", cls:="form-select fw-bold", option(value:="L", "Izquierda"), option(value:="C", "Centro"), option(value:="R", "Derecha")))), div(cls:="form-check mb-3", input(cls:="form-check-input", tpe:="checkbox", name:="esGol", id:="golCheck"), label(cls:="form-check-label text-white fw-bold", attr("for"):="golCheck", "Fue Gol")), button(tpe:="submit", cls:="btn btn-danger w-100 fw-bold", "Registrar Penalti")))), div(cls:="card bg-dark border-secondary shadow", div(cls:="card-header text-center text-white", "MAPA DE CALOR (TIROS RIVALES)"), div(cls:="card-body d-flex justify-content-center", div(cls:="goal-grid-3x3", style:="width: 250px; height: 180px;", (for(z <- Seq("TL","TM","TR","ML","MM","MR","BL","BM","BR")) yield { val s = stats.find(_.zona == z).getOrElse(PenaltyStat(z,0,0)); val color = if(s.total > 0) "rgba(220, 53, 69, 0.6)" else "rgba(255,255,255,0.1)"; div(cls:="goal-cell d-flex justify-content-center align-items-center flex-column", style:=s"background-color:$color; border:1px solid #444;", span(cls:="fw-bold text-white", s.total.toString), span(cls:="xx-small text-light", if(s.total>0) s"${(s.goles.toDouble/s.total*100).toInt}% G" else "")) }).toSeq)))))); cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.postForm("/penalties/save") def savePenalty(rival: String, zTiro: String, zSalto: String, esGol: Boolean) = { DatabaseManager.logPenalty(rival, zTiro, zSalto, esGol); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/penalties")) }

  // 8. PIZARRA TÁCTICA
  @cask.get("/tactics") def tacticsPage(request: cask.Request) = withAuth(request) {
    val content = basePage("tactics",
      div(cls := "row justify-content-center",
        div(cls := "col-12",
          h2(cls := "text-center text-info mb-3", "PIZARRA TÁCTICA"),
          div(cls:="d-flex justify-content-center gap-2 mb-3",
            button(cls:="btn btn-outline-light", onclick:="setColor('#ffffff')", "⚪"),
            button(cls:="btn btn-outline-warning", onclick:="setColor('#ffc107')", "🟡"),
            button(cls:="btn btn-outline-danger", onclick:="setColor('#dc3545')", "🔴"),
            button(cls:="btn btn-outline-info", onclick:="setColor('#0dcaf0')", "🔵"),
            button(cls:="btn btn-secondary", onclick:="clearBoard()", "🗑️ BORRAR")
          ),
          div(cls:="field-container shadow border border-secondary",
            canvas(id:="tacticsBoard", width:="350", height:="500")
          ),
          div(cls:="text-center text-muted small mt-2", "Dibuja con el dedo para explicar la jugada.")
        )
      ),
      script(raw("""
        const canvas = document.getElementById('tacticsBoard');
        const ctx = canvas.getContext('2d');
        let painting = false;
        let color = '#ffffff';

        function resize() {
          const parent = canvas.parentElement;
          canvas.width = parent.clientWidth;
          canvas.height = parent.clientHeight;
          drawField();
        }
        window.addEventListener('resize', resize);

        function drawField() {
          ctx.fillStyle = '#2e7d32';
          ctx.fillRect(0, 0, canvas.width, canvas.height);
          ctx.fillStyle = 'rgba(255,255,255,0.05)';
          for(let i=0; i<canvas.height; i+=40) ctx.fillRect(0, i, canvas.width, 20);
          ctx.strokeStyle = 'rgba(255,255,255,0.8)';
          ctx.lineWidth = 2;
          ctx.beginPath();
          ctx.rect(10, 10, canvas.width-20, canvas.height-20);
          const midX = canvas.width / 2;
          const topBoxY = 10;
          const botBoxY = canvas.height - 10;
          ctx.rect(midX - 100, topBoxY, 200, 100);
          ctx.rect(midX - 100, botBoxY - 100, 200, 100);
          ctx.rect(midX - 40, topBoxY, 80, 40);
          ctx.rect(midX - 40, botBoxY - 40, 80, 40);
          ctx.moveTo(10, canvas.height/2);
          ctx.lineTo(canvas.width-10, canvas.height/2);
          ctx.moveTo(midX + 40, canvas.height/2);
          ctx.arc(midX, canvas.height/2, 40, 0, Math.PI * 2);
          ctx.stroke();
          ctx.fillStyle = 'rgba(255,255,255,0.8)';
          ctx.beginPath();
          ctx.arc(midX, topBoxY + 80, 3, 0, Math.PI * 2);
          ctx.arc(midX, botBoxY - 80, 3, 0, Math.PI * 2);
          ctx.arc(midX, canvas.height/2, 3, 0, Math.PI * 2);
          ctx.fill();
        }

        function startPosition(e) { painting = true; draw(e); }
        function finishedPosition() { painting = false; ctx.beginPath(); }
        function draw(e) {
          if (!painting) return;
          e.preventDefault();
          const rect = canvas.getBoundingClientRect();
          const clientX = e.touches ? e.touches[0].clientX : e.clientX;
          const clientY = e.touches ? e.touches[0].clientY : e.clientY;
          const x = clientX - rect.left;
          const y = clientY - rect.top;
          ctx.lineWidth = 3;
          ctx.lineCap = 'round';
          ctx.strokeStyle = color;
          ctx.lineTo(x, y);
          ctx.stroke();
          ctx.beginPath();
          ctx.moveTo(x, y);
        }

        canvas.addEventListener('mousedown', startPosition);
        canvas.addEventListener('mouseup', finishedPosition);
        canvas.addEventListener('mousemove', draw);
        canvas.addEventListener('touchstart', startPosition);
        canvas.addEventListener('touchend', finishedPosition);
        canvas.addEventListener('touchmove', draw);

        function setColor(c) { color = c; }
        function clearBoard() { drawField(); }
        setTimeout(resize, 100);
      """))
    )
    renderHtml(content)
  }

  // --- BASE PAGE ---
  def basePage(activeLink: String, pageContents: Modifier*) = {
    "<!DOCTYPE html>" +
      html(
        head(
          meta(charset := "utf-8"),
          meta(name := "viewport", content := "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0"),
          link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
          link(rel := "stylesheet", href := "https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;700&display=swap"),
          tags2.title("GUARDIAN ELITE"),
          tags2.style(raw(getCss()))
        ),
        body(
          div(cls := "app-header d-flex justify-content-between align-items-center px-3",
            div(span(cls := "text-warning", "G"), " GUARDIAN ELITE"),
            div(cls:="d-flex align-items-center gap-3",
              a(href:="/logout", style:="text-decoration:none; color:#ff4d4d; font-size:11px; font-weight:bold; border: 1px solid #ff4d4d; padding: 2px 8px; border-radius: 4px;", "SALIR"),
              a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "⚙️")
            )
          ),
          div(cls := "container main-content", pageContents), tags2.nav(cls := "bottom-nav", a(href:="/", cls:=s"nav-item ${if(activeLink=="home") "active" else ""}", div(cls:="nav-icon", "H"), span(cls:="nav-label", "Inicio")), a(href:="/match-center", cls:=s"nav-item ${if(activeLink=="match-center") "active" else ""}", div(cls:="nav-icon", "P"), span(cls:="nav-label", "Jugar")), a(href:="/bio", cls:=s"nav-item ${if(activeLink=="bio") "active" else ""}", div(cls:="nav-icon", "B"), span(cls:="nav-label", "Bio")), a(href:="/career/legacy", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon text-warning", "⭐"), span(cls:="nav-label text-warning", "Legado")), a(href:="/tactics", cls:=s"nav-item ${if(activeLink=="tactics") "active" else ""}", div(cls:="nav-icon", "📋"), span(cls:="nav-label", "Pizarra")), a(href:="/career", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon", "T"), span(cls:="nav-label", "Trayect.")), a(href:="/history", cls:=s"nav-item ${if(activeLink=="history") "active" else ""}", div(cls:="nav-icon", "L"), span(cls:="nav-label", "Historial"))))
      ).render
  }

  def getCss() = """
    body { background-color: #121212; color: #f0f0f0; font-family: 'Oswald', sans-serif; padding-bottom: 80px; margin: 0; font-weight: 500; }

    /* MODO OSCURO FORZADO PARA INPUTS Y SELECTS */
    input, select, textarea, .form-control, .form-select {
    background-color: #2b2b2b !important;
    color: #ffffff !important;
    border: 1px solid #444 !important;
    font-weight: 600 !important;
    position: relative;
    z-index: 10;
    pointer-events: auto !important;
  }
    option { background-color: #2b2b2b; color: #ffffff; }

    /* Placeholders en gris claro para que se lean */
    ::placeholder { color: #aaa !important; opacity: 1; }

    .text-muted { color: #aaa !important; }
    .app-header { background: #1a1a1a; color: white; text-align: center; padding: 15px; font-size: 20px; font-weight: bold; border-bottom: 1px solid #333; position: sticky; top: 0; z-index: 1000; letter-spacing: 2px; }
    .main-content { padding-top: 20px; }
    .bottom-nav { position: fixed; bottom: 0; width: 100%; background: #1a1a1a; border-top: 1px solid #333; display: flex; justify-content: space-around; padding: 8px 0; z-index: 1000; box-shadow: 0 -2px 10px rgba(0,0,0,0.5); overflow-x: auto; }
    .nav-item { text-align: center; color: #888; text-decoration: none; flex: 1; transition: color 0.2s; min-width: 55px; } .nav-item.active { color: #d4af37; }
    .nav-icon { font-size: 20px; margin-bottom: 2px; } .nav-label { font-size: 9px; display: block; text-transform: uppercase; letter-spacing: 0.5px; font-weight: bold; }
    .fut-card { width: 300px; height: 500px; margin: 0 auto; position: relative; background: #d4af37; border-radius: 25px; box-shadow: 0 10px 30px rgba(0,0,0,0.5); color: #2f2f2f; overflow: hidden; text-transform: uppercase; transition: transform 0.3s ease; }
    @media (max-width: 380px) { .mobile-scale { transform: scale(0.9); transform-origin: top center; margin-bottom: -40px; } }
    .fut-card::before { content: ""; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: linear-gradient(to bottom, rgba(255,255,255,0.4) 0%, rgba(255,255,255,0) 60%); z-index: 0; pointer-events: none; }
    .left-info { position: absolute; top: 30px; left: 25px; text-align: center; z-index: 2; } .rating { font-size: 64px; font-weight: 700; line-height: 1; margin-bottom: 5px; } .position { font-size: 24px; font-weight: 400; margin-bottom: 10px; } .nation { width: 35px; border: 1px solid rgba(0,0,0,0.1); box-shadow: 1px 1px 2px rgba(0,0,0,0.2); } .club-badge { position: absolute; top: 40px; right: 25px; width: 50px; filter: drop-shadow(2px 2px 2px rgba(0,0,0,0.2)); z-index: 2; }
    .player-circle-container { position: absolute; top: 100px; left: 50%; transform: translateX(-50%); width: 190px; height: 190px; background-color: #789fc2; border: 5px solid #c2a25e; border-radius: 50%; overflow: hidden; z-index: 1; box-shadow: 0 5px 15px rgba(0,0,0,0.3); } .player-img { width: 100%; height: 100%; object-fit: cover; }
    .name-container { position: absolute; top: 300px; width: 100%; text-align: center; z-index: 2; } .player-name { font-size: 38px; font-weight: 700; letter-spacing: 2px; margin: 0; }
    .stats-container { position: absolute; bottom: 25px; width: 100%; display: flex; justify-content: center; padding: 0 20px; z-index: 2; } .stats-grid { display: grid; grid-template-columns: 1fr 1fr; column-gap: 40px; row-gap: 5px; width: 85%; } .stat-item { font-size: 18px; display: flex; align-items: center; justify-content: flex-start; } .stat-val { font-weight: 700; margin-right: 8px; font-size: 22px; min-width: 30px; text-align: right; } .stat-label { font-weight: 400; font-size: 16px; color: #4a4a4a; }
    .tm-table { background-color: white; font-size: 14px; border-radius: 5px; overflow: hidden; } .tm-table thead { background-color: #f2f2f2; color: #666; font-size: 12px; } .tm-table th, .tm-table td { padding: 10px; vertical-align: middle; }
    .achievement-box { max-width: 100% !important; } input.form-control-lg { height: 50px; font-size: 18px; } .btn-lg { height: 55px; font-size: 20px; text-transform: uppercase; letter-spacing: 2px; }
    .goal-grid-3x3 { display: grid; grid-template-columns: 1fr 1fr 1fr; grid-template-rows: 60px 60px 60px; gap: 2px; background: white; padding: 2px; border: 4px solid #aaa; margin: 10px auto; width: 220px; }
    .goal-cell { background: #eee; border: 1px solid #ccc; display: flex; align-items: center; justify-content: center; cursor: pointer; font-size: 10px; position: relative; }
    .action-marker { font-size: 20px; display: flex; gap: 2px; flex-wrap: wrap; justify-content: center; width: 100%; }
    .shot-btn.active { background-color: #ffc107; color: black; border-color: #ffc107; font-weight: bold; }
    .xx-small { font-size: 10px; display: block; }
    .field-container { width: 100%; height: 60vh; background-color: #2e7d32; border-radius: 8px; overflow: hidden; touch-action: none; }
    #rivalInput {
      position: relative !important;
      z-index: 2000 !important;
      pointer-events: auto !important;
      -webkit-user-select: text !important;
      user-select: text !important;
    }
  """
  // ==========================================
  // PAGINAS FALTANTES (RESTAURADAS)
  // ==========================================

  // --- 1. EL ORÁCULO (Predicción de Altura) ---
  @cask.get("/oracle")
  def oraclePage(request: cask.Request, hDad: String = "180", hMom: String = "170") = withAuth(request) {
    val hd = try hDad.toDouble catch { case _: Exception => 180.0 }
    val hm = try hMom.toDouble catch { case _: Exception => 170.0 }

    val card = DatabaseManager.getLatestCardData()
    val edadActual = DatabaseManager.calcularEdadExacta(card.fechaNacimiento)
    val (p50H, p15H, p85H, p50W, p15W, p85W) = DatabaseManager.getOMSPercents()

    val predictionHtml = DatabaseManager.getOraclePrediction(hd, hm)
    val bioInsights = DatabaseManager.getOracleInsights()
    val growthJson = DatabaseManager.getGrowthHistory()

    // 1. Definimos el contenido (Solo Modifiers de Scalatags)
    val mainContent = div(
      div(cls:="row justify-content-center",
        div(cls:="col-md-8 col-12",
          h2(cls:="text-center text-info mb-4", "🔮 EL ORÁCULO"),

          // Tarjeta Inteligencia
          div(cls:="card bg-dark border-info shadow mb-4",
            div(cls:="card-header bg-info text-dark fw-bold d-flex justify-content-between align-items-center",
              span("🧠 INTELIGENCIA DEPORTIVA"),
              span(cls:="badge bg-dark text-info", s"Edad: $edadActual años")
            ),
            div(cls:="card-body", raw(bioInsights))
          ),

          // Tarjeta Gráfico
          div(cls:="card bg-dark border-secondary shadow mb-4",
            div(cls:="card-header text-white small", "Evolución Biométrica Histórica"),
            div(cls:="card-body", style:="height: 300px; position: relative;",
              canvas(id:="growthChart")
            )
          ),

          // Tabla OMS
          div(cls:="card bg-dark border-secondary shadow mb-4",
            div(cls:="card-header text-muted small fw-bold text-uppercase", s"📊 Referencia OMS para $edadActual años"),
            div(cls:="card-body p-0",
              table(cls:="table table-dark table-sm mb-0 small text-center",
                thead(tr(th("Percentil"), th("Altura (cm)"), th("Peso (kg)"))),
                tbody(
                  tr(td("P15 (Bajo)"), td(f"$p15H%.1f"), td(f"$p15W%.1f")),
                  tr(cls:="table-active text-info", td("P50 (Media)"), td(f"$p50H%.1f"), td(f"$p50W%.1f")),
                  tr(td("P85 (Alto)"), td(f"$p85H%.1f"), td(f"$p85W%.1f"))
                )
              )
            )
          ),

          // Predicción Genética
          div(cls:="card bg-dark text-white border-secondary shadow p-4",
            h4(cls:="text-center text-warning mb-3", "Predicción Altura Final"),
            div(cls:="bg-secondary bg-opacity-10 p-3 rounded mb-3", raw(predictionHtml)),
            form(action:="/oracle", method:="get", cls:="mt-4 border-top border-secondary pt-3",
              div(cls:="row",
                div(cls:="col-6", label(cls:="small text-muted fw-bold", "Papá (cm)"), input(tpe:="number", name:="hDad", value:=hDad, cls:="form-control bg-dark text-white text-center")),
                div(cls:="col-6", label(cls:="small text-muted fw-bold", "Mamá (cm)"), input(tpe:="number", name:="hMom", value:=hMom, cls:="form-control bg-dark text-white text-center"))
              ),
              div(cls:="d-grid mt-3", button(tpe:="submit", cls:="btn btn-outline-info fw-bold", "🔄 Recalcular"))
            )
          )
        )
      ),
      // 2. Script inyectado (Aseguramos que Chart.js esté cargado en basePage)
      script(src := "https://cdn.jsdelivr.net/npm/chart.js"),
      script(raw(s"""
      window.addEventListener('load', function() {
        const rawData = $growthJson;
        const ctx = document.getElementById('growthChart').getContext('2d');
        new Chart(ctx, {
            type: 'line',
            data: {
                labels: rawData.labels,
                datasets: [
                    { label: 'Altura (cm)', data: rawData.altura, borderColor: '#0dcaf0', yAxisID: 'y', tension: 0.3, fill: true, backgroundColor: 'rgba(13, 202, 240, 0.1)' },
                    { label: 'Peso (kg)', data: rawData.peso, borderColor: '#ffc107', yAxisID: 'y1', tension: 0.3, fill: false }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                scales: {
                    y: { type: 'linear', position: 'left', ticks: { color: '#0dcaf0' }, title: { display: true, text: 'cm', color: '#0dcaf0' }, grid: { color: '#333' } },
                    y1: { type: 'linear', position: 'right', ticks: { color: '#ffc107' }, title: { display: true, text: 'kg', color: '#ffc107' }, grid: { drawOnChartArea: false } }
                },
                plugins: {
                    legend: { labels: { color: '#fff', font: { family: 'Oswald' } } }
                }
            }
        });
      });
    """))
    )

    // 3. Renderizado final
    renderHtml(basePage("bio", mainContent))
  }
  // --- 2. MONEYBALL (Distribución Táctica) ---
  @cask.get("/distribution")
  def distributionPage() = {
    val tac = DatabaseManager.getTacticalStats()

    // Función auxiliar para calcular porcentajes seguros
    def pct(n: Double, d: Double): Int = if(d > 0) ((n/d)*100).toInt else 0

    val totG = if(tac("g_tot") > 0) tac("g_tot").toDouble else 1.0
    val (ga, gm, gr) = (pct(tac("g_alt"), totG), pct(tac("g_med"), totG), pct(tac("g_ras"), totG))
    val (gl, gc, gd) = (pct(tac("g_izq"), totG), pct(tac("g_cen"), totG), pct(tac("g_der"), totG))

    val totP = if(tac("p_tot") > 0) tac("p_tot").toDouble else 1.0
    val (pa, pm, pr) = (pct(tac("p_alt"), totP), pct(tac("p_med"), totP), pct(tac("p_ras"), totP))
    val (pl, pc, pd) = (pct(tac("p_izq"), totP), pct(tac("p_cen"), totP), pct(tac("p_der"), totP))

    // Celda táctica visual
    def tCell(label: String, p: Int, color: String) = div(
      cls:=s"flex-fill text-center p-3 border border-secondary $color",
      style:="color: #000; font-weight: 800;",
      div(style:="font-size:14px;", label),
      div(style:="font-size:24px;", s"$p%")
    )

    val content = basePage("bio", div(cls:="row justify-content-center",
      div(cls:="col-md-8 col-12",
        h2(cls:="text-center text-warning mb-4", "📊 MONEYBALL TACTICS"),

        // Sección Goles Encajados
        div(cls:="card bg-dark text-white border-danger shadow mb-4",
          div(cls:="card-header bg-danger text-white fw-bold text-center", "ZONA DE ENCAJE (Debilidades)"),
          div(cls:="card-body p-0",
            div(cls:="d-flex", tCell("ALTA", ga, "bg-danger bg-opacity-75"), tCell("MEDIA", gm, "bg-warning bg-opacity-75"), tCell("BAJA", gr, "bg-light bg-opacity-75")),
            div(cls:="d-flex", tCell("IZQ", gl, "bg-danger bg-opacity-75"), tCell("CEN", gc, "bg-warning bg-opacity-75"), tCell("DER", gd, "bg-danger bg-opacity-75"))
          )
        ),

        // Sección Paradas
        div(cls:="card bg-dark text-white border-success shadow mb-4",
          div(cls:="card-header bg-success text-white fw-bold text-center", "ZONA DE SEGURIDAD (Fortalezas)"),
          div(cls:="card-body p-0",
            div(cls:="d-flex", tCell("ALTA", pa, "bg-success bg-opacity-75"), tCell("MEDIA", pm, "bg-info bg-opacity-75"), tCell("BAJA", pr, "bg-light bg-opacity-75")),
            div(cls:="d-flex", tCell("IZQ", pl, "bg-success bg-opacity-75"), tCell("CEN", pc, "bg-info bg-opacity-75"), tCell("DER", pd, "bg-success bg-opacity-75"))
          )
        ),

        div(cls:="text-center", a(href:="/bio", cls:="btn btn-outline-light", "Volver"))
      )
    ))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/bio/medical/upload")
def uploadMedical(fecha: String,
                  tipo: String,
                  esPrevio: String = "false",
                  archivo: cask.model.FormFile) = {
    
    val isPrevio = esPrevio == "on"

    // 1. Corrección de los miembros de FormFile
    // En Cask, los campos se llaman 'data' (bytes) y 'fileName' (nombre)
    val fileBytes = archivo.data 
    val nameOfFile = archivo.fileName 

    if (fileBytes.nonEmpty) {
      // 2. Proceso para Gemini
      val base64Content = java.util.Base64.getEncoder.encodeToString(fileBytes)
      val mimeType = if (nameOfFile.toLowerCase.endsWith(".pdf")) "application/pdf" else "image/jpeg"

      // 3. Sistema de persistencia (Caché)
      // Usamos saveMedicalReport para que se gestione la IA y la tabla medical_vault
      DatabaseManager.saveMedicalReport(fecha, tipo, base64Content, isPrevio)
    }

    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio"))
}
  // --- 3. MODO LEGADO (RPG) ---
  @cask.get("/career/legacy")
  def legacyPage() = {
    val rpg = DatabaseManager.getRPGStatus()
    val percent = if(rpg.nextLevelXp > 0) (rpg.xp.toDouble / rpg.nextLevelXp.toDouble * 100).toInt else 100

    val content = basePage("career", div(cls:="row justify-content-center",
      div(cls:="col-md-8 col-12",
        h2(cls:="text-center text-warning mb-4", "⭐ MODO LEGADO"),

        div(cls:="card bg-dark text-white border-warning shadow mb-4",
          div(cls:="card-body text-center",
            h6(cls:="text-muted text-uppercase letter-spacing-2", "Rango Actual"),
            h1(cls:="display-4 fw-bold text-warning mb-0", rpg.titulo),
            div(cls:="badge bg-secondary mb-3", s"Nivel ${rpg.nivel}"),

            div(cls:="progress bg-secondary mb-2", style:="height: 25px;",
              div(cls:="progress-bar bg-warning progress-bar-striped progress-bar-animated",
                style:=s"width: $percent%", s"${rpg.xp} XP")
            ),
            div(cls:="d-flex justify-content-between small text-muted",
              span("Inicio Nivel"),
              span(s"Siguiente: ${rpg.nextLevelXp} XP")
            )
          )
        ),

        div(cls:="row g-2",
          div(cls:="col-6", div(cls:="p-3 border border-secondary rounded text-center bg-secondary bg-opacity-10", h3("🛡️"), h6("Muro"), small("Bonus por Portería a Cero"))),
          div(cls:="col-6", div(cls:="p-3 border border-secondary rounded text-center bg-secondary bg-opacity-10", h3("🧤"), h6("Manos de Oro"), small("Bonus por Paradas")))
        ),

        div(cls:="alert alert-dark border-info mt-4 text-center",
          h5(cls:="text-info", "Sistema de Puntos"),
          ul(cls:="list-unstyled small text-start d-inline-block",
            li("• Partido Jugado: +50 XP"),
            li("• Portería a Cero: +100 XP"),
            li("• Parada: +5 XP"),
            li("• Nota > 7.0: +100 XP (Bonus)")
          )
        )
      )
    ))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }



  def renderHtml(content: String, headers: Seq[(String, String)] = Nil): cask.Response[Array[Byte]] = {
    cask.Response(
      data = content.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8") ++ headers
    )
  }

  def renderRedirect(url: String): cask.Response[Array[Byte]] = {
    val red = cask.Redirect(url)
    cask.Response(Array.empty[Byte], red.statusCode, red.headers, red.cookies)
  }
  initialize()

}



