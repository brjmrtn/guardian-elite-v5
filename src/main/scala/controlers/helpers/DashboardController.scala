import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object DashboardController extends cask.Routes {

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

  initialize()
}