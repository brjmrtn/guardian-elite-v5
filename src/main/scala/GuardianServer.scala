import cask._
import upickle.default._
import scalatags.Text.all._
import scalatags.Text.tags2
import java.net.URLEncoder

object GuardianServer extends cask.MainRoutes {
  override def host: String = "0.0.0.0"
  override def port: Int = sys.env.getOrElse("PORT", "8081").toInt

  // --- HELPER: CORRECCIÓN DE CARACTERES ---
  def fixEncoding(s: String): String = {
    try {
      if (s.contains("Ã")) new String(s.getBytes("ISO-8859-1"), "UTF-8") else s
    } catch {
      case e: Exception => s
    }
  }

  // --- HELPER: RENDERIZADO DE FILA DE PARTIDO ---
  def renderMatchRow(m: MatchLog) = {
    val pParts = m.resultado.split("-").map(s => try s.trim.toInt catch { case _:Exception => 0 })
    val colorClass = if(pParts.length >= 2) {
      if (pParts(0) > pParts(1)) "text-success" else if (pParts(0) == pParts(1)) "text-warning" else "text-danger"
    } else "text-muted"

    val extraBtn = if(m.video.nonEmpty) a(href:=m.video, target:="_blank", cls:="btn btn-sm btn-danger py-0 ms-1", style:="font-size:10px", "V") else span("")
    val waText = URLEncoder.encode(s"MATCH: ${m.rival} ${m.resultado}", "UTF-8").replace("+", "%20")

    tr(
      td(cls:="fw-bold small", fixEncoding(m.rival), extraBtn, br, span(cls:="text-muted xx-small", s"${m.fecha} ${m.clima}")),
      td(cls:=s"text-center fw-bold $colorClass", m.resultado),
      td(cls:="text-center", span(cls:="badge bg-dark text-warning", m.nota)),
      td(cls:="text-end",
        a(href:=s"https://wa.me/?text=$waText", target:="_blank", cls:="btn btn-sm btn-success me-1", style:="padding:2px 6px;", "W"),
        a(href:=s"/match/edit/${m.id}", cls:="btn btn-sm btn-outline-primary me-1", style:="padding:2px 6px;", "E"),
        a(href:=s"/match/delete/${m.id}", onclick:="return confirm('Borrar?');", cls:="btn btn-sm btn-outline-danger", style:="padding:2px 6px;", "X")
      )
    )
  }

  // 1. DASHBOARD
  @cask.get("/")
  def dashboard() = {
    val card = DatabaseManager.getLatestCardData()
    val matches = DatabaseManager.getMatchesList()
    val chartData = DatabaseManager.getChartData()
    val logros = DatabaseManager.getAchievements()
    val aiMessage = DatabaseManager.getDeepAnalysis()
    val tac = DatabaseManager.getTacticalStats()
    val objs = DatabaseManager.getSeasonObjectives()
    val upcoming = DatabaseManager.getUpcomingMatches().headOption

    val last5 = matches.take(5)
    val avgLast5 = if (last5.nonEmpty) last5.map(_.nota).sum / last5.length else 0.0
    val avgSeason = if (matches.nonEmpty) matches.map(_.nota).sum / matches.length else 0.0
    val trendDiff = avgLast5 - avgSeason
    val trendText = if (trendDiff > 0.5) "MEJORA" else if (trendDiff < -0.5) "BAJA" else "IGUAL"
    val trendColor = if (trendDiff > 0) "text-success" else if (trendDiff < 0) "text-danger" else "text-muted"
    val radarData = s"""[${card.div}, ${card.han}, ${card.kic}, ${card.ref}, ${card.spd}, ${card.pos}]"""

    val rawMedia = (card.divRaw * 0.20) + (card.hanRaw * 0.20) + (card.kicRaw * 0.15) + (card.refRaw * 0.20) + (card.spdRaw * 0.05) + (card.posRaw * 0.20)
    val xpPercent = ((rawMedia - rawMedia.floor) * 100).toInt

    def pct(n: Double, d: Double): Int = if(d > 0) ((n/d)*100).toInt else 0
    val totG = if(tac("g_tot") > 0) tac("g_tot").toDouble else 1.0
    val (ga, gm, gr) = (pct(tac("g_alt"),totG), pct(tac("g_med"),totG), pct(tac("g_ras"),totG))
    val (gl, gc, gd) = (pct(tac("g_izq"),totG), pct(tac("g_cen"),totG), pct(tac("g_der"),totG))
    val totP = if(tac("p_tot") > 0) tac("p_tot").toDouble else 1.0
    val (pa, pm, pr) = (pct(tac("p_alt"),totP), pct(tac("p_med"),totP), pct(tac("p_ras"),totP))
    val (pl, pc, pd) = (pct(tac("p_izq"),totP), pct(tac("p_cen"),totP), pct(tac("p_der"),totP))

    def tactCell(label: String, valPct: Int, colorBg: String) = div(cls:=s"flex-fill text-center p-1 border border-secondary $colorBg", style:="font-size: 10px; color: black; font-weight: bold;", div(label), div(s"$valPct%"))

    val nextMatchWidget = upcoming match {
      case Some(m) => div(cls:="alert alert-dark border-warning shadow p-3 mb-3", div(cls:="d-flex justify-content-between align-items-center", div(h6(cls:="text-muted mb-0 small", "PROXIMO PARTIDO"), h4(cls:="text-white fw-bold mb-0", fixEncoding(m.rival))), div(cls:="text-end", span(cls:="badge bg-warning text-dark", m.fecha), a(href:=s"/match-center?scheduleId=${m.id}", cls:="btn btn-sm btn-outline-light ms-2", "JUGAR"))))
      case None => div(cls:="alert alert-dark border-secondary p-2 mb-3 text-center text-muted small", "Sin partidos programados.")
    }

    val objectivesContent = if(objs.nonEmpty) {
      val items = for(o <- objs) yield {
        val p = Math.min(100, (o.actual / o.meta.toDouble * 100).toInt)
        val valStr = if(o.tipo=="MediaNota") f"${o.actual}%.1f" else o.actual.toInt.toString
        div(cls:="mb-2", div(cls:="d-flex justify-content-between xx-small text-white mb-1", span(o.descripcion), span(s"$valStr / ${o.meta}")), div(cls:="progress", style:="height: 6px;", div(cls:="progress-bar bg-info", style:=s"width:$p%")))
      }
      div(cls:="card bg-dark border-info shadow p-2 mt-3", h6(cls:="text-center text-info mb-2 small fw-bold", "OBJETIVOS"), items)
    } else div()

    val palmaresContent = if (logros.nonEmpty) {
      val items = for (l <- logros) yield div(cls := "d-inline-block m-1 p-1 rounded achievement-item", div(style := "font-size: 24px;", l.icono + (if(l.cantidad > 1) s" x${l.cantidad}" else "")), div(cls := "small text-warning fw-bold", style:="font-size: 10px;", l.nombre))
      div(cls := "card bg-dark border-warning shadow mt-3 mx-auto achievement-box", div(cls := "card-header bg-warning text-dark fw-bold text-center py-1", "PALMARES"), div(cls := "card-body text-center p-2", items))
    } else div()

    val content = basePage("home", div(cls := "row justify-content-center",
      div(cls := "col-md-5 mb-4",
        div(cls := "d-flex justify-content-center mobile-scale", div(cls := "fut-card", div(cls := "left-info", div(cls := "rating", card.media), div(cls := "position", card.posicion), img(src := card.flagUrl, cls := "nation")), img(src := card.clubUrl, cls := "club-badge"), div(cls := "player-circle-container", img(src := card.fotoUrl, cls := "player-img")), div(cls := "name-container", div(cls := "player-name", card.nombre), div(style:="font-size:12px; margin-top:-5px; opacity:0.8;", card.clubNombre)), div(cls := "stats-container", div(cls := "stats-grid", div(cls := "stat-item", span(cls:="stat-val", card.div), span(cls:="stat-label", "DIV")), div(cls := "stat-item", span(cls:="stat-val", card.kic), span(cls:="stat-label", "KIC")), div(cls := "stat-item", span(cls:="stat-val", card.spd), span(cls:="stat-label", "SPD")), div(cls := "stat-item", span(cls:="stat-val", card.han), span(cls:="stat-label", "HAN")), div(cls := "stat-item", span(cls:="stat-val", card.ref), span(cls:="stat-label", "REF")), div(cls := "stat-item", span(cls:="stat-val", card.pos), span(cls:="stat-label", "POS")))))),
        div(cls:="mt-1 mb-4 text-center", div(cls:="d-flex justify-content-between text-white xx-small px-4", span(s"Nivel ${card.media}"), span(s"${xpPercent}% XP"), span(s"Nivel ${card.media+1}")), div(cls:="progress mx-4", style:="height: 6px; background-color: #333;", div(cls:="progress-bar bg-warning", style:=s"width: $xpPercent%"))),
        div(cls:="d-grid mb-3 mt-3", a(href:="/scouting", cls:="btn btn-outline-info shadow fw-bold", "SCOUTING RIVALES"), a(href:="/penalties", cls:="btn btn-outline-danger shadow fw-bold mt-2", "LABORATORIO PENALTIS")),
        div(cls:="card bg-dark border-secondary shadow p-3 mb-3", div(cls:="d-flex justify-content-between align-items-center", div(h6(cls:="text-muted mb-0", "Racha (Ultimos 5)"), h3(cls:=s"mb-0 $trendColor fw-bold", f"$avgLast5%2.2f $trendText")), div(cls:="text-end", span(cls:="small text-muted", "Media Temp"), div(cls:="fw-bold text-white", f"$avgSeason%2.2f")))),
        objectivesContent
      ),
      div(cls := "col-md-5",
        nextMatchWidget,
        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header border-secondary text-warning fw-bold py-1 text-uppercase text-center small", "Scouting Profile"), div(cls := "card-body p-1 d-flex justify-content-center", div(style:="width: 250px; height: 250px;", canvas(id := "radarChart")))),
        div(cls := "alert alert-dark border-info shadow p-3", role:="alert", div(cls:="d-flex align-items-center mb-2", span(style:="font-size: 24px; margin-right: 10px;", "🧠"), strong(cls:="text-info", "IA NEURO-SCOUT")), div(cls:="text-light small fst-italic lh-sm", raw(aiMessage))),
        div(cls := "card bg-dark text-white border-secondary shadow mb-4", div(cls := "card-header border-secondary text-info fw-bold py-2 small", "Rendimiento"), div(cls := "card-body p-2", canvas(id := "growthChart", style := "max-height: 150px;"))),
        div(cls:="row mt-3", div(cls:="col-6 pe-1", div(cls:="card bg-dark border-danger shadow p-1", h6(cls:="text-center text-danger mb-2 small fw-bold", "DONDE TE MARCAN"), div(cls:="d-flex mb-1", tactCell("ALTA", ga, "bg-danger bg-opacity-75"), tactCell("MEDIA", gm, "bg-warning bg-opacity-75"), tactCell("BAJA", gr, "bg-light bg-opacity-75")), div(cls:="d-flex", tactCell("IZQ", gl, "bg-danger bg-opacity-75"), tactCell("CEN", gc, "bg-warning bg-opacity-75"), tactCell("DER", gd, "bg-danger bg-opacity-75")))), div(cls:="col-6 ps-1", div(cls:="card bg-dark border-success shadow p-1", h6(cls:="text-center text-success mb-2 small fw-bold", "DONDE PARAS"), div(cls:="d-flex mb-1", tactCell("ALTA", pa, "bg-success bg-opacity-75"), tactCell("MEDIA", pm, "bg-info bg-opacity-75"), tactCell("BAJA", pr, "bg-light bg-opacity-75")), div(cls:="d-flex", tactCell("IZQ", pl, "bg-success bg-opacity-75"), tactCell("CEN", pc, "bg-info bg-opacity-75"), tactCell("DER", pd, "bg-success bg-opacity-75"))))),
        palmaresContent
      )
    ), script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""const ctxLine=document.getElementById('growthChart');const dbData=$chartData;if(ctxLine){new Chart(ctxLine,{type:'line',data:{labels:dbData.labels,datasets:[{label:'Media',data:dbData.data,borderColor:'#36A2EB',backgroundColor:'rgba(54,162,235,0.2)',borderWidth:2,tension:0.3,pointBackgroundColor:'#d4af37',fill:true}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{y:{ticks:{color:'#aaa'},grid:{color:'#444'},pointLabels:{color:'#fff'}},x:{display:false}}}});}const ctxRadar=document.getElementById('radarChart');if(ctxRadar){new Chart(ctxRadar,{type:'radar',data:{labels:['DIV','HAN','KIC','REF','SPD','POS'],datasets:[{label:'Stats',data:$radarData,backgroundColor:'rgba(212,175,55,0.4)',borderColor:'#d4af37',pointBackgroundColor:'#fff',pointBorderColor:'#d4af37',borderWidth:2}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{r:{angleLines:{color:'#444'},grid:{color:'#444'},pointLabels:{color:'#fff',font:{size:12,family:'Oswald'}},ticks:{display:false,backdropColor:'transparent'},suggestedMin:40,suggestedMax:90}}}});}""")))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  // 2. MATCH CENTER
  @cask.get("/match-center")
  def matchCenterPage(scheduleId: Int = 0) = {
    val today = java.time.LocalDate.now().toString
    var preRival = ""; var preFecha = today; var isScheduled = false
    if(scheduleId > 0) {
      val matches = DatabaseManager.getUpcomingMatches()
      matches.find(_.id == scheduleId).foreach { m => preRival = m.rival; preFecha = m.fecha; isScheduled = true }
    }

    val gridCells = for(r <- Seq("T","M","B"); c <- Seq("L","C","R")) yield {
      val zoneId = r + c
      div(cls:=s"goal-cell zone-$zoneId", onclick:=s"registerAction('$zoneId')", span(cls:="action-marker", ""))
    }

    val content = basePage("match-center", div(cls := "row justify-content-center", div(cls := "col-md-6 col-12", div(cls := "card bg-dark text-white border-warning shadow",
      div(cls := "card-header bg-warning text-dark fw-bold text-center", "MATCH TRACKER PRO"),
      div(cls := "card-body p-3", form(action := "/match-center/save", method := "post", attr("accept-charset") := "UTF-8",
        input(tpe:="hidden", name:="scheduleId", value:=scheduleId.toString),
        div(cls := "mb-3", label(cls := "form-label text-warning small", "RIVAL"), input(tpe := "text", name := "rival", cls := "form-control form-control-lg", value:=fixEncoding(preRival), placeholder := "Ej: Rayo", required := true)),
        div(cls := "mb-3", label(cls := "form-label text-white small", "FECHA"), input(tpe := "date", name := "fecha", cls := "form-control", value := preFecha)),
        div(cls := "row mb-3 bg-secondary bg-opacity-25 p-2 rounded mx-0", div(cls := "col-4 text-center", label(cls := "small fw-bold", "GOLES (GC)"), input(tpe := "number", name := "gc", id:="gcInput", cls := "form-control text-center bg-danger text-white border-0 fw-bold fs-4", value := "0", readonly:=true)), div(cls := "col-4 text-center", label(cls := "small fw-bold", "PARADAS"), input(tpe := "number", name := "paradas", id:="parInput", cls := "form-control text-center bg-success text-white border-0 fw-bold fs-4", value := "0", readonly:=true)), div(cls := "col-4 text-center", label(cls := "small fw-bold", "A FAVOR (GF)"), input(tpe := "number", name := "gf", cls := "form-control text-center", value := "0", attr("inputmode"):="numeric"))),

        div(cls:="mb-4 p-2 border border-info rounded bg-info bg-opacity-10", label(cls:="form-label text-info small fw-bold w-100 text-center", "DISTRIBUCIÓN"),
          div(cls:="row mb-2 align-items-center", div(cls:="col-4 text-end small", "CORTO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pc', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pc', false)", "❌"), input(tpe:="text", id:="display_pc", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true)))),
          div(cls:="row align-items-center", div(cls:="col-4 text-end small", "LARGO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pl', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pl', false)", "❌"), input(tpe:="text", id:="display_pl", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true)))),
          input(tpe:="hidden", name:="passData", id:="passData", value:="0,0,0,0"), input(tpe:="hidden", id:="pcTot", value:="0"), input(tpe:="hidden", id:="pcOk", value:="0"), input(tpe:="hidden", id:="plTot", value:="0"), input(tpe:="hidden", id:="plOk", value:="0")
        ),

        div(cls:="tactical-section mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10", div(cls:="d-flex justify-content-center mb-2", div(cls:="btn-group w-100", role:="group", input(tpe:="radio", cls:="btn-check", name:="mode", id:="modeSave", autocomplete:="off", checked:=true, onclick:="setMode('save')"), label(cls:="btn btn-outline-success", attr("for"):="modeSave", "MODO PARADA"), input(tpe:="radio", cls:="btn-check", name:="mode", id:="modeGoal", autocomplete:="off", onclick:="setMode('goal')"), label(cls:="btn btn-outline-danger", attr("for"):="modeGoal", "MODO GOL"))),

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

          label(cls:="form-label text-white small fw-bold w-100 text-center mt-3", "ZONAS DE ATAQUE (Tiros)"), div(cls:="shot-origin d-flex gap-2 justify-content-center", div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Left')", "Izquierda"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Center')", "Centro"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Right')", "Derecha"), input(tpe:="hidden", name:="zonaTiros", id:="hiddenOrigin"))),
        div(cls:="mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10", label(cls:="form-label text-white small fw-bold w-100 text-center", "ENTORNO"), div(cls:="row mb-2", div(cls:="col-6", label(cls:="small text-muted", "Clima"), select(name:="clima", cls:="form-select form-select-sm bg-dark text-white", option(value:="Sol", "Sol"), option(value:="Nubes", "Nubes"), option(value:="Lluvia", "Lluvia"), option(value:="Nublado", "Nublado"), option(value:="Frio", "Frio"), option(value:="Viento", "Viento"))), div(cls:="col-6", label(cls:="small text-muted", "Temp (C)"), input(tpe:="number", name:="temp", cls:="form-control form-control-sm bg-dark text-white", value:="20")))),

        div(cls:="mb-3 p-2 border border-danger rounded bg-danger bg-opacity-10",
          label(cls:="form-label text-danger small fw-bold w-100 text-center", "SALA DE VIDEO"),
          input(tpe:="url", name:="video", cls:="form-control form-control-sm bg-dark text-white", placeholder:="Link Video (Youtube/Drive)")
        ),

        div(cls:="mb-3", label(cls:="form-label text-white small fw-bold", "ANOTACIONES DEL ENTRENADOR"), textarea(name:="notas", cls:="form-control form-control-sm bg-dark text-white", rows:="3", placeholder:="Notas generales: Saques, posicionamiento, lectura del juego, voz de mando...")),
        div(cls:="mb-4", label(cls:="form-label text-danger small fw-bold", "ANALISIS GOLES / REACCION"), textarea(name:="reaccion", cls:="form-control form-control-sm bg-dark text-white border-danger", rows:="3", placeholder:="Descripcion goles encajados y reaccion mental posterior.")),
        div(cls := "mb-3", label(cls := "form-label small", "MINUTOS"), input(tpe := "number", name := "minutos", cls := "form-control", value := "40", attr("inputmode") := "numeric")),
        div(cls := "mb-4", label(cls := "form-label text-warning fw-bold small", "NOTA (0-10)"), input(tpe := "number", step := "0.1", name := "nota", cls := "form-control form-control-lg text-center fw-bold", placeholder := "Ej: 7.5", required := true, attr("inputmode") := "decimal")),
        div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg py-3", "GUARDAR PARTIDO"))
      ), script(raw("""
        var currentMode='save';var goals=[];var saves=[];var origins=[];
        function setMode(mode){currentMode=mode;}
        function registerAction(zone){const cell=document.querySelector('.zone-'+zone);const marker=cell.querySelector('.action-marker');if(currentMode==='save'){saves.push(zone);marker.innerHTML+='<span style="color:#198754; font-weight:bold;">●</span>';document.getElementById('parInput').value=parseInt(document.getElementById('parInput').value||0)+1;document.getElementById('hiddenParadas').value=saves.join(',');}else{goals.push(zone);marker.innerHTML+='<span style="color:#dc3545; font-weight:bold;">●</span>';document.getElementById('gcInput').value=parseInt(document.getElementById('gcInput').value||0)+1;document.getElementById('hiddenGoles').value=goals.join(',');}}
        function incCounter(key){var el=document.getElementById('cnt_'+key); var val=parseInt(el.value||0)+1; el.value=val; document.getElementById('disp_'+key).value=val; updateActionData();}
        function updateActionData(){var d = [document.getElementById('cnt_p1v1').value, document.getElementById('cnt_pAir').value, document.getElementById('cnt_pPie').value]; document.getElementById('actionData').value = d.join(',');}
        function toggleOrigin(el,origin){el.classList.toggle('active');el.classList.toggle('btn-warning');if(origins.includes(origin)){origins=origins.filter(o=>o!==origin);}else{origins.push(origin);}document.getElementById('hiddenOrigin').value=origins.join(',');}
        function pass(type, success) { var totEl = document.getElementById(type+'Tot'); var okEl = document.getElementById(type+'Ok'); var dispEl = document.getElementById('display_'+type); var t = parseInt(totEl.value)+1; var o = parseInt(okEl.value) + (success ? 1 : 0); totEl.value=t; okEl.value=o; dispEl.value = o + '/' + t; updatePassData(); }
        function updatePassData(){var d = [document.getElementById('pcTot').value, document.getElementById('pcOk').value, document.getElementById('plTot').value, document.getElementById('plOk').value]; document.getElementById('passData').value = d.join(',');}
      """))
      )))))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.postForm("/match-center/save")
  def saveMatch(scheduleId: Int, rival: String, gf: Int, gc: Int, minutos: Int, nota: Double, paradas: Int, zonaGoles: String, zonaTiros: String, zonaParadas: String, clima: String, temp: Int, notas: String, video: String, reaccion: String, fecha: String, mode: String, passData: String, actionData: String) = {
    val pArr = passData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (pcTot, pcOk, plTot, plOk) = if(pArr.length >= 4) (pArr(0), pArr(1), pArr(2), pArr(3)) else (0,0,0,0)
    val aArr = actionData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (p1v1, pAir, pPie) = if(aArr.length >= 3) (aArr(0), aArr(1), aArr(2)) else (0,0,0)
    val cleanRival = fixEncoding(rival); val cleanNotas = fixEncoding(notas); val cleanReaccion = fixEncoding(reaccion)
    val c = DatabaseManager.getLatestCardData()
    val n = StatsCalculator.calculateGrowth(c, minutos, gc, nota, paradas, pcTot, pcOk, plTot, plOk)
    DatabaseManager.updateStats(n)
    if (scheduleId > 0) DatabaseManager.playScheduledMatch(scheduleId, gf, gc, minutos, nota, paradas, cleanNotas, video, cleanReaccion, clima, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, pcTot, pcOk, plTot, plOk)
    else DatabaseManager.logMatch(cleanRival, gf, gc, minutos, nota, n.media, paradas, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, clima, temp, cleanNotas, video, cleanReaccion, fecha, pcTot, pcOk, plTot, plOk)
    val d = n.media - c.media
    val msg = if(d > 0) s"SUBIDA DE NIVEL! +$d" else "Experiencia acumulada..."
    val htmlStr = doctype("html")(html(head(meta(charset := "utf-8"), tags2.title("Guardado"), tags2.style(raw(getCss()))), body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';", h1("OK"), h2(style := "color: #d4af37;", "ANALISIS GUARDADO"), div(style := "margin: 30px auto; width: 300px; background: #333; padding: 20px; border-radius: 10px;", h3("Media Global"), div(style := "font-size: 50px; font-weight: bold;", c.media, span(style:="color: #28a745; margin-left: 10px;", "-> " + n.media)), p(style := "color: #ffc107;", msg)), div(style := "margin-top: 40px;", a(href := "/", cls := "btn btn-outline-light btn-lg", "Volver a Inicio"))))).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  // 3. SCOUTING
  @cask.get("/scouting")
  def scoutingPage(query: String = "") = {
    val (matches, stats) = if(query.nonEmpty) DatabaseManager.getRivalScouting(query) else (List[MatchLog](), Map[String,Int]())
    val rivalInfo = if(query.nonEmpty) DatabaseManager.getRivalInfo(query) else None

    val rivalCardWidget = if(query.nonEmpty) {
      val estiloVal = rivalInfo.map(_.estilo).getOrElse("Desconocido")
      val clavesVal = rivalInfo.map(_.claves).getOrElse("")
      val notasVal = rivalInfo.map(_.notas).getOrElse("")
      div(cls:="card bg-dark border-secondary shadow mb-4", div(cls:="card-header bg-secondary text-white fw-bold", "FICHA RIVAL"), div(cls:="card-body", form(action:="/scouting/save_rival", method:="post",
        input(tpe:="hidden", name:="nombre", value:=query),
        div(cls:="mb-2", label(cls:="small text-muted", "Estilo"), select(name:="estilo", cls:="form-select form-select-sm bg-dark text-white", option(value:="Desconocido","?"), option(value:="Directo","Balon Largo"), option(value:="Combinativo","Toque"), option(value:="Contra","Contraataque"), attr("value"):=estiloVal)),
        div(cls:="mb-2", label(cls:="small text-muted", "Claves"), textarea(name:="claves", cls:="form-control form-control-sm bg-dark text-white", rows:="2", fixEncoding(clavesVal))),
        div(cls:="mb-2", label(cls:="small text-muted", "Notas"), textarea(name:="notas", cls:="form-control form-control-sm bg-dark text-white", rows:="2", fixEncoding(notasVal))),
        button(tpe:="submit", cls:="btn btn-sm btn-outline-warning w-100", "Guardar Ficha")
      )))
    } else div()

    val resultsWidget = if(query.nonEmpty && matches.isEmpty) div(cls:="alert alert-warning text-center", s"Sin datos vs '$query'")
    else if(matches.nonEmpty) {
      val rows = for(m <- matches) yield {
        val extra = if(m.video.nonEmpty) a(href:=m.video, target:="_blank", cls:="btn btn-sm btn-outline-danger w-100", "Video") else span("")
        div(cls:="card bg-dark border-secondary shadow mb-3", div(cls:="card-body", div(cls:="d-flex justify-content-between align-items-center mb-2", div(strong(cls:="text-warning", m.fecha), span(cls:="ms-2 badge bg-secondary", m.clima)), div(cls:="fs-5 fw-bold text-white", m.resultado)), if(m.notas.nonEmpty) div(cls:="alert alert-dark border-secondary p-2 small text-light fst-italic mb-2", s"Nota: ${fixEncoding(m.notas)}"), extra))
      }
      div(
        div(cls:="card bg-secondary bg-opacity-25 border-info mb-4 p-3", h5(cls:="text-center text-white mb-3", s"Vs ${matches.head.rival}"), div(cls:="d-flex justify-content-around text-center text-white", div(h3(stats("pj")), span(cls:="small text-muted", "PJ")), div(h3(cls:="text-success", stats("ganados")), span(cls:="small text-muted", "G")), div(h3(cls:="text-danger", stats("gc")), span(cls:="small text-muted", "GC")))),
        h6(cls:="text-white border-bottom border-secondary pb-2 mb-3", "Partidos"),
        div(rows)
      )
    } else div(cls:="text-center text-muted mt-5", "Busca un rival...")

    val content = basePage("scouting", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12",
      h2(cls := "text-info mb-4 text-center", "SCOUTING"),
      form(action:="/scouting", method:="get", cls:="mb-4", div(cls:="input-group", input(tpe:="text", name:="query", cls:="form-control form-control-lg bg-dark text-white border-secondary", placeholder:="Nombre equipo", value:=query), button(tpe:="submit", cls:="btn btn-info", "Buscar"))),
      rivalCardWidget, resultsWidget
    )))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/scouting/save_rival") def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String) = { DatabaseManager.saveRivalInfo(fixEncoding(nombre), estilo, fixEncoding(claves), fixEncoding(notas)); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> s"/scouting?query=$nombre")) }

  // 4. GARAGE
  @cask.get("/gear")
  def gearPage() = {
    val items = DatabaseManager.getActiveGear()
    val gearList = if(items.isEmpty) div(cls:="alert alert-secondary text-center", "Sin material.")
    else {
      val rows = for(i <- items) yield {
        val pct = if(i.maxUsos>0) (i.usos.toDouble/i.maxUsos.toDouble*100).toInt else 0
        val color = if(pct > 90) "bg-danger" else if(pct > 75) "bg-warning" else "bg-success"
        val imgTag = if(i.img.length > 50) img(src:=i.img, style:="width:50px; height:50px; object-fit:cover; border-radius:50%; margin-right:10px;") else div(cls:="me-3", style:="font-size: 30px;", if(i.tipo=="Guantes") "🧤" else "👟")
        div(cls:="col-12 mb-3", div(cls:="card bg-dark border-secondary shadow", div(cls:="card-body d-flex align-items-center", imgTag, div(cls:="flex-grow-1", h5(cls:="text-white mb-0", i.nombre), div(cls:="small text-muted mb-1", i.tipo), div(cls:="progress", style:="height: 10px;", div(cls:=s"progress-bar $color", style:=s"width: $pct%"))), div(cls:="ms-3 text-end", div(cls:="fw-bold text-white", s"${i.usos}/${i.maxUsos}"), div(style:="font-size:10px", "USOS")))))
      }
      div(cls:="row", rows)
    }
    val content = basePage("gear", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", h2(cls := "text-warning mb-4 text-center", "MATERIAL"), gearList,
      div(cls:="card bg-secondary bg-opacity-10 border-secondary mt-4", div(cls:="card-body", h5(cls:="text-white mb-3", "Nuevo"), form(action:="/gear/add", method:="post", div(cls:="row", div(cls:="col-6 mb-2", input(tpe:="text", name:="nombre", cls:="form-control", placeholder:="Nombre", required:=true)), div(cls:="col-6 mb-2", select(name:="tipo", cls:="form-select", option(value:="Guantes", "Guantes"), option(value:="Botas", "Botas"))), div(cls:="col-12 mb-2", input(tpe:="number", name:="vida", cls:="form-control", value:="30", placeholder:="Vida util")), div(cls:="col-12 mb-2", label("Foto"), input(tpe:="file", cls:="form-control", onchange:="convertToBase64(this, 'gearImg')")), input(tpe:="hidden", name:="img", id:="gearImg"), div(cls:="col-12", button(tpe:="submit", cls:="btn btn-warning w-100", "Anadir"))))), script(raw("""function convertToBase64(i,t){if(i.files&&i.files[0]){var r=new FileReader();r.onload=function(e){document.getElementById(t).value=e.target.result;};r.readAsDataURL(i.files[0]);}}"""))))))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/gear/add") def addGear(nombre: String, tipo: String, vida: Int, img: String) = { DatabaseManager.addNewGear(nombre, tipo, vida, if(img!=null) img else ""); gearPage() }

  // 5. BIO PAGE
  // 5. BIO PAGE
  @cask.get("/bio")
  def bioPage() = {
    val activeDrills = DatabaseManager.getActiveDrills()
    val growthData = DatabaseManager.getGrowthHistory()

    val drillList = if (activeDrills.nonEmpty) {
      val dItems = for(d <- activeDrills) yield div(cls:="mb-2", div(cls:="d-flex justify-content-between small", span(fixEncoding(d.nombre)), span(s"${d.actual}/${d.objetivo}")), div(cls:="progress", style:="height: 6px;", div(cls:="progress-bar bg-warning", style:=s"width:${(d.actual.toDouble/d.objetivo.toDouble*100).toInt}%")))
      div(id:="drillsContainer", style:="display:none;", cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", h6(cls:="text-warning small fw-bold mb-2", "🎯 MISIONES ACTIVAS"), dItems)
    } else div(id:="drillsContainer", style:="display:none;", cls:="alert alert-dark p-2 small text-center", "Sin misiones activas.")

    val content = basePage("bio", div(cls := "row justify-content-center",
      div(cls := "col-md-6 mb-4", div(cls := "card bg-dark text-white border-info shadow",
        div(cls := "card-header bg-info text-dark fw-bold text-center", "WELLNESS & STATUS"),
        div(cls := "card-body p-3", form(action := "/bio/save_wellness", method := "post",
          div(cls:="mb-3", label(cls:="small text-danger fw-bold", "Estado Fisico"), select(name:="estadoFisico", cls:="form-select bg-dark text-white border-secondary", option(value:="DISPONIBLE", "✅ Disponible"), option(value:="MOLESTIAS", "⚠️ Molestias"), option(value:="LESION", "❌ Lesionado"), option(value:="ENFERMO", "🤒 Enfermo"))),
          div(cls:="row mb-3 align-items-end", div(cls:="col-6 text-center", label(cls:="small", "Calidad Sueño (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="sueno")), div(cls:="col-6", label(cls:="small text-warning", "Horas Dormidas"), input(tpe:="number", step:="0.5", name:="horas", cls:="form-control text-center bg-dark text-white border-warning", value:="9.0"))),
          div(cls:="mb-3 border-top pt-2", label(cls:="small", "Energia (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="energia")),
          div(cls:="mb-3", label(cls:="small text-info fw-bold", "Estado Animico (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="animo"), div(cls:="d-flex justify-content-between xx-small text-muted", span("Crisis"), span("Top"))),
          div(cls:="mb-2", label(cls:="small text-muted", "Notas conducta"), input(tpe:="text", name:="notas_conducta", cls:="form-control form-control-sm bg-dark text-white", placeholder:="... ")),
          div(cls:="mb-3 row", div(cls:="col-6", select(name:="dolor", cls:="form-select", option(value:="1","Nada"), option(value:="2","Molestia"), option(value:="3","Dolor"), option(value:="5","Lesion"))), div(cls:="col-6", input(tpe:="text", name:="zona", cls:="form-control", placeholder:="Zona?"))),
          div(cls:="row mb-3 border-top pt-3", div(cls:="col-6", label(cls:="small text-info", "Altura (cm)"), input(tpe:="number", name:="altura", cls:="form-control bg-dark text-white", placeholder:="Actualizar")), div(cls:="col-6", label(cls:="small text-info", "Peso (kg)"), input(tpe:="number", step:="0.1", name:="peso", cls:="form-control bg-dark text-white", placeholder:="Actualizar"))),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-info", "Guardar Bio"))
        ))
      )),
      div(cls := "col-md-6",
        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header text-secondary fw-bold text-center small", "CURVA DE CRECIMIENTO"), div(cls := "card-body p-2", canvas(id:="growthChart", style:="max-height:150px;"))),
        div(cls := "card bg-dark text-white border-success shadow mb-3",
          div(cls := "card-header bg-success text-dark fw-bold text-center", "REGISTRO ENTRENO"),
          div(cls := "card-body p-3", form(action := "/bio/save_training", method := "post",
            div(cls:="mb-3", label(cls:="small", "Tipo"),
              select(name:="tipo", id:="trainingType", onchange:="toggleDrills()", cls:="form-select bg-dark text-white",
                option(value:="Club", "Club"), option(value:="Academia", "Academia"), option(value:="Papa", "Papa (Portero)"), option(value:="Papa (Jugador)", "Papa (Jugador)")
              )
            ),
            drillList,
            // CAMBIO 1: El Foco ahora es visible SIEMPRE
            div(cls:="mb-3", label(cls:="small", "Foco / Actividad"),
              div(cls:="d-flex gap-2",
                input(tpe:="text", name:="foco", id:="drillFocus", cls:="form-control", placeholder:="Ej: Tiros, Resistencia...", required:=true),
                // El botón de IA se oculta/muestra con JS si no es Papa
                button(tpe:="button", id:="aiBtn", cls:="btn btn-warning", onclick:="generateAI()", style:="display:none;", "🤖 IA")
              )
            ),
            div(id:="manualDesign", style:="display:none;",
              textarea(name:="rutina", id:="rutinaText", cls:="form-control mb-3", rows:="4", placeholder:="Detalle de la sesión...")
            ),
            // CAMBIO 2: Fila de Stats completa (RPE, Calidad y ATENCION)
            div(cls:="row mb-3",
              div(cls:="col-4 text-center", label(cls:="small", "RPE"), input(tpe:="number", cls:="form-control text-center p-1", name:="rpe", value:="7", min:="1", max:="10")),
              div(cls:="col-4 text-center", label(cls:="small", "Calidad"), input(tpe:="number", cls:="form-control text-center p-1", name:="calidad", value:="8", min:="1", max:="10")),
              div(cls:="col-4 text-center", label(cls:="small", "Atención"), input(tpe:="number", cls:="form-control text-center p-1", name:="atencion", value:="8", min:="1", max:="10"))
            ),
            div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-success", "Guardar Sesión"))
          )
          )
        ),
        div(cls:="card bg-secondary bg-opacity-10 border-secondary", div(cls:="card-body p-2", h6(cls:="text-muted small mb-2", "+ Añadir Misión Técnica (10 Sesiones)"), form(action:="/bio/add_drill", method:="post", cls:="d-flex gap-2", input(tpe:="text", name:="nombre", cls:="form-control form-control-sm", placeholder:="Ej: Control Orientado", required:=true), button(tpe:="submit", cls:="btn btn-sm btn-secondary", "Crear"))))
      )
    ), script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""const gCtx=document.getElementById('growthChart');const gData=$growthData;if(gCtx){new Chart(gCtx,{type:'line',data:{labels:gData.labels,datasets:[{label:'Altura (cm)',data:gData.data,borderColor:'#0dcaf0',borderWidth:2,tension:0.3,pointBackgroundColor:'#fff'}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{y:{ticks:{color:'#aaa'},grid:{color:'#444'}},x:{display:false}}}});}
    function toggleDrills(){
        var type=document.getElementById('trainingType').value;
        var container=document.getElementById('drillsContainer');
        var manual=document.getElementById('manualDesign');
        var aiBtn = document.getElementById('aiBtn');

        // Logica para mostrar/ocultar segun tipo
        if(container){if(type.includes('Papa') && !type.includes('Jugador')) container.style.display='block'; else container.style.display='none';}
        if(manual){if(type.includes('Papa')) manual.style.display='block'; else manual.style.display='none';}
        if(aiBtn){if(type.includes('Papa')) aiBtn.style.display='block'; else aiBtn.style.display='none';}
    }
    function generateAI(){
        var focus = document.getElementById('drillFocus').value;
        var type = document.getElementById('trainingType').value;
        if(!focus) { alert('Pon un objetivo primero (ej: Velocidad)'); return; }
        document.getElementById('rutinaText').value = "Generando...";
        fetch('/bio/ai_gen?focus='+encodeURIComponent(focus)+'&mode='+encodeURIComponent(type)).then(r=>r.text()).then(t => document.getElementById('rutinaText').value = t);
    }
    window.addEventListener('DOMContentLoaded', toggleDrills);""")))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/bio/save_wellness") def saveWellness(sueno: Int, horas: String, energia: Int, dolor: Int, zona: String, altura: String, peso: String, animo: Int, notas_conducta: String, estadoFisico: String) = { val h = if(horas.nonEmpty) horas.toDouble else 0.0; val alt = if(altura.nonEmpty) altura.toInt else 0; val pes = if(peso.nonEmpty) peso.toDouble else 0.0; DatabaseManager.logWellness(sueno, h, energia, dolor, zona, alt, pes, animo, notas_conducta, estadoFisico); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio")) }
  @cask.postForm("/bio/save_training") def saveTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: String, rutina: String) = { val att = if(atencion != null && atencion.nonEmpty) atencion.toInt else 3; DatabaseManager.logTraining(tipo, foco, rpe, calidad, att, rutina); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio")) }
  @cask.get("/bio/ai_gen") def aiGenDrill(focus: String, mode: String) = { cask.Response(DatabaseManager.generateTrainingSession(mode, focus)) }
  @cask.postForm("/bio/add_drill") def addDrill(nombre: String) = { DatabaseManager.addNewDrill(fixEncoding(nombre), ""); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio")) }

  // 6. HISTORIAL
  @cask.get("/history") def historyPage() = {
    val matches = DatabaseManager.getMatchesList()
    val tableRows = if (matches.isEmpty) Seq(tr(td(colspan := 4, cls := "text-center p-4", "Sin partidos"))) else matches.map(m => renderMatchRow(m))
    val content = basePage("history", div(cls := "row justify-content-center", div(cls := "col-md-10 col-12", h2(cls := "text-warning mb-3 text-center", "Historial"), div(cls := "card shadow-sm border-0", div(cls := "card-body p-0", table(cls := "table table-hover tm-table mb-0", thead(tr(th("Rival"), th(cls:="text-center", "Res"), th(cls:="text-center", "Nota"), th(cls:="text-end", "Accion"))), tbody(tableRows)))))))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.get("/match/delete/:id") def deleteMatchAction(id: Int) = { DatabaseManager.deleteMatch(id); cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history")) }
  @cask.get("/match/edit/:id") def editMatchPage(id: Int) = {
    val m = DatabaseManager.getMatchById(id)
    if (m.isEmpty) { cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history")) } else {
      val matchData = m.get; val (gf, gc) = if(matchData.resultado.contains("-")) (matchData.resultado.split("-")(0), matchData.resultado.split("-")(1)) else ("0", "0")
      val tags = DatabaseManager.getVideoTags(id)
      val tagList = if(matchData.video.nonEmpty) {
        val tItems = for(t <- tags) yield {
          val link = if(matchData.video.contains("?")) s"${matchData.video}&t=${t.minuto*60 + t.segundo}" else s"${matchData.video}?t=${t.minuto*60 + t.segundo}"
          a(href:=link, target:="_blank", cls:="list-group-item list-group-item-action bg-dark text-white border-secondary d-flex justify-content-between align-items-center",
            div(span(cls:="badge bg-danger me-2", s"${t.minuto}:${t.segundo}"), span(t.tipo)),
            a(href:=s"/video/delete_tag/${t.id}/${id}", cls:="text-danger fw-bold text-decoration-none", "X")
          )
        }
        div(form(action:="/video/add_tag", method:="post", cls:="row g-2 mb-3",
          input(tpe:="hidden", name:="matchId", value:=id.toString),
          div(cls:="col-3", input(tpe:="number", name:="min", cls:="form-control form-control-sm", placeholder:="Min", required:=true)),
          div(cls:="col-3", input(tpe:="number", name:="sec", cls:="form-control form-control-sm", placeholder:="Sec", required:=true)),
          div(cls:="col-6", div(cls:="input-group input-group-sm", select(name:="tipo", cls:="form-select", option("PARADA"), option("ERROR"), option("GOL"), option("PASE")), button(tpe:="submit", cls:="btn btn-warning", "+")))
        ), div(cls:="list-group", tItems))
      } else div(cls:="alert alert-secondary small", "Añade URL de video para usar tags.")

      // FIX: .getBytes("UTF-8") to return RESPONSE[BYTES] for strict typing
      val content = basePage("history", div(cls := "row justify-content-center", div(cls := "col-md-6 col-12", div(cls := "card bg-dark text-white border-primary shadow",
        div(cls := "card-header bg-primary text-white fw-bold text-center", "EDITAR PARTIDO & VIDEO"),
        div(cls := "card-body p-3", form(action := "/match/update", method := "post", attr("accept-charset") := "UTF-8", input(tpe:="hidden", name:="id", value:=id.toString),
          div(cls:="mb-3", label("Rival"), input(tpe:="text", name:="rival", value:=matchData.rival, cls:="form-control")),
          div(cls:="mb-3", label("Fecha"), input(tpe:="date", name:="fecha", value:=matchData.fecha, cls:="form-control")),
          div(cls:="row mb-3", div(cls:="col-6", label("GF"), input(tpe:="number", name:="gf", value:=gf, cls:="form-control")), div(cls:="col-6", label("GC"), input(tpe:="number", name:="gc", value:=gc, cls:="form-control"))),
          div(cls:="mb-3", label("Nota"), input(tpe:="number", step:="0.1", name:="nota", value:=matchData.nota.toString, cls:="form-control")),
          div(cls:="mb-3", label("Notas Texto"), textarea(name:="notas", cls:="form-control", rows:="3", matchData.notas)),
          div(cls:="mb-3", label("Reaccion/Goles"), textarea(name:="reaccion", cls:="form-control", rows:="3", matchData.reaccion)),
          div(cls:="mb-3", label("Video URL (Youtube)"), input(tpe:="text", name:="video", value:=matchData.video, cls:="form-control")),
          div(cls:="d-grid gap-2 mb-4", button(tpe:="submit", cls:="btn btn-success", "Guardar Cambios"), a(href:="/history", cls:="btn btn-outline-secondary", "Cancelar"))
        )),
        div(cls:="card-footer bg-secondary bg-opacity-25", h6(cls:="text-white small fw-bold", "CORTES DE VIDEO (TAGS)"), tagList)
      ))))

        cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
    }
  }

  @cask.postForm("/video/add_tag") def addVideoTag(matchId: Int, min: Int, sec: Int, tipo: String) = { DatabaseManager.addVideoTag(matchId, min, sec, tipo, ""); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> s"/match/edit/$matchId")) }
  @cask.get("/video/delete_tag/:id/:matchId") def deleteVideoTag(id: Int, matchId: Int) = { DatabaseManager.deleteVideoTag(id); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> s"/match/edit/$matchId")) }
  @cask.postForm("/match/update") def updateMatchAction(id: Int, rival: String, gf: Int, gc: Int, nota: Double, notas: String, video: String, reaccion: String, fecha: String) = { val cleanRival = fixEncoding(rival); val cleanNotas = fixEncoding(notas); val cleanReaccion = fixEncoding(reaccion); DatabaseManager.updateMatch(id, cleanRival, gf, gc, 60, nota, "Sol", 20, cleanNotas, video, cleanReaccion, fecha); cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history")) }
  @cask.get("/settings") def settingsPage() = { val content = basePage("settings", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", div(cls := "card bg-dark text-white border-secondary shadow p-4 mb-3", h2(cls := "text-warning mb-4", "Fotos y Club"), form(action := "/settings/save_base64", method := "post", div(cls := "mb-4", label(cls := "form-label text-info", "Nombre del Club"), input(tpe := "text", name := "nombreClub", cls := "form-control", placeholder := "Ej: Rayo Vallecano")), div(cls := "mb-4", label(cls := "form-label text-info", "Foto Jugador"), input(tpe := "file", cls := "form-control", accept := "image/*", onchange := "convertToBase64(this, 'hidden_foto')"), input(tpe := "hidden", name := "fotoBase64", id := "hidden_foto")), div(cls := "mb-4", label(cls := "form-label text-warning", "Escudo Club"), input(tpe := "file", cls := "form-control", accept := "image/*", onchange := "convertToBase64(this, 'hidden_club')"), input(tpe := "hidden", name := "clubBase64", id := "hidden_club")), div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg", "Guardar"))), script(raw("""function convertToBase64(i,t){if(i.files&&i.files[0]){var r=new FileReader();r.onload=function(e){document.getElementById(t).value=e.target.result;};r.readAsDataURL(i.files[0]);}}"""))), div(cls:="d-grid", a(href:="/admin", cls:="btn btn-outline-danger", "ZONA ADMIN"))))); cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.postForm("/settings/save_base64") def saveSettingsBase64(fotoBase64: String, clubBase64: String, nombreClub: String) = { val res = DatabaseManager.updateSeasonSettings(if(fotoBase64!=null) fotoBase64 else "", if(clubBase64!=null) clubBase64 else "", if(nombreClub!=null) nombreClub else ""); val htmlStr = doctype("html")(html(head(meta(charset := "utf-8"), tags2.title("Exito"), tags2.style(raw(getCss()))), body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';", h1("OK"), h2(res), div(style := "margin-top: 20px;", a(href := "/", cls := "btn btn-warning", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.get("/career") def careerPage() = { val c = DatabaseManager.getCareerSummary(); val listRows = for (s <- c) yield tr(td(cls:="fw-bold text-primary small", s.categoria), td(img(src := s.fotoUrl, style := "height: 35px; width: 35px; border-radius: 50%; object-fit: cover; border: 2px solid #ddd;")), td(cls:="text-center fw-bold small", s.partidosJugados), td(cls:="text-center text-danger small", s.golesContra), td(cls:="text-center", span(cls:="badge bg-dark text-warning", s.mediaFinal))); val content=basePage("career", div(cls := "row justify-content-center", div(cls := "col-md-10 col-12", div(cls := "d-flex flex-column justify-content-center align-items-center mb-4 text-center", h2(cls := "text-warning m-0 mb-2", "Trayectoria"), div(cls:="card bg-secondary p-2 w-100", form(action := "/career/new-season", method := "post", cls:="d-flex flex-column gap-2", div(label(cls:="form-label text-white small m-0", "Nueva Categoria:"), input(tpe := "text", name := "categoria", cls := "form-control form-control-sm", placeholder := "Ej: Benjamin A", required := true)), button(tpe := "submit", cls := "btn btn-danger btn-sm", onclick := "return confirm('Seguro?');", "Cerrar & Empezar")))), div(cls := "card shadow-sm border-0", div(cls := "card-body p-0 table-responsive", table(cls := "table table-hover tm-table mb-0", thead(tr(th("Cat"), th("Ficha"), th("PJ"), th("GC"), th("Media"))), tbody(listRows))))))); cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.postForm("/career/new-season") def newSeasonAction(categoria: String) = { val msg = DatabaseManager.startNewSeason(categoria); val htmlStr = doctype("html")(html(head(meta(charset := "utf-8"), tags2.title("Nueva Temp"), tags2.style(raw(getCss()))), body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';", h1("OK"), h2(msg), p(s"Etapa iniciada: $categoria"), div(style := "margin-top: 20px;", a(href := "/", cls := "btn btn-warning", "Ir a Inicio"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.get("/admin") def adminPage() = { val objs = DatabaseManager.getSeasonObjectives(); val content = basePage("settings", div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", h2(cls:="text-danger text-center mb-4", "ADMINISTRACION"), div(cls:="card bg-secondary bg-opacity-25 border-secondary mb-4 p-3", h5(cls:="text-white", "Copia de Seguridad"), p(cls:="small text-muted", "Descarga todos los partidos en formato Excel/CSV."), a(href:="/admin/download_csv", cls:="btn btn-primary w-100", "Descargar CSV")), div(cls:="card bg-secondary bg-opacity-25 border-secondary mb-4 p-3", h5(cls:="text-white", "Informe PDF"), p(cls:="small text-muted", "Genera un informe limpio para imprimir o guardar como PDF."), a(href:="/admin/print_report", target:="_blank", cls:="btn btn-info w-100", "Generar Informe")),
    div(cls:="card bg-dark border-info shadow p-3", h5(cls:="text-info", "Gestionar Objetivos"), if(objs.isEmpty) div("Sin objetivos.") else div((for(o <- objs) yield form(action:="/admin/update_obj", method:="post", cls:="row align-items-center mb-2", div(cls:="col-7 small text-white", o.descripcion), div(cls:="col-3", input(tpe:="number", name:="meta", value:=o.meta.toString, cls:="form-control form-control-sm text-center")), input(tpe:="hidden", name:="id", value:=o.id.toString), div(cls:="col-2", button(tpe:="submit", cls:="btn btn-sm btn-outline-success", "S")))).toSeq)), div(cls:="d-grid mt-4", a(href:="/admin/importer", cls:="btn btn-warning fw-bold", "IMPORTAR DATOS MASIVOS (CSV)"))))); cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.postForm("/admin/update_obj") def updateObj(id: Int, meta: Int) = { DatabaseManager.updateObjective(id, meta); cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/admin")) }
  @cask.get("/admin/download_csv") def downloadCsv() = { cask.Response(DatabaseManager.getBackupCSV().getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/csv; charset=utf-8", "Content-Disposition" -> "attachment; filename=guardian_backup.csv")) }
  @cask.get("/admin/print_report") def printReport() = { val card = DatabaseManager.getLatestCardData(); val matches = DatabaseManager.getMatchesList(); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.title("Informe Temporada"), tags2.style("""body{font-family:sans-serif;color:black;background:white;padding:20px;}h1,h2{text-align:center;color:#333;}table{width:100%;border-collapse:collapse;margin-top:20px;font-size:12px;}th,td{border:1px solid #ddd;padding:8px;text-align:left;}th{background-color:#f2f2f2;}.header{text-align:center;margin-bottom:30px;border-bottom:2px solid #333;padding-bottom:20px;}@media print{.no-print{display:none;}}""")), body(div(cls:="no-print", style:="text-align:center;margin-bottom:20px;", button(onclick:="window.print()", style:="padding:10px 20px;font-size:16px;cursor:pointer;", "IMPRIMIR / GUARDAR PDF")), div(cls:="header", h1(s"INFORME DE TEMPORADA - ${card.nombre}"), div(s"Media: ${card.media} | Posicion: ${card.posicion}"), div(s"Estirada: ${card.div} | Manos: ${card.han} | Saque: ${card.kic}"), div(s"Reflejos: ${card.ref} | Velocidad: ${card.spd} | Posicion: ${card.pos}")), h2("Historial de Partidos"), table(thead(tr(th("Fecha"), th("Rival"), th("Resultado"), th("Min"), th("Nota"), th("Clima"))), tbody((for(m <- matches) yield tr(td(m.fecha), td(m.rival), td(m.resultado), td(m.minutos), td(m.nota), td(m.clima))).toSeq))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8")) }
  @cask.get("/admin/importer") def importerPage() = { val content = basePage("settings", div(cls:="row justify-content-center", div(cls:="col-md-8", h2(cls:="text-info text-center mb-4", "IMPORTADOR DE DATOS"), div(cls:="card bg-dark text-white border-primary shadow p-4 mb-4", h4("📅 Importar Calendario"), p(cls:="small text-muted", "Formato: FECHA, RIVAL, TIPO"), form(action:="/admin/upload_calendar", method:="post", textarea(name:="csvContent", cls:="form-control mb-3", rows:="3"), button(tpe:="submit", cls:="btn btn-primary w-100", "Cargar"))), div(cls:="card bg-dark text-white border-warning shadow p-4 mb-4", h4("Importar Historial"), form(action:="/admin/upload_matches", method:="post", textarea(name:="csvContent", cls:="form-control mb-3", rows:="3"), button(tpe:="submit", cls:="btn btn-warning w-100", "Procesar"))), div(cls:="card bg-dark text-white border-info shadow p-4", h4("Importar Wellness"), form(action:="/admin/upload_wellness", method:="post", textarea(name:="csvContent", cls:="form-control mb-3", rows:="3"), button(tpe:="submit", cls:="btn btn-info w-100", "Procesar"))), div(cls:="mt-3 text-center", a(href:="/admin", cls:="btn btn-outline-light", "Volver"))))); cask.Response(content.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/upload_calendar") def uploadCalendar(csvContent: String) = { val res = DatabaseManager.importCalendarCSV(fixEncoding(csvContent)); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("CALENDARIO"), h3(res), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-primary", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/upload_matches") def uploadMatches(csvContent: String) = { val cleanCsv = fixEncoding(csvContent); val res = DatabaseManager.importMatchesCSV(cleanCsv); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("IMPORTACION"), h3(res), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-warning", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }
  @cask.postForm("/admin/upload_wellness") def uploadWellness(csvContent: String) = { val cleanCsv = fixEncoding(csvContent); val res = DatabaseManager.importWellnessCSV(cleanCsv); val htmlStr = doctype("html")(html(head(meta(charset:="utf-8"), tags2.style(raw(getCss()))), body(style:="background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';", h1("IMPORTACION"), h3(res), div(style:="margin-top:20px;", a(href:="/admin/importer", cls:="btn btn-info", "Volver"))))).render; cask.Response(htmlStr.getBytes("UTF-8"), headers=Seq("Content-Type"->"text/html; charset=utf-8")) }

  // 7. PENALTY LAB
  @cask.get("/penalties")
  def penaltiesPage() = {
    val stats = DatabaseManager.getPenaltyStats()
    val content = basePage("match-center", div(cls := "row justify-content-center",
      div(cls := "col-md-8 col-12",
        h2(cls := "text-center text-danger mb-4", "LABORATORIO PENALTIS"),
        div(cls:="card bg-dark border-danger shadow mb-4",
          div(cls:="card-header bg-danger text-white fw-bold", "REGISTRAR LANZAMIENTO"),
          div(cls:="card-body",
            form(action:="/penalties/save", method:="post",
              div(cls:="mb-3", input(tpe:="text", name:="rival", cls:="form-control", placeholder:="Nombre Rival (Opcional)")),
              div(cls:="row mb-3",
                div(cls:="col-6", label(cls:="small text-white", "Zona Tiro"), select(name:="zTiro", cls:="form-select",
                  option(value:="TL", "Arriba Izq"), option(value:="TM", "Arriba Cen"), option(value:="TR", "Arriba Der"),
                  option(value:="ML", "Media Izq"), option(value:="MM", "Media Cen"), option(value:="MR", "Media Der"),
                  option(value:="BL", "Baja Izq"), option(value:="BM", "Baja Cen"), option(value:="BR", "Baja Der")
                )),
                div(cls:="col-6", label(cls:="small text-warning", "Salto Hector"), select(name:="zSalto", cls:="form-select",
                  option(value:="L", "Izquierda"), option(value:="C", "Centro"), option(value:="R", "Derecha")
                ))
              ),
              div(cls:="form-check mb-3", input(cls:="form-check-input", tpe:="checkbox", name:="esGol", id:="golCheck"), label(cls:="form-check-label text-white", attr("for"):="golCheck", "Fue Gol")),
              button(tpe:="submit", cls:="btn btn-danger w-100", "Registrar Penalti")
            )
          )
        ),
        div(cls:="card bg-dark border-secondary shadow",
          div(cls:="card-header text-center text-white", "MAPA DE CALOR (TIROS RIVALES)"),
          div(cls:="card-body d-flex justify-content-center",
            div(cls:="goal-grid-3x3", style:="width: 250px; height: 180px;",
              (for(z <- Seq("TL","TM","TR","ML","MM","MR","BL","BM","BR")) yield {
                val s = stats.find(_.zona == z).getOrElse(PenaltyStat(z,0,0))
                val color = if(s.total > 0) "rgba(220, 53, 69, 0.6)" else "rgba(255,255,255,0.1)"
                div(cls:="goal-cell d-flex justify-content-center align-items-center flex-column", style:=s"background-color:$color; border:1px solid #444;",
                  span(cls:="fw-bold text-white", s.total.toString),
                  span(cls:="xx-small text-light", if(s.total>0) s"${(s.goles.toDouble/s.total*100).toInt}% G" else "")
                )
              }).toSeq
            )
          )
        )
      )
    ))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/penalties/save") def savePenalty(rival: String, zTiro: String, zSalto: String, esGol: Boolean) = { DatabaseManager.logPenalty(rival, zTiro, zSalto, esGol); cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/penalties")) }

  // 8. PIZARRA TÁCTICA
  @cask.get("/tactics")
  def tacticsPage() = {
    val content = basePage("tactics", div(cls := "row justify-content-center",
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
        ctx.strokeStyle = 'rgba(255,255,255,0.6)';
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.rect(10, 10, canvas.width-20, canvas.height-20);
        const midX = canvas.width / 2;
        ctx.rect(midX - 60, 10, 120, 60);
        ctx.rect(midX - 100, 10, 200, 120);
        ctx.rect(midX - 60, canvas.height-70, 120, 60);
        ctx.rect(midX - 100, canvas.height-130, 200, 120);
        ctx.moveTo(midX + 40, canvas.height/2);
        ctx.arc(midX, canvas.height/2, 40, 0, Math.PI * 2);
        ctx.moveTo(10, canvas.height/2);
        ctx.lineTo(canvas.width-10, canvas.height/2);
        ctx.stroke();
      }

      function startPosition(e) { painting = true; draw(e); }
      function finishedPosition() { painting = false; ctx.beginPath(); }
      function draw(e) {
        if (!painting) return;
        e.preventDefault();
        const rect = canvas.getBoundingClientRect();
        const x = (e.clientX || e.touches[0].clientX) - rect.left;
        const y = (e.clientY || e.touches[0].clientY) - rect.top;

        ctx.lineWidth = 3;
        ctx.lineCap = 'round';
        ctx.strokeStyle = color;

        ctx.lineTo(x, y);
        ctx.stroke();
        ctx.beginPath();
        ctx.moveTo(x, y);
      }

      canvas.addEventListener('mousedown', startPosition);
      canvas.addEventListener('touchstart', startPosition);
      canvas.addEventListener('mouseup', finishedPosition);
      canvas.addEventListener('touchend', finishedPosition);
      canvas.addEventListener('mousemove', draw);
      canvas.addEventListener('touchmove', draw);

      function setColor(c) { color = c; }
      function clearBoard() { drawField(); }
      setTimeout(resize, 100);
    """)))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  // --- BASE PAGE (MENU) ---
  def basePage(activeLink: String, pageContents: Modifier*) = {
    // ESTO DEVUELVE STRING DIRECTAMENTE
    "<!DOCTYPE html>" +
      html(head(meta(charset := "utf-8"), meta(name := "viewport", content := "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0"), link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"), link(rel := "stylesheet", href := "https://fonts.googleapis.com/css2?family=Oswald:wght@400;700&display=swap"), tags2.title("Guardian Elite"), tags2.style(raw(getCss()))),
        body(div(cls := "app-header d-flex justify-content-between align-items-center", div(span(cls := "text-warning", "G"), " GUARDIAN ELITE"), a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "S")), div(cls := "container main-content", pageContents), tags2.nav(cls := "bottom-nav", a(href:="/", cls:=s"nav-item ${if(activeLink=="home") "active" else ""}", div(cls:="nav-icon", "H"), span(cls:="nav-label", "Inicio")), a(href:="/match-center", cls:=s"nav-item ${if(activeLink=="match-center") "active" else ""}", div(cls:="nav-icon", "P"), span(cls:="nav-label", "Jugar")), a(href:="/bio", cls:=s"nav-item ${if(activeLink=="bio") "active" else ""}", div(cls:="nav-icon", "B"), span(cls:="nav-label", "Bio")), a(href:="/gear", cls:=s"nav-item ${if(activeLink=="gear") "active" else ""}", div(cls:="nav-icon", "M"), span(cls:="nav-label", "Material")), a(href:="/tactics", cls:=s"nav-item ${if(activeLink=="tactics") "active" else ""}", div(cls:="nav-icon", "📋"), span(cls:="nav-label", "Pizarra")), a(href:="/career", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon", "T"), span(cls:="nav-label", "Trayect.")), a(href:="/history", cls:=s"nav-item ${if(activeLink=="history") "active" else ""}", div(cls:="nav-icon", "L"), span(cls:="nav-label", "Historial"))))
      ).render
  }

  def getCss() = """
    body { background-color: #121212; color: white; font-family: 'Oswald', sans-serif; padding-bottom: 80px; margin: 0; }
    .app-header { background: #1a1a1a; color: white; text-align: center; padding: 15px; font-size: 20px; font-weight: bold; border-bottom: 1px solid #333; position: sticky; top: 0; z-index: 1000; letter-spacing: 2px; }
    .main-content { padding-top: 20px; }
    .bottom-nav { position: fixed; bottom: 0; width: 100%; background: #1a1a1a; border-top: 1px solid #333; display: flex; justify-content: space-around; padding: 8px 0; z-index: 1000; box-shadow: 0 -2px 10px rgba(0,0,0,0.5); overflow-x: auto; }
    .nav-item { text-align: center; color: #888; text-decoration: none; flex: 1; transition: color 0.2s; min-width: 55px; } .nav-item.active { color: #d4af37; }
    .nav-icon { font-size: 20px; margin-bottom: 2px; } .nav-label { font-size: 9px; display: block; text-transform: uppercase; letter-spacing: 0.5px; }
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
  """

  initialize()
}