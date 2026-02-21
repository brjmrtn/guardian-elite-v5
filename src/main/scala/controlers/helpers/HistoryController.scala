import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object HistoryController extends cask.Routes {

  @cask.get("/scouting")
  def scoutingPage(request: cask.Request, query: String = "") = withAuth(request) {

    val (matches, stats) = if (query.nonEmpty) DatabaseManager.getRivalScouting(query)
    else (List[MatchLog](), Map[String, Int]())
    val rivalInfo = if (query.nonEmpty) DatabaseManager.getRivalInfo(query) else None

    // --- Widget ficha del rival (editable) ---
    val rivalCardWidget = if (query.nonEmpty) {
      val estiloVal = rivalInfo.map(_.estilo).getOrElse("Desconocido")
      val clavesVal = rivalInfo.map(_.claves).getOrElse("")
      val notasVal  = rivalInfo.map(_.notas).getOrElse("")
      div(cls := "card bg-dark border-secondary shadow mb-4",
        div(cls := "card-header bg-secondary text-white fw-bold", "FICHA RIVAL"),
        div(cls := "card-body",
          form(action := "/scouting/save_rival", method := "post",
            input(tpe := "hidden", name := "nombre", value := query),
            div(cls := "mb-2",
              label(cls := "small text-muted fw-bold", "Estilo"),
              select(name := "estilo", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                option(value := "Desconocido", "?"),
                option(value := "Directo",     "Balon Largo"),
                option(value := "Combinativo", "Toque"),
                option(value := "Contra",      "Contraataque"),
                attr("value") := estiloVal
              )
            ),
            div(cls := "mb-2",
              label(cls := "small text-muted fw-bold", "Claves"),
              textarea(name := "claves", cls := "form-control form-control-sm bg-dark text-white fw-bold",
                rows := "2", fixEncoding(clavesVal))
            ),
            div(cls := "mb-2",
              label(cls := "small text-muted fw-bold", "Notas"),
              textarea(name := "notas", cls := "form-control form-control-sm bg-dark text-white fw-bold",
                rows := "2", fixEncoding(notasVal))
            ),
            button(tpe := "submit", cls := "btn btn-sm btn-outline-warning w-100 fw-bold", "Guardar Ficha")
          )
        )
      )
    } else div()

    // --- Widget resultados vs el rival ---
    val resultsWidget = if (query.nonEmpty && matches.isEmpty) {
      div(cls := "alert alert-warning text-center", s"Sin datos vs '$query'")
    } else if (matches.nonEmpty) {
      val rows = for (m <- matches) yield {
        val extra = if (m.video.nonEmpty)
          a(href := m.video, target := "_blank", cls := "btn btn-sm btn-outline-danger w-100", "Video")
        else span("")
        div(cls := "card bg-dark border-secondary shadow mb-3",
          div(cls := "card-body",
            div(cls := "d-flex justify-content-between align-items-center mb-2",
              div(strong(cls := "text-warning", m.fecha), span(cls := "ms-2 badge bg-secondary", m.clima)),
              div(cls := "fs-5 fw-bold text-white", m.resultado)
            ),
            if (m.estadio.nonEmpty) div(cls := "small text-muted mb-2 fw-bold", s"📍 ${m.estadio}"),
            if (m.notas.nonEmpty)   div(cls := "alert alert-dark border-secondary p-2 small text-light fst-italic mb-2 fw-bold",
              s"Nota: ${fixEncoding(m.notas)}"),
            extra
          )
        )
      }
      div(
        div(cls := "card bg-secondary bg-opacity-25 border-info mb-4 p-3",
          h5(cls := "text-center text-white mb-3", s"Vs ${matches.head.rival}"),
          div(cls := "d-flex justify-content-around text-center text-white",
            div(h3(stats("pj")),              span(cls := "small text-muted", "PJ")),
            div(h3(cls := "text-success", stats("ganados")), span(cls := "small text-muted", "G")),
            div(h3(cls := "text-danger",  stats("gc")),      span(cls := "small text-muted", "GC"))
          )
        ),
        h6(cls := "text-white border-bottom border-secondary pb-2 mb-3", "Partidos"),
        div(rows)
      )
    } else div(cls := "text-center text-muted mt-5", "Busca un rival...")

    val content = basePage("scouting",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-info mb-4 text-center", "SCOUTING"),
          form(action := "/scouting", method := "get", cls := "mb-4",
            div(cls := "input-group",
              input(tpe := "text", name := "query",
                cls := "form-control form-control-lg bg-dark text-white border-secondary fw-bold",
                placeholder := "Nombre equipo", value := query),
              button(tpe := "submit", cls := "btn btn-info fw-bold", "Buscar")
            )
          ),
          rivalCardWidget,
          resultsWidget
        )
      )
    )
    renderHtml(content)
  }
  @cask.postForm("/scouting/save_rival")
  def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String) = {
    DatabaseManager.saveRivalInfo(fixEncoding(nombre), estilo, fixEncoding(claves), fixEncoding(notas))
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> s"/scouting?query=$nombre"))
  }

  @cask.get("/history")
  def historyPage(request: cask.Request) = withAuth(request) {
    val matches = DatabaseManager.getMatchesList()

    // 1. Generamos las filas de la tabla
    val tableRows = if (matches.isEmpty) {
      Seq(tr(td(colspan := 4, cls := "text-center p-4", "Sin partidos")))
    } else {
      matches.map(m => renderMatchRow(m))
    }

    // 2. Definimos el contenido central (SIN llamar a basePage aqui)
    val mainContent = div(cls := "row justify-content-center",
      div(cls := "col-md-10 col-12",
        div(cls := "d-flex justify-content-between align-items-center mb-3",
          h2(cls := "text-warning mb-0", "HISTORIAL"),
          a(href := "/mapa-goles", cls := "btn btn-outline-danger btn-sm fw-bold", "MAPA DE GOLES")
        ),
        div(cls := "card shadow-sm border-0",
          div(cls := "card-body p-0",
            table(cls := "table table-hover tm-table mb-0",
              thead(tr(
                th("Rival"),
                th(cls:="text-center", "Res"),
                th(cls:="text-center", "Nota"),
                th(cls:="text-end", "Accion")
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

  @cask.get("/mapa-goles")
  def mapaGolesPage(request: cask.Request, temporada: String = "", rival: String = "") = withAuth(request) {

    val heatmap = if (rival.nonEmpty)
      DatabaseManager.getGoalHeatmapByRival(rival)
    else
      DatabaseManager.getGoalHeatmap(temporada)

    val matches    = DatabaseManager.getMatchesList()
    val totalGoles = heatmap.values.sum

    // Temporadas disponibles (anos distintos en el historial)
    val temporadas = matches.map(_.fecha.take(4)).distinct.sorted.reverse

    // Rivales con goles encajados
    val rivalesConGoles = matches
      .filter(m => m.zGoles.nonEmpty && m.resultado.split("-").lastOption.exists(g => g.trim.toInt > 0))
      .map(_.rival).distinct.sorted

    // Intensidad de cada celda: 0.0 - 1.0
    val maxVal = if (heatmap.values.max > 0) heatmap.values.max.toDouble else 1.0

    def cellColor(count: Int): String = {
      val intensity = count / maxVal
      if (intensity == 0) "rgba(255,255,255,0.04)"
      else if (intensity < 0.25) "rgba(220,53,69,0.20)"
      else if (intensity < 0.50) "rgba(220,53,69,0.45)"
      else if (intensity < 0.75) "rgba(220,53,69,0.70)"
      else "rgba(220,53,69,0.92)"
    }

    def cellLabel(zone: String): String = zone match {
      case "TL" => "Arr Izq"; case "TC" => "Arr Cen"; case "TR" => "Arr Der"
      case "ML" => "Med Izq"; case "MC" => "Med Cen"; case "MR" => "Med Der"
      case "BL" => "Baj Izq"; case "BC" => "Baj Cen"; case "BR" => "Baj Der"
      case _ => zone
    }

    // Grid 3x3 de porteria (vista desde atras del portero)
    val zonaRows = Seq(
      Seq("TL","TC","TR"),
      Seq("ML","MC","MR"),
      Seq("BL","BC","BR")
    )

    def renderCell(zone: String) = {
      val count = heatmap.getOrElse(zone, 0)
      val pct   = if (totalGoles > 0) (count * 100.0 / totalGoles).toInt else 0
      val bg    = cellColor(count)
      div(
        cls   := "goal-heatmap-cell d-flex flex-column justify-content-center align-items-center",
        style := s"background:$bg; border:1px solid rgba(255,255,255,0.08); cursor:default;",
        attr("title") := s"${cellLabel(zone)}: $count goles ($pct%)",
        if (count > 0) Seq(
          div(cls := "fw-bold text-white", style := "font-size:20px;", count.toString),
          div(cls := "xx-small text-light opacity-75", s"$pct%")
        ) else Seq(
          div(cls := "text-muted", style := "font-size:18px; opacity:0.3;", "—")
        )
      )
    }

    // Stats resumen por zona
    val golsAlto  = Seq("TL","TC","TR").map(heatmap.getOrElse(_, 0)).sum
    val golsMedio = Seq("ML","MC","MR").map(heatmap.getOrElse(_, 0)).sum
    val golsBajo  = Seq("BL","BC","BR").map(heatmap.getOrElse(_, 0)).sum
    val golsIzq   = Seq("TL","ML","BL").map(heatmap.getOrElse(_, 0)).sum
    val golsCen   = Seq("TC","MC","BC").map(heatmap.getOrElse(_, 0)).sum
    val golsDer   = Seq("TR","MR","BR").map(heatmap.getOrElse(_, 0)).sum

    def pct(n: Int) = if (totalGoles > 0) s"${(n*100.0/totalGoles).toInt}%" else "0%"

    val tituloFiltro = if (rival.nonEmpty) s"vs ${rival.toUpperCase}"
    else if (temporada.nonEmpty) s"Temporada $temporada"
    else "Todas las temporadas"

    val content = basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-10 col-lg-9 col-12",

          // Header
          h2(cls := "text-center text-danger mb-1", "MAPA DE GOLES ENCAJADOS"),
          p(cls  := "text-center text-muted small mb-4", s"$tituloFiltro — $totalGoles goles en total"),

          // Filtros
          div(cls := "card bg-dark border-secondary shadow mb-4",
            div(cls := "card-body",
              div(cls := "row g-2 align-items-end",
                // Filtro temporada
                div(cls := "col-md-4",
                  label(cls := "small text-muted fw-bold", "Temporada"),
                  div(cls := "d-flex gap-1 flex-wrap mt-1",
                    a(href := "/mapa-goles",
                      cls := s"btn btn-sm fw-bold ${if(temporada.isEmpty && rival.isEmpty) "btn-danger" else "btn-outline-secondary"}",
                      "TODAS"),
                    temporadas.map { t =>
                      a(href := s"/mapa-goles?temporada=$t",
                        cls := s"btn btn-sm fw-bold ${if(temporada == t) "btn-danger" else "btn-outline-secondary"}",
                        t)
                    }
                  )
                ),
                // Filtro rival
                div(cls := "col-md-5",
                  label(cls := "small text-muted fw-bold", "Filtrar por rival"),
                  form(action := "/mapa-goles", method := "get", cls := "d-flex gap-1 mt-1",
                    input(tpe := "text", name := "rival", cls := "form-control form-control-sm bg-dark text-white fw-bold",
                      placeholder := "Nombre rival...", value := rival),
                    button(tpe := "submit", cls := "btn btn-sm btn-outline-danger fw-bold", "Ir")
                  )
                ),
                // Volver al historial
                div(cls := "col-md-3 text-end",
                  a(href := "/history", cls := "btn btn-sm btn-outline-secondary fw-bold", "← Historial")
                )
              )
            )
          ),

          if (totalGoles == 0) {
            div(cls := "alert alert-secondary text-center py-5",
              div(style := "font-size:48px; opacity:0.3;", "🥅"),
              div(cls := "fw-bold mt-2", "Sin goles encajados registrados"),
              div(cls := "small text-muted mt-1", "Los goles se registran en Match Center usando el Modo Gol")
            )
          } else {
            div(cls := "row g-4",

              // --- PORTERIA HEATMAP ---
              div(cls := "col-md-7",
                div(cls := "card bg-dark border-danger shadow h-100",
                  div(cls := "card-header bg-danger bg-opacity-10 border-danger text-center",
                    span(cls := "text-danger fw-bold", "PORTERIA — Vista frontal"),
                    span(cls := "text-muted small ms-2", "(zona mas caliente = mas goles)")
                  ),
                  div(cls := "card-body d-flex flex-column justify-content-center",
                    // Poste superior
                    div(cls := "d-flex justify-content-center mb-1",
                      div(style := "width:100%; max-width:360px; height:6px; background:linear-gradient(90deg,#888,#ccc,#888); border-radius:3px;")
                    ),
                    // Grid porteria
                    div(cls := "d-flex justify-content-center",
                      div(style := "width:100%; max-width:360px;",
                        // Poste izq + grid + poste der
                        div(cls := "d-flex align-items-stretch",
                          // Poste izquierdo
                          div(style := "width:6px; background:linear-gradient(180deg,#888,#ccc,#888); border-radius:3px; min-height:210px;"),
                          // Grid 3x3
                          div(cls := "flex-grow-1",
                            style := "display:grid; grid-template-columns:1fr 1fr 1fr; grid-template-rows:1fr 1fr 1fr; min-height:210px; gap:2px; padding:2px;",
                            zonaRows.flatten.map(renderCell)
                          ),
                          // Poste derecho
                          div(style := "width:6px; background:linear-gradient(180deg,#888,#ccc,#888); border-radius:3px;")
                        )
                      )
                    ),
                    // Linea de fondo
                    div(cls := "d-flex justify-content-center mt-1",
                      div(style := "width:100%; max-width:360px; height:4px; background:rgba(255,255,255,0.15); border-radius:2px;")
                    ),
                    // Leyenda gradiente
                    div(cls := "d-flex justify-content-center align-items-center gap-2 mt-3",
                      span(cls := "xx-small text-muted", "0 goles"),
                      div(style := "width:100px; height:8px; border-radius:4px; background:linear-gradient(90deg, rgba(220,53,69,0.05), rgba(220,53,69,0.9));"),
                      span(cls := "xx-small text-muted", s"$maxVal.toInt goles")
                    )
                  )
                )
              ),

              // --- ESTADISTICAS ---
              div(cls := "col-md-5",
                // Por altura
                div(cls := "card bg-dark border-secondary shadow mb-3",
                  div(cls := "card-header text-white fw-bold small text-center", "POR ALTURA"),
                  div(cls := "card-body p-2",
                    Seq(("Alto", golsAlto, "danger"), ("Medio", golsMedio, "warning"), ("Bajo", golsBajo, "info")).map {
                      case (label, n, color) =>
                        val p = if (totalGoles > 0) (n * 100.0 / totalGoles).toInt else 0
                        div(cls := "mb-2",
                          div(cls := "d-flex justify-content-between small mb-1",
                            span(cls := "fw-bold text-white", label),
                            span(cls := s"text-$color fw-bold", s"$n ($p%)")
                          ),
                          div(cls := "progress", style := "height:8px;",
                            div(cls := s"progress-bar bg-$color", style := s"width:$p%;")
                          )
                        )
                    }
                  )
                ),
                // Por lado
                div(cls := "card bg-dark border-secondary shadow mb-3",
                  div(cls := "card-header text-white fw-bold small text-center", "POR LADO"),
                  div(cls := "card-body p-2",
                    Seq(("Izquierda", golsIzq, "danger"), ("Centro", golsCen, "warning"), ("Derecha", golsDer, "info")).map {
                      case (label, n, color) =>
                        val p = if (totalGoles > 0) (n * 100.0 / totalGoles).toInt else 0
                        div(cls := "mb-2",
                          div(cls := "d-flex justify-content-between small mb-1",
                            span(cls := "fw-bold text-white", label),
                            span(cls := s"text-$color fw-bold", s"$n ($p%)")
                          ),
                          div(cls := "progress", style := "height:8px;",
                            div(cls := s"progress-bar bg-$color", style := s"width:$p%;")
                          )
                        )
                    }
                  )
                ),
                // Zona mas vulnerable
                div(cls := "card bg-dark border-danger shadow",
                  div(cls := "card-body text-center p-3",
                    div(cls := "text-muted small fw-bold mb-1", "ZONA MAS VULNERABLE"),
                    if (totalGoles > 0) {
                      val worstZone = heatmap.maxBy(_._2)
                      val wpct = (worstZone._2 * 100.0 / totalGoles).toInt
                      div(
                        div(cls := "text-danger fw-bold", style := "font-size:24px;", cellLabel(worstZone._1).toUpperCase),
                        div(cls := "text-white fw-bold", s"${worstZone._2} goles ($wpct%)")
                      )
                    } else div()
                  )
                )
              )
            )
          }
        )
      )
    )
    renderHtml(content)
  }

  // ── SEGUIMIENTO DE LESIONES ──────────────────────────────────────────────────
  @cask.get("/lesiones")
  def lesionesPage(request: cask.Request) = withAuth(request) {
    val injuries  = DatabaseManager.getInjuries()
    val activa    = injuries.find(_.activa)
    val historico = injuries.filter(!_.activa)
    val totalDias = historico.map(_.diasBaja).sum

    def gravedadBadge(g: String) = g match {
      case "GRAVE"   => span(cls:="badge bg-danger fw-bold", g)
      case "MODERADA"=> span(cls:="badge bg-warning text-dark fw-bold", g)
      case _         => span(cls:="badge bg-success fw-bold", g)
    }

    def zonaIcon(z: String) = z.toLowerCase match {
      case s if s.contains("rodilla") => "🦵"
      case s if s.contains("tobillo") => "🦶"
      case s if s.contains("hombro")  => "💪"
      case s if s.contains("muneca") || s.contains("mano") => "🤚"
      case s if s.contains("espalda") || s.contains("lumbar") => "🔙"
      case _ => "🩹"
    }

    val content = basePage("history",
      div(cls:="row justify-content-center",
        div(cls:="col-md-10 col-12",
          div(cls:="d-flex justify-content-between align-items-center mb-3",
            h2(cls:="text-danger mb-0", "SEGUIMIENTO DE LESIONES"),
            a(href:="/history", cls:="btn btn-outline-secondary btn-sm fw-bold", "← Historial")
          ),

          // KPIs
          div(cls:="row g-2 mb-4",
            Seq(
              ("Lesiones totales", injuries.size.toString, "secondary"),
              ("Dias de baja total", totalDias.toString, "danger"),
              ("Estado actual", if(activa.isDefined) "LESIONADO" else "DISPONIBLE",
                if(activa.isDefined) "danger" else "success")
            ).map { case (lbl, v, c) =>
              div(cls:="col-4",
                div(cls:=s"card bg-dark border-$c text-center py-3",
                  div(cls:=s"text-$c fw-bold fs-4", v),
                  div(cls:="xx-small text-muted", lbl)
                )
              )
            }
          ),

          // Alerta lesion activa
          activa.map { inj =>
            div(cls:="alert alert-danger border-danger d-flex align-items-center gap-3 mb-4",
              div(style:="font-size:32px;", zonaIcon(inj.zona)),
              div(
                div(cls:="fw-bold fs-6", s"LESION ACTIVA — ${inj.zona.toUpperCase}"),
                div(cls:="small", s"${inj.tipo} | Desde: ${inj.fechaInicio}"),
                div(cls:="small text-warning", inj.descripcion),
                form(action:="/lesiones/alta", method:="post", cls:="d-flex gap-2 mt-2 align-items-center",
                  input(tpe:="hidden", name:="id", value:=inj.id.toString),
                  input(tpe:="date", name:="fechaAlta", cls:="form-control form-control-sm bg-dark text-white", style:="width:150px;"),
                  input(tpe:="number", name:="diasBaja", cls:="form-control form-control-sm bg-dark text-white", placeholder:="Dias baja", style:="width:120px;"),
                  button(tpe:="submit", cls:="btn btn-success btn-sm fw-bold", "DAR DE ALTA")
                )
              )
            )
          }.getOrElse(div()),

          // Formulario nueva lesion
          div(cls:="card bg-dark border-danger shadow mb-4",
            div(cls:="card-header text-danger fw-bold small", "REGISTRAR LESION"),
            div(cls:="card-body",
              form(action:="/lesiones/nueva", method:="post",
                div(cls:="row g-2 mb-2",
                  div(cls:="col-6",
                    label(cls:="xx-small text-muted fw-bold", "Zona"),
                    input(tpe:="text", name:="zona", cls:="form-control form-control-sm fw-bold",
                      placeholder:="Ej: Tobillo derecho", required:=true)
                  ),
                  div(cls:="col-6",
                    label(cls:="xx-small text-muted fw-bold", "Tipo"),
                    select(name:="tipo", cls:="form-select form-select-sm fw-bold",
                      option(value:="Esguince", "Esguince"),
                      option(value:="Contractura", "Contractura"),
                      option(value:="Fractura", "Fractura"),
                      option(value:="Tendinitis", "Tendinitis"),
                      option(value:="Contusion", "Contusion"),
                      option(value:="Sobrecarga", "Sobrecarga"),
                      option(value:="Otro", "Otro")
                    )
                  )
                ),
                div(cls:="row g-2 mb-2",
                  div(cls:="col-5",
                    label(cls:="xx-small text-muted fw-bold", "Gravedad"),
                    select(name:="gravedad", cls:="form-select form-select-sm fw-bold",
                      option(value:="LEVE", "Leve"),
                      option(value:="MODERADA", "Moderada"),
                      option(value:="GRAVE", "Grave")
                    )
                  ),
                  div(cls:="col-7",
                    label(cls:="xx-small text-muted fw-bold", "Descripcion"),
                    input(tpe:="text", name:="desc", cls:="form-control form-control-sm fw-bold",
                      placeholder:="Que ocurrio...")
                  )
                ),
                button(tpe:="submit", cls:="btn btn-danger w-100 btn-sm fw-bold mt-1", "Registrar")
              )
            )
          ),

          // Historial
          if (historico.nonEmpty) div(cls:="card bg-dark border-secondary shadow",
            div(cls:="card-header text-white fw-bold small", s"HISTORIAL (${historico.size} lesiones)"),
            div(cls:="card-body p-0",
              div(cls:="table-responsive",
                table(cls:="table table-dark table-sm mb-0 small",
                  thead(tr(
                    th("Zona"), th("Tipo"), th(cls:="text-center","Gravedad"),
                    th(cls:="text-center","Inicio"), th(cls:="text-center","Alta"),
                    th(cls:="text-center","Dias")
                  )),
                  tbody(historico.map { inj =>
                    tr(
                      td(s"${zonaIcon(inj.zona)} ${inj.zona}"),
                      td(inj.tipo),
                      td(cls:="text-center", gravedadBadge(inj.gravedad)),
                      td(cls:="text-center text-muted", inj.fechaInicio),
                      td(cls:="text-center text-muted", inj.fechaAlta),
                      td(cls:="text-center fw-bold text-warning", inj.diasBaja.toString)
                    )
                  })
                )
              )
            )
          ) else div(cls:="alert alert-secondary text-center small", "Sin lesiones registradas")
        )
      )
    )
    renderHtml(content)
  }

  @cask.postForm("/lesiones/nueva")
  def nuevaLesion(request: cask.Request, zona: String, tipo: String, gravedad: String, desc: String) = {
    DatabaseManager.logInjury(zona, tipo, gravedad, desc)
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location"->"/lesiones"))
  }

  @cask.postForm("/lesiones/alta")
  def darAlta(request: cask.Request, id: Int, fechaAlta: String, diasBaja: Int) = {
    DatabaseManager.closeInjury(id, fechaAlta, diasBaja)
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location"->"/lesiones"))
  }

  // ── FLASH-CARDS PRE-PARTIDO ───────────────────────────────────────────────
  @cask.get("/flash-cards")
  def flashCardsPage(request: cask.Request, rival: String = "") = withAuth(request) {
    val nextMatches = DatabaseManager.getUpcomingMatches()
    val targetRival = if (rival.nonEmpty) rival
    else nextMatches.headOption.map(_.rival).getOrElse("")

    val data = if (targetRival.nonEmpty) DatabaseManager.getFlashCardData(targetRival)
    else Map.empty[String, Any]

    val partidos = data.getOrElse("partidos", List.empty).asInstanceOf[List[(String,Double,String,String,String)]]
    val zonaMasVulnerable = data.getOrElse("zonaMasVulnerable", "—").toString
    val clips = data.getOrElse("clips", List.empty).asInstanceOf[List[(Int,Int,String,String,String)]]
    val estilo = data.getOrElse("estilo", "").toString
    val claves = data.getOrElse("claves", "").toString

    val winRate = if (partidos.nonEmpty) {
      val wins = partidos.count { case (res, _, _, _, _) =>
        val p = res.split("-"); p.headOption.flatMap(_.trim.toIntOption).getOrElse(0) >
        p.lastOption.flatMap(_.trim.toIntOption).getOrElse(0)
      }
      s"${(wins * 100 / partidos.size)}%"
    } else "—"

    val avgNotaVsRival = if (partidos.nonEmpty)
      f"${partidos.map(_._2).sum / partidos.size}%.1f" else "—"

    def zoneLabel(z: String) = z match {
      case "TL"=>"Arr Izq"; case "TC"=>"Arr Cen"; case "TR"=>"Arr Der"
      case "ML"=>"Med Izq"; case "MC"=>"Med Cen"; case "MR"=>"Med Der"
      case "BL"=>"Baj Izq"; case "BC"=>"Baj Cen"; case "BR"=>"Baj Der"
      case _ => z
    }

    def ytId(url: String) = {
      val patterns = List("v=", "youtu.be/", "embed/")
      patterns.flatMap { p =>
        val idx = url.indexOf(p)
        if (idx >= 0) Some(url.substring(idx + p.length).takeWhile(c => c != '&' && c != '?'))
        else None
      }.headOption.getOrElse("")
    }

    val content = basePage("match-center",
      div(cls:="row justify-content-center",
        div(cls:="col-md-10 col-12",

          // Header
          div(cls:="d-flex justify-content-between align-items-center mb-3",
            h2(cls:="text-warning mb-0", "FLASH-CARDS PRE-PARTIDO"),
            a(href:="/match-center", cls:="btn btn-outline-secondary btn-sm fw-bold", "← Match Center")
          ),

          // Selector de rival
          div(cls:="card bg-dark border-secondary shadow mb-4",
            div(cls:="card-body d-flex gap-2 align-items-center",
              span(cls:="text-muted small fw-bold", "Rival:"),
              form(action:="/flash-cards", method:="get", cls:="d-flex gap-2 flex-grow-1",
                input(tpe:="text", name:="rival", cls:="form-control form-control-sm bg-dark text-white fw-bold",
                  placeholder:="Buscar rival...", value:=targetRival),
                button(tpe:="submit", cls:="btn btn-warning btn-sm fw-bold", "ANALIZAR")
              ),
              if (nextMatches.nonEmpty) div(cls:="d-flex gap-1 flex-wrap",
                nextMatches.take(3).map { m =>
                  a(href:=s"/flash-cards?rival=${java.net.URLEncoder.encode(m.rival, "UTF-8")}",
                    cls:=s"btn btn-sm fw-bold ${if(m.rival==targetRival)"btn-warning"else"btn-outline-secondary"}",
                    m.rival)
                }
              ) else div()
            )
          ),

          if (targetRival.isEmpty) {
            div(cls:="alert alert-secondary text-center py-5",
              div(style:="font-size:40px; opacity:0.3;", "🗂️"),
              div(cls:="fw-bold mt-2", "Introduce el nombre del rival para generar el briefing")
            )
          } else div(
            // CARD PRINCIPAL — resumen rival
            div(cls:="card border-warning shadow mb-4", style:="background: linear-gradient(135deg, #1a1a1a 0%, #2a2000 100%);",
              div(cls:="card-body",
                div(cls:="d-flex justify-content-between align-items-start mb-3",
                  div(
                    div(cls:="xx-small text-muted fw-bold text-uppercase", "PROXIMO RIVAL"),
                    h3(cls:="text-warning fw-bold mb-0", style:="font-size:28px; letter-spacing:2px;",
                      targetRival.toUpperCase)
                  ),
                  div(cls:="text-end",
                    div(cls:="xx-small text-muted", "NOTA MEDIA vs"),
                    div(cls:="text-warning fw-bold fs-3", avgNotaVsRival)
                  )
                ),
                div(cls:="row g-2",
                  Seq(
                    ("Partidos vs", partidos.size.toString, "secondary"),
                    ("Win Rate", winRate, "success"),
                    ("Zona vulnerable", zoneLabel(zonaMasVulnerable), "danger")
                  ).map { case (lbl, v, c) =>
                    div(cls:="col-4",
                      div(cls:=s"text-center p-2 rounded border border-$c",
                        style:="background:rgba(0,0,0,0.3);",
                        div(cls:=s"fw-bold text-$c", v),
                        div(cls:="xx-small text-muted", lbl)
                      )
                    )
                  }
                ),
                if (estilo.nonEmpty) div(cls:="mt-3 pt-3 border-top border-secondary",
                  div(cls:="xx-small text-muted fw-bold", "ESTILO DE JUEGO"),
                  div(cls:="text-light small", estilo)
                ) else div(),
                if (claves.nonEmpty) div(cls:="mt-2",
                  div(cls:="xx-small text-muted fw-bold", "CLAVES TACTICAS"),
                  div(cls:="text-warning small fw-bold", claves)
                ) else div()
              )
            ),

            div(cls:="row g-3",
              // Historial vs rival
              div(cls:=s"col-md-${if(clips.nonEmpty)"5"else"12"}",
                div(cls:="card bg-dark border-secondary shadow h-100",
                  div(cls:="card-header text-white fw-bold small", s"ULTIMOS PARTIDOS vs ${targetRival.toUpperCase}"),
                  div(cls:="card-body p-2",
                    if (partidos.isEmpty)
                      div(cls:="text-muted text-center small py-3", "Sin historial registrado vs este rival")
                    else div(
                      partidos.map { case (res, nota, fecha, zona, notas) =>
                        val partes = res.split("-")
                        val (gf, gc) = (partes.headOption.flatMap(_.trim.toIntOption).getOrElse(0),
                          partes.lastOption.flatMap(_.trim.toIntOption).getOrElse(0))
                        val (resCls, resLabel) = if (gf > gc) ("success","V") else if (gf==gc) ("warning","E") else ("danger","D")
                        div(cls:="d-flex align-items-center gap-2 p-2 mb-1 rounded",
                          style:="background:rgba(255,255,255,0.03);",
                          span(cls:=s"badge bg-$resCls fw-bold", style:="width:22px;", resLabel),
                          span(cls:="text-white fw-bold small", res),
                          span(cls:="text-muted xx-small", fecha),
                          div(cls:="ms-auto",
                            span(cls:=s"badge ${if(nota>=7)"bg-success"else if(nota>=5)"bg-warning text-dark"else"bg-danger"} fw-bold",
                              nota.toString)
                          )
                        )
                      }
                    )
                  )
                )
              ),

              // Clips de paradas
              if (clips.nonEmpty) div(cls:="col-md-7",
                div(cls:="card bg-dark border-warning shadow",
                  div(cls:="card-header text-warning fw-bold small", "CLIPS DE MOTIVACION — Tus mejores paradas"),
                  div(cls:="card-body p-2",
                    div(cls:="row g-2",
                      clips.map { case (min, seg, tipo, rivalClip, url) =>
                        val vid = ytId(url)
                        div(cls:="col-12",
                          if (vid.nonEmpty) div(
                            div(cls:="ratio ratio-16x9 mb-1",
                              iframe(src:=s"https://www.youtube.com/embed/$vid?start=${min*60+seg}&mute=1",
                                attr("allowfullscreen"):="true",
                                attr("frameborder"):="0",
                                style:="border-radius:6px;")
                            ),
                            div(cls:="d-flex justify-content-between xx-small text-muted",
                              span(s"$tipo — ${min}m${seg}s"),
                              span(fixEncoding(rivalClip))
                            )
                          ) else div()
                        )
                      }
                    )
                  )
                )
              ) else div()
            )
          )
        )
      )
    )
    renderHtml(content)
  }

  initialize()
}