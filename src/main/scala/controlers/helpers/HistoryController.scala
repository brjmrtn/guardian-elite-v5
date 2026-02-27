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
          div(cls := "text-muted", style := "font-size:18px; opacity:0.3;", "--")
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
          p(cls  := "text-center text-muted small mb-4", s"$tituloFiltro -- $totalGoles goles en total"),

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
                    span(cls := "text-danger fw-bold", "PORTERIA -- Vista frontal"),
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
                div(cls:="fw-bold fs-6", s"LESION ACTIVA -- ${inj.zona.toUpperCase}"),
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
    val zonaMasVulnerable = data.getOrElse("zonaMasVulnerable", "--").toString
    val clips = data.getOrElse("clips", List.empty).asInstanceOf[List[(Int,Int,String,String,String)]]
    val estilo = data.getOrElse("estilo", "").toString
    val claves = data.getOrElse("claves", "").toString

    val winRate = if (partidos.nonEmpty) {
      val wins = partidos.count { case (res, _, _, _, _) =>
        val p = res.split("-"); p.headOption.flatMap(_.trim.toIntOption).getOrElse(0) >
        p.lastOption.flatMap(_.trim.toIntOption).getOrElse(0)
      }
      s"${(wins * 100 / partidos.size)}%"
    } else "--"

    val avgNotaVsRival = if (partidos.nonEmpty)
      f"${partidos.map(_._2).sum / partidos.size}%.1f" else "--"

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
            // CARD PRINCIPAL -- resumen rival
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
                  div(cls:="card-header text-warning fw-bold small", "CLIPS DE MOTIVACION -- Tus mejores paradas"),
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
                              span(s"$tipo -- ${min}m${seg}s"),
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

  // ── GK INFLUENCE ANALYTICS ───────────────────────────────────────────────
  @cask.get("/gk-influence")
  def gkInfluencePage(request: cask.Request) = withAuth(request) {
    val stats = DatabaseManager.getGKInfluenceStats()
    if (stats.isEmpty) renderHtml(basePage("history",
      div(cls:="text-center text-muted py-5", "Sin partidos jugados aun")
    )) else {

      val avgPie      = stats("avgPie").asInstanceOf[Double]
      val totalPie    = stats("totalPie").asInstanceOf[Int]
      val notaAltaPie = stats("notaAltaPie").asInstanceOf[Double]
      val notaBajaPie = stats("notaBajaPie").asInstanceOf[Double]
      val pctCentros  = stats("pctCentros").asInstanceOf[Double]
      val pctLargos   = stats("pctLargos").asInstanceOf[Double]
      val pj          = stats("pj").asInstanceOf[Int]
      val pcs         = stats("pcs").asInstanceOf[Int]
      val centTotal   = stats("centTotal").asInstanceOf[Int]
      val centOk      = stats("centOk").asInstanceOf[Int]
      val largTotal   = stats("largTotal").asInstanceOf[Int]
      val largOk      = stats("largOk").asInstanceOf[Int]
      val serie       = stats("serie").asInstanceOf[List[(String,Int,Double,String)]]

      val influenceScore = {
        var s = 50.0
        if (avgPie > 8) s += 15 else if (avgPie > 5) s += 8
        if (pctCentros > 0.7) s += 10 else if (pctCentros > 0.5) s += 5
        if (pctLargos > 0.7) s += 10 else if (pctLargos > 0.5) s += 5
        if (pj > 0 && pcs.toDouble/pj > 0.4) s += 10
        if (notaAltaPie > notaBajaPie + 0.5) s += 10
        math.min(99, math.max(1, s.toInt))
      }
      val (scoreColor, scoreLabel) = if (influenceScore >= 75) ("success","ALTO")
      else if (influenceScore >= 50) ("warning","MEDIO")
      else ("danger","BAJO")

      val serieLabels = serie.reverse.map(_._1.take(5)).map(s => "\"" + s + "\"").mkString("[", ",", "]")
      val seriePie    = serie.reverse.map(_._2.toString).mkString("[",",","]")
      val serieNota   = serie.reverse.map(_._3.toString).mkString("[",",","]")

      val content = basePage("history",
        div(cls:="row justify-content-center",
          div(cls:="col-md-10 col-12",
            div(cls:="d-flex justify-content-between align-items-center mb-3",
              h2(cls:="text-info mb-0", "GK INFLUENCE ANALYTICS"),
              a(href:="/history", cls:="btn btn-outline-secondary btn-sm fw-bold", "Historial")
            ),

            // Score principal
            div(cls:=s"card bg-dark border-$scoreColor shadow mb-4",
              div(cls:=s"card-header bg-$scoreColor bg-opacity-10 border-$scoreColor d-flex justify-content-between align-items-center",
                span(cls:=s"text-$scoreColor fw-bold", "INDICE DE INFLUENCIA DEL PORTERO"),
                span(cls:=s"badge bg-$scoreColor fw-bold fs-6", s"$influenceScore / 100")
              ),
              div(cls:="card-body",
                div(cls:="d-flex align-items-center gap-4 mb-3",
                  div(style:=s"width:80px;height:80px;border-radius:50%;border:5px solid ${if(scoreColor=="success")"#28a745"else if(scoreColor=="warning")"#ffc107"else"#dc3545"};display:flex;align-items:center;justify-content:center;flex-shrink:0;",
                    div(cls:=s"fw-bold fs-4 text-$scoreColor", s"$influenceScore")
                  ),
                  div(
                    div(cls:="fw-bold text-white fs-6", s"IMPACTO $scoreLabel EN EL JUEGO"),
                    div(cls:="text-muted small mt-1", "Basado en distribuciones, centros controlados y correlacion con resultado")
                  )
                ),
                div(cls:="row g-2",
                  Seq(
                    ("Acc. con pie / partido", f"$avgPie%.1f", if(avgPie>6)"success"else"secondary"),
      ("Total distribuciones", totalPie.toString, "info"),
      ("% Centros controlados", if(centTotal>0) f"${pctCentros*100}%.0f%%" else "--", if(pctCentros>0.6)"success"else"warning"),
      ("% Balones largos OK", if(largTotal>0) f"${pctLargos*100}%.0f%%" else "--", if(pctLargos>0.6)"success"else"warning"),
      ("Nota con +pie", if(notaAltaPie>0) f"$notaAltaPie%.1f" else "--", if(notaAltaPie>7)"success"else"secondary"),
      ("Nota con -pie", if(notaBajaPie>0) f"$notaBajaPie%.1f" else "--", if(notaBajaPie>7)"success"else"secondary")
      ).map { case (lbl, v, c) =>
        div(cls:="col-4",
          div(cls:=s"text-center p-2 rounded border border-$c bg-dark",
            div(cls:=s"fw-bold text-$c", v),
            div(cls:="xx-small text-muted", lbl)
          )
        )
      }
      )
      )
      ),

      div(cls:="row g-3",
        // Grafico distribucion vs nota
        div(cls:="col-md-7",
          div(cls:="card bg-dark border-info shadow",
            div(cls:="card-header text-info fw-bold small", "DISTRIBUCIONES CON PIE vs NOTA (ultimos 20 partidos)"),
            div(cls:="card-body p-2",
              tag("canvas")(id:="chartInfluence", style:="max-height:220px;")
            )
          )
        ),
        // Balones parados
        div(cls:="col-md-5",
          div(cls:="card bg-dark border-secondary shadow",
            div(cls:="card-header text-white fw-bold small", "BALONES PARADOS"),
            div(cls:="card-body p-3",
              div(cls:="mb-3",
                div(cls:="d-flex justify-content-between small mb-1",
                  span(cls:="text-muted fw-bold", "CENTROS"),
                  span(cls:="text-info fw-bold", s"$centOk / $centTotal")
                ),
                div(cls:="progress", style:="height:10px;",
                  div(cls:="progress-bar bg-info",
                    style:=s"width:${if(centTotal>0)(centOk*100/centTotal)else 0}%;")
                )
              ),
              div(cls:="mb-3",
                div(cls:="d-flex justify-content-between small mb-1",
                  span(cls:="text-muted fw-bold", "BALONES LARGOS"),
                  span(cls:="text-warning fw-bold", s"$largOk / $largTotal")
                ),
                div(cls:="progress", style:="height:10px;",
                  div(cls:="progress-bar bg-warning",
                    style:=s"width:${if(largTotal>0)(largOk*100/largTotal)else 0}%;")
                )
              ),
              hr(cls:="border-secondary"),
              div(cls:="text-center",
                div(cls:="xx-small text-muted fw-bold", "CORRELACION PIE - RENDIMIENTO"),
                if (notaAltaPie > 0 && notaBajaPie > 0) {
                  val diff = notaAltaPie - notaBajaPie
                  val (diffCls, diffTxt) = if (diff > 0.3) ("success", s"Con mas pie juegas un ${f"$diff%.1f"} mejor")
                  else if (diff < -0.3) ("warning", "El pie no parece clave para tu rendimiento")
                  else ("secondary", "Impacto neutro del juego con pie")
                  div(cls:=s"text-$diffCls small fw-bold mt-2", diffTxt)
                } else div(cls:="text-muted small mt-2", "Sin suficientes datos")
              )
            )
          )
        )
      ),

      script(src:="https://cdn.jsdelivr.net/npm/chart.js"),
      {
        val jsGK: String =
          "var ctxGK=document.getElementById('chartInfluence');" +
            "if(ctxGK){new Chart(ctxGK,{type:'bar'," +
            "data:{labels:" + serieLabels + ",datasets:[" +
            "{label:'Acc. con pie',data:" + seriePie + ",backgroundColor:'rgba(13,202,240,0.5)',borderColor:'#0dcaf0',borderWidth:2,yAxisID:'y'}," +
            "{label:'Nota partido',data:" + serieNota + ",type:'line',borderColor:'#ffc107',borderWidth:2,pointRadius:3,tension:0.3,yAxisID:'y1'}]}," +
            "options:{responsive:true,maintainAspectRatio:false," +
            "scales:{y:{position:'left',ticks:{color:'#0dcaf0'},grid:{color:'#333'},min:0}," +
            "y1:{position:'right',ticks:{color:'#ffc107'},grid:{display:false},min:0,max:10}," +
            "x:{ticks:{color:'#aaa'},grid:{display:false}}}," +
            "plugins:{legend:{labels:{color:'#fff'}}}}})}"
        script(raw(jsGK))
      }
      )
      )
      )
      renderHtml(content)
    } // end else
  }

  // ── BIOMECANICA POSICIONAL ────────────────────────────────────────────────
  @cask.get("/biomecanica")
  def biomecanicaPage(request: cask.Request) = withAuth(request) {
    val stats = DatabaseManager.getBiomecPosicional()
    if (stats.isEmpty) renderHtml(basePage("history",
      div(cls:="text-center text-muted py-5", "Sin partidos jugados aun")
    )) else {

      val goles     = stats("goles").asInstanceOf[Map[String,Int]]
      val paradas   = stats("paradas").asInstanceOf[Map[String,Int]]
      val tiros     = stats("tiros").asInstanceOf[Map[String,Int]]
      val efic      = stats("eficiencia").asInstanceOf[Map[String,Int]]
      val puntosCiegos  = stats("puntosCiegos").asInstanceOf[Seq[String]]
      val zonasFuertes  = stats("zonasFuertes").asInstanceOf[Seq[String]]
      val stopRate: Map[String,String] = stats("stopRate").asInstanceOf[Map[String,String]]
      val zones     = stats("zones").asInstanceOf[Seq[String]]

      def zoneLabel(z: String) = z match {
        case "TL"=>"Arr-Izq"; case "TC"=>"Arr-Cen"; case "TR"=>"Arr-Der"
        case "ML"=>"Med-Izq"; case "MC"=>"Med-Cen"; case "MR"=>"Med-Der"
        case "BL"=>"Baj-Izq"; case "BC"=>"Baj-Cen"; case "BR"=>"Baj-Der"
        case _ => z
      }

      def zoneCellColor(z: String): String = {
        val e = efic.getOrElse(z, -1)
        if (e == -1) "#333"
        else if (e >= 70) "rgba(40,167,69,0.4)"
        else if (e >= 50) "rgba(255,193,7,0.3)"
        else "rgba(220,53,69,0.4)"
      }

      def zoneBorder(z: String): String = {
        val e = efic.getOrElse(z, -1)
        if (e == -1) "#555"
        else if (e >= 70) "#28a745"
        else if (e >= 50) "#ffc107"
        else "#dc3545"
      }

      // Grid 3x3 de la porteria
      def porteriaGrid(showMode: String) = {
        val rows = Seq(
          Seq("TL","TC","TR"),
          Seq("ML","MC","MR"),
          Seq("BL","BC","BR")
        )
        div(style:="border:3px solid #fff; border-radius:4px; overflow:hidden; background:#1a1a1a;",
          frag(rows.map { row =>
            div(cls:="d-flex", style:="border-bottom:1px solid #444;",
              row.map { z =>
                val g = goles.getOrElse(z, 0)
                val p = paradas.getOrElse(z, 0)
                val t = tiros.getOrElse(z, 0)
                val e = efic.getOrElse(z, -1)
                val (mainVal, mainColor) = showMode match {
                  case "goles"   => (if(g>0) g.toString else "-", if(g>=2)"#dc3545"else if(g==1)"#ffc107"else"#555")
                  case "paradas" => (if(p>0) p.toString else "-", if(p>=3)"#28a745"else if(p>=1)"#0dcaf0"else"#555")
                  case _ =>
                    val label = if(e == -1) "--" else s"$e%%"
                    val c = if(e == -1)"#555" else if(e>=70)"#28a745" else if(e>=50)"#ffc107" else "#dc3545"
                    (label, c)
                }
                div(
                  style:=s"flex:1; padding:12px 4px; text-align:center; background:${zoneCellColor(z)}; border-right:1px solid #444; cursor:default;",
                  div(style:=s"font-size:20px; font-weight:700; color:$mainColor;", mainVal),
                  div(style:="font-size:9px; color:#888; text-transform:uppercase; letter-spacing:1px;", zoneLabel(z))
                )
              }
            )
          }: _*)
        )
      }

      val content = basePage("history",
        div(cls:="row justify-content-center",
          div(cls:="col-md-10 col-12",
            div(cls:="d-flex justify-content-between align-items-center mb-3",
              h2(cls:="text-warning mb-0", "BIOMECANICA POSICIONAL"),
              a(href:="/history", cls:="btn btn-outline-secondary btn-sm fw-bold", "Historial")
            ),

            // Alertas puntos ciegos y zonas fuertes
            div(cls:="row g-2 mb-4",
              div(cls:="col-md-6",
                div(cls:="card bg-dark border-danger shadow h-100",
                  div(cls:="card-header text-danger fw-bold small", "PUNTOS CIEGOS -- Zonas vulnerables"),
                  div(cls:="card-body p-2",
                    if (puntosCiegos.isEmpty)
                      div(cls:="text-muted text-center small py-2", "Sin puntos ciegos detectados")
                    else div(
                      puntosCiegos.take(3).map { z =>
                        val g = goles.getOrElse(z, 0)
                        val p = paradas.getOrElse(z, 0)
                        div(cls:="d-flex align-items-center gap-2 p-2 mb-1 rounded",
                          style:="background:rgba(220,53,69,0.15); border-left:3px solid #dc3545;",
                          div(cls:="fw-bold text-danger", style:="min-width:70px;", zoneLabel(z)),
                          div(cls:="flex-grow-1",
                            div(cls:="progress", style:="height:8px;",
                              div(cls:="progress-bar bg-danger",
                                style:=s"width:${if(g+p>0)(g*100/(g+p))else 0}%;")
                            )
                          ),
                          span(cls:="text-danger fw-bold small", s"$g goles"),
                          span(cls:="text-muted xx-small", s"$p paradas")
                        )
                      }
                    )
                  )
                )
              ),
              div(cls:="col-md-6",
                div(cls:="card bg-dark border-success shadow h-100",
                  div(cls:="card-header text-success fw-bold small", "ZONAS FUERTES -- Mayor dominio"),
                  div(cls:="card-body p-2",
                    if (zonasFuertes.isEmpty)
                      div(cls:="text-muted text-center small py-2", "Sin datos suficientes")
                    else div(
                      zonasFuertes.take(3).map { z =>
                        val g = goles.getOrElse(z, 0)
                        val p = paradas.getOrElse(z, 0)
                        div(cls:="d-flex align-items-center gap-2 p-2 mb-1 rounded",
                          style:="background:rgba(40,167,69,0.15); border-left:3px solid #28a745;",
                          div(cls:="fw-bold text-success", style:="min-width:70px;", zoneLabel(z)),
                          div(cls:="flex-grow-1",
                            div(cls:="progress", style:="height:8px;",
                              div(cls:="progress-bar bg-success",
                                style:=s"width:${if(g+p>0)(p*100/(g+p))else 0}%;")
                            )
                          ),
                          span(cls:="text-success fw-bold small", s"$p paradas"),
                          span(cls:="text-muted xx-small", s"$g goles")
                        )
                      }
                    )
                  )
                )
              )
            ),

            // Grid porteria -- 3 vistas
            div(cls:="card bg-dark border-secondary shadow mb-4",
              div(cls:="card-header text-white fw-bold small d-flex justify-content-between align-items-center",
                span("MAPA DE PORTERIA INTERACTIVO"),
                div(cls:="d-flex gap-1",
                  Seq(("goles","Goles","danger"), ("paradas","Paradas","success"), ("efic","Eficiencia","warning")).map { case (mode, lbl, c) =>
                    button(cls:=s"btn btn-sm btn-outline-$c fw-bold xx-small",
                      onclick:=s"switchMode('$mode')", id:=s"btn-$mode", lbl)
                  }
                )
              ),
              div(cls:="card-body p-3",
                div(cls:="row g-3",
                  div(cls:="col-md-5",
                    div(id:="grid-goles", porteriaGrid("goles")),
                    div(id:="grid-paradas", style:="display:none;", porteriaGrid("paradas")),
                    div(id:="grid-efic", style:="display:none;", porteriaGrid("efic")),
                    div(cls:="d-flex justify-content-center gap-3 mt-2",
                      Seq(("#28a745","Alto (70%+)"),("#ffc107","Medio (50-70%)"),("#dc3545","Bajo (<50%)")).map { case (c, lbl) =>
                        div(cls:="d-flex align-items-center gap-1",
                          div(style:=s"width:12px;height:12px;background:$c;border-radius:2px;"),
                          div(cls:="xx-small text-muted", lbl)
                        )
                      }
                    )
                  ),
                  div(cls:="col-md-7",
                    div(cls:="table-responsive",
                      table(cls:="table table-dark table-sm small mb-0",
                        thead(tr(
                          th("Zona"), th(cls:="text-center","Tiros"), th(cls:="text-center","Goles"),
                          th(cls:="text-center","Paradas"), th(cls:="text-center","Stop%")
                        )),
                        tbody(frag(zones.toSeq.map { z =>
                          val e = efic.getOrElse(z, -1)
                          val rowCls = if(e == -1) "" else if(e >= 70) "table-success" else if(e >= 50) "table-warning" else "table-danger"
                          tr(cls:=s"$rowCls bg-opacity-25",
                            td(cls:="fw-bold", zoneLabel(z)),
                            td(cls:="text-center", tiros.getOrElse(z, 0).toString),
                            td(cls:="text-center fw-bold text-danger", goles.getOrElse(z, 0).toString),
                            td(cls:="text-center fw-bold text-success", paradas.getOrElse(z, 0).toString),
                            td(cls:="text-center fw-bold", stopRate.getOrElse(z, "--").asInstanceOf[String])
                          )
                        }: _*))
                      )
                    )
                  )
                )
              )
            ),

            script(raw("""
            function switchMode(mode) {
              ['goles','paradas','efic'].forEach(m => {
                document.getElementById('grid-'+m).style.display = m === mode ? 'block' : 'none';
                document.getElementById('btn-'+m).classList.toggle('active', m === mode);
              });
            }
            switchMode('goles');
          """))
          )
        )
      )
      renderHtml(content)
    } // end else
  }

  // ── EMOTIONAL INTELLIGENCE ENGINE ───────────────────────────────────────
  @cask.get("/emocional")
  def emocionalPage(request: cask.Request) = withAuth(request) {
    val stats = DatabaseManager.getEmotionalData()

    val entries    = stats("entries").asInstanceOf[List[DatabaseManager.EmotionalEntry]]
    val correlacion= stats("correlacion").asInstanceOf[Double]
    val diasBajos  = stats("diasBajosConsecutivos").asInstanceOf[Int]
    val avgAnimo   = stats("avgAnimoReciente").asInstanceOf[Double]
    val avgEnergia = stats("avgEnergiaReciente").asInstanceOf[Double]
    val score      = stats("resilienciaScore").asInstanceOf[Int]
    val analisisIA = stats("analisisIA").asInstanceOf[String]
    val notasCount = stats("notasCount").asInstanceOf[Int]
    val total      = stats("totalEntries").asInstanceOf[Int]

    val (scoreColor, scoreLabel) =
      if (score >= 75) ("success","ALTA") else if (score >= 50) ("warning","MEDIA") else ("danger","BAJA")

    // Parsear las 3 partes del analisis IA
    def extractIA(tag: String): String = {
      val idx = analisisIA.indexOf(tag + ":")
      if (idx == -1) ""
      else {
        val start = idx + tag.length + 1
        val nextTag = Seq("PATRON:", "FORTALEZA:", "CONSEJO:").filter(_ != tag + ":").flatMap { t =>
          val i = analisisIA.indexOf(t, start); if (i > 0) Some(i) else None
        }
        val end = if (nextTag.nonEmpty) nextTag.min else analisisIA.length
        analisisIA.substring(start, end).trim
      }
    }
    val patron    = extractIA("PATRON")
    val fortaleza = extractIA("FORTALEZA")
    val consejo   = extractIA("CONSEJO")

    // Datos para graficos
    val ultimos = entries.takeRight(30)
    val fechas  = ultimos.map(e => "\"" + e.fecha.drop(5) + "\"").mkString("[", ",", "]")
    val animos  = ultimos.map(_.animo.toString).mkString("[", ",", "]")
    val energias= ultimos.map(_.energia.toString).mkString("[", ",", "]")
    val notasPartido = ultimos.map(e => e.notaPartido.map(_.toString).getOrElse("null")).mkString("[",",","]")

    // Tabla de entradas recientes con notas
    val conNotas = entries.filter(_.notas.nonEmpty).takeRight(10).reverse

    // Emojis por nivel
    def animoEmoji(n: Int): String  = n match { case 5=>"😄"; case 4=>"🙂"; case 3=>"😐"; case 2=>"😕"; case _=>"😞" }
    def energiaEmoji(n: Int): String = n match { case 5=>"⚡"; case 4=>"🔋"; case 3=>"➖"; case 2=>"🪫"; case _=>"😴" }

    if (total == 0) {
      renderHtml(basePage("bio",
        div(cls:="text-center py-5",
          div(style:="font-size:48px;","🧠"),
          h4(cls:="text-info mt-3","Motor Emocional"),
          p(cls:="text-muted","Registra tu estado fisico diario para activar este modulo."),
          a(href:="/bio", cls:="btn btn-outline-info mt-2 fw-bold","Ir a Bio")
        )
      ))
    } else {
      val pageContent = basePage("bio",
        div(cls:="row justify-content-center",
          div(cls:="col-md-10 col-12",
            div(cls:="d-flex justify-content-between align-items-center mb-3",
              h2(cls:="text-info mb-0","🧠 MOTOR EMOCIONAL"),
              a(href:="/bio", cls:="btn btn-outline-secondary btn-sm fw-bold","Bio")
            ),

            // Score resiliencia + KPIs
            div(cls:="row g-2 mb-3",
              div(cls:="col-md-4",
                div(cls:=s"card bg-dark border-$scoreColor shadow h-100",
                  div(cls:=s"card-header bg-$scoreColor bg-opacity-10 text-$scoreColor fw-bold small text-center",
                    "RESILIENCIA MENTAL"
                  ),
                  div(cls:="card-body text-center py-3",
                    div(style:=s"font-size:52px; font-weight:900; color:${if(scoreColor=="success")"#28a745"else if(scoreColor=="warning")"#ffc107"else"#dc3545"};",
                      score.toString
                    ),
                    div(cls:=s"badge bg-$scoreColor fw-bold mt-1", scoreLabel),
                    div(cls:="text-muted xx-small mt-2", "Basado en patron emocional, correlacion con rendimiento y estabilidad de animo")
                  )
                )
              ),
              div(cls:="col-md-8",
                div(cls:="row g-2 h-100",
                  frag(Seq(
                    ("Animo medio (7d)", f"$avgAnimo%.1f/5", animoEmoji(avgAnimo.round.toInt), if(avgAnimo>=4)"success"else if(avgAnimo>=3)"warning"else"danger"),
      ("Energia media (7d)", f"$avgEnergia%.1f/5", energiaEmoji(avgEnergia.round.toInt), if(avgEnergia>=4)"success"else if(avgEnergia>=3)"warning"else"danger"),
      ("Correlacion animo-nota", if(correlacion>0.3) f"+$correlacion%.1f pts" else if(correlacion < -0.3) f"$correlacion%.1f pts" else "Neutro", if(correlacion>0.3)":"+"📈"else if(correlacion < -0.3)":"+"📉"else":-", if(correlacion>0.3)"success"else if(correlacion < -0.3)"warning"else"secondary"),
      ("Dias bajos consecutivos", if(diasBajos==0)"Ninguno"else s"$diasBajos dias", if(diasBajos==0)":)"else if(diasBajos<=2)"(!)"else"[!]", if(diasBajos==0)"success"else if(diasBajos<=2)"warning"else"danger"),
      ("Registros con notas", s"$notasCount / $total", ":memo:", "info"),
      ("Dias analizados", total.toString, ":cal:", "secondary")
      ).map { case (lbl, v, ico, c) =>
        div(cls:="col-4",
          div(cls:=s"card bg-dark border-$c h-100",
            div(cls:="card-body p-2 text-center",
              div(style:="font-size:20px;", ico),
              div(cls:=s"fw-bold text-$c small", v),
              div(cls:="xx-small text-muted", lbl)
            )
          )
        )
      }: _*)
      )
      )
      ),

      // Analisis IA
      if (patron.nonEmpty || fortaleza.nonEmpty || consejo.nonEmpty) {
        div(cls:="card bg-dark border-info shadow mb-3",
          div(cls:="card-header text-info fw-bold small", "🤖 ANALISIS PSICOPEDAGOGICO (IA)"),
          div(cls:="card-body p-3",
            div(cls:="row g-3",
              frag(Seq(
                ("PATRON EMOCIONAL", patron, "info", "?"),
                ("FORTALEZA MENTAL", fortaleza, "success", "*"),
                ("CONSEJO DE LA SEMANA", consejo, "warning", ">")
              ).filter(_._2.nonEmpty).map { case (titulo, texto, c, ico) =>
                div(cls:="col-md-4",
                  div(cls:=s"p-3 rounded h-100",
                    style:=s"background:rgba(${if(c=="info")"13,202,240"else if(c=="success")"40,167,69"else"255,193,7"},0.1); border-left:3px solid ${if(c=="info")"#0dcaf0"else if(c=="success")"#28a745"else"#ffc107"};",
                    div(cls:=s"text-$c fw-bold xx-small mb-2", s"$ico $titulo"),
                    div(cls:="text-white small", texto)
                  )
                )
              }: _*)
            )
          )
        )
      } else div(cls:="card bg-dark border-secondary shadow mb-3",
        div(cls:="card-body p-3 text-muted small text-center",
          "Escribe notas de conducta en tu registro diario para activar el analisis IA"
        )
      ),

      // Grafico animo + energia + nota
      div(cls:="card bg-dark border-secondary shadow mb-3",
        div(cls:="card-header text-white fw-bold small", "EVOLUCION EMOCIONAL (ultimos 30 dias)"),
        div(cls:="card-body p-2",
          tag("canvas")(id:="chartEmocional", style:="max-height:220px;")
        )
      ),

      // Tabla diario emocional con notas
      if (conNotas.nonEmpty) div(cls:="card bg-dark border-secondary shadow",
        div(cls:="card-header text-white fw-bold small", "DIARIO EMOCIONAL -- Ultimas entradas con notas"),
        div(cls:="card-body p-0",
          div(cls:="table-responsive",
            table(cls:="table table-dark table-sm small mb-0",
              thead(tr(
                th("Fecha"), th(cls:="text-center","Animo"), th(cls:="text-center","Energia"),
                th(cls:="text-center","Nota partido"), th("Notas conducta")
              )),
              tbody(frag(conNotas.map { e =>
                val animoC: String = if(e.animo>=4)"success"else if(e.animo>=3)"warning"else"danger"
                val energC: String = if(e.energia>=4)"success"else if(e.energia>=3)"warning"else"danger"
                val notaStr: String = e.notaPartido match { case Some(n) => "%.1f".format(n); case None => "--" }
                val notasStr: String = if(e.notas.length>80) e.notas.take(80)+"..." else e.notas
                tr(
                  td(cls:="text-muted", e.fecha.drop(5)),
                  td(cls:="text-center", span(cls:=s"badge bg-$animoC", animoEmoji(e.animo)+" "+e.animo.toString)),
                  td(cls:="text-center", span(cls:=s"badge bg-$energC", energiaEmoji(e.energia)+" "+e.energia.toString)),
                  td(cls:="text-center fw-bold", notaStr),
                  td(cls:="text-muted small", notasStr)
                )
              }: _*))
            )
          )
        )
      ) else div(),

      script(src:="https://cdn.jsdelivr.net/npm/chart.js"),
      {
        val jsEmoc: String =
          "var ctxE=document.getElementById('chartEmocional');" +
            "if(ctxE){new Chart(ctxE,{type:'line'," +
            "data:{labels:" + fechas + ",datasets:[" +
            "{label:'Animo',data:" + animos + ",borderColor:'#0dcaf0',backgroundColor:'rgba(13,202,240,0.1)',borderWidth:2,tension:0.4,pointRadius:3,fill:true}," +
            "{label:'Energia',data:" + energias + ",borderColor:'#ffc107',backgroundColor:'rgba(255,193,7,0.05)',borderWidth:2,tension:0.4,pointRadius:3}," +
            "{label:'Nota partido',data:" + notasPartido + ",borderColor:'#28a745',borderWidth:2,tension:0.4,pointRadius:5,spanGaps:true,yAxisID:'y1'}]}," +
            "options:{responsive:true,maintainAspectRatio:false," +
            "scales:{y:{min:0,max:5,ticks:{color:'#aaa',stepSize:1},grid:{color:'#333'}}," +
            "y1:{position:'right',min:0,max:10,ticks:{color:'#28a745'},grid:{display:false}}," +
            "x:{ticks:{color:'#888'},grid:{display:false}}}," +
            "plugins:{legend:{labels:{color:'#fff'}},tooltip:{mode:'index',intersect:false}}}}});"
        script(raw(jsEmoc))
      }
      )
      )
      )
      renderHtml(pageContent)
    }
  }



  // == FASE 6.5: MONEYBALL =====================================================
  @cask.get("/moneyball")
  def moneyballPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getMoneyballData()

    val xtScore: Double      = d("xtScore").asInstanceOf[Double]
    val xtCorr: Double       = d("xtCorr").asInstanceOf[Double]
    val xpMedia: Double      = d("xpMedia").asInstanceOf[Double]
    val xpTotal: Double      = d("xpTotal").asInstanceOf[Double]
    val clutchRating: Int    = d("clutchRating").asInstanceOf[Int]
    val spvScore: Int        = d("spvScore").asInstanceOf[Int]
    val spvMedia: Double     = d("spvMedia").asInstanceOf[Double]
    val spv1v1Pct: Int       = d("spv1v1Pct").asInstanceOf[Int]
    val spvAerPct: Int       = d("spvAerPct").asInstanceOf[Int]
    val spvNorPct: Int       = d("spvNorPct").asInstanceOf[Int]
    val bpMedia: Double      = d("bpMedia").asInstanceOf[Double]
    val bpEfic: Double       = d("bpEfic").asInstanceOf[Double]
    val bpConDatos: Int      = d("bpConDatos").asInstanceOf[Int]
    val roiCorrCal: Double   = d("roiCorrCalidad").asInstanceOf[Double]
    val roiCorrAte: Double   = d("roiCorrAtencion").asInstanceOf[Double]
    val roiCorrCar: Double   = d("roiCorrCarga").asInstanceOf[Double]
    val roiPartidos: Int     = d("roiPartidos").asInstanceOf[Int]
    val roiSesiones: Double  = d("roiSesiones").asInstanceOf[Double]
    val analisisIA: String   = d("analisisIA").asInstanceOf[String]

    // Analisis de goles encajados
    val g = DatabaseManager.getGoalsAnalysis()
    val gTotal: Int         = g("total").asInstanceOf[Int]
    val gEvitables: Int     = g("evitables").asInstanceOf[Int]
    val gInevitables: Int   = g("inevitables").asInstanceOf[Int]
    val gDudosos: Int       = g("dudosos").asInstanceOf[Int]
    val gNotaReal: Double   = g("notaReal").asInstanceOf[Double]
    val gNotaAdj: Double    = g("notaAjustada").asInstanceOf[Double]
    val gPorOrigen: Map[String,Int]   = g("porOrigen").asInstanceOf[Map[String,Int]]
    val gPorSit: Map[String,Int]      = g("porSituacion").asInstanceOf[Map[String,Int]]
    val gPorResp: Map[String,Int]     = g("porResponsabilidad").asInstanceOf[Map[String,Int]]
    val gRows: List[Map[String,String]] = g("rows").asInstanceOf[List[Map[String,String]]]
    val gNotaRealStr: String  = f"$gNotaReal%.1f"
    val gNotaAdjStr: String   = f"$gNotaAdj%.1f"
    val gDelta: Double        = gNotaAdj - gNotaReal
    val gDeltaStr: String     = (if(gDelta >= 0) "+" else "") + f"$gDelta%.1f"
    val gDeltaColor: String   = if (gDelta >= 0.3) "success" else if (gDelta >= 0) "warning" else "danger"

    val xtSerie: List[Double]  = d("xtSerie").asInstanceOf[List[Double]]
    val xpSerie: List[Double]  = d("xpSerie").asInstanceOf[List[Double]]
    val spvSerie: List[Double] = d("spvSerie").asInstanceOf[List[Double]]
    val bpSerie: List[Int]     = d("bpSerie").asInstanceOf[List[Int]]
    val labels: List[String]   = d("labels").asInstanceOf[List[String]]

    // Strings para UI
    val xtScoreStr: String    = f"$xtScore%.2f"
    val xtCorrStr: String     = f"$xtCorr%.2f"
    val xtCorrColor: String   = if (xtCorr >= 0.4) "success" else if (xtCorr >= 0.2) "warning" else "secondary"
    val xpMediaStr: String    = f"$xpMedia%.2f"
    val xpTotalStr: String    = f"$xpTotal%.1f"
    val clutchColor: String   = if (clutchRating >= 70) "success" else if (clutchRating >= 45) "warning" else "danger"
    val spvMediaStr: String   = f"$spvMedia%.1f"
    val bpMediaStr: String    = f"$bpMedia%.1f"
    val bpEficStr: String     = f"${bpEfic * 100}%.0f"
    val roiCalStr: String     = f"$roiCorrCal%.2f"
    val roiAteStr: String     = f"$roiCorrAte%.2f"
    val roiCarStr: String     = f"$roiCorrCar%.2f"
    val roiCalColor: String   = if (roiCorrCal >= 0.4) "success" else if (roiCorrCal >= 0.2) "warning" else "secondary"
    val roiAteColor: String   = if (roiCorrAte >= 0.4) "success" else if (roiCorrAte >= 0.2) "warning" else "secondary"
    val roiCarColor: String   = if (roiCorrCar <= -0.3) "danger" else if (roiCorrCar >= 0.3) "warning" else "secondary"
    val roiSesStr: String     = f"$roiSesiones%.1f"

    def extractIA(tag: String): String = {
      val idx = analisisIA.indexOf(tag + ":")
      if (idx == -1) "" else {
        val start = idx + tag.length + 1
        val nexts = Seq("PATRON:", "VENTAJA:", "CONSEJO:").filter(_ != tag + ":").flatMap { t =>
          val i = analisisIA.indexOf(t, start); if (i > 0) Some(i) else None
        }
        val end = if (nexts.nonEmpty) nexts.min else analisisIA.length
        analisisIA.substring(start, end).trim
      }
    }
    val iaPatron: String  = extractIA("PATRON")
    val iaVentaja: String = extractIA("VENTAJA")
    val iaConsejo: String = extractIA("CONSEJO")

    // JSON para graficos
    val labelsJson: String = labels.map(l => """ + l + """).mkString("[", ",", "]")
    val xtJson: String     = xtSerie.map(v => f"$v%.2f").mkString("[", ",", "]")
    val xpJson: String     = xpSerie.map(v => f"$v%.2f").mkString("[", ",", "]")
    val spvJson: String    = spvSerie.map(v => f"$v%.1f").mkString("[", ",", "]")
    val bpJson: String     = bpSerie.map(_.toString).mkString("[", ",", "]")

    renderHtml(basePage("history",
      div(cls:="row justify-content-center",
        div(cls:="col-md-11 col-12",

          div(cls:="d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls:="text-warning mb-0", "MONEYBALL | Deep Influence Analytics"),
              span(cls:="badge bg-dark border border-warning text-warning", "FASE 6.5")
            ),
            a(href:="/dashboard", cls:="btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
          ),

          // ── SCORES PRINCIPALES ─────────────────────────────────────────────
          div(cls:="row g-3 mb-3",
            frag(Seq(
              ("xT_GK", "Distribucion", xtScoreStr, "pts/partido", "info",
                "Amenaza generada con el pie", xtCorrStr, "corr. nota"),
              ("xPoints", "Clutch Factor", xpMediaStr, "pts/partido", "warning",
                "Valor ponderado de paradas", clutchRating.toString + "/100", "clutch rating"),
              ("SPV", "Sweeper Keeper", spvMediaStr, "pts/partido", "primary",
                "Shot Prevention Value", spvScore.toString + "/100", "score"),
              ("Bypass", "Rate", bpMediaStr, "lineas/partido", "success",
                "Lineas superadas en salida", bpEficStr + "%", "eficiencia")
            ).map { case (titulo, sub, valor, unidad, c, desc, extra, extraLabel) =>
              div(cls:="col-md-3 col-6",
                div(cls:=s"card bg-dark border-$c h-100 shadow",
                  div(cls:="card-body p-3",
                    div(cls:="d-flex justify-content-between align-items-start",
                      div(
                        div(cls:=s"text-$c fw-bold", titulo),
                        div(cls:="xx-small text-muted", sub)
                      ),
                      div(cls:=s"badge bg-$c bg-opacity-25 text-$c xx-small", desc)
                    ),
                    div(cls:=s"display-6 fw-black text-$c mt-2", valor),
                    div(cls:="xx-small text-muted", unidad),
                    div(cls:="mt-2 pt-2 border-top border-secondary",
                      span(cls:=s"text-$c fw-bold small", extra),
                      span(cls:="xx-small text-muted ms-1", extraLabel)
                    )
                  )
                )
              )
            }: _*)
          ),

          // ── SPV BREAKDOWN ──────────────────────────────────────────────────
          div(cls:="row g-3 mb-3",
            div(cls:="col-md-4",
              div(cls:="card bg-dark border-primary shadow h-100",
                div(cls:="card-header text-primary fw-bold small", "SPV | Desglose por tipo de parada"),
                div(cls:="card-body p-3",
                  frag(Seq(
                    ("Paradas 1 vs 1", spv1v1Pct, "danger", "Alto riesgo x1.5"),
                    ("Paradas aereas", spvAerPct, "info",   "Dominio espacio x1.2"),
                    ("Paradas normales", spvNorPct, "secondary", "Estandar x1.0")
                  ).map { case (lbl, pct, c, nota2) =>
                    div(cls:="mb-3",
                      div(cls:="d-flex justify-content-between xx-small mb-1",
                        span(cls:="text-white", lbl),
                        span(cls:=s"text-$c fw-bold", pct.toString + "%")
                      ),
                      div(cls:="progress", style:="height:8px;",
                        div(cls:=s"progress-bar bg-$c", style:=s"width:$pct%;")
                      ),
                      div(cls:="xx-small text-muted mt-1", nota2)
                    )
                  }: _*)
                )
              )
            ),

            // ── ROI ENTRENAMIENTO ──────────────────────────────────────────
            div(cls:="col-md-4",
              div(cls:="card bg-dark border-success shadow h-100",
                div(cls:="card-header text-success fw-bold small", "ROI Entrenamiento | Correlacion con rendimiento"),
                div(cls:="card-body p-3",
                  div(cls:="xx-small text-muted mb-3",
                    s"Basado en $roiPartidos partidos | Media $roiSesStr sesiones/semana"
                  ),
                  frag(Seq(
                    ("Calidad sesion", roiCalStr, roiCalColor, "Efecto calidad tecnica"),
                    ("Atencion/foco", roiAteStr, roiAteColor, "Efecto concentracion"),
                    ("Carga (RPE)",   roiCarStr, roiCarColor, "Efecto fatiga acumulada")
                  ).map { case (lbl, corr, c, desc) =>
                    val corrNum: Double = corr.toDouble
                    val corrPct: Int    = math.min(100, math.max(0, ((corrNum + 1.0) / 2.0 * 100).toInt))
                    val corrPctStr: String = corrPct.toString
                    div(cls:="mb-3",
                      div(cls:="d-flex justify-content-between xx-small mb-1",
                        span(cls:="text-white", lbl),
                        span(cls:=s"text-$c fw-bold", "r=" + corr)
                      ),
                      div(cls:="progress", style:="height:8px;",
                        div(cls:=s"progress-bar bg-$c", style:=s"width:$corrPctStr%;")
                      ),
                      div(cls:="xx-small text-muted mt-1", desc)
                    )
                  }: _*)
                )
              )
            ),

            // ── BYPASS RATE ────────────────────────────────────────────────
            div(cls:="col-md-4",
              div(cls:="card bg-dark border-success shadow h-100",
                div(cls:="card-header text-success fw-bold small", "Bypass Rate | Lineas superadas en salida"),
                div(cls:="card-body p-3",
                  if (bpConDatos > 0) frag(
                    div(cls:="row g-2 mb-3",
                      div(cls:="col-6",
                        div(cls:="text-center",
                          div(cls:="display-6 fw-black text-success", bpMediaStr),
                          div(cls:="xx-small text-muted", "lineas/partido")
                        )
                      ),
                      div(cls:="col-6",
                        div(cls:="text-center",
                          div(cls:="display-6 fw-black text-info", bpEficStr + "%"),
                          div(cls:="xx-small text-muted", "eficiencia")
                        )
                      )
                    ),
                    div(cls:="xx-small text-muted mt-2",
                      "Registrado en " + bpConDatos.toString + " partidos. " +
                        "Anota las lineas superadas al registrar cada partido en el Match Center.")
                  ) else frag(
                    div(cls:="text-center py-3",
                      div(style:="font-size:36px;", "?"),
                      div(cls:="text-muted small mt-2", "Sin datos de Bypass Rate"),
                      div(cls:="xx-small text-secondary mt-1",
                        "Activa el campo en Match Center al registrar partidos")
                    )
                  )
                )
              )
            )
          ),

          // ── ANALISIS IA ────────────────────────────────────────────────────
          if (iaPatron.nonEmpty) div(cls:="card bg-dark border-warning shadow mb-3",
            div(cls:="card-header text-warning fw-bold small", "Analisis de Scout IA | Moneyball"),
            div(cls:="card-body p-3",
              div(cls:="row g-3",
                frag(Seq(
                  ("PATRON", iaPatron, "info"),
                  ("VENTAJA", iaVentaja, "success"),
                  ("CONSEJO", iaConsejo, "warning")
                ).filter(_._2.nonEmpty).map { case (titulo, texto, c) =>
                  div(cls:="col-md-4",
                    div(cls:="p-3 rounded h-100",
                      style:=s"border-left:3px solid ${if(c=="info")"#0dcaf0"else if(c=="success")"#28a745"else"#ffc107"};background:rgba(255,255,255,0.03);",
                      div(cls:=s"text-$c fw-bold xx-small mb-2", titulo),
                      div(cls:="text-white small", texto)
                    )
                  )
                }: _*)
              )
            )
          ) else div(),

          // ── GRAFICOS ───────────────────────────────────────────────────────
          div(cls:="row g-3 mb-3",
            div(cls:="col-md-6",
              div(cls:="card bg-dark border-secondary shadow h-100",
                div(cls:="card-header text-white fw-bold small", "xT_GK & xPoints por partido"),
                div(cls:="card-body p-2",
                  tag("canvas")(id:="chartXT", style:="max-height:200px;")
                )
              )
            ),
            div(cls:="col-md-6",
              div(cls:="card bg-dark border-secondary shadow h-100",
                div(cls:="card-header text-white fw-bold small", "SPV & Bypass Rate por partido"),
                div(cls:="card-body p-2",
                  tag("canvas")(id:="chartSPV", style:="max-height:200px;")
                )
              )
            )
          ),

          // ── ANALISIS DE GOLES ENCAJADOS ───────────────────────────────
          div(cls:="card bg-dark border-danger shadow mb-3",
            div(cls:="card-header d-flex justify-content-between align-items-center",
              span(cls:="text-danger fw-bold small", "PSxG | Responsabilidad en goles encajados"),
              if (gTotal > 0) span(cls:="badge bg-danger", gTotal.toString + " goles registrados")
              else span(cls:="badge bg-secondary", "Sin datos aun")
            ),
            div(cls:="card-body p-3",
              if (gTotal == 0) div(cls:="text-center text-muted py-3 small",
                div(style:="font-size:32px;", "⚽"),
                div(cls:="mt-2", "Registra el contexto de cada gol desde el Match Center"),
                div(cls:="xx-small mt-1 text-secondary",
                  "Al guardar un partido, usa la seccion 'Analisis de Goles Encajados' para clasificar cada gol")
              ) else frag(
                // KPIs principales
                div(cls:="row g-3 mb-3",
                  div(cls:="col-4 text-center",
                    div(cls:="text-danger fw-bold", style:="font-size:2rem;", gEvitables.toString),
                    div(cls:="xx-small text-muted", "Evitables"),
                    div(cls:="xx-small text-secondary", "Resp. Alta/Media + parable")
                  ),
                  div(cls:="col-4 text-center",
                    div(cls:="text-warning fw-bold", style:="font-size:2rem;", gDudosos.toString),
                    div(cls:="xx-small text-muted", "Dudosos"),
                    div(cls:="xx-small text-secondary", "Situaciones ambiguas")
                  ),
                  div(cls:="col-4 text-center",
                    div(cls:="text-success fw-bold", style:="font-size:2rem;", gInevitables.toString),
                    div(cls:="xx-small text-muted", "Inevitables"),
                    div(cls:="xx-small text-secondary", "Sin responsabilidad")
                  )
                ),
                // Nota ajustada
                div(cls:="p-2 rounded mb-3",
                  style:="background:rgba(255,255,255,0.04); border:1px solid rgba(255,255,255,0.1);",
                  div(cls:="d-flex justify-content-around text-center",
                    div(
                      div(cls:="text-muted xx-small", "Nota media real"),
                      div(cls:="text-white fw-bold", style:="font-size:1.4rem;", gNotaRealStr)
                    ),
                    div(cls:="text-muted d-flex align-items-center", "→"),
                    div(
                      div(cls:="text-muted xx-small", "Nota ajustada (sin errores ajenos)"),
                      div(cls:=s"text-$gDeltaColor fw-bold", style:="font-size:1.4rem;",
                        gNotaAdjStr + " (" + gDeltaStr + ")")
                    )
                  ),
                  div(cls:="xx-small text-secondary text-center mt-1",
                    "Cada gol inevitable suma +0.5 a la nota ajustada (rendimiento real de Hector)")
                ),
                // Desglose por origen y situacion
                div(cls:="row g-2 mb-3",
                  div(cls:="col-md-6",
                    div(cls:="xx-small text-muted fw-bold mb-2", "POR ORIGEN"),
                    frag(gPorOrigen.toList.sortBy(-_._2).map { case (origen, cnt) =>
                      val pct: Int = if (gTotal > 0) cnt * 100 / gTotal else 0
                      val pctStr: String = pct.toString
                      div(cls:="mb-2",
                        div(cls:="d-flex justify-content-between xx-small mb-1",
                          span(cls:="text-white", origen),
                          span(cls:="text-warning", cnt.toString)
                        ),
                        div(cls:="progress", style:="height:6px;",
                          div(cls:="progress-bar bg-warning", style:=s"width:$pctStr%;")
                        )
                      )
                    }: _*)
                  ),
                  div(cls:="col-md-6",
                    div(cls:="xx-small text-muted fw-bold mb-2", "POR SITUACION"),
                    frag(gPorSit.toList.sortBy(-_._2).map { case (sit, cnt) =>
                      val pct: Int = if (gTotal > 0) cnt * 100 / gTotal else 0
                      val pctStr: String = pct.toString
                      div(cls:="mb-2",
                        div(cls:="d-flex justify-content-between xx-small mb-1",
                          span(cls:="text-white", sit),
                          span(cls:="text-info", cnt.toString)
                        ),
                        div(cls:="progress", style:="height:6px;",
                          div(cls:="progress-bar bg-info", style:=s"width:$pctStr%;")
                        )
                      )
                    }: _*)
                  )
                ),
                // Ultimos goles registrados
                if (gRows.nonEmpty) div(
                  div(cls:="xx-small text-muted fw-bold mb-2", "ULTIMOS GOLES REGISTRADOS"),
                  div(style:="max-height:200px; overflow-y:auto;",
                    frag(gRows.take(10).map { r =>
                      val respColor: String = r("responsabilidad") match {
                        case "Alta" => "danger"
                        case "Media" => "warning"
                        case _ => "success"
                      }
                      div(cls:="d-flex align-items-center gap-2 py-1",
                        style:="border-bottom:1px solid rgba(255,255,255,0.05);",
                        span(cls:="text-muted xx-small", r("fecha").take(5)),
                        span(cls:="xx-small text-white", r("rival")),
                        span(cls:="badge bg-secondary xx-small", "m." + r("minuto")),
                        span(cls:="xx-small text-muted", r("situacion")),
                        span(cls:=s"badge bg-$respColor xx-small ms-auto", r("responsabilidad"))
                      )
                    }: _*)
                  )
                ) else div()
              )
            )
          ),

          script(src:="https://cdn.jsdelivr.net/npm/chart.js"),
          {
            val js: String =
              "var ctxXT=document.getElementById('chartXT');" +
                "if(ctxXT){new Chart(ctxXT,{type:'line'," +
                "data:{labels:" + labelsJson + ",datasets:[" +
                "{label:'xT_GK',data:" + xtJson + ",borderColor:'#0dcaf0',backgroundColor:'rgba(13,202,240,0.1)',borderWidth:2,tension:0.3,yAxisID:'y'}," +
                "{label:'xPoints',data:" + xpJson + ",borderColor:'#ffc107',backgroundColor:'rgba(255,193,7,0.05)',borderWidth:2,tension:0.3,yAxisID:'y'}]}," +
                "options:{responsive:true,maintainAspectRatio:false," +
                "scales:{y:{ticks:{color:'#aaa'},grid:{color:'#333'}},x:{ticks:{color:'#888',maxTicksLimit:8},grid:{display:false}}}," +
                "plugins:{legend:{labels:{color:'#fff',font:{size:10}}}}}});}" +
                "var ctxSPV=document.getElementById('chartSPV');" +
                "if(ctxSPV){new Chart(ctxSPV,{type:'bar'," +
                "data:{labels:" + labelsJson + ",datasets:[" +
                "{label:'SPV',data:" + spvJson + ",backgroundColor:'rgba(13,110,253,0.6)',borderColor:'#0d6efd',borderWidth:1,yAxisID:'y'}," +
                "{label:'Bypass',data:" + bpJson + ",type:'line',borderColor:'#28a745',borderWidth:2,pointRadius:3,tension:0.3,yAxisID:'y1'}]}," +
                "options:{responsive:true,maintainAspectRatio:false," +
                "scales:{y:{ticks:{color:'#aaa'},grid:{color:'#333'}},y1:{position:'right',ticks:{color:'#28a745'},grid:{display:false}},x:{ticks:{color:'#888',maxTicksLimit:8},grid:{display:false}}}," +
                "plugins:{legend:{labels:{color:'#fff',font:{size:10}}}}}});}"
            script(raw(js))
          }
        )
      )
    ))
  }

  // == COGNITIVE RESET RATE ====================================================
  @cask.get("/cognitive-reset")
  def cognitiveResetPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getCognitiveResetData()

    val n: Int               = d("n").asInstanceOf[Int]
    val resetScore: Int      = d("resetScore").asInstanceOf[Int]
    val clasificacion: String = d("clasificacion").asInstanceOf[String]
    val clasColor: String    = d("clasificacionColor").asInstanceOf[String]
    val rebounds: Int        = d("rebounds").asInstanceOf[Int]
    val positivos: Int       = d("positivos").asInstanceOf[Int]
    val negativos: Int       = d("negativos").asInstanceOf[Int]
    val avgNotaError: Double = d("avgNotaError").asInstanceOf[Double]
    val avgNotaSig: Double   = d("avgNotaSig").asInstanceOf[Double]
    val avgDelta: Double     = d("avgDelta").asInstanceOf[Double]
    val mediaGeneral: Double = d("mediaGeneral").asInstanceOf[Double]
    val tablaRows: List[Map[String,String]] = d("tablaRows").asInstanceOf[List[Map[String,String]]]
    val fechasSerie: List[String]     = d("fechasSerie").asInstanceOf[List[String]]
    val notaErrorSerie: List[Double]  = d("notaErrorSerie").asInstanceOf[List[Double]]
    val notaSigSerie: List[Double]    = d("notaSigSerie").asInstanceOf[List[Double]]

    val avgNotaErrorStr = f"$avgNotaError%.1f"
    val avgNotaSigStr   = f"$avgNotaSig%.1f"
    val avgDeltaStr     = (if (avgDelta >= 0) "+" else "") + f"$avgDelta%.2f"
    val avgDeltaColor   = if (avgDelta >= 0.2) "success" else if (avgDelta >= -0.2) "warning" else "danger"
    val mediaGeneralStr = f"$mediaGeneral%.1f"
    val estabPct        = if (n > 0) ((rebounds - positivos).toDouble / n * 100).toInt else 0
    val positivosPct    = if (n > 0) (positivos.toDouble / n * 100).toInt else 0
    val negativosPct    = if (n > 0) (negativos.toDouble / n * 100).toInt else 0

    val labelsJson    = fechasSerie.map(l => "\"" + l + "\"").mkString("[", ",", "]")
    val errorJson     = notaErrorSerie.map(v => f"$v%.1f").mkString("[", ",", "]")
    val sigJson       = notaSigSerie.map(v => f"$v%.1f").mkString("[", ",", "]")
    val mediaJson     = fechasSerie.map(_ => f"$mediaGeneral%.1f").mkString("[", ",", "]")

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          // Header
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-danger mb-0", "COGNITIVE RESET RATE"),
              span(cls := "badge bg-dark border border-danger text-danger", "FASE 8 — Early Access")
            ),
            div(cls := "d-flex gap-2",
              a(href := "/moneyball", cls := "btn btn-outline-warning btn-sm fw-bold", "Moneyball"),
              a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
            )
          ),

          // Alerta si no hay datos
          if (n == 0) div(cls := "alert alert-secondary",
            "Sin datos suficientes. Registra partidos con goles y clasifica su responsabilidad en el Match Center."
          ) else frag(),

          // KPI principal
          div(cls := "row g-3 mb-3",
            div(cls := "col-md-4",
              div(cls := s"card bg-dark border-$clasColor shadow text-center h-100",
                div(cls := "card-body p-4",
                  div(cls := s"display-1 fw-black text-$clasColor", resetScore.toString),
                  div(cls := "text-white fw-bold fs-5 mt-1", "Reset Score"),
                  div(cls := s"badge bg-$clasColor mt-2 fs-6", clasificacion),
                  div(cls := "xx-small text-muted mt-3",
                    s"Basado en $n partidos con gol evitable"
                  )
                )
              )
            ),
            div(cls := "col-md-8",
              div(cls := "row g-3 h-100",
                // Nota en partido con error
                div(cls := "col-6",
                  div(cls := "card bg-dark border-secondary shadow h-100",
                    div(cls := "card-body p-3",
                      div(cls := "text-muted small fw-bold", "NOTA TRAS ERROR"),
                      div(cls := "display-5 fw-black text-warning mt-1", avgNotaErrorStr),
                      div(cls := "xx-small text-muted", "media en partido con gol evitable")
                    )
                  )
                ),
                // Nota partido siguiente
                div(cls := "col-6",
                  div(cls := s"card bg-dark border-$avgDeltaColor shadow h-100",
                    div(cls := "card-body p-3",
                      div(cls := "text-muted small fw-bold", "NOTA PARTIDO SIGUIENTE"),
                      div(cls := s"display-5 fw-black text-$avgDeltaColor mt-1", avgNotaSigStr),
                      div(cls := s"badge bg-$avgDeltaColor bg-opacity-25 text-$avgDeltaColor mt-1",
                        s"$avgDeltaStr vs partido con error")
                    )
                  )
                ),
                // Media general
                div(cls := "col-6",
                  div(cls := "card bg-dark border-secondary shadow h-100",
                    div(cls := "card-body p-3",
                      div(cls := "text-muted small fw-bold", "MEDIA GLOBAL"),
                      div(cls := "display-5 fw-black text-info mt-1", mediaGeneralStr),
                      div(cls := "xx-small text-muted", "todos los partidos")
                    )
                  )
                ),
                // Distribucion de respuestas
                div(cls := "col-6",
                  div(cls := "card bg-dark border-secondary shadow h-100",
                    div(cls := "card-body p-3",
                      div(cls := "text-muted small fw-bold mb-2", "RESPUESTA POST-ERROR"),
                      div(cls := "d-flex justify-content-between xx-small mb-1",
                        span(cls := "text-success", s"Rebote $positivosPct%"),
                        span(cls := "text-warning", s"Estable $estabPct%"),
                        span(cls := "text-danger", s"Impacto $negativosPct%")
                      ),
                      div(cls := "progress", style := "height:10px;",
                        div(cls := "progress-bar bg-success", style := s"width:${positivosPct}%"),
                        div(cls := "progress-bar bg-warning",  style := s"width:${estabPct}%"),
                        div(cls := "progress-bar bg-danger",   style := s"width:${negativosPct}%")
                      )
                    )
                  )
                )
              )
            )
          ),

          // Explicacion de la metrica
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "Metodologia: "),
              "Detecta cada partido donde Hector encajo al menos un gol catalogado como ",
              span(cls := "text-warning fw-bold", "Evitable"),
              " y compara su nota con la del siguiente partido. ",
              span(cls := "text-success fw-bold", "Rebote"),
              s": nota siguiente > +0.4. ",
              span(cls := "text-warning fw-bold", "Estable"),
              s": variacion <= 0.2. ",
              span(cls := "text-danger fw-bold", "Impacto"),
              s": caida > 0.5. Score = % de veces que recupero o mantuvo nivel."
            )
          ),

          // Grafico de lineas
          if (n > 0) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "Evolucion: Nota con error vs Nota siguiente"),
            div(cls := "card-body p-3",
              div(style := "height:240px;",
                tags2.canvas(id := "resetChart")
              )
            )
          ) else frag(),

          // Tabla detalle
          if (n > 0) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", s"Detalle de $n episodios"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "FECHA"),
                    th(cls := "xx-small text-muted", "RIVAL"),
                    th(cls := "xx-small text-muted text-center", "NOTA"),
                    th(cls := "xx-small text-muted text-center", "GOL EV."),
                    th(cls := "xx-small text-muted", "SIGUIENTE"),
                    th(cls := "xx-small text-muted text-center", "NOTA SIG."),
                    th(cls := "xx-small text-muted text-center", "DELTA"),
                    th(cls := "xx-small text-muted text-center", "RESULTADO")
                  )),
                  tbody(
                    frag(tablaRows.map { r =>
                      val resCls = r("resultado") match {
                        case "REBOTE"  => "success"
                        case "IMPACTO" => "danger"
                        case _         => "warning"
                      }
                      tr(
                        td(cls := "xx-small text-muted", r("fecha")),
                        td(cls := "xx-small", r("rival")),
                        td(cls := "xx-small text-center text-warning fw-bold", r("notaError")),
                        td(cls := "xx-small text-center text-danger", r("nEvitables")),
                        td(cls := "xx-small text-muted", r("rivalSig")),
                        td(cls := "xx-small text-center fw-bold", r("notaSig")),
                        td(cls := s"xx-small text-center fw-bold text-$resCls", r("delta")),
                        td(cls := "text-center",
                          span(cls := s"badge bg-$resCls bg-opacity-25 text-$resCls xx-small",
                            r("resultado"))
                        )
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag(),

          script(raw(s"""
            (function() {
              if ($n === 0) return;
              var ctx = document.getElementById('resetChart');
              if (!ctx) return;
              new Chart(ctx.getContext('2d'), {
                type: 'line',
                data: {
                  labels: $labelsJson,
                  datasets: [
                    {
                      label: 'Nota con error',
                      data: $errorJson,
                      borderColor: 'rgba(255,193,7,0.9)',
                      backgroundColor: 'rgba(255,193,7,0.1)',
                      tension: 0.3,
                      pointRadius: 5,
                      borderDash: [5,3]
                    },
                    {
                      label: 'Nota siguiente',
                      data: $sigJson,
                      borderColor: 'rgba(40,167,69,0.9)',
                      backgroundColor: 'rgba(40,167,69,0.1)',
                      tension: 0.3,
                      pointRadius: 5
                    },
                    {
                      label: 'Media global',
                      data: $mediaJson,
                      borderColor: 'rgba(13,202,240,0.4)',
                      borderDash: [2,4],
                      pointRadius: 0,
                      fill: false
                    }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                  scales: {
                    x: { ticks: { color: '#888', font: { size: 10 } }, grid: { color: '#333' } },
                    y: { min: 0, max: 10, ticks: { color: '#888', stepSize: 1 }, grid: { color: '#333' } }
                  }
                }
              });
            })();
          """))
        )
      )
    ))
  }

  // == DIGITAL TWIN ============================================================
  @cask.get("/digital-twin")
  def digitalTwinPage(request: cask.Request, hPadre: Double = 0.0, hMadre: Double = 0.0) = withAuth(request) {
    val usePadre: Double = if (hPadre > 0) hPadre else 180.0
    val useMadre: Double = if (hMadre > 0) hMadre else 168.0
    val d = DatabaseManager.getDigitalTwinData(usePadre, useMadre)
    val bioInsights: String = DatabaseManager.getOracleInsights()

    // Pre-computar todo con tipos explicitos
    val edadAnios: Int         = d("edadAnios").asInstanceOf[Int]
    val alturaActual: Double   = d("alturaActual").asInstanceOf[Double]
    val pesoActual: Double     = d("pesoActual").asInstanceOf[Double]
    val alturaProy: Double     = d("alturaProyectada").asInstanceOf[Double]
    val alturaMin: Double      = d("alturaMin").asInstanceOf[Double]
    val alturaMax: Double      = d("alturaMax").asInstanceOf[Double]
    val enverActual: Double    = d("envergaduraActual").asInstanceOf[Double]
    val enverAdulta: Double    = d("envergaduraAdulta").asInstanceOf[Double]
    val alcanceActual: Double  = d("alcanceActual").asInstanceOf[Double]
    val alcanceAdulto: Double  = d("alcanceAdulto").asInstanceOf[Double]
    val cobActual: Double      = d("coberturaActual").asInstanceOf[Double]
    val cobAdulta: Double      = d("coberturaAdulta").asInstanceOf[Double]
    val pctAltura: Int         = d("pctAltura").asInstanceOf[Int]
    val phvVel: Double         = d("phvVelocidad").asInstanceOf[Double]
    val fasePhv: String        = d("fasePhv").asInstanceOf[String]
    val notaActual: Double     = d("notaActual").asInstanceOf[Double]
    val notaProy: Double       = d("notaProyectada").asInstanceOf[Double]
    val analisisIA: String     = d("analisisIA").asInstanceOf[String]
    val advertenciaFecha: Boolean = d("advertenciaFecha").asInstanceOf[Boolean]
    val midParentStr: String   = d("midParent").asInstanceOf[Double].toInt.toString
    val hPadreFmt: String      = f"$usePadre%.0f"
    val hMadreFmt: String      = f"$useMadre%.0f"

    val alturaProyStr: String  = alturaProy.toInt.toString
    val alturaMinStr: String   = alturaMin.toInt.toString
    val alturaMaxStr: String   = alturaMax.toInt.toString
    val enverAdultaStr: String = enverAdulta.toInt.toString
    val alcanceAdultoStr: String = alcanceAdulto.toInt.toString
    val cobActualStr: String   = f"$cobActual%.1f"
    val cobAdultaStr: String   = f"$cobAdulta%.1f"
    val cobActualPct: Int      = cobActual.toInt
    val cobAdultaPct: Int      = cobAdulta.toInt
    val alturaActualStr: String = alturaActual.toInt.toString
    val enverActualStr: String = enverActual.toInt.toString
    val alcanceActualStr: String = alcanceActual.toInt.toString
    val notaActualStr: String  = notaActual.toInt.toString
    val notaPrStr: String      = notaProy.toInt.toString
    val phvStr: String         = if (phvVel > 0) f"$phvVel%.1f cm/anio" else "Sin datos suficientes"

    val phvColor: String = fasePhv match {
      case "PICO ACTIVO" => "warning"
      case "PRE-PICO"    => "info"
      case _             => "success"
    }
    val notaDiff: String = {
      val d2 = notaProy - notaActual
      if (d2 > 0) "+" + d2.toInt.toString else d2.toInt.toString
    }

    // Parsear bloques IA
    def extractIA(tag: String): String = {
      val idx = analisisIA.indexOf(tag + ":")
      if (idx == -1) ""
      else {
        val start = idx + tag.length + 1
        val nexts = Seq("BIOTIPO:", "VENTAJA:", "RIESGO:", "PROYECCION:").filter(_ != tag + ":").flatMap { t =>
          val i = analisisIA.indexOf(t, start); if (i > 0) Some(i) else None
        }
        val end = if (nexts.nonEmpty) nexts.min else analisisIA.length
        analisisIA.substring(start, end).trim
      }
    }
    val iaBiotipo: String    = extractIA("BIOTIPO")
    val iaVentaja: String    = extractIA("VENTAJA")
    val iaRiesgo: String     = extractIA("RIESGO")
    val iaProyeccion: String = extractIA("PROYECCION")

    // Datos para graficos (JSON)
    val growthRows = d("growthRows").asInstanceOf[List[(String, Double, Double, Double)]]
    val curvaProy  = d("curvaProyeccion").asInstanceOf[List[(Int, Double)]]
    val notaTemps  = d("notaTemps").asInstanceOf[List[(String, Double)]]

    val histFechas: String  = growthRows.map(r => "\"" + r._1 + "\"").mkString("[", ",", "]")
    val histAltura: String  = growthRows.map(_._2.toString).mkString("[", ",", "]")
    val proyEdades: String  = curvaProy.map(r => "\"" + r._1.toString + "a\"").mkString("[", ",", "]")
    val proyAlturas: String = curvaProy.map(_._2.formatted("%.1f")).mkString("[", ",", "]")
    val tempLabels: String  = notaTemps.map(r => "\"" + r._1 + "\"").mkString("[", ",", "]")
    val tempNotas: String   = notaTemps.map(_._2.toString).mkString("[", ",", "]")

    // Comparativa porteros elite
    val referencia189: String = "189"
    val referenciaEnv: String = "200"
    val referenciaAlc: String = "251"

    renderHtml(basePage("bio",
      div(cls:="row justify-content-center",
        div(cls:="col-md-10 col-12",

          div(cls:="d-flex justify-content-between align-items-center mb-3",
            h2(cls:="text-warning mb-0", "HECTOR 2035 | Digital Twin"),
            a(href:="/bio", cls:="btn btn-outline-secondary btn-sm fw-bold", "Bio")
          ),

          // Formulario alturas padres
          div(cls:="card bg-dark border-secondary shadow mb-3",
            div(cls:="card-header text-secondary fw-bold small", "Calibrar proyeccion (alturas parentales)"),
            div(cls:="card-body p-2",
              div(cls:="row g-2 align-items-end",
                div(cls:="col-4",
                  tag("label")(cls:="xx-small text-muted", "Padre (cm)"),
                  tag("input")(id:="hPadreInput", tpe:="number", cls:="form-control form-control-sm bg-dark text-white border-secondary",
                    value:=hPadreFmt, style:="max-width:100px;")
                ),
                div(cls:="col-4",
                  tag("label")(cls:="xx-small text-muted", "Madre (cm)"),
                  tag("input")(id:="hMadreInput", tpe:="number", cls:="form-control form-control-sm bg-dark text-white border-secondary",
                    value:=hMadreFmt, style:="max-width:100px;")
                ),
                div(cls:="col-4",
                  tag("button")(onclick:="recalcular()", cls:="btn btn-warning btn-sm fw-bold", "Recalcular")
                )
              )
            )
          ),

          // HERO: Carta del Twin
          div(cls:="card shadow mb-3",
            style:="background: linear-gradient(135deg, #0a0a1a 0%, #1a1a3e 50%, #0d2b0d 100%); border: 2px solid #ffc107;",
            div(cls:="card-body p-3",
              div(cls:="row align-items-center",
                div(cls:="col-md-4 text-center border-end border-secondary",
                  div(style:="font-size:14px; color:#888; letter-spacing:3px;", "PROYECCION ADULTA"),
                  div(style:="font-size:72px; font-weight:900; color:#ffc107; line-height:1;",
                    alturaProyStr),
                  div(style:="font-size:18px; color:#aaa;", "cm"),
                  div(cls:="mt-2",
                    span(cls:="badge bg-dark border border-warning text-warning me-1", s"Min $alturaMinStr"),
                    span(cls:="badge bg-dark border border-warning text-warning", s"Max $alturaMaxStr")
                  ),
                  div(cls:="mt-2 small text-muted", s"$pctAltura percentil porteros elite"),
                  div(cls:="mt-1 xx-small text-secondary", "Midparent genetico: " + midParentStr + " cm")
                ),
                div(cls:="col-md-8",
                  div(cls:="row g-2",
                    frag(Seq(
                      ("Envergadura adulta", enverAdultaStr + " cm", enverActualStr + " cm", "success"),
                      ("Alcance de parada", alcanceAdultoStr + " cm", alcanceActualStr + " cm", "info"),
                      ("Cobertura porteria", cobAdultaStr + "%", cobActualStr + "%", "warning"),
                      ("Nota proyectada (18a)", notaPrStr, notaActualStr, "primary"),
                      ("Fase PHV", fasePhv, phvStr, phvColor),
                      ("Diferencial nota", notaDiff + " pts", "tendencia", "secondary")
                    ).map { case (lbl, vProy, vActual, c) =>
                      div(cls:="col-6",
                        div(cls:=s"card bg-dark border-$c h-100",
                          div(cls:="card-body p-2",
                            div(cls:="xx-small text-muted", lbl),
                            div(cls:=s"fw-bold text-$c", vProy),
                            div(cls:="xx-small text-secondary", "Ahora: " + vActual)
                          )
                        )
                      )
                    }: _*)
                  )
                )
              )
            )
          ),

          // Barras de comparativa con elite
          div(cls:="card bg-dark border-secondary shadow mb-3",
            div(cls:="card-header text-white fw-bold small", "Comparativa vs Porteros de Elite (Media Profesional)"),
            div(cls:="card-body p-3",
              div(cls:="row g-3",
                frag(Seq(
                  ("Altura", alturaActualStr + " cm actual", alturaProyStr + " cm adulto", referencia189 + " cm pro", alturaActual.toInt, alturaProy.toInt, 189, 210),
                  ("Envergadura", enverActualStr + " cm actual", enverAdultaStr + " cm adulto", referenciaEnv + " cm pro", enverActual.toInt, enverAdulta.toInt, 200, 220),
                  ("Alcance", alcanceActualStr + " cm actual", alcanceAdultoStr + " cm adulto", referenciaAlc + " cm pro", alcanceActual.toInt, alcanceAdulto.toInt, 251, 280)
                ).map { case (lbl, vActL, vPrL, vRefL, vAct, vPr, vRef, vMax) =>
                  val pctAct: Int = math.min(100, (vAct * 100 / vMax))
                  val pctPr: Int  = math.min(100, (vPr  * 100 / vMax))
                  val pctRef: Int = math.min(100, (vRef * 100 / vMax))
                  val pctActStr: String = pctAct.toString
                  val pctPrStr: String  = pctPr.toString
                  val pctRefStr: String = pctRef.toString
                  div(cls:="col-md-4",
                    div(cls:="fw-bold text-white small mb-2", lbl),
                    div(cls:="xx-small text-info mb-1", vActL),
                    div(cls:="progress mb-1", style:="height:8px;",
                      div(cls:="progress-bar bg-info", style:=s"width:$pctActStr%;")),
                    div(cls:="xx-small text-warning mb-1", vPrL),
                    div(cls:="progress mb-1", style:="height:8px;",
                      div(cls:="progress-bar bg-warning", style:=s"width:$pctPrStr%;")),
                    div(cls:="xx-small text-success mb-1", vRefL),
                    div(cls:="progress", style:="height:8px;",
                      div(cls:="progress-bar bg-success", style:=s"width:$pctRefStr%;"))
                  )
                }: _*)
              )
            )
          ),

          // Analisis IA
          if (iaBiotipo.nonEmpty || iaVentaja.nonEmpty) {
            div(cls:="card bg-dark border-warning shadow mb-3",
              div(cls:="card-header text-warning fw-bold small", "Informe de Ojeador IA | Proyeccion 2035"),
              div(cls:="card-body p-3",
                div(cls:="row g-3",
                  frag(Seq(
                    ("BIOTIPO", iaBiotipo, "info", "Perfil fisico"),
                    ("VENTAJA", iaVentaja, "success", "Punto fuerte"),
                    ("RIESGO", iaRiesgo, "danger", "Area de mejora"),
                    ("PROYECCION", iaProyeccion, "warning", "Alcance potencial")
                  ).filter(_._2.nonEmpty).map { case (titulo, texto, c, sub) =>
                    div(cls:="col-md-6",
                      div(cls:=s"p-3 rounded h-100",
                        style:=s"border-left: 3px solid ${if(c=="info")"#0dcaf0"else if(c=="success")"#28a745"else if(c=="danger")"#dc3545"else"#ffc107"}; background: rgba(255,255,255,0.03);",
                        div(cls:=s"text-$c fw-bold xx-small mb-1", titulo + " | " + sub),
                        div(cls:="text-white small", texto)
                      )
                    )
                  }: _*)
                )
              )
            )
          } else div(),

          // Inteligencia Deportiva (ACWR + Biotipo)
          div(cls:="card bg-dark border-info shadow mb-3",
            div(cls:="card-header bg-info text-dark fw-bold d-flex justify-content-between align-items-center",
              span("INTELIGENCIA DEPORTIVA"),
              span(cls:="badge bg-dark text-info", s"Edad: $edadAnios anos")
            ),
            div(cls:="card-body p-3", raw(bioInsights))
          ),

          // Graficos
          div(cls:="row g-3 mb-3",
            div(cls:="col-md-6",
              div(cls:="card bg-dark border-secondary shadow h-100",
                div(cls:="card-header text-white fw-bold small", "Curva de Crecimiento | Historico + Proyeccion"),
                div(cls:="card-body p-2",
                  tag("canvas")(id:="chartCrecimiento", style:="max-height:220px;")
                )
              )
            ),
            div(cls:="col-md-6",
              div(cls:="card bg-dark border-secondary shadow h-100",
                div(cls:="card-header text-white fw-bold small", "Evolucion de Rendimiento por Temporada"),
                div(cls:="card-body p-2",
                  tag("canvas")(id:="chartRendimiento", style:="max-height:220px;")
                )
              )
            )
          ),

          script(src:="https://cdn.jsdelivr.net/npm/chart.js"),
          {
            val jsCode: String =
              "function recalcular(){" +
                "var p=document.getElementById('hPadreInput').value;" +
                "var m=document.getElementById('hMadreInput').value;" +
                "window.location.href='/digital-twin?hPadre='+p+'&hMadre='+m;}" +
                "var ctxC=document.getElementById('chartCrecimiento');" +
                "if(ctxC){new Chart(ctxC,{type:'line'," +
                "data:{labels:" + histFechas + ",datasets:[" +
                "{label:'Historico real',data:" + histAltura + ",borderColor:'#0dcaf0'," +
                "backgroundColor:'rgba(13,202,240,0.1)',borderWidth:2,pointRadius:4,fill:true}," +
                "{label:'Proyeccion',data:" + proyAlturas + ",borderColor:'#ffc107'," +
                "borderDash:[6,3],borderWidth:2,pointRadius:3}]}," +
                "options:{responsive:true,maintainAspectRatio:false," +
                "scales:{x:{ticks:{color:'#888'},grid:{color:'#333'}}," +
                "y:{ticks:{color:'#aaa'},grid:{color:'#333'}}}," +
                "plugins:{legend:{labels:{color:'#fff'}}}}});}" +
                "var ctxR=document.getElementById('chartRendimiento');" +
                "if(ctxR){new Chart(ctxR,{type:'bar'," +
                "data:{labels:" + tempLabels + ",datasets:[{label:'Nota media',data:" + tempNotas + "," +
                "backgroundColor:'rgba(255,193,7,0.7)',borderColor:'#ffc107',borderWidth:1}]}," +
                "options:{responsive:true,maintainAspectRatio:false," +
                "scales:{y:{min:0,max:100,ticks:{color:'#aaa'},grid:{color:'#333'}}," +
                "x:{ticks:{color:'#888'},grid:{display:false}}}," +
                "plugins:{legend:{labels:{color:'#fff'}}}}});}"
            script(raw(jsCode))
          }
        )
      )
    ))
  }

  initialize()
}