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





  // == BIO-BANDING =============================================================
  @cask.get("/bio-banding")
  def bioBandingPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getBioBandingData()

    val edadAnios: Int        = d("edadAnios").asInstanceOf[Int]
    val alturaActual: Double  = d("alturaActual").asInstanceOf[Double]
    val pesoActual: Double    = d("pesoActual").asInstanceOf[Double]
    val velCrecimiento: Double= d("velCrecimiento").asInstanceOf[Double]
    val phvActivo: Boolean    = d("phvActivo").asInstanceOf[Boolean]
    val phvVelocidad: Double  = d("phvVelocidad").asInstanceOf[Double]
    val faseBio: String       = d("faseBio").asInstanceOf[String]
    val faseBioColor: String  = d("faseBioColor").asInstanceOf[String]
    val factorAjuste: Double  = d("factorAjuste").asInstanceOf[Double]
    val avgNota: Double       = d("avgNota").asInstanceOf[Double]
    val avgNotaAdj: Double    = d("avgNotaAdj").asInstanceOf[Double]
    val deltaMedia: Double    = d("deltaMedia").asInstanceOf[Double]
    val percentilAltura: String = d("percentilAltura").asInstanceOf[String]
    val n: Int                = d("n").asInstanceOf[Int]
    val matchRows: List[Map[String,Any]] = d("matchRows").asInstanceOf[List[Map[String,Any]]]
    val fechasSerie: List[String]  = d("fechasSerie").asInstanceOf[List[String]]
    val notaSerie: List[Double]    = d("notaSerie").asInstanceOf[List[Double]]
    val notaAdjSerie: List[Double] = d("notaAdjSerie").asInstanceOf[List[Double]]

    val factorStr    = f"×${factorAjuste}%.2f"
    val avgNotaStr   = f"$avgNota%.1f"
    val avgNotaAdjStr= f"$avgNotaAdj%.1f"
    val deltaStr     = (if (deltaMedia >= 0) "+" else "") + f"$deltaMedia%.2f"
    val altStr       = if (alturaActual > 0) f"$alturaActual%.0f cm" else "Sin datos"
    val pesoStr      = if (pesoActual > 0) f"$pesoActual%.1f kg" else "Sin datos"
    val velStr       = if (velCrecimiento > 0) f"+$velCrecimiento%.1f cm" else "—"

    val labelsJson   = fechasSerie.map(l => """ + l + """).mkString("[",",","]")
    val notaJson     = notaSerie.map(v => f"$v%.1f").mkString("[",",","]")
    val notaAdjJson  = notaAdjSerie.map(v => f"$v%.1f").mkString("[",",","]")

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-info mb-0", "BIO-BANDING"),
              span(cls := "badge bg-dark border border-info text-info", "FASE 5 — Madurez Biológica")
            ),
            div(cls := "d-flex gap-2",
              a(href := "/digital-twin", cls := "btn btn-outline-primary btn-sm fw-bold", "Digital Twin"),
              a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
            )
          ),

          div(cls := "card bg-dark border-secondary mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "¿Qué es el Bio-Banding? "),
              "El cuerpo consume energía en crecer. Durante el Pico de Velocidad de Altura (PHV), ",
              "una nota de 6.5 vale más que un 8 fuera del pico. Este módulo ajusta las métricas de Héctor ",
              "por su fase biológica real para una valoración justa de su rendimiento."
            )
          ),

          // Fase biológica actual
          div(cls := "row g-3 mb-3",
            div(cls := "col-md-4",
              div(cls := s"card bg-dark border-$faseBioColor shadow text-center h-100",
                div(cls := "card-body p-3",
                  div(cls := "xx-small text-muted fw-bold mb-1", "FASE BIOLÓGICA"),
                  div(cls := s"fs-4 fw-black text-$faseBioColor mt-1", faseBio),
                  if (phvActivo) div(cls := "badge bg-danger mt-2", s"PHV: ${f"$phvVelocidad%.1f"} cm/año")
                  else div(cls := s"badge bg-$faseBioColor bg-opacity-25 text-$faseBioColor mt-2",
                    s"$edadAnios años"),
                  div(cls := "xx-small text-muted mt-2", percentilAltura)
                )
              )
            ),
            div(cls := "col-md-4",
              div(cls := "card bg-dark border-secondary shadow text-center h-100",
                div(cls := "card-body p-3",
                  div(cls := "xx-small text-muted fw-bold", "FACTOR DE AJUSTE"),
                  div(cls := s"display-4 fw-black text-$faseBioColor", factorStr),
                  div(cls := "xx-small text-muted", "multiplicador sobre la nota real"),
                  div(cls := "xx-small text-muted mt-1",
                    altStr + " | " + pesoStr),
                  div(cls := "xx-small text-muted", s"Últ. crecimiento: $velStr")
                )
              )
            ),
            div(cls := "col-md-4",
              div(cls := "row g-2 h-100",
                div(cls := "col-12",
                  div(cls := "card bg-dark border-secondary shadow",
                    div(cls := "card-body p-3 text-center",
                      div(cls := "xx-small text-muted fw-bold", "NOTA REAL MEDIA"),
                      div(cls := "fs-3 fw-black text-warning", avgNotaStr),
                      div(cls := "xx-small text-muted", s"últimos $n partidos")
                    )
                  )
                ),
                div(cls := "col-12",
                  div(cls := s"card bg-dark border-$faseBioColor shadow",
                    div(cls := "card-body p-3 text-center",
                      div(cls := "xx-small text-muted fw-bold", "NOTA BIO-AJUSTADA"),
                      div(cls := s"fs-3 fw-black text-$faseBioColor", avgNotaAdjStr),
                      div(cls := s"badge bg-$faseBioColor bg-opacity-25 text-$faseBioColor xx-small",
                        s"$deltaStr sobre la nota real")
                    )
                  )
                )
              )
            )
          ),

          // Grafico nota real vs bio-ajustada
          if (n > 0) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              "Nota real vs Nota bio-ajustada"),
            div(cls := "card-body p-3",
              div(style := "height:220px;",
                tag("canvas")(id := "bioChart")
              )
            )
          ) else frag(),

          // Tabla de partidos con ajuste
          if (matchRows.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              s"Últimos $n partidos con ajuste biológico"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "FECHA"),
                    th(cls := "xx-small text-muted", "RIVAL"),
                    th(cls := "xx-small text-muted text-center", "NOTA REAL"),
                    th(cls := "xx-small text-muted text-center", "BIO-AJUSTADA"),
                    th(cls := "xx-small text-muted text-center", "GC"),
                    th(cls := "xx-small text-muted text-center", "PAR.")
                  )),
                  tbody(
                    frag(matchRows.map { r =>
                      val nota    = r("nota").asInstanceOf[Double]
                      val notaAdj = r("notaAdj").asInstanceOf[Double]
                      val diff    = notaAdj - nota
                      val adjCls  = if (diff >= 0.5) "success" else if (diff >= 0.1) "info" else "secondary"
                      tr(
                        td(cls := "xx-small text-muted", r("fecha").asInstanceOf[String]),
                        td(cls := "xx-small", r("rival").asInstanceOf[String]),
                        td(cls := "xx-small text-center text-warning fw-bold", f"$nota%.1f"),
                        td(cls := s"xx-small text-center fw-bold text-$adjCls", f"$notaAdj%.1f"),
                        td(cls := "xx-small text-center text-danger", r("gc").asInstanceOf[Int].toString),
                        td(cls := "xx-small text-center text-info", r("paradas").asInstanceOf[Int].toString)
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag(),

          script(raw(s"""
            (function() {
              var ctx = document.getElementById('bioChart');
              if (!ctx || ${notaSerie.size} === 0) return;
              new Chart(ctx.getContext('2d'), {
                type: 'line',
                data: {
                  labels: $labelsJson,
                  datasets: [
                    {
                      label: 'Nota real',
                      data: $notaJson,
                      borderColor: 'rgba(255,193,7,0.8)',
                      backgroundColor: 'rgba(255,193,7,0.05)',
                      tension: 0.3, pointRadius: 4,
                      borderDash: [5,3]
                    },
                    {
                      label: 'Bio-ajustada ($factorStr)',
                      data: $notaAdjJson,
                      borderColor: 'rgba(13,202,240,0.9)',
                      backgroundColor: 'rgba(13,202,240,0.08)',
                      tension: 0.3, pointRadius: 5,
                      fill: true
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

  // == DEVELOPMENT PATHWAY MATCHER =============================================
  @cask.get("/pathway")
  def pathwayPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getPathwayData()

    val perArq: List[Map[String,Any]] = d("perArq").asInstanceOf[List[Map[String,Any]]]
    val mejorArq: Map[String,Any]     = d("mejorArq").asInstanceOf[Map[String,Any]]
    val peorArq: Map[String,Any]      = d("peorArq").asInstanceOf[Map[String,Any]]
    val recomendacion: String         = d("recomendacion").asInstanceOf[String]
    val areasMejora: String           = d("areasMejora").asInstanceOf[String]
    val totalPartidos: Int            = d("totalPartidos").asInstanceOf[Int]

    val mejorLabel = mejorArq("arquetipo").asInstanceOf[String]
    val mejorColor = mejorArq("color").asInstanceOf[String]
    val peorLabel  = peorArq("arquetipo").asInstanceOf[String]
    val peorColor  = peorArq("color").asInstanceOf[String]

    val arquetipoEmoji: Map[String,String] = Map(
      "RAPIDO" -> "💨", "AEREO" -> "✈️", "COLECTIVO" -> "🤝",
      "DIRECTO" -> "🎯", "EQUILIBRADO" -> "⚖️"
    )

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-warning mb-0", "DEVELOPMENT PATHWAY"),
              span(cls := "badge bg-dark border border-warning text-warning",
                "FASE 8 — Entorno Óptimo de Crecimiento")
            ),
            a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
          ),

          div(cls := "card bg-dark border-secondary mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "¿Qué mide? "),
              "Cruza el perfil de ataque de cada rival (Striker Clustering) con el rendimiento real de Héctor. ",
              "Detecta contra qué estilo de equipo crece más como portero y dónde necesita más trabajo."
            )
          ),

          if (totalPartidos < 5) div(cls := "alert alert-secondary",
            s"Datos insuficientes ($totalPartidos partidos). Registra más partidos con análisis de goles."
          ) else frag(),

          // Mejor y peor entorno
          div(cls := "row g-3 mb-3",
            div(cls := "col-md-6",
              div(cls := s"card bg-dark border-$mejorColor shadow h-100",
                div(cls := "card-header text-white fw-bold small", "🌱 ENTORNO DE MÁXIMO CRECIMIENTO"),
                div(cls := "card-body p-3",
                  div(cls := "d-flex align-items-center gap-3 mb-2",
                    div(style := "font-size:2.5rem;",
                      raw(arquetipoEmoji.getOrElse(mejorLabel, "&#x26BD;"))),
                    div(
                      div(cls := s"fs-4 fw-black text-$mejorColor", mejorLabel),
                      div(cls := "xx-small text-muted",
                        s"Nota media: ${f"${mejorArq("nota").asInstanceOf[Double]}%.1f"} | " +
                          s"${mejorArq("n").asInstanceOf[Int]} partidos")
                    )
                  ),
                  div(cls := "small text-muted", recomendacion)
                )
              )
            ),
            div(cls := "col-md-6",
              div(cls := s"card bg-dark border-$peorColor shadow h-100",
                div(cls := "card-header text-white fw-bold small", "⚠️ ÁREA DE MEJORA PRIORITARIA"),
                div(cls := "card-body p-3",
                  div(cls := "d-flex align-items-center gap-3 mb-2",
                    div(style := "font-size:2.5rem;",
                      raw(arquetipoEmoji.getOrElse(peorLabel, "&#x26BD;"))),
                    div(
                      div(cls := s"fs-4 fw-black text-$peorColor", peorLabel),
                      div(cls := "xx-small text-muted",
                        s"Nota media: ${f"${peorArq("nota").asInstanceOf[Double]}%.1f"} | " +
                          s"${peorArq("n").asInstanceOf[Int]} partidos")
                    )
                  ),
                  div(cls := "small text-muted", areasMejora)
                )
              )
            )
          ),

          // Tabla por arquetipo
          if (perArq.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "Rendimiento por estilo de rival"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "ARQUETIPO"),
                    th(cls := "xx-small text-muted text-center", "PJ"),
                    th(cls := "xx-small text-muted text-center", "NOTA MEDIA"),
                    th(cls := "xx-small text-muted text-center", "PARADAS/PJ"),
                    th(cls := "xx-small text-muted text-center", "PIE/PJ"),
                    th(cls := "xx-small text-muted text-center", "BYPASS/PJ")
                  )),
                  tbody(
                    frag(perArq.sortBy(r => -r("nota").asInstanceOf[Double]).map { a =>
                      val arq   = a("arquetipo").asInstanceOf[String]
                      val color = a("color").asInstanceOf[String]
                      val nota  = a("nota").asInstanceOf[Double]
                      val notaCls = if (nota >= 7) "success" else if (nota >= 5) "warning" else "danger"
                      tr(
                        td(
                          span(cls := s"badge bg-$color bg-opacity-25 text-$color xx-small", arq),
                          if (arq == mejorLabel) span(cls := "ms-1 text-success", "★") else frag()
                        ),
                        td(cls := "xx-small text-center text-muted", a("n").asInstanceOf[Int].toString),
                        td(cls := s"xx-small text-center fw-black text-$notaCls", f"$nota%.1f"),
                        td(cls := "xx-small text-center text-info",
                          f"${a("paradas").asInstanceOf[Double]}%.1f"),
                        td(cls := "xx-small text-center text-secondary",
                          f"${a("pie").asInstanceOf[Double]}%.1f"),
                        td(cls := "xx-small text-center text-success",
                          f"${a("lineas").asInstanceOf[Double]}%.1f")
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag()
        )
      )
    ))
  }

  // == DOJO v2: MODO ENTRENADOR ================================================
  @cask.get("/dojo/entrenador")
  def dojoEntrenadorPage(request: cask.Request) = withAuth(request) {
    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-10 col-12",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-warning mb-0", "DOJO — MODO ENTRENADOR"),
              span(cls := "badge bg-dark border border-warning text-warning",
                "FASE 5 — Situaciones Personalizadas")
            ),
            a(href := "/dojo", cls := "btn btn-outline-info btn-sm fw-bold", "Dojo Normal")
          ),

          div(cls := "card bg-dark border-warning shadow mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "Modo Entrenador. "),
              "Crea situaciones tácticas personalizadas para Héctor. ",
              "Define la situación, tres opciones y marca cuál es la correcta. ",
              span(cls := "text-warning fw-bold", "Las sesiones se guardan para reutilizarlas.")
            )
          ),

          // Formulario de creación de situación
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "➕ Nueva situación"),
            div(cls := "card-body p-3",
              div(cls := "mb-3",
                label(cls := "small text-white fw-bold", "SITUACIÓN"),
                input(tpe := "text", id := "sit", cls := "form-control bg-dark text-white",
                  placeholder := "Ej: 1v1 con el delantero en velocidad")
              ),
              div(cls := "mb-3",
                label(cls := "small text-white fw-bold", "CONTEXTO"),
                input(tpe := "text", id := "ctx", cls := "form-control bg-dark text-white",
                  placeholder := "Ej: El delantero viene por el lado izquierdo a 10m")
              ),
              div(cls := "mb-3",
                label(cls := "small text-white fw-bold", "EMOJI"),
                input(tpe := "text", id := "emoji", cls := "form-control bg-dark text-white",
                  value := "⚽", style := "width:80px;")
              ),
              frag(Seq(1,2,3).map { i =>
                div(cls := "mb-3 p-2 border border-secondary rounded",
                  label(cls := s"small fw-bold text-${if(i==1) "success" else "muted"}",
                    s"OPCIÓN $i ${if(i==1) "← CORRECTA" else ""}"),
                  input(tpe := "text", id := s"op$i",
                    cls := "form-control bg-dark text-white mb-1",
                    placeholder := s"Descripción de la opción $i"),
                  input(tpe := "text", id := s"exp$i",
                    cls := "form-control bg-dark text-white form-control-sm",
                    placeholder := "Explicación táctica..."),
                  input(tpe := "number", id := s"pts$i",
                    cls := "form-control bg-dark text-white form-control-sm mt-1",
                    value := (if(i==1) "10" else "0"), style := "width:100px;",
                    attr("placeholder") := "Puntos")
                )
              }: _*),
              button(tpe := "button", cls := "btn btn-warning fw-bold w-100",
                onclick := "addSituacion()", "AÑADIR SITUACIÓN")
            )
          ),

          // Lista de situaciones creadas
          div(id := "listaCustom", cls := "mb-3"),

          // Botón iniciar sesión custom
          div(id := "startBtnDiv", cls := "d-none d-grid mb-3",
            button(tpe := "button", cls := "btn btn-success fw-bold",
              onclick := "startCustomSession()", "▶ INICIAR SESIÓN CON ESTAS SITUACIONES")
          ),

          // Panel de juego (reutiliza misma estructura que Dojo normal)
          div(id := "dojoPanel", cls := "d-none",
            div(cls := "row g-3 mb-3",
              div(cls := "col-6 text-center",
                div(cls := "card bg-dark border-warning shadow",
                  div(cls := "card-body p-2",
                    div(cls := "xx-small text-muted", "SCORE"),
                    div(id := "dojoScore2", cls := "display-5 fw-black text-warning", "0")
                  )
                )
              ),
              div(cls := "col-6 text-center",
                div(cls := "card bg-dark border-info shadow",
                  div(cls := "card-body p-2",
                    div(cls := "xx-small text-muted", "PREGUNTA"),
                    div(id := "dojoQ2", cls := "display-5 fw-black text-info", "1")
                  )
                )
              )
            ),
            div(id := "sitCard2", cls := "card bg-dark border-warning shadow mb-3",
              div(cls := "card-header text-warning fw-bold small", "SITUACIÓN"),
              div(cls := "card-body p-3 text-center",
                div(id := "dojoEmoji2", cls := "mb-2", style := "font-size:3rem;", "⚽"),
                div(id := "dojoSit2", cls := "fs-5 fw-bold text-white mb-1", ""),
                div(id := "dojoCtx2", cls := "small text-muted", "")
              )
            ),
            div(id := "opcionesDiv2", cls := "row g-2 mb-3"),
            div(id := "feedbackDiv2", cls := "d-none",
              div(id := "feedbackCard2", cls := "card shadow mb-3",
                div(cls := "card-body p-3 text-center",
                  div(id := "feedbackEmoji2"),
                  div(id := "feedbackTxt2", cls := "fw-bold fs-5"),
                  div(id := "feedbackExp2", cls := "small text-muted mt-1")
                )
              ),
              div(cls := "d-grid",
                button(tpe := "button", cls := "btn btn-warning fw-bold",
                  onclick := "next2()", "SIGUIENTE →"))
            ),
            div(id := "final2", cls := "d-none text-center",
              div(cls := "card bg-dark border-warning shadow p-4",
                div(style := "font-size:3rem;", "🏆"),
                h4(cls := "text-warning", "SESIÓN COMPLETADA"),
                div(id := "finalScore2", cls := "display-3 fw-black text-warning"),
                div(cls := "small text-muted", "puntos de decisión"),
                div(cls := "d-grid mt-3",
                  button(tpe := "button", cls := "btn btn-warning fw-bold",
                    onclick := "resetCustom()", "REPETIR"))
              )
            )
          ),

          script(raw("""
            var customSituaciones = [];
            var idx2 = 0, score2 = 0;

            function addSituacion() {
              var sit = document.getElementById('sit').value.trim();
              var ctx = document.getElementById('ctx').value.trim();
              var emoji = document.getElementById('emoji').value.trim() || '⚽';
              if (!sit) { alert('Escribe la situación'); return; }
              var ops = [];
              for (var i = 1; i <= 3; i++) {
                var txt = document.getElementById('op'+i).value.trim();
                var exp = document.getElementById('exp'+i).value.trim();
                var pts = parseInt(document.getElementById('pts'+i).value) || 0;
                if (!txt) { alert('Rellena la opción ' + i); return; }
                ops.push({ txt: txt, exp: exp || '—', pts: pts, ok: i === 1 });
              }
              customSituaciones.push({ sit: sit, ctx: ctx, emoji: emoji, opciones: ops });
              renderLista();
              document.getElementById('sit').value = '';
              document.getElementById('ctx').value = '';
              document.getElementById('emoji').value = '⚽';
              for (var j = 1; j <= 3; j++) {
                document.getElementById('op'+j).value = '';
                document.getElementById('exp'+j).value = '';
                document.getElementById('pts'+j).value = j === 1 ? '10' : '0';
              }
            }

            function renderLista() {
              var div = document.getElementById('listaCustom');
              div.innerHTML = customSituaciones.map(function(s, i) {
                return '<div class="card bg-dark border-secondary mb-2 p-2 d-flex flex-row justify-content-between align-items-center">' +
                  '<span class="text-white small fw-bold">' + (i+1) + '. ' + s.emoji + ' ' + s.sit + '</span>' +
                  '<button class="btn btn-outline-danger btn-sm" onclick="removeSit('+i+')">✕</button></div>';
              }).join('');
              document.getElementById('startBtnDiv').classList.toggle('d-none', customSituaciones.length === 0);
            }

            function removeSit(i) {
              customSituaciones.splice(i, 1); renderLista();
            }

            function startCustomSession() {
              if (customSituaciones.length === 0) return;
              idx2 = 0; score2 = 0;
              document.getElementById('dojoPanel').classList.remove('d-none');
              document.getElementById('startBtnDiv').classList.add('d-none');
              loadQ2();
            }

            function shuffle(arr) {
              for (var i = arr.length - 1; i > 0; i--) {
                var j = Math.floor(Math.random() * (i + 1));
                var t = arr[i]; arr[i] = arr[j]; arr[j] = t;
              }
              return arr;
            }

            function loadQ2() {
              if (idx2 >= customSituaciones.length) { showFinal2(); return; }
              var p = customSituaciones[idx2];
              document.getElementById('dojoQ2').textContent = (idx2+1) + '/' + customSituaciones.length;
              document.getElementById('dojoEmoji2').textContent = p.emoji;
              document.getElementById('dojoSit2').textContent = p.sit;
              document.getElementById('dojoCtx2').textContent = p.ctx;
              document.getElementById('feedbackDiv2').classList.add('d-none');
              document.getElementById('sitCard2').classList.remove('d-none');
              var ops = document.getElementById('opcionesDiv2');
              ops.innerHTML = '';
              var shuffled = shuffle(p.opciones.slice());
              shuffled.forEach(function(op, i) {
                var idx = p.opciones.indexOf(op);
                var col = document.createElement('div');
                col.className = 'col-12';
                col.innerHTML = '<button class="btn btn-outline-light w-100 text-start fw-bold p-3" onclick="answer2(' + idx + ')">' + op.txt + '</button>';
                ops.appendChild(col);
              });
            }

            function answer2(opIdx) {
              var p = customSituaciones[idx2];
              var op = p.opciones[opIdx];
              score2 += op.pts;
              document.getElementById('dojoScore2').textContent = score2;
              var fc = document.getElementById('feedbackCard2');
              fc.className = 'card shadow mb-3 ' + (op.ok ? 'border-success bg-success bg-opacity-10' : 'border-danger bg-danger bg-opacity-10');
              document.getElementById('feedbackEmoji2').textContent = op.ok ? '✅' : '❌';
              document.getElementById('feedbackTxt2').textContent = op.ok ? '¡Correcto! +' + op.pts + ' pts' : 'No era esa. +' + op.pts + ' pts';
              document.getElementById('feedbackExp2').textContent = op.exp;
              document.getElementById('sitCard2').classList.add('d-none');
              document.getElementById('opcionesDiv2').innerHTML = '';
              document.getElementById('feedbackDiv2').classList.remove('d-none');
              idx2++;
            }

            function next2() { loadQ2(); }

            function showFinal2() {
              document.getElementById('feedbackDiv2').classList.add('d-none');
              document.getElementById('sitCard2').classList.add('d-none');
              document.getElementById('opcionesDiv2').innerHTML = '';
              document.getElementById('final2').classList.remove('d-none');
              document.getElementById('finalScore2').textContent = score2;
            }

            function resetCustom() {
              idx2 = 0; score2 = 0;
              document.getElementById('final2').classList.add('d-none');
              document.getElementById('dojoScore2').textContent = '0';
              loadQ2();
            }
          """))
        )
      )
    ))
  }

  // == DOJO COGNITIVO ==========================================================
  @cask.get("/dojo")
  def dojoPage(request: cask.Request) = withAuth(request) {
    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-10 col-12",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-info mb-0", "DOJO COGNITIVO"),
              span(cls := "badge bg-dark border border-info text-info", "FASE 5 — Decisiones Bajo Presión")
            ),
            a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
          ),

          // Descripcion
          div(cls := "card bg-dark border-info shadow mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "Entrena tu mente. "),
              "Cada situacion tiene una respuesta optima. Acumula puntos y mejora tu Decision Score. ",
              span(cls := "text-info fw-bold", "El portero que piensa mas rapido gana el duelo antes de que empiece.")
            )
          ),

          // Panel de juego
          div(id := "dojoPanel",
            // Score y nivel
            div(cls := "row g-3 mb-3",
              div(cls := "col-4 text-center",
                div(cls := "card bg-dark border-warning shadow",
                  div(cls := "card-body p-2",
                    div(cls := "xx-small text-muted", "DECISION SCORE"),
                    div(id := "dojoScore", cls := "display-4 fw-black text-warning", "0")
                  )
                )
              ),
              div(cls := "col-4 text-center",
                div(cls := "card bg-dark border-info shadow",
                  div(cls := "card-body p-2",
                    div(cls := "xx-small text-muted", "PREGUNTA"),
                    div(id := "dojoQ", cls := "display-4 fw-black text-info", "1/10")
                  )
                )
              ),
              div(cls := "col-4 text-center",
                div(cls := "card bg-dark border-secondary shadow",
                  div(cls := "card-body p-2",
                    div(cls := "xx-small text-muted", "RACHA"),
                    div(id := "dojoStreak", cls := "display-4 fw-black text-success", "0")
                  )
                )
              )
            ),

            // Situacion
            div(id := "situacionCard", cls := "card bg-dark border-warning shadow mb-3",
              div(cls := "card-header text-warning fw-bold small", "SITUACION"),
              div(cls := "card-body p-3 text-center",
                div(id := "dojoEmoji", cls := "mb-2", style := "font-size:3rem;", "⚽"),
                div(id := "dojoSit", cls := "fs-5 fw-bold text-white mb-1", "Cargando..."),
                div(id := "dojoCtx", cls := "small text-muted", "")
              )
            ),

            // Opciones
            div(id := "opcionesDiv", cls := "row g-2 mb-3"),

            // Feedback
            div(id := "feedbackDiv", cls := "d-none",
              div(id := "feedbackCard", cls := "card shadow mb-3",
                div(cls := "card-body p-3 text-center",
                  div(id := "feedbackEmoji", cls := "mb-1", style := "font-size:2rem;"),
                  div(id := "feedbackTxt", cls := "fw-bold fs-5"),
                  div(id := "feedbackExp", cls := "small text-muted mt-1")
                )
              ),
              div(cls := "d-grid",
                button(tpe := "button", cls := "btn btn-warning fw-bold",
                  onclick := "nextQuestion()", "SIGUIENTE SITUACION →")
              )
            ),

            // Final
            div(id := "finalDiv", cls := "d-none",
              div(cls := "card bg-dark border-warning shadow text-center",
                div(cls := "card-body p-4",
                  div(style := "font-size:3rem;", "🏆"),
                  h3(cls := "text-warning", "SESION COMPLETADA"),
                  div(id := "finalScore", cls := "display-3 fw-black text-warning", ""),
                  div(cls := "small text-muted mb-3", "puntos de decision"),
                  div(id := "finalLabel", cls := "badge fs-5 mb-3", ""),
                  div(cls := "d-grid",
                    button(tpe := "button", cls := "btn btn-warning fw-bold",
                      onclick := "resetDojo()", "JUGAR DE NUEVO")
                  )
                )
              )
            )
          ),

          script(raw("""
            var preguntas = [
              {
                sit: "1 vs 1 con el delantero",
                ctx: "El delantero viene solo, a 8 metros. El equipo no llega.",
                emoji: "🔥",
                opciones: [
                  { txt: "Salir a achicar angulo rapidamente", pts: 10, ok: true,
                    exp: "Correcto. Salir reduce el angulo de tiro y te hace grande." },
                  { txt: "Quedarte en la linea de gol", pts: 0, ok: false,
                    exp: "Mal. Quedarte le das todo el angulo al delantero." },
                  { txt: "Intentar hablar con la defensa", pts: 2, ok: false,
                    exp: "Demasiado tarde. La decision debia ser tuya." }
                ]
              },
              {
                sit: "Corner al segundo palo",
                ctx: "El balon viene centrado al segundo palo. Hay un rival entre ti y el balon.",
                emoji: "🎯",
                opciones: [
                  { txt: "Salir a por el balon gritando PORTERO", pts: 10, ok: true,
                    exp: "Correcto. La iniciativa vocal y la salida temprana son clave." },
                  { txt: "Esperar a ver si llega alguien de la defensa", pts: 2, ok: false,
                    exp: "Mal. La inaccion en el area propia genera caos." },
                  { txt: "Quedarte en el primer palo por si hay remate", pts: 4, ok: false,
                    exp: "Parcial. Cubriste un riesgo pero abandonaste el principal." }
                ]
              },
              {
                sit: "Penalty",
                ctx: "Penalty en contra. El tirador corre hacia el balon.",
                emoji: "⚡",
                opciones: [
                  { txt: "Lanzarte a un lado justo antes del chut", pts: 8, ok: true,
                    exp: "Buena decision. Comprometerte tarde reduce la lectura del rival." },
                  { txt: "Lanzarte muy pronto", pts: 3, ok: false,
                    exp: "Mal. Si te lanzas pronto el tirador cambia el lado facilmente." },
                  { txt: "Quedarte quieto en el centro", pts: 5, ok: false,
                    exp: "Aceptable solo si lees el lado. Sin lectura es pasividad." }
                ]
              },
              {
                sit: "Balon largo en profundidad",
                ctx: "El rival lanza un balon largo. Tu defensa y el delantero van a por el.",
                emoji: "💨",
                opciones: [
                  { txt: "Salir decidido a despejar antes de que llegue el rival", pts: 10, ok: true,
                    exp: "Perfecto. El portero que manda el area evita el duelo." },
                  { txt: "Quedarte en la porteria por si el rival llega primero", pts: 3, ok: false,
                    exp: "Demasiado pasivo. Perdiste la oportunidad de dominar el area." },
                  { txt: "Gritar al defensa para que despeje el", pts: 5, ok: false,
                    exp: "Aceptable si el defensa tiene ventaja, pero tu decias ser protagonista." }
                ]
              },
              {
                sit: "2 vs 1 en contraataque",
                ctx: "Dos rivales solos contra tu porteria. Solo tu puedes actuar.",
                emoji: "😰",
                opciones: [
                  { txt: "Avanzar lentamente para cerrar angulo al que tiene el balon", pts: 10, ok: true,
                    exp: "Correcto. Cortas el tiro y fuerzas el pase, que puede interceptar la defensa." },
                  { txt: "Lanzarte al suelo a por el balon", pts: 1, ok: false,
                    exp: "Error grave. Si te anticipa el pase, gol seguro." },
                  { txt: "Quedarte en la porteria", pts: 4, ok: false,
                    exp: "Parcial. Al menos cubres la porteria pero no haces nada proactivo." }
                ]
              },
              {
                sit: "Saque de puerta bajo presion",
                ctx: "Rivales presionando. Tus companeros se ofrecen en corto y en largo.",
                emoji: "👟",
                opciones: [
                  { txt: "Pase corto seguro al defensa mas cercano con espacio", pts: 8, ok: true,
                    exp: "Bien. El balon seguro construye juego desde atras." },
                  { txt: "Chut largo hacia adelante sin mirar", pts: 3, ok: false,
                    exp: "Mal. Pierdes posesion y cedes terreno." },
                  { txt: "Esperar a que los rivales se alejen antes de sacar", pts: 6, ok: false,
                    exp: "Aceptable, pero la espera da ventaja tactica al rival." }
                ]
              },
              {
                sit: "Tiro libre rasante a la escuadra",
                ctx: "Tiro libre a 20 metros. El balon se dirige a la escuadra izquierda.",
                emoji: "🧤",
                opciones: [
                  { txt: "Lanzarte con las dos manos hacia la escuadra", pts: 10, ok: true,
                    exp: "Correcto. Extension maxima con las dos manos es la tecnica optima." },
                  { txt: "Lanzarte pero con una sola mano", pts: 5, ok: false,
                    exp: "Parcial. Reduces el area de cobertura innecesariamente." },
                  { txt: "Intentar desviar con el pie", pts: 1, ok: false,
                    exp: "Incorrecto. El pie no da el control ni la extension necesaria." }
                ]
              },
              {
                sit: "Error propio en el partido anterior",
                ctx: "Cometiste un error grave el partido pasado. Hoy vuelves a jugar.",
                emoji: "🧠",
                opciones: [
                  { txt: "Concentrarte solo en el partido de hoy, error superado", pts: 10, ok: true,
                    exp: "Perfecto. El reset mental es una habilidad de elite." },
                  { txt: "Jugar con mas precaucion para no volver a fallar", pts: 4, ok: false,
                    exp: "El exceso de precaucion genera nuevos errores." },
                  { txt: "Pensar en el error para no repetirlo", pts: 2, ok: false,
                    exp: "Mal. Pensar en el error durante el partido bloquea la decision rapida." }
                ]
              },
              {
                sit: "Defensa mal colocada, rival solo",
                ctx: "Tu defensa se quedo adelantada. Un rival queda solo en offside dudoso.",
                emoji: "🚩",
                opciones: [
                  { txt: "Pedir al linier que levante el baston y seguir atento", pts: 8, ok: true,
                    exp: "Bien. Confias en el arbitro pero te preparas igual." },
                  { txt: "Protestar al arbitro en ese momento", pts: 0, ok: false,
                    exp: "Error grave. Te distraes justo cuando debes estar listo." },
                  { txt: "Salir a por el delantero asumiendo que es offside", pts: 3, ok: false,
                    exp: "Riesgo innecesario. Si el arbitro no pita, gol cantado." }
                ]
              },
              {
                sit: "Remate de cabeza a quemarropa",
                ctx: "Centro al area, remate de cabeza a 2 metros. No hay tiempo para pensar.",
                emoji: "💥",
                opciones: [
                  { txt: "Reaccion pura, tirarse al lado del balon", pts: 10, ok: true,
                    exp: "Correcto. En remates a quemarropa solo cuenta el reflejo y la posicion inicial." },
                  { txt: "Intentar leer la trayectoria antes de moverse", pts: 2, ok: false,
                    exp: "Mal. No hay tiempo. La posicion previa lo decide todo." },
                  { txt: "Saltar para achcar", pts: 5, ok: false,
                    exp: "Parcial. Util si estas bien colocado, pero el lateral es mas seguro." }
                ]
              }
            ];

            var idx = 0, score = 0, streak = 0, maxStreak = 0;

            function shuffle(arr) {
              for (var i = arr.length - 1; i > 0; i--) {
                var j = Math.floor(Math.random() * (i + 1));
                var t = arr[i]; arr[i] = arr[j]; arr[j] = t;
              }
              return arr;
            }

            preguntas = shuffle(preguntas);

            function loadQuestion() {
              if (idx >= preguntas.length) { showFinal(); return; }
              var p = preguntas[idx];
              document.getElementById('dojoQ').textContent = (idx+1) + '/' + preguntas.length;
              document.getElementById('dojoEmoji').textContent = p.emoji;
              document.getElementById('dojoSit').textContent = p.sit;
              document.getElementById('dojoCtx').textContent = p.ctx;
              document.getElementById('feedbackDiv').classList.add('d-none');
              document.getElementById('situacionCard').classList.remove('d-none');

              var ops = document.getElementById('opcionesDiv');
              ops.innerHTML = '';
              var shuffled = shuffle(p.opciones.slice());
              shuffled.forEach(function(op, i) {
                var col = document.createElement('div');
                col.className = 'col-12';
                col.innerHTML = '<button class="btn btn-outline-light w-100 text-start fw-bold p-3" onclick="answer(' + p.opciones.indexOf(op) + ')">' + op.txt + '</button>';
                ops.appendChild(col);
              });
            }

            function answer(opIdx) {
              var p = preguntas[idx];
              var op = p.opciones[opIdx];
              score += op.pts;
              if (op.ok) { streak++; if (streak > maxStreak) maxStreak = streak; }
              else streak = 0;

              document.getElementById('dojoScore').textContent = score;
              document.getElementById('dojoStreak').textContent = streak;

              var fc = document.getElementById('feedbackCard');
              fc.className = 'card shadow mb-3 ' + (op.ok ? 'border-success bg-success bg-opacity-10' : 'border-danger bg-danger bg-opacity-10');
              document.getElementById('feedbackEmoji').textContent = op.ok ? '✅' : '❌';
              document.getElementById('feedbackTxt').textContent = op.ok ? '¡Correcto! +' + op.pts + ' pts' : 'No era esa. +' + op.pts + ' pts';
              document.getElementById('feedbackExp').textContent = op.exp;
              document.getElementById('situacionCard').classList.add('d-none');
              document.getElementById('opcionesDiv').innerHTML = '';
              document.getElementById('feedbackDiv').classList.remove('d-none');
              idx++;
            }

            function nextQuestion() { loadQuestion(); }

            function showFinal() {
              document.getElementById('feedbackDiv').classList.add('d-none');
              document.getElementById('situacionCard').classList.add('d-none');
              document.getElementById('opcionesDiv').innerHTML = '';
              document.getElementById('finalDiv').classList.remove('d-none');
              document.getElementById('finalScore').textContent = score;
              var max = preguntas.length * 10;
              var pct = Math.round(score / max * 100);
              var lbl = document.getElementById('finalLabel');
              if (pct >= 85) { lbl.textContent = 'ELITE MENTAL'; lbl.className = 'badge bg-warning text-dark fs-5 mb-3'; }
              else if (pct >= 65) { lbl.textContent = 'SOLIDO'; lbl.className = 'badge bg-success fs-5 mb-3'; }
              else if (pct >= 45) { lbl.textContent = 'EN DESARROLLO'; lbl.className = 'badge bg-info fs-5 mb-3'; }
              else { lbl.textContent = 'SIGUE ENTRENANDO'; lbl.className = 'badge bg-danger fs-5 mb-3'; }
            }

            function resetDojo() {
              idx = 0; score = 0; streak = 0; maxStreak = 0;
              preguntas = shuffle(preguntas);
              document.getElementById('finalDiv').classList.add('d-none');
              document.getElementById('dojoScore').textContent = '0';
              document.getElementById('dojoStreak').textContent = '0';
              document.getElementById('situacionCard').classList.remove('d-none');
              loadQuestion();
            }

            loadQuestion();
          """))
        )
      )
    ))
  }

  // == STRIKER CLUSTERING ======================================================
  @cask.get("/striker-clustering")
  def strikerClusteringPage(request: cask.Request) = withAuth(request) {
    val clusters = DatabaseManager.getStrikerClusters()

    val nRivales = clusters.size
    val arquetipos = Map(
      "RAPIDO"     -> clusters.count(_("arquetipo") == "RAPIDO"),
      "AEREO"      -> clusters.count(_("arquetipo") == "AEREO"),
      "COLECTIVO"  -> clusters.count(_("arquetipo") == "COLECTIVO"),
      "DIRECTO"    -> clusters.count(_("arquetipo") == "DIRECTO"),
      "EQUILIBRADO"-> clusters.count(_("arquetipo") == "EQUILIBRADO")
    )
    val masComun = if (arquetipos.nonEmpty) arquetipos.maxBy(_._2)._1 else "—"

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-warning mb-0", "STRIKER CLUSTERING"),
              span(cls := "badge bg-dark border border-warning text-warning", "FASE 7 — Análisis de Rivales")
            ),
            a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
          ),

          if (nRivales == 0) div(cls := "alert alert-secondary",
            "Sin datos suficientes. Registra partidos y clasifica los goles en el Match Center."
          ) else frag(),

          // Resumen de arquetipos
          div(cls := "row g-3 mb-3",
            frag(Seq(
              ("RAPIDO",     "danger",    "1v1 y contraataques"),
              ("AEREO",      "info",      "Remates de cabeza"),
              ("COLECTIVO",  "warning",   "Jugadas en equipo 2v1"),
              ("DIRECTO",    "primary",   "Alto GC, juego directo"),
              ("EQUILIBRADO","secondary", "Perfil mixto")
            ).map { case (arq, color, desc) =>
              div(cls := "col-6 col-md",
                div(cls := s"card bg-dark border-$color shadow text-center",
                  div(cls := "card-body p-2",
                    div(cls := s"fs-2 fw-black text-$color",
                      arquetipos.getOrElse(arq, 0).toString),
                    div(cls := s"xx-small fw-bold text-$color", arq),
                    div(cls := "xx-small text-muted", desc)
                  )
                )
              )
            }: _*)
          ),

          // Alerta del arquetipo mas comun
          if (nRivales > 0) div(cls := "card bg-dark border-warning shadow mb-3",
            div(cls := "card-body p-3",
              span(cls := "text-warning fw-bold", "⚠️ Perfil más frecuente: "),
              span(cls := "text-white fw-bold fs-5", masComun),
              span(cls := "text-muted small ms-2",
                masComun match {
                  case "RAPIDO"     => "— Trabaja la salida en 1v1 y el achique de ángulo."
                  case "AEREO"      => "— Domina el área aérea. Posición y grito son clave."
                  case "COLECTIVO"  => "— Anticipa el pase en jugadas 2v1. No te lances al primer toque."
                  case "DIRECTO"    => "— Cuidado con los balones largos. Sal a por ellos."
                  case _            => "— Perfil variado. Estudia cada rival individualmente."
                }
              )
            )
          ) else frag(),

          // Tabla de rivales
          if (clusters.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", s"Directorio de $nRivales rivales"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "RIVAL"),
                    th(cls := "xx-small text-muted text-center", "PJ"),
                    th(cls := "xx-small text-muted text-center", "GC"),
                    th(cls := "xx-small text-muted text-center", "GC/PJ"),
                    th(cls := "xx-small text-muted text-center", "ARQUETIPO"),
                    th(cls := "xx-small text-muted text-center", "AMENAZA"),
                    th(cls := "xx-small text-muted text-center", "TU NOTA")
                  )),
                  tbody(
                    frag(clusters.map { c =>
                      val arqColor  = c("arquetipoColor").asInstanceOf[String]
                      val amenColor = c("amenazaColor").asInstanceOf[String]
                      val nota      = c("notaHec").asInstanceOf[Double]
                      val notaCls   = if (nota >= 7) "success" else if (nota >= 5) "warning" else "danger"
                      tr(
                        td(cls := "small fw-bold", c("rival").asInstanceOf[String]),
                        td(cls := "xx-small text-center text-muted", c("pj").asInstanceOf[Int].toString),
                        td(cls := "xx-small text-center text-danger fw-bold", c("gcTotal").asInstanceOf[Int].toString),
                        td(cls := "xx-small text-center text-warning",
                          f"${c("gcMedia").asInstanceOf[Double]}%.1f"),
                        td(cls := "text-center",
                          span(cls := s"badge bg-$arqColor bg-opacity-25 text-$arqColor xx-small",
                            c("arquetipo").asInstanceOf[String])
                        ),
                        td(cls := "text-center",
                          span(cls := s"badge bg-$amenColor bg-opacity-25 text-$amenColor xx-small",
                            c("amenaza").asInstanceOf[String])
                        ),
                        td(cls := s"xx-small text-center fw-bold text-$notaCls",
                          f"$nota%.1f")
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag()
        )
      )
    ))
  }

  // == SCANNING RATE ===========================================================
  @cask.get("/scanning-rate")
  def scanningRatePage(request: cask.Request) = withAuth(request) {
    val conn = DatabaseManager.getConnection()
    val (partidos, avgScan, avgNota, corrData) = try {
      val rs = conn.createStatement().executeQuery(
        "SELECT fecha, rival, nota, scanning_rate, goles_contra " +
          "FROM matches WHERE status='PLAYED' AND nota > 0 " +
          "ORDER BY fecha DESC LIMIT 30")
      var rows = List[(String, String, Double, Int, Int)]()
      while (rs.next()) rows = rows :+ (
        rs.getString("fecha").take(10),
        Option(rs.getString("rival")).getOrElse(""),
        rs.getDouble("nota"),
        rs.getInt("scanning_rate"),
        rs.getInt("goles_contra")
      )
      val conDatos = rows.filter(_._4 > 0)
      val avg  = if (conDatos.nonEmpty) conDatos.map(_._4.toDouble).sum / conDatos.size else 0.0
      val nota = if (conDatos.nonEmpty) conDatos.map(_._3).sum / conDatos.size else 0.0
      // Correlacion simple Pearson scan vs nota
      val n = conDatos.size.toDouble
      val corrVal = if (n >= 3) {
        val mx = conDatos.map(_._4.toDouble).sum / n
        val my = conDatos.map(_._3).sum / n
        val num = conDatos.map(r => (r._4 - mx) * (r._3 - my)).sum
        val den = math.sqrt(conDatos.map(r => math.pow(r._4 - mx, 2)).sum *
          conDatos.map(r => math.pow(r._3 - my, 2)).sum)
        if (den > 0) num / den else 0.0
      } else 0.0
      (rows, avg, nota, corrVal)
    } finally { conn.close() }

    val conDatos   = partidos.count(_._4 > 0)
    val avgScanStr = f"$avgScan%.1f"
    val corrStr    = (if (corrData >= 0) "+" else "") + f"$corrData%.2f"
    val corrColor  = if (corrData >= 0.4) "success" else if (corrData >= 0.2) "info"
    else if (corrData >= -0.1) "secondary" else "danger"
    val corrLabel  = if (corrData >= 0.4) "CORRELACIÓN FUERTE"
    else if (corrData >= 0.2) "CORRELACIÓN LEVE"
    else if (corrData >= -0.1) "SIN CORRELACIÓN"
    else "CORRELACIÓN NEGATIVA"

    val labelsJson = partidos.reverse.map(r => """ + r._1.takeRight(5) + """).mkString("[",",","]")
    val scanJson   = partidos.reverse.map(_._4.toString).mkString("[",",","]")
    val notaJson   = partidos.reverse.map(r => f"${r._3}%.1f").mkString("[",",","]")

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-info mb-0", "SCANNING RATE"),
              span(cls := "badge bg-dark border border-info text-info", "FASE 7 — Conciencia Situacional")
            ),
            a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
          ),

          div(cls := "card bg-dark border-secondary mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "¿Qué mide? "),
              "Número de escaneos de campo que Héctor realiza antes de recibir una cesión. ",
              "Un portero que escanea más ve el campo antes de tocar el balón — mejor decisión, ",
              "más velocidad de juego. Se registra manualmente en el Match Center."
            )
          ),

          if (conDatos == 0) div(cls := "alert alert-secondary",
            "Sin datos de Scanning Rate aún. Registra los escaneos en el campo 👁️ del Match Center."
          ) else frag(),

          // KPIs
          div(cls := "row g-3 mb-3",
            div(cls := "col-4 text-center",
              div(cls := "card bg-dark border-info shadow",
                div(cls := "card-body p-3",
                  div(cls := "xx-small text-muted fw-bold", "MEDIA ESCANEOS"),
                  div(cls := "display-5 fw-black text-info", avgScanStr),
                  div(cls := "xx-small text-muted", s"$conDatos partidos con datos")
                )
              )
            ),
            div(cls := "col-4 text-center",
              div(cls := s"card bg-dark border-$corrColor shadow",
                div(cls := "card-body p-3",
                  div(cls := "xx-small text-muted fw-bold", "CORRELACIÓN"),
                  div(cls := s"display-5 fw-black text-$corrColor", corrStr),
                  div(cls := s"badge bg-$corrColor bg-opacity-25 text-$corrColor xx-small", corrLabel)
                )
              )
            ),
            div(cls := "col-4 text-center",
              div(cls := "card bg-dark border-secondary shadow",
                div(cls := "card-body p-3",
                  div(cls := "xx-small text-muted fw-bold", "NOTA CON DATOS"),
                  div(cls := "display-5 fw-black text-warning", f"$avgNota%.1f"),
                  div(cls := "xx-small text-muted", "en partidos registrados")
                )
              )
            )
          ),

          // Grafico dual
          if (conDatos >= 3) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              "Escaneos vs Nota — últimos 30 partidos"),
            div(cls := "card-body p-3",
              div(style := "height:220px;",
                tag("canvas")(id := "scanChart")
              )
            )
          ) else frag(),

          // Tabla
          if (partidos.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "Registro por partido"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "FECHA"),
                    th(cls := "xx-small text-muted", "RIVAL"),
                    th(cls := "xx-small text-muted text-center", "ESCANEOS"),
                    th(cls := "xx-small text-muted text-center", "GC"),
                    th(cls := "xx-small text-muted text-center", "NOTA")
                  )),
                  tbody(
                    frag(partidos.map { case (fecha, rival, nota, scan, gc) =>
                      val notaCls = if (nota >= 7) "success" else if (nota >= 5) "warning" else "danger"
                      tr(
                        td(cls := "xx-small text-muted", fecha),
                        td(cls := "xx-small", rival),
                        td(cls := s"xx-small text-center fw-bold text-info",
                          if (scan > 0) scan.toString else "—"),
                        td(cls := "xx-small text-center text-danger", gc.toString),
                        td(cls := s"xx-small text-center fw-bold text-$notaCls", f"$nota%.1f")
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag(),

          script(raw(s"""
            (function() {
              var ctx = document.getElementById('scanChart');
              if (!ctx) return;
              new Chart(ctx.getContext('2d'), {
                data: {
                  labels: $labelsJson,
                  datasets: [
                    {
                      type: 'bar',
                      label: 'Escaneos',
                      data: $scanJson,
                      backgroundColor: 'rgba(13,202,240,0.4)',
                      borderColor: 'rgba(13,202,240,0.8)',
                      yAxisID: 'y1'
                    },
                    {
                      type: 'line',
                      label: 'Nota',
                      data: $notaJson,
                      borderColor: 'rgba(255,193,7,0.9)',
                      tension: 0.3,
                      pointRadius: 4,
                      yAxisID: 'y2'
                    }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                  scales: {
                    x:  { ticks: { color: '#888', font: { size: 10 } }, grid: { color: '#333' } },
                    y1: { position: 'left',  beginAtZero: true,
                          ticks: { color: '#0dcaf0', stepSize: 1 }, grid: { color: '#333' } },
                    y2: { position: 'right', min: 0, max: 10,
                          ticks: { color: '#ffc107', stepSize: 2 }, grid: { drawOnChartArea: false } }
                  }
                }
              });
            })();
          """))
        )
      )
    ))
  }

  // == PSxG DELTA ==============================================================
  @cask.get("/psxg-delta")
  def psxgDeltaPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getPSxGDeltaData()

    val nGoles: Int           = d("nGoles").asInstanceOf[Int]
    val xgTotal: Double       = d("xgTotal").asInstanceOf[Double]
    val psxgDelta: Double     = d("psxgDelta").asInstanceOf[Double]
    val psxgDeltaStr: String  = d("psxgDeltaStr").asInstanceOf[String]
    val psxgDeltaColor: String= d("psxgDeltaColor").asInstanceOf[String]
    val psxgLabel: String     = d("psxgLabel").asInstanceOf[String]
    val golesAltaDif: Int     = d("golesAltaDif").asInstanceOf[Int]
    val golesMediaDif: Int    = d("golesMediaDif").asInstanceOf[Int]
    val golesBajaDif: Int     = d("golesBajaDif").asInstanceOf[Int]
    val xgPorPartido: Double  = d("xgPorPartido").asInstanceOf[Double]
    val nPartidos: Int        = d("nPartidos").asInstanceOf[Int]
    val porZona: List[Map[String,Any]] = d("porZona").asInstanceOf[List[Map[String,Any]]]
    val tablaGoles: List[Map[String,String]] = d("tablaGoles").asInstanceOf[List[Map[String,String]]]

    val xgTotalStr     = f"$xgTotal%.2f"
    val xgPorPartidoStr= f"$xgPorPartido%.2f"
    val pctAltaDif     = if (nGoles > 0) (golesAltaDif * 100 / nGoles) else 0
    val pctMediaDif    = if (nGoles > 0) (golesMediaDif * 100 / nGoles) else 0
    val pctBajaDif     = if (nGoles > 0) (golesBajaDif * 100 / nGoles) else 0

    // Datos para grafico de barras por zona
    val zonaLabels = porZona.map(z => """ + z("zona").asInstanceOf[String] + """).mkString("[",",","]")
    val zonaGoles  = porZona.map(z => z("goles").asInstanceOf[Int].toString).mkString("[",",","]")
    val zonaXG     = porZona.map(z => f"${z("xg").asInstanceOf[Double]}%.2f").mkString("[",",","]")

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          // Header
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-warning mb-0", "PSxG DELTA"),
              span(cls := "badge bg-dark border border-warning text-warning",
                "FASE 8 — Post-Shot xG Analysis")
            ),
            div(cls := "d-flex gap-2",
              a(href := "/moneyball", cls := "btn btn-outline-warning btn-sm fw-bold", "Moneyball"),
              a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
            )
          ),

          if (nGoles == 0) div(cls := "alert alert-secondary",
            "Sin goles registrados con análisis. Clasifica los goles en el Match Center para ver el PSxG Delta."
          ) else frag(),

          // Explicacion breve
          div(cls := "card bg-dark border-secondary mb-3",
            div(cls := "card-body p-3 small text-muted",
              span(cls := "text-white fw-bold", "¿Qué mide? "),
              "Compara los goles reales encajados con los esperados estadisticamente (xG) segun la zona y situacion del disparo. ",
              span(cls := "text-success fw-bold", "Negativo = mejor que la estadística. "),
              span(cls := "text-danger fw-bold", "Positivo = por encima de lo esperado.")
            )
          ),

          // KPIs principales
          div(cls := "row g-3 mb-3",
            // Delta principal
            div(cls := "col-md-4",
              div(cls := s"card bg-dark border-$psxgDeltaColor shadow text-center h-100",
                div(cls := "card-body p-4",
                  div(cls := "xx-small text-muted fw-bold mb-1", "PSxG DELTA"),
                  div(cls := s"display-3 fw-black text-$psxgDeltaColor", psxgDeltaStr),
                  div(cls := s"badge bg-$psxgDeltaColor mt-2 fs-6", psxgLabel),
                  div(cls := "xx-small text-muted mt-2",
                    s"$nGoles goles reales vs ${xgTotalStr} xG esperados")
                )
              )
            ),
            // Columna de stats
            div(cls := "col-md-8",
              div(cls := "row g-3",
                div(cls := "col-6",
                  div(cls := "card bg-dark border-secondary shadow h-100",
                    div(cls := "card-body p-3",
                      div(cls := "xx-small text-muted fw-bold", "xG ACUMULADO"),
                      div(cls := "fs-3 fw-black text-info", xgTotalStr),
                      div(cls := "xx-small text-muted", s"esperado en $nPartidos partidos")
                    )
                  )
                ),
                div(cls := "col-6",
                  div(cls := "card bg-dark border-secondary shadow h-100",
                    div(cls := "card-body p-3",
                      div(cls := "xx-small text-muted fw-bold", "xG / PARTIDO"),
                      div(cls := "fs-3 fw-black text-warning", xgPorPartidoStr),
                      div(cls := "xx-small text-muted", "dificultad media de tiros")
                    )
                  )
                ),
                // Desglose por dificultad
                div(cls := "col-12",
                  div(cls := "card bg-dark border-secondary shadow",
                    div(cls := "card-body p-3",
                      div(cls := "xx-small text-muted fw-bold mb-2", "GOLES POR DIFICULTAD DEL TIRO"),
                      div(cls := "d-flex justify-content-between xx-small mb-1",
                        span(cls := "text-success", s"Difícil $pctAltaDif% ($golesAltaDif)"),
                        span(cls := "text-warning", s"Media $pctMediaDif% ($golesMediaDif)"),
                        span(cls := "text-danger",  s"Fácil $pctBajaDif% ($golesBajaDif)")
                      ),
                      div(cls := "progress", style := "height:12px;",
                        div(cls := "progress-bar bg-success", style := s"width:${pctAltaDif}%"),
                        div(cls := "progress-bar bg-warning", style := s"width:${pctMediaDif}%"),
                        div(cls := "progress-bar bg-danger",  style := s"width:${pctBajaDif}%")
                      ),
                      div(cls := "xx-small text-muted mt-1",
                        "Verde = gol de tiro difícil (inevitable). Rojo = gol de tiro fácil (evitable).")
                    )
                  )
                )
              )
            )
          ),

          // Grafico goles reales vs xG por zona
          if (porZona.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              "Goles encajados vs xG esperado por zona de portería"),
            div(cls := "card-body p-3",
              div(style := "height:220px;",
                tag("canvas")(id := "psxgChart")
              )
            )
          ) else frag(),

          // Grid de zonas de portería
          if (porZona.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "Análisis por zona"),
            div(cls := "card-body p-3",
              div(cls := "row g-2",
                frag(porZona.map { z =>
                  val zGoles = z("goles").asInstanceOf[Int]
                  val zXG    = z("xg").asInstanceOf[Double]
                  val zDelta = z("delta").asInstanceOf[Double]
                  val zLabel = z("label").asInstanceOf[String]
                  val zColor = if (zDelta <= -0.3) "success" else if (zDelta >= 0.3) "danger" else "secondary"
                  div(cls := "col-4",
                    div(cls := s"card bg-dark border-$zColor text-center p-2",
                      div(cls := "fw-bold text-white", z("zona").asInstanceOf[String]),
                      div(cls := s"fs-5 fw-black text-$zColor", zGoles.toString),
                      div(cls := "xx-small text-muted", s"xG: ${f"$zXG%.2f"}"),
                      div(cls := s"xx-small text-$zColor fw-bold", zLabel)
                    )
                  )
                }: _*)
              )
            )
          ) else frag(),

          // Tabla de goles individuales
          if (tablaGoles.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              s"Últimos ${tablaGoles.size} goles analizados"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "FECHA"),
                    th(cls := "xx-small text-muted", "RIVAL"),
                    th(cls := "xx-small text-muted text-center", "ZONA"),
                    th(cls := "xx-small text-muted", "SITUACIÓN"),
                    th(cls := "xx-small text-muted text-center", "xG"),
                    th(cls := "xx-small text-muted text-center", "DIFIC."),
                    th(cls := "xx-small text-muted text-center", "RESP.")
                  )),
                  tbody(
                    frag(tablaGoles.map { g =>
                      val c = g("color")
                      tr(
                        td(cls := "xx-small text-muted", g("fecha")),
                        td(cls := "xx-small", g("rival")),
                        td(cls := "xx-small text-center fw-bold text-white", g("zona")),
                        td(cls := "xx-small text-muted", g("sit")),
                        td(cls := s"xx-small text-center fw-bold text-$c", g("xg")),
                        td(cls := "text-center",
                          span(cls := s"badge bg-$c bg-opacity-25 text-$c xx-small", g("dific"))
                        ),
                        td(cls := "xx-small text-center text-muted", g("resp"))
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag(),

          script(raw(s"""
            (function() {
              var ctx = document.getElementById('psxgChart');
              if (!ctx || ${porZona.size} === 0) return;
              new Chart(ctx.getContext('2d'), {
                type: 'bar',
                data: {
                  labels: $zonaLabels,
                  datasets: [
                    {
                      label: 'Goles reales',
                      data: $zonaGoles,
                      backgroundColor: 'rgba(220,53,69,0.7)',
                      borderColor: 'rgba(220,53,69,1)',
                      borderWidth: 1
                    },
                    {
                      label: 'xG esperado',
                      data: $zonaXG,
                      backgroundColor: 'rgba(13,202,240,0.4)',
                      borderColor: 'rgba(13,202,240,0.8)',
                      borderWidth: 1,
                      type: 'line',
                      tension: 0.3,
                      pointRadius: 5
                    }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                  scales: {
                    x: { ticks: { color: '#aaa', font: { size: 11 } }, grid: { color: '#333' } },
                    y: { ticks: { color: '#aaa', stepSize: 1 }, grid: { color: '#333' }, beginAtZero: true }
                  }
                }
              });
            })();
          """))
        )
      )
    ))
  }

  // == RED-ZONE ANALYTICS ======================================================
  @cask.get("/red-zone")
  def redZonePage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getRedZoneData()

    val totalPartidos: Int    = d("totalPartidos").asInstanceOf[Int]
    val avgNotaGlobal: Double = d("avgNotaGlobal").asInstanceOf[Double]
    val avgGcGlobal: Double   = d("avgGcGlobal").asInstanceOf[Double]
    val nAsedio: Int          = d("nAsedio").asInstanceOf[Int]
    val avgNotaAsedio: Double = d("avgNotaAsedio").asInstanceOf[Double]
    val avgParadasAsedio: Double = d("avgParadasAsedio").asInstanceOf[Double]
    val resilienceIndex: Int  = d("resilienceIndex").asInstanceOf[Int]
    val resilienceLabel: String = d("resilienceLabel").asInstanceOf[String]
    val resilienceColor: String = d("resilienceColor").asInstanceOf[String]
    val nFatiga: Int          = d("nFatiga").asInstanceOf[Int]
    val avgNotaFatiga: Double = d("avgNotaFatiga").asInstanceOf[Double]
    val avgParadasFatiga: Double = d("avgParadasFatiga").asInstanceOf[Double]
    val fatigueIndex: Int     = d("fatigueIndex").asInstanceOf[Int]
    val fatigueLabel: String  = d("fatigueLabel").asInstanceOf[String]
    val fatigueColor: String  = d("fatigueColor").asInstanceOf[String]
    val nColapso: Int         = d("nColapso").asInstanceOf[Int]
    val avgNotaColapso: Double = d("avgNotaColapso").asInstanceOf[Double]
    val asedioRows: List[Map[String,Any]] = d("asedioRows").asInstanceOf[List[Map[String,Any]]]
    val fatigaRows: List[Map[String,Any]] = d("fatigaRows").asInstanceOf[List[Map[String,Any]]]
    val asedioSerie: List[Double]  = d("asedioSerie").asInstanceOf[List[Double]]
    val asedioLabels: List[String] = d("asedioLabels").asInstanceOf[List[String]]
    val globalLine: List[Double]   = d("globalLine").asInstanceOf[List[Double]]

    val avgNotaGlobalStr  = f"$avgNotaGlobal%.1f"
    val avgNotaAsedioStr  = f"$avgNotaAsedio%.1f"
    val avgNotaFatigaStr  = f"$avgNotaFatiga%.1f"
    val avgNotaColapsoStr = f"$avgNotaColapso%.1f"
    val deltaAsedio   = avgNotaAsedio - avgNotaGlobal
    val deltaFatiga   = avgNotaFatiga - avgNotaGlobal
    val deltaAsedioStr = (if (deltaAsedio >= 0) "+" else "") + f"$deltaAsedio%.1f"
    val deltaFatigaStr = (if (deltaFatiga >= 0) "+" else "") + f"$deltaFatiga%.1f"
    val deltaAsedioColor = if (deltaAsedio >= 0) "success" else if (deltaAsedio >= -0.5) "warning" else "danger"
    val deltaFatigaColor = if (deltaFatiga >= 0) "success" else if (deltaFatiga >= -0.5) "warning" else "danger"

    val labelsJson  = asedioLabels.map(l => "\"" + l + "\"").mkString("[", ",", "]")
    val asedioJson  = asedioSerie.map(v => f"$v%.1f").mkString("[", ",", "]")
    val globalJson  = globalLine.map(v => f"$v%.1f").mkString("[", ",", "]")

    renderHtml(basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",

          // Header
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls := "text-danger mb-0", "RED-ZONE ANALYTICS"),
              span(cls := "badge bg-dark border border-danger text-danger", "FASE 7 — Under Pressure")
            ),
            div(cls := "d-flex gap-2",
              a(href := "/moneyball", cls := "btn btn-outline-warning btn-sm fw-bold", "Moneyball"),
              a(href := "/dashboard", cls := "btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
            )
          ),

          if (totalPartidos < 5) div(cls := "alert alert-secondary",
            s"Datos insuficientes. Necesitas al menos 5 partidos registrados (tienes $totalPartidos)."
          ) else frag(),

          // Referencia global
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-body p-3",
              div(cls := "row g-3 text-center",
                div(cls := "col-4",
                  div(cls := "text-muted xx-small fw-bold", "MEDIA GLOBAL"),
                  div(cls := "fs-3 fw-black text-white", avgNotaGlobalStr),
                  div(cls := "xx-small text-muted", s"$totalPartidos partidos")
                ),
                div(cls := "col-4",
                  div(cls := "text-muted xx-small fw-bold", "GC MEDIA"),
                  div(cls := "fs-3 fw-black text-white", f"$avgGcGlobal%.1f"),
                  div(cls := "xx-small text-muted", "goles/partido")
                ),
                div(cls := "col-4",
                  div(cls := "text-muted xx-small fw-bold", "PARTIDOS ANÁLISIS"),
                  div(cls := "fs-3 fw-black text-warning", s"$nAsedio"),
                  div(cls := "xx-small text-muted", "con GC >= 2")
                )
              )
            )
          ),

          // Dos KPIs principales
          div(cls := "row g-3 mb-3",

            // Resilience Index — asedio
            div(cls := "col-md-6",
              div(cls := s"card bg-dark border-$resilienceColor shadow h-100",
                div(cls := "card-header fw-bold small text-white", "🔥 BAJO ASEDIO (GC ≥ 2)"),
                div(cls := "card-body p-3",
                  div(cls := "row g-2 align-items-center",
                    div(cls := "col-4 text-center",
                      div(cls := s"display-4 fw-black text-$resilienceColor", resilienceIndex.toString),
                      div(cls := s"badge bg-$resilienceColor mt-1", resilienceLabel)
                    ),
                    div(cls := "col-8",
                      div(cls := "row g-2",
                        div(cls := "col-6",
                          div(cls := "xx-small text-muted", "NOTA EN ASEDIO"),
                          div(cls := s"fs-4 fw-black text-$resilienceColor", avgNotaAsedioStr),
                          div(cls := s"badge bg-$deltaAsedioColor bg-opacity-25 text-$deltaAsedioColor xx-small",
                            s"$deltaAsedioStr vs media")
                        ),
                        div(cls := "col-6",
                          div(cls := "xx-small text-muted", "PARADAS MEDIA"),
                          div(cls := "fs-4 fw-black text-white", f"$avgParadasAsedio%.1f"),
                          div(cls := "xx-small text-muted", s"$nAsedio partidos")
                        )
                      )
                    )
                  ),
                  // Barra de progreso
                  div(cls := "mt-2",
                    div(cls := "d-flex justify-content-between xx-small text-muted mb-1",
                      span("Rendimiento bajo presion"),
                      span(s"$resilienceIndex/100")
                    ),
                    div(cls := "progress", style := "height:8px;",
                      div(cls := s"progress-bar bg-$resilienceColor",
                        style := s"width:${resilienceIndex}%; transition:width 1s;")
                    )
                  )
                )
              )
            ),

            // Fatigue Index — partidos largos
            div(cls := "col-md-6",
              div(cls := s"card bg-dark border-$fatigueColor shadow h-100",
                div(cls := "card-header fw-bold small text-white", "⏱️ FATIGA FINAL (≥ 70 min)"),
                div(cls := "card-body p-3",
                  div(cls := "row g-2 align-items-center",
                    div(cls := "col-4 text-center",
                      div(cls := s"display-4 fw-black text-$fatigueColor", fatigueIndex.toString),
                      div(cls := s"badge bg-$fatigueColor mt-1", fatigueLabel)
                    ),
                    div(cls := "col-8",
                      div(cls := "row g-2",
                        div(cls := "col-6",
                          div(cls := "xx-small text-muted", "NOTA CON FATIGA"),
                          div(cls := s"fs-4 fw-black text-$fatigueColor", avgNotaFatigaStr),
                          div(cls := s"badge bg-$deltaFatigaColor bg-opacity-25 text-$deltaFatigaColor xx-small",
                            s"$deltaFatigaStr vs media")
                        ),
                        div(cls := "col-6",
                          div(cls := "xx-small text-muted", "PARADAS MEDIA"),
                          div(cls := "fs-4 fw-black text-white", f"$avgParadasFatiga%.1f"),
                          div(cls := "xx-small text-muted", s"$nFatiga partidos")
                        )
                      )
                    )
                  ),
                  div(cls := "mt-2",
                    div(cls := "d-flex justify-content-between xx-small text-muted mb-1",
                      span("Resistencia a la fatiga"),
                      span(s"$fatigueIndex/100")
                    ),
                    div(cls := "progress", style := "height:8px;",
                      div(cls := s"progress-bar bg-$fatigueColor",
                        style := s"width:${fatigueIndex}%; transition:width 1s;")
                    )
                  )
                )
              )
            )
          ),

          // Colapso total (GC >= 3)
          if (nColapso > 0) div(cls := "card bg-dark border-danger shadow mb-3",
            div(cls := "card-header text-danger fw-bold small", s"💥 COLAPSO TOTAL (GC ≥ 3) — $nColapso partidos"),
            div(cls := "card-body p-3",
              div(cls := "row g-3 text-center",
                div(cls := "col-4",
                  div(cls := "xx-small text-muted", "NOTA MEDIA"),
                  div(cls := "fs-3 fw-black text-danger", avgNotaColapsoStr)
                ),
                div(cls := "col-4",
                  div(cls := "xx-small text-muted", "VS MEDIA GLOBAL"),
                  div(cls := s"fs-3 fw-black text-${if (avgNotaColapso >= avgNotaGlobal - 0.5) "warning" else "danger"}",
                    (if (avgNotaColapso - avgNotaGlobal >= 0) "+" else "") + f"${avgNotaColapso - avgNotaGlobal}%.1f")
                ),
                div(cls := "col-4",
                  div(cls := "xx-small text-muted", "INTERPRETACION"),
                  div(cls := "small fw-bold text-white",
                    if (avgNotaColapso >= avgNotaGlobal - 0.3) "Aguanta el tipo"
                    else if (avgNotaColapso >= avgNotaGlobal - 1.0) "Leve impacto"
                    else "Necesita trabajo"
                  )
                )
              )
            )
          ) else frag(),

          // Grafico evolucion bajo asedio
          if (asedioSerie.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "Evolucion de nota en partidos de asedio"),
            div(cls := "card-body p-3",
              div(style := "height:220px;",
                tag("canvas")(id := "redZoneChart")
              )
            )
          ) else frag(),

          // Tabla partidos de asedio
          if (asedioRows.nonEmpty) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", s"Partidos bajo asedio (GC ≥ 2) — últimos ${math.min(asedioRows.size, 10)}"),
            div(cls := "card-body p-2",
              div(cls := "table-responsive",
                table(cls := "table table-dark table-sm table-hover mb-0",
                  thead(tr(
                    th(cls := "xx-small text-muted", "FECHA"),
                    th(cls := "xx-small text-muted", "RIVAL"),
                    th(cls := "xx-small text-muted text-center", "RES."),
                    th(cls := "xx-small text-muted text-center", "PAR."),
                    th(cls := "xx-small text-muted text-center", "NOTA"),
                    th(cls := "xx-small text-muted text-center", "VS MEDIA")
                  )),
                  tbody(
                    frag(asedioRows.take(10).map { r =>
                      val nota   = r("nota").asInstanceOf[Double]
                      val gc     = r("gc").asInstanceOf[Int]
                      val gf     = r("gf").asInstanceOf[Int]
                      val par    = r("paradas").asInstanceOf[Int]
                      val delta  = nota - avgNotaGlobal
                      val notaCls = if (nota >= avgNotaGlobal) "success" else if (nota >= avgNotaGlobal - 0.5) "warning" else "danger"
                      tr(
                        td(cls := "xx-small text-muted", r("fecha").asInstanceOf[String]),
                        td(cls := "xx-small", r("rival").asInstanceOf[String]),
                        td(cls := "xx-small text-center text-danger fw-bold", s"$gf-$gc"),
                        td(cls := "xx-small text-center text-info", par.toString),
                        td(cls := s"xx-small text-center fw-black text-$notaCls", f"$nota%.1f"),
                        td(cls := s"xx-small text-center text-$notaCls",
                          (if (delta >= 0) "+" else "") + f"$delta%.1f")
                      )
                    }: _*)
                  )
                )
              )
            )
          ) else frag(),

          script(raw(s"""
            (function() {
              var ctx = document.getElementById('redZoneChart');
              if (!ctx || ${ asedioSerie.size } === 0) return;
              new Chart(ctx.getContext('2d'), {
                type: 'line',
                data: {
                  labels: $labelsJson,
                  datasets: [
                    {
                      label: 'Nota en asedio',
                      data: $asedioJson,
                      borderColor: 'rgba(220,53,69,0.9)',
                      backgroundColor: 'rgba(220,53,69,0.1)',
                      tension: 0.3,
                      pointRadius: 5,
                      fill: true
                    },
                    {
                      label: 'Media global',
                      data: $globalJson,
                      borderColor: 'rgba(13,202,240,0.5)',
                      borderDash: [4,3],
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
                tag("canvas")(id := "resetChart", style := "max-height:240px;")
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