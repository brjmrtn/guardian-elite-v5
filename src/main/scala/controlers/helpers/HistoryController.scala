import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object HistoryController extends cask.Routes {

  // Parseo manual de body application/x-www-form-urlencoded (mas fiable que @cask.postForm con fetch)
  private def parseBody(request: cask.Request): Map[String, String] = {
    val body = new String(request.data.readAllBytes(), "UTF-8")
    body.split("&").filter(_.nonEmpty).map { p =>
      val kv = p.split("=", 2)
      java.net.URLDecoder.decode(kv(0), "UTF-8") -> (if (kv.length > 1) java.net.URLDecoder.decode(kv(1), "UTF-8") else "")
    }.toMap
  }

  // ── Parseo manual de multipart/form-data (subida binaria de video) ─────────
  private val B_CR: Byte = '\r'.toByte
  private val B_LF: Byte = '\n'.toByte
  private val B_DASH: Byte = '-'.toByte

  private def indexOfBytes(hay: Array[Byte], needle: Array[Byte], from: Int): Int = {
    val n = needle.length
    if (n == 0 || from < 0) return -1
    var i = from
    val limit = hay.length - n
    while (i <= limit) {
      var j = 0
      while (j < n && hay(i + j) == needle(j)) j += 1
      if (j == n) return i
      i += 1
    }
    -1
  }

  private case class MultipartField(filename: Option[String], contentType: Option[String], data: Array[Byte])

  private def parseMultipart(bodyBytes: Array[Byte], contentTypeHeader: String): Map[String, MultipartField] = {
    val marker = "boundary="
    val bIdx = if (contentTypeHeader == null) -1 else contentTypeHeader.indexOf(marker)
    if (bIdx < 0) return Map.empty
    var boundary = contentTypeHeader.substring(bIdx + marker.length).split(";").head.trim
    if (boundary.startsWith("\"") && boundary.endsWith("\"")) boundary = boundary.substring(1, boundary.length - 1)
    val delim = ("--" + boundary).getBytes("ISO-8859-1")
    val headerEnd = Array(B_CR, B_LF, B_CR, B_LF)

    var fields = Map[String, MultipartField]()
    var searchFrom = 0
    var continue = true
    while (continue) {
      val delimPos = indexOfBytes(bodyBytes, delim, searchFrom)
      if (delimPos < 0) { continue = false } else {
        var partStart = delimPos + delim.length
        val isFinal = partStart + 1 < bodyBytes.length && bodyBytes(partStart) == B_DASH && bodyBytes(partStart + 1) == B_DASH
        if (isFinal) { continue = false } else {
          if (partStart + 1 < bodyBytes.length && bodyBytes(partStart) == B_CR && bodyBytes(partStart + 1) == B_LF) partStart += 2
          val hEnd = indexOfBytes(bodyBytes, headerEnd, partStart)
          if (hEnd < 0) { continue = false } else {
            val headersStr = new String(bodyBytes, partStart, hEnd - partStart, "UTF-8")
            val dataStart = hEnd + headerEnd.length
            val nextDelimPos = indexOfBytes(bodyBytes, delim, dataStart)
            if (nextDelimPos < 0) { continue = false } else {
              var dataEnd = nextDelimPos
              if (dataEnd >= dataStart + 2 && bodyBytes(dataEnd - 2) == B_CR && bodyBytes(dataEnd - 1) == B_LF) dataEnd -= 2
              val nameOpt = """name="([^"]*)"""".r.findFirstMatchIn(headersStr).map(_.group(1))
              val fileOpt = """filename="([^"]*)"""".r.findFirstMatchIn(headersStr).map(_.group(1))
              val ctOpt = """(?i)Content-Type:\s*([^\r\n]+)""".r.findFirstMatchIn(headersStr).map(_.group(1).trim)
              nameOpt.foreach { name =>
                val data = java.util.Arrays.copyOfRange(bodyBytes, dataStart, math.max(dataStart, dataEnd))
                fields = fields + (name -> MultipartField(fileOpt, ctOpt, data))
              }
              searchFrom = nextDelimPos
            }
          }
        }
      }
    }
    fields
  }

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
  def historyPage(request: cask.Request, temporadaId: Int = 0, msg: String = "") = withAuth(request) {
    val temporadas = DatabaseManager.getTodasTemporadas()
    val activaId = DatabaseManager.getTemporadaActivaId()
    val efectivo = if (temporadaId > 0) temporadaId else activaId

    val matches = DatabaseManager.getMatchesList(efectivo)

    // B1: Z-Score de rendimiento por contexto (solo activo con >=15 partidos totales)
    val zScoresByMatchId: Map[Int, Double] = DatabaseManager.getZScoreRendimiento(efectivo)
      .map(z => z("matchId").asInstanceOf[Int] -> z("zScore").asInstanceOf[Double]).toMap

    // BLOQUE B6: temporada archivada — solo lectura + resumen de cierre
    val temporadaSeleccionada = temporadas.find(_("id").asInstanceOf[Int] == efectivo)
    val esArchivada = temporadaSeleccionada.exists(t => t("fechaFin").asInstanceOf[String].nonEmpty)

    val archivadaBanner: Modifier = if (!esArchivada) div() else {
      val nombre = temporadaSeleccionada.get("nombre").asInstanceOf[String]
      div(cls := "alert alert-warning small p-2 mb-3",
        s"📦 Viendo temporada archivada: $nombre. Esta temporada está cerrada.")
    }

    val resumenArchivada: Modifier = if (!esArchivada) div() else {
      val cerrada = DatabaseManager.getTemporadasCerradas().find(_("id").asInstanceOf[Int] == efectivo)
      cerrada match {
        case Some(c) =>
          val mejorPartidoTxt: String = matches.maxByOption(_.nota) match {
            case Some(m) => s"vs ${fixEncoding(m.rival)} (${m.nota})"
            case None => "—"
          }
          div(cls := "card bg-dark border-secondary shadow-sm mb-3 p-3",
            div(cls := "row text-center g-2",
              div(cls := "col-3", div(cls := "xx-small text-muted", "Nota media final"), div(cls := "fw-bold text-warning", f"${c("mediaFinal").asInstanceOf[Double]}%.1f")),
              div(cls := "col-3", div(cls := "xx-small text-muted", "PJ"), div(cls := "fw-bold text-white", c("pj").asInstanceOf[Int].toString)),
              div(cls := "col-3", div(cls := "xx-small text-muted", "Porterías a cero"), div(cls := "fw-bold text-info", c("porteriasCero").asInstanceOf[Int].toString)),
              div(cls := "col-3", div(cls := "xx-small text-muted", "Mejor partido"),
                div(cls := "fw-bold text-success small", mejorPartidoTxt))
            ),
            if (c("tieneInforme").asInstanceOf[Boolean])
              div(cls := "text-center mt-2",
                a(href := s"/admin/season-report/$efectivo", target := "_blank", cls := "small text-info", "📄 Ver informe de fin de temporada"))
            else div()
          )
        case None => div()
      }
    }

    // 1. Generamos las filas de la tabla (solo lectura si la temporada esta archivada)
    val tableRows = if (matches.isEmpty) {
      Seq(tr(td(colspan := 4, cls := "text-center p-4", "Sin partidos")))
    } else {
      val sourceBadges = DatabaseManager.getMatchSourceBadges(efectivo)
      matches.map(m => renderMatchRow(m, zScoresByMatchId.get(m.id), readOnly = esArchivada, sourceBadge = sourceBadges.get(m.id)))
    }

    // 2. Definimos el contenido central (SIN llamar a basePage aqui)
    val mainContent = div(cls := "row justify-content-center",
      div(cls := "col-md-10 col-12",
        div(cls := "d-flex justify-content-between align-items-center mb-3",
          h2(cls := "text-warning mb-0", "HISTORIAL"),
          a(href := "/mapa-goles", cls := "btn btn-outline-danger btn-sm fw-bold", "MAPA DE GOLES")
        ),
        if (msg.nonEmpty) div(cls := "alert alert-warning small p-2 mb-3", msg) else frag(),
        seasonSelector(temporadas, efectivo, "/history"),
        archivadaBanner,
        resumenArchivada,
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

  // ── BLOQUE D: DESGLOSE TECNICO DE PARADAS (opcional, post-registro) ───────
  @cask.get("/history/paradas/:matchId")
  def paradasDesglosePage(request: cask.Request, matchId: Int) = withAuth(request) {
    DatabaseManager.getMatchById(matchId) match {
      case None => renderHtml(basePage("history", div(cls:="text-center text-muted py-5", "Partido no encontrado")))
      case Some(m) =>
        val guardadas = DatabaseManager.getParadasDetalleMatch(matchId).map(p => p("numeroParada").asInstanceOf[Int] -> p).toMap
        val tecnicas = Seq("DOS_MANOS", "UNA_MANO", "TIP_OVER", "BLOQUEO_CUERPO", "PARADA_PIES", "OTRO")
        val resultados = Seq("ATRAPADO_LIMPIO", "DESPEJADO_ZONA_SEGURA", "DESPEJADO_PELIGRO", "RECHAZADO")
        val content = basePage("history",
          div(cls:="row justify-content-center",
            div(cls:="col-md-8 col-12",
              div(cls:="d-flex justify-content-between align-items-center mb-3",
                h2(cls:="text-warning mb-0", "📊 DESGLOSE DE PARADAS"),
                a(href:="/history", cls:="btn btn-outline-secondary btn-sm fw-bold", "← Historial")
              ),
              div(cls:="text-muted small mb-3", s"vs ${fixEncoding(m.rival)} — ${m.fecha.take(10)} — ${m.paradas} paradas registradas"),
              form(action := s"/history/paradas/$matchId/save", method := "post",
                (1 to m.paradas).map { n =>
                  val previa = guardadas.get(n)
                  div(cls:="card bg-dark border-secondary shadow-sm mb-2 p-2",
                    div(cls:="fw-bold small text-info mb-1", s"Parada #$n"),
                    div(cls:="row g-2",
                      div(cls:="col-6",
                        label(cls:="xx-small text-muted", "Técnica"),
                        select(name := s"tecnica_$n", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                          option(value := "", "— Sin especificar —"),
                          tecnicas.map(t => if (previa.exists(_("tecnica") == t)) option(value := t, selected := "selected", t) else option(value := t, t))
                        )
                      ),
                      div(cls:="col-6",
                        label(cls:="xx-small text-muted", "Resultado"),
                        select(name := s"resultado_$n", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                          option(value := "", "— Sin especificar —"),
                          resultados.map(r => if (previa.exists(_("resultado") == r)) option(value := r, selected := "selected", r) else option(value := r, r))
                        )
                      )
                    )
                  )
                },
                div(cls:="d-grid mt-3", button(tpe:="submit", cls:="btn btn-info fw-bold", "Guardar desglose"))
              )
            )
          )
        )
        renderHtml(content)
    }
  }

  @cask.post("/history/paradas/:matchId/save")
  def paradasDesguardar(request: cask.Request, matchId: Int) = withAuth(request) {
    val bodyString = new String(request.data.readAllBytes(), "UTF-8")
    val formData = bodyString.split("&").filter(_.nonEmpty).map { part =>
      val pair = part.split("=", 2)
      java.net.URLDecoder.decode(pair(0), "UTF-8") -> (if (pair.length > 1) java.net.URLDecoder.decode(pair(1), "UTF-8") else "")
    }.toMap
    DatabaseManager.getMatchById(matchId).foreach { m =>
      DatabaseManager.deleteParadasDetalle(matchId) // limpiar si es re-guardado, igual que match_goals
      (1 to m.paradas).foreach { n =>
        val tecnica = formData.getOrElse(s"tecnica_$n", "")
        val resultado = formData.getOrElse(s"resultado_$n", "")
        if (tecnica.nonEmpty || resultado.nonEmpty) DatabaseManager.saveParadaDetalle(matchId, n, tecnica, "", resultado, "")
      }
    }
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history"))
  }

  @cask.get("/mapa-goles")
  def mapaGolesPage(request: cask.Request, temporada: String = "", rival: String = "", temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()

    val heatmap = if (rival.nonEmpty)
      DatabaseManager.getGoalHeatmapByRival(rival)
    else
      DatabaseManager.getGoalHeatmap(temporada, efectivo)

    val matches    = DatabaseManager.getMatchesList(efectivo)
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

          seasonSelector(temporadasDb, efectivo, "/mapa-goles"),

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
    val totalDias = injuries.map(_.diasBaja).sum
    val zonasRecurrentes = DatabaseManager.getZonasRecurrentes()
    val analisisIA = DatabaseManager.getInjuryPatternAnalysisCached().getOrElse("")

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

    val zonaMasAfectada = if (injuries.nonEmpty) injuries.groupBy(_.zona).maxBy(_._2.size)._1 else "—"
    val tipoMasFrecuente = if (injuries.nonEmpty) injuries.groupBy(_.tipoClasificado).maxBy(_._2.size)._1 else "—"

    // Timeline visual: bloques horizontales ordenados cronologicamente
    val timelineOrdenado = injuries.sortBy(_.fechaInicio)
    val timelineBlocks = if (timelineOrdenado.isEmpty) div(cls := "text-muted small text-center py-3", "Sin lesiones para mostrar en la línea de tiempo")
    else div(cls := "d-flex flex-column gap-1",
      frag(timelineOrdenado.map { inj =>
        val color = DatabaseManager.injuryTipoColor(inj.tipoClasificado)
        val zonaRec = zonasRecurrentes.contains(inj.zona.toLowerCase.trim)
        div(cls := "d-flex align-items-center gap-2",
          div(style := "width:90px; font-size:9px; color:#94a3b8; text-align:right; flex-shrink:0;", inj.fechaInicio),
          div(style := s"flex:1; height:22px; background:$color; border-radius:4px; display:flex; align-items:center; padding:0 8px; overflow:hidden;",
            span(style := "font-size:10px; font-weight:700; color:#111; white-space:nowrap;",
              s"${inj.zona} — ${inj.tipoClasificado}${if (inj.diasBaja > 0) s" (${inj.diasBaja}d)" else ""}")
          ),
          if (zonaRec) span(cls := "badge bg-danger", "⚠️ Zona recurrente") else span()
        )
      }: _*)
    )

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
              ("Zona más afectada", zonaMasAfectada, "warning"),
              ("Tipo más frecuente", tipoMasFrecuente, "info")
            ).map { case (lbl, v, c) =>
              div(cls:="col-3",
                div(cls:=s"card bg-dark border-$c text-center py-3",
                  div(cls:=s"text-$c fw-bold fs-5", v),
                  div(cls:="xx-small text-muted", lbl)
                )
              )
            }
          ),

          // Linea de tiempo
          div(cls := "card bg-dark border-secondary shadow mb-4",
            div(cls := "card-header text-white fw-bold small", "📅 LÍNEA DE TIEMPO"),
            div(cls := "card-body", timelineBlocks)
          ),

          // Analisis IA de patrones
          div(cls := "card bg-dark border-warning shadow mb-4",
            div(cls := "card-header text-warning fw-bold small", "🧠 DETECTOR DE PATRONES"),
            div(cls := "card-body",
              if (analisisIA.nonEmpty) div(cls := "text-light small mb-3", style := "white-space:pre-wrap;", analisisIA)
              else div(cls := "text-muted small mb-3", "Sin análisis generado todavía"),
              form(action := "/lesiones/patrones", method := "post",
                button(tpe := "submit", cls := "btn btn-outline-warning w-100 btn-sm fw-bold", "🧠 Detectar patrones")
              )
            )
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
                div(cls:="row g-2 mb-2",
                  div(cls:="col-4",
                    label(cls:="xx-small text-muted fw-bold", "Tipo clasificado"),
                    select(name:="tipoClasificado", cls:="form-select form-select-sm fw-bold",
                      option(value:="MUSCULAR", "Muscular"),
                      option(value:="OSEA", "Ósea"),
                      option(value:="ARTICULAR", "Articular"),
                      option(value:="SOBREUSO", "Sobreuso"),
                      option(value:="APOFISITIS", "Apofisitis"),
                      option(value:="CONTUSION", "Contusión"),
                      option(value:="ENFERMEDAD", "Enfermedad"),
                      option(value:="OTRO", "Otro")
                    )
                  ),
                  div(cls:="col-4",
                    label(cls:="xx-small text-muted fw-bold", "Lado"),
                    select(name:="lado", cls:="form-select form-select-sm fw-bold",
                      option(value:="NA", "N/A"), option(value:="IZQUIERDO", "Izquierdo"), option(value:="DERECHO", "Derecho")
                    )
                  ),
                  div(cls:="col-4",
                    label(cls:="xx-small text-muted fw-bold", "Partidos perdidos"),
                    input(tpe:="number", name:="partidosPerdidos", cls:="form-control form-control-sm fw-bold", value:="0", min:="0")
                  )
                ),
                div(cls:="mb-2",
                  label(cls:="xx-small text-muted fw-bold", "Causa probable"),
                  input(tpe:="text", name:="causaProbable", cls:="form-control form-control-sm fw-bold",
                    placeholder:="Ej: sobrecarga tras carga alta, gesto brusco...")
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

  @cask.post("/lesiones/nueva")
  def nuevaLesion(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    DatabaseManager.logInjury(
      p.getOrElse("zona", ""), p.getOrElse("tipo", "Otro"), p.getOrElse("gravedad", "LEVE"), p.getOrElse("desc", ""),
      p.getOrElse("tipoClasificado", "OTRO"), p.getOrElse("lado", "NA"), p.getOrElse("causaProbable", ""),
      p.getOrElse("partidosPerdidos", "0").toIntOption.getOrElse(0)
    )
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location"->"/lesiones"))
  }

  @cask.post("/lesiones/alta")
  def darAlta(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val id = p.getOrElse("id", "0").toIntOption.getOrElse(0)
    DatabaseManager.closeInjury(id, p.getOrElse("fechaAlta", ""), p.getOrElse("diasBaja", "0").toIntOption.getOrElse(0))
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location"->"/lesiones"))
  }

  @cask.post("/lesiones/patrones")
  def detectarPatronesLesiones(request: cask.Request) = withAuth(request) {
    DatabaseManager.generateInjuryPatternAnalysis()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/lesiones"))
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

    // BLOQUE E2: lo que detecto la IA en video, conectado a la flash-card (solo si hay analisis reciente)
    val videoErrorSection: Modifier = {
      val histVideo = DatabaseManager.getVideoAnalysisHistoryAll()
      val hayReciente = histVideo.lastOption.exists { h =>
        scala.util.Try(java.time.LocalDate.parse(h("fecha").asInstanceOf[String])).toOption
          .exists(_.isAfter(java.time.LocalDate.now().minusDays(30)))
      }
      if (!hayReciente) div()
      else DatabaseManager.getUltimoErrorRecurrente() match {
        case Some(error) =>
          div(cls := "card bg-dark border-warning shadow mb-4",
            div(cls := "card-header text-warning fw-bold small", "🎬 LO QUE DETECTÓ LA IA EN VÍDEO"),
            div(cls := "card-body p-3",
              div(cls := "text-light small", error),
              div(cls := "xx-small text-warning fw-bold mt-2", "👁️ Observa específicamente esto hoy desde la grada.")
            )
          )
        case None => div()
      }
    }

    // BLOQUE F: scouting conectado a la flash-card del rival
    val rivalScoutingSection: Modifier = if (targetRival.isEmpty) div() else {
      val notas = DatabaseManager.getRivalScoutingNotas(targetRival)
      val historial = DatabaseManager.getRivalHistorialCompleto(targetRival)
      val pjHist = historial.getOrElse("pj", 0).asInstanceOf[Int]
      val arquetipoRival = DatabaseManager.getStrikerClusters()
        .find(c => c("rival").asInstanceOf[String].toLowerCase.contains(targetRival.toLowerCase))
        .map(_("arquetipo").asInstanceOf[String])

      val hayScouting = notas.get("estilo").exists(_.nonEmpty) || notas.get("claves").exists(_.nonEmpty) || pjHist > 0

      if (!hayScouting)
        div(cls := "alert alert-secondary small",
          "No hay datos de este rival — registra el partido de hoy y ve a SCOUTING para añadir notas.")
      else
        div(cls := "card bg-dark border-info shadow mb-4",
          div(cls := "card-header text-info fw-bold small", "🔍 LO QUE SABEMOS DE ESTE RIVAL"),
          div(cls := "card-body p-3",
            if (notas.getOrElse("estilo", "").nonEmpty) div(cls := "mb-2",
              div(cls := "xx-small text-muted fw-bold", "ESTILO DE JUEGO"),
              div(cls := "text-light small", fixEncoding(notas("estilo")))
            ) else div(),
            if (notas.getOrElse("claves", "").nonEmpty) div(cls := "mb-2",
              div(cls := "xx-small text-muted fw-bold", "CLAVES TÁCTICAS"),
              div(cls := "text-warning small fw-bold", fixEncoding(notas("claves")))
            ) else div(),
            if (pjHist > 0) div(cls := "mb-2 pt-2 border-top border-secondary",
              div(cls := "xx-small text-muted fw-bold", "HISTORIAL CONTRA ESTE RIVAL"),
              div(cls := "text-light small",
                s"PJ ${historial("pj")} · GF ${historial("gf")} · GC ${historial("gc")} · Nota media de Héctor ${f"${historial("notaMedia").asInstanceOf[Double]}%.1f"}")
            ) else div(),
            arquetipoRival match {
              case Some(arq) => div(cls := "mb-0 pt-2 border-top border-secondary",
                div(cls := "xx-small text-muted fw-bold", "TIPO DE DELANTERO PREDOMINANTE"),
                span(cls := "badge bg-info text-dark fw-bold", arq)
              )
              case None => div()
            }
          )
        )
    }

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

    // B3: Predictor de nota pre-partido (regresion OLS, activo con >=30 partidos historicos)
    val prediccionData = DatabaseManager.getRendimientoPrediccionAuto()
    val prediccionWidget = if (prediccionData.getOrElse("activo", false).asInstanceOf[Boolean]) {
      val prediccion = prediccionData("prediccion").asInstanceOf[Double]
      val factorPositivo = prediccionData("factorPositivo").asInstanceOf[String]
      val factorNegativo = prediccionData("factorNegativo").asInstanceOf[String]
      div(cls := "card bg-dark border-info shadow mb-4",
        div(cls := "card-header text-info fw-bold small", "📊 PREDICTOR DE RENDIMIENTO"),
        div(cls := "card-body p-3 text-center",
          div(cls := "display-6 fw-bold text-info", f"$prediccion%.1f ± 0.8"),
          div(cls := "xx-small text-muted mb-2", "Nota esperada según su modelo histórico"),
          div(cls := "d-flex justify-content-center gap-3 xx-small mt-2",
            span(cls := "text-success", s"↑ $factorPositivo"),
            span(cls := "text-danger", s"↓ $factorNegativo")
          )
        )
      )
    } else div()

    val content = basePage("match-center",
      div(cls:="row justify-content-center",
        div(cls:="col-md-10 col-12",

          // Header
          div(cls:="d-flex justify-content-between align-items-center mb-3",
            h2(cls:="text-warning mb-0", "FLASH-CARDS PRE-PARTIDO"),
            a(href:="/match-center", cls:="btn btn-outline-secondary btn-sm fw-bold", "← Match Center")
          ),

          prediccionWidget,

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

            videoErrorSection,
            rivalScoutingSection,

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
  def gkInfluencePage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val stats = DatabaseManager.getGKInfluenceStats(efectivo)
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
            seasonSelector(temporadasDb, efectivo, "/gk-influence"),

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
  def biomecanicaPage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val stats = DatabaseManager.getBiomecPosicional(efectivo)
    val setPieceStats = DatabaseManager.getSetPieceStats(efectivo) // BLOQUE C

    // BLOQUE P: mapa de calor de goles encajados en 6 zonas — requiere >=10 goles con zona
    val heat6 = DatabaseManager.getGoalHeatmap6Zonas(efectivo)
    val heatmapGolesWidget: Modifier =
      if (!heat6("suficiente").asInstanceOf[Boolean])
        div(cls := "card bg-dark border-secondary shadow mb-4 p-3",
          div(cls := "fw-bold small text-white mb-1", "🥅 MAPA DE CALOR — GOLES ENCAJADOS"),
          div(cls := "xx-small text-muted", s"Se necesitan al menos 10 goles con zona registrada (hay ${heat6("total")})."))
      else {
        val zonas = heat6("zonas").asInstanceOf[Map[String, Int]]
        val total = heat6("total").asInstanceOf[Int]
        val maximo = math.max(1, zonas.values.max)
        val etiqueta = DatabaseManager.zonasPorteria6.toMap
        // azul (0 goles) -> rojo intenso (maximo), interpolando en RGB
        def color(n: Int): String = {
          val t = n.toDouble / maximo
          val (r, g, b) = ((37 + (220 - 37) * t).toInt, (99 + (38 - 99) * t).toInt, (235 + (38 - 235) * t).toInt)
          s"rgb($r,$g,$b)"
        }
        val (ancho, alto, x0, y0) = (300, 100, 20, 20)
        val celdas = for {
          (fila, iFila) <- Seq("ALTO", "BAJO").zipWithIndex
          (col, iCol) <- Seq("IZQ", "CEN", "DER").zipWithIndex
        } yield {
          val z = s"${fila}_$col"; val n = zonas.getOrElse(z, 0)
          val (x, y) = (x0 + iCol * ancho / 3, y0 + iFila * alto / 2)
          s"""<rect x="$x" y="$y" width="${ancho / 3}" height="${alto / 2}" fill="${color(n)}" stroke="#0f172a" stroke-width="2"><title>${etiqueta(z)}: $n</title></rect>""" +
          s"""<text x="${x + ancho / 6}" y="${y + alto / 4 + 7}" text-anchor="middle" font-size="20" font-weight="700" fill="#fff">$n</text>"""
        }
        val svg = s"""<svg viewBox="0 0 340 130" width="100%" style="max-width:420px;" role="img" aria-label="Goles encajados por zona de la portería">
          ${celdas.mkString}
          <path d="M${x0 - 6} ${y0 + alto + 4} L${x0 - 6} ${y0 - 6} L${x0 + ancho + 6} ${y0 - 6} L${x0 + ancho + 6} ${y0 + alto + 4}" fill="none" stroke="#e2e8f0" stroke-width="6" stroke-linejoin="round"/>
          <line x1="0" y1="${y0 + alto + 4}" x2="340" y2="${y0 + alto + 4}" stroke="#475569" stroke-width="2"/>
        </svg>"""
        val zMax = heat6("zonaMax").asInstanceOf[String]; val zMin = heat6("zonaMin").asInstanceOf[String]
        div(cls := "card bg-dark border-secondary shadow mb-4 p-3",
          div(cls := "fw-bold small text-white mb-2", "🥅 MAPA DE CALOR — GOLES ENCAJADOS"),
          div(cls := "text-center", raw(svg)),
          div(cls := "d-flex justify-content-between xx-small text-muted mt-1", span("🔵 0 goles"), span(s"🔴 $maximo goles")),
          div(cls := "small mt-2",
            div(cls := "text-danger", s"Más goles: ${etiqueta(zMax)} (${zonas(zMax)} de $total)"),
            div(cls := "text-info", s"Menos goles: ${etiqueta(zMin)} (${zonas(zMin)} de $total)")),
          div(cls := "xx-small text-muted mt-1", "La media altura cuenta como zona baja."))
      }

    // BLOQUE F: correccion del paso negativo — requiere >=8 goles con posicion_set
    val pasoNegativo = DatabaseManager.getPasoNegativoTrend(efectivo)
    val pasoNegativoWidget: Modifier =
      if (!pasoNegativo("suficiente").asInstanceOf[Boolean]) div()
      else {
        val serie = pasoNegativo("serie").asInstanceOf[List[Map[String, Any]]]
        val tendencia = pasoNegativo("tendencia").asInstanceOf[String]
        val labelsJs = serie.map(m => s"'Mes ${m("mes").asInstanceOf[Int]}'").mkString("[", ",", "]")
        val dataJs = serie.map(m => m("pctPasoNegativo").asInstanceOf[Int].toString).mkString("[", ",", "]")
        val mensaje = tendencia match {
          case "CORRIGIENDO" => div(cls := "xx-small text-success fw-bold", "✅ Corrigiendo")
          case "ESTABLE_O_PEOR" => div(cls := "xx-small text-warning fw-bold", "⚠️ Comunicar al entrenador de academia.")
          case _ => div()
        }
        div(cls := "card bg-dark border-secondary shadow mb-4",
          div(cls := "card-header text-white fw-bold small", "📉 CORRECCIÓN DEL PASO NEGATIVO"),
          div(cls := "card-body p-3",
            div(style := "height:180px;", tag("canvas")(id := "chartPasoNegativo")),
            mensaje,
            script(raw(s"""
              var ctxPN = document.getElementById('chartPasoNegativo');
              if (ctxPN) {
                new Chart(ctxPN, { type: 'line',
                  data: { labels: $labelsJs, datasets: [{ label: '% goles con paso negativo', data: $dataJs,
                    borderColor: '#dc3545', backgroundColor: 'rgba(220,53,69,0.1)', borderWidth:2, pointRadius:4, fill:true, tension:0.3 }] },
                  options: { responsive:true, maintainAspectRatio:false, plugins:{ legend:{ display:false } },
                    scales: { y: { min:0, max:100, ticks:{color:'#aaa'}, grid:{color:'#333'} }, x: { ticks:{color:'#888'}, grid:{color:'#333'} } } }
                });
              }
            """))
          )
        )
      }

    // BLOQUE Q: vulnerabilidad temporal — goles encajados por cuarto del partido
    val rendimientoFase = DatabaseManager.getRendimientoPorFase(efectivo)
    val vulnerabilidadWidget: Modifier =
      if (!rendimientoFase("suficiente").asInstanceOf[Boolean]) div()
      else {
        val pctQ1 = rendimientoFase("pctQ1").asInstanceOf[Double]; val pctQ4 = rendimientoFase("pctQ4").asInstanceOf[Double]
        val labelsJs = List("Q1 (1-12min)", "Q2 (13-25min)", "Q3 (26-37min)", "Q4 (38-50min)").map(l => s"'$l'").mkString("[", ",", "]")
        val dataJs = List(rendimientoFase("q1").asInstanceOf[Int], rendimientoFase("q2").asInstanceOf[Int], rendimientoFase("q3").asInstanceOf[Int], rendimientoFase("q4").asInstanceOf[Int]).mkString("[", ",", "]")
        div(cls := "card bg-dark border-secondary shadow mb-4",
          div(cls := "card-header text-white fw-bold small", "⏱️ VULNERABILIDAD TEMPORAL"),
          div(cls := "card-body p-3",
            div(style := "height:180px;", tag("canvas")(id := "chartVulnerabilidad")),
            if (pctQ1 > 40) div(cls := "xx-small text-warning fw-bold mt-2", "⚠️ Héctor encaja muchos goles en el primer cuarto — problema de arranque en frío. Revisar calentamiento.") else div(),
            if (pctQ4 > 40) div(cls := "xx-small text-warning fw-bold mt-1", "⚠️ Muchos goles en el último cuarto — problema de concentración tardía o fatiga.") else div(),
            script(raw(s"""
              var ctxVuln = document.getElementById('chartVulnerabilidad');
              if (ctxVuln) {
                new Chart(ctxVuln, { type: 'bar',
                  data: { labels: $labelsJs, datasets: [{ label: 'Goles encajados', data: $dataJs,
                    backgroundColor: ['#0dcaf0','#20c997','#ffc107','#dc3545'] }] },
                  options: { responsive:true, maintainAspectRatio:false, plugins:{ legend:{ display:false } },
                    scales: { y: { beginAtZero:true, ticks:{color:'#aaa', stepSize:1}, grid:{color:'#333'} }, x: { ticks:{color:'#888'}, grid:{color:'#333'} } } }
                });
              }
            """))
          )
        )
      }

    // BLOQUE H: exito en 1v1 por angulo de entrada — requiere >=15 acciones con angulo
    val angulo1v1 = DatabaseManager.get1v1ByAngulo(efectivo)
    val angulo1v1Widget: Modifier =
      if (!angulo1v1("suficiente").asInstanceOf[Boolean]) div()
      else {
        val central = angulo1v1("central").asInstanceOf[Map[String, Any]]
        val izquierda = angulo1v1("izquierda").asInstanceOf[Map[String, Any]]
        val derecha = angulo1v1("derecha").asInstanceOf[Map[String, Any]]
        def barra(nombre: String, datos: Map[String, Any]): Modifier = {
          val pct = datos("pct").asInstanceOf[Double]
          val ok = datos("ok").asInstanceOf[Int]; val gc = datos("gc").asInstanceOf[Int]
          val color = if (pct < 40) "danger" else if (pct < 60) "warning" else "success"
          div(cls := "mb-2",
            div(cls := "d-flex justify-content-between xx-small", span(nombre), span(cls := s"text-$color fw-bold", f"$pct%.0f%% ($ok/${ok + gc})")),
            div(cls := "progress", style := "height:10px;", div(cls := s"progress-bar bg-$color", style := f"width:$pct%.0f%%;"))
          )
        }
        val alertas = Seq("CENTRAL" -> central, "DIAGONAL IZQUIERDA" -> izquierda, "DIAGONAL DERECHA" -> derecha)
          .filter { case (_, d) => d("pct").asInstanceOf[Double] < 40 && (d("ok").asInstanceOf[Int] + d("gc").asInstanceOf[Int]) >= 3 }
        div(cls := "card bg-dark border-warning shadow mb-4",
          div(cls := "card-header text-warning fw-bold small", "🎯 ÉXITO EN 1V1 POR ÁNGULO"),
          div(cls := "card-body p-3",
            barra("CENTRAL", central), barra("DIAGONAL IZQUIERDA", izquierda), barra("DIAGONAL DERECHA", derecha),
            alertas.map { case (nombre, _) => div(cls := "xx-small text-danger fw-bold mt-1", s"⚠️ Tasa de éxito baja en $nombre — foco de trabajo para el entrenador.") }
          )
        )
      }

    // BLOQUE D: desglose tecnico de paradas — solo se muestra con >=20 paradas con detalle
    val paradasAnalysis = DatabaseManager.getParadasAnalysis(efectivo)
    val paradasAnalysisWidget: Modifier = {
      val totalDetalle = paradasAnalysis("total").asInstanceOf[Int]
      if (totalDetalle < 20) div()
      else {
        val porTecnica = paradasAnalysis("porTecnica").asInstanceOf[List[Map[String, Any]]]
        val masUsada = paradasAnalysis("masUsada").asInstanceOf[Option[String]]
        val mejorTasa = paradasAnalysis("mejorTasaLimpia").asInstanceOf[Option[String]]
        div(cls:="card bg-dark border-info shadow mb-4",
          div(cls:="card-header text-info fw-bold small", "🧤 DESGLOSE TÉCNICO DE PARADAS"),
          div(cls:="card-body p-3",
            div(cls:="small text-white mb-2",
              masUsada.map(t => s"Técnica más usada: $t.").getOrElse("") +
                mejorTasa.map(t => s" Mejor tasa de resultado limpio: $t.").getOrElse("")
            ),
            table(cls:="table table-sm table-dark mb-0",
              thead(tr(th("Técnica"), th(cls:="text-center","Usos"), th(cls:="text-center","% Limpio"))),
              tbody(
                porTecnica.map { t =>
                  tr(
                    td(t("tecnica").asInstanceOf[String]),
                    td(cls:="text-center", t("usos").asInstanceOf[Int].toString),
                    td(cls:="text-center", f"${t("pctLimpio").asInstanceOf[Double]}%.0f%%")
                  )
                }
              )
            )
          )
        )
      }
    }
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
            seasonSelector(temporadasDb, efectivo, "/biomecanica"),

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

            script(src:="https://cdn.jsdelivr.net/npm/chart.js"),

            // BLOQUE C: CONTROL DE BALON PARADO ──────────────────────────
            {
              val nSP           = setPieceStats("nPartidosConDatos").asInstanceOf[Int]
              val ratioDominio  = setPieceStats("ratioDominio").asInstanceOf[Double]
              val totalDom      = setPieceStats("totalDominados").asInstanceOf[Int]
              val totalCed      = setPieceStats("totalCedidos").asInstanceOf[Int]
              val totalFaltasSP = setPieceStats("totalFaltas").asInstanceOf[Int]
              val tendenciaSP   = setPieceStats("tendencia").asInstanceOf[String]
              val notaDominioSP   = setPieceStats("notaMediaDominio").asInstanceOf[Double]
              val notaNoDominioSP = setPieceStats("notaMediaNoDominio").asInstanceOf[Double]
              val serieFechasSP = setPieceStats("serieFechas").asInstanceOf[List[String]]
              val serieRatiosSP = setPieceStats("serieRatios").asInstanceOf[List[Double]]

              if (nSP == 0) div(cls:="card bg-dark border-secondary shadow mb-4",
                div(cls:="card-header text-white fw-bold small", "🏴 CONTROL DE BALÓN PARADO"),
                div(cls:="card-body text-center text-muted small py-4", "Sin datos de balón parado registrados todavía.")
              ) else {
                val barColor = if (ratioDominio >= 70) "success" else if (ratioDominio >= 50) "warning" else "danger"
                val fraseAuto: Modifier =
                  if (nSP >= 5) {
                    val interpretacion =
                      if (ratioDominio >= 70) "un dominio claro del juego aéreo — un diferencial de élite para su edad"
                      else if (ratioDominio >= 50) "un control razonable, con margen de mejora en la toma de decisión de salida"
                      else "dificultad para imponerse en el área — foco recomendado en salidas aéreas"
                    div(cls:="alert alert-secondary small mt-2",
                      f"Héctor domina el $ratioDominio%.0f%% de los córners — $interpretacion.")
                  } else div()

                val labelsJson = serieFechasSP.map(f => "\"" + f + "\"").mkString("[",",","]")
                val ratiosJson = serieRatiosSP.map(r => f"$r%.0f").mkString("[",",","]")

                div(cls:="card bg-dark border-warning shadow mb-4",
                  div(cls:="card-header text-warning fw-bold small", "🏴 CONTROL DE BALÓN PARADO"),
                  div(cls:="card-body p-3",
                    div(cls:="d-flex justify-content-between align-items-center mb-1",
                      span(cls:="small text-muted", "Dominio aéreo en córners"),
                      span(cls:=s"fw-bold text-$barColor", f"$ratioDominio%.0f%%")
                    ),
                    div(cls:="progress mb-3", style:="height:10px;",
                      div(cls:=s"progress-bar bg-$barColor", style:=f"width:$ratioDominio%.0f%%;")
                    ),
                    div(cls:="row g-2 text-center mb-3",
                      div(cls:="col-4", div(cls:="fw-bold text-warning", totalDom.toString), div(cls:="xx-small text-muted", "Dominados")),
                      div(cls:="col-4", div(cls:="fw-bold text-secondary", totalCed.toString), div(cls:="xx-small text-muted", "Cedidos")),
                      div(cls:="col-4", div(cls:="fw-bold text-info", totalFaltasSP.toString), div(cls:="xx-small text-muted", "Faltas dominadas"))
                    ),
                    if (serieRatiosSP.nonEmpty) div(
                      div(style:="height:180px;", tag("canvas")(id:="chartSetPiece")),
                      script(raw(s"""
                        var ctxSP = document.getElementById('chartSetPiece');
                        if (ctxSP) {
                          new Chart(ctxSP, {
                            type: 'line',
                            data: { labels: $labelsJson, datasets: [{ label: '% Dominio córners', data: $ratiosJson,
                              borderColor: '#ffc107', backgroundColor: 'rgba(255,193,7,0.15)', borderWidth:2, pointRadius:3, fill:true, tension:0.3 }] },
                            options: { responsive:true, maintainAspectRatio:false, plugins:{ legend:{ display:false } }, scales:{ y:{ min:0, max:100 } } }
                          });
                        }
                      """))
                    ) else div(),
                    if (notaDominioSP > 0 && notaNoDominioSP > 0) div(cls:="xx-small text-muted mt-2",
                      f"Nota media cuando domina el área: $notaDominioSP%.1f · cuando no: $notaNoDominioSP%.1f")
                    else div(),
                    fraseAuto
                  )
                )
              }
            },

            paradasAnalysisWidget,
            heatmapGolesWidget,
            pasoNegativoWidget,
            angulo1v1Widget,
            vulnerabilidadWidget,

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

      // BLOQUE S: toolkit de regulacion emocional tras gol encajado
      {
        val re = DatabaseManager.getRegulacionEmocional()
        if (!re("suficiente").asInstanceOf[Boolean]) div()
        else {
          val distribucion = re("distribucion").asInstanceOf[List[Map[String, Any]]]
          val notaReorganiza = re("notaReorganiza").asInstanceOf[Option[Double]]
          val notaDecaido = re("notaDecaido").asInstanceOf[Option[Double]]
          val pctDecaido = re("pctDecaido").asInstanceOf[Double]
          val pctSaludable = re("pctSaludable").asInstanceOf[Double]
          val labelsJs = distribucion.map(d => s""""${d("comportamiento")}"""").mkString("[", ",", "]")
          val dataJs = distribucion.map(d => d("n").asInstanceOf[Int].toString).mkString("[", ",", "]")
          val comparativa: Modifier = (notaReorganiza, notaDecaido) match {
            case (Some(nr), Some(nd)) => div(cls := "small text-white mt-2", f"Cuando se reorganiza con la defensa, la nota media del resto del partido es $nr%.1f. Cuando decae, es $nd%.1f.")
            case _ => div()
          }
          div(cls := "card bg-dark border-info shadow mb-3",
            div(cls := "card-header text-info fw-bold small", "🧠 TOOLKIT DE REGULACIÓN EMOCIONAL"),
            div(cls := "card-body p-3",
              div(style := "height:200px;", tag("canvas")(id := "chartRegulacionEmocional")),
              comparativa,
              if (pctDecaido > 40) div(cls := "xx-small text-warning fw-bold mt-2", "⚠️ Más del 40% de las veces decae tras un gol — trabajar con el entrenador.")
              else if (pctSaludable >= 50) div(cls := "xx-small text-success fw-bold mt-2", "✅ Héctor muestra señales de regulación emocional saludable")
              else div(),
              script(raw(s"""
                var ctxRE = document.getElementById('chartRegulacionEmocional');
                if (ctxRE) {
                  new Chart(ctxRE, { type: 'doughnut',
                    data: { labels: $labelsJs, datasets: [{ data: $dataJs,
                      backgroundColor: ['#0dcaf0','#20c997','#dc3545','#6c757d','#ffc107','#8b5cf6'] }] },
                    options: { responsive:true, maintainAspectRatio:false, plugins:{ legend:{ position:'bottom', labels:{color:'#eee', font:{size:10}} } } }
                  });
                }
              """))
            )
          )
        }
      },

      // BLOQUE O: nota media segun si siguio la rutina pre-partido o no
      {
        val ra = DatabaseManager.getRutinaAnalysis()
        if (!ra("suficiente").asInstanceOf[Boolean]) div()
        else {
          val notaSigue = ra("notaMediaSigue").asInstanceOf[Double]
          val notaNoSigue = ra("notaMediaNoSigue").asInstanceOf[Double]
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "🔄 RUTINA PRE-PARTIDO"),
            div(cls := "card-body p-3",
              div(cls := "small text-white", f"Cuando sigue su rutina pre-partido, la nota media es $notaSigue%.1f. Cuando no la sigue, es $notaNoSigue%.1f.")
            )
          )
        }
      },

      // BLOQUE B: impacto de la conducta del padre en la banda — solo visible aqui, nunca en publico
      {
        val cp = DatabaseManager.getConductaPadreAnalysis()
        if (!cp("suficiente").asInstanceOf[Boolean]) div()
        else {
          val mediaConducta = cp("mediaConducta").asInstanceOf[Double]
          val notaInterv = cp("notaMediaIntervencionista").asInstanceOf[Double]
          val notaObs = cp("notaMediaObservador").asInstanceOf[Double]
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "👨 IMPACTO DEL PADRE EN EL RENDIMIENTO"),
            div(cls := "card-body p-3",
              div(cls := "row text-center g-2",
                div(cls := "col-6",
                  div(cls := "xx-small text-muted", "Nota media (banda intervencionista 1-2)"),
                  div(cls := "fw-bold text-warning", if (notaInterv > 0) f"$notaInterv%.1f" else "—")
                ),
                div(cls := "col-6",
                  div(cls := "xx-small text-muted", "Nota media (banda observadora 4-5)"),
                  div(cls := "fw-bold text-success", if (notaObs > 0) f"$notaObs%.1f" else "—")
                )
              ),
              div(cls := "xx-small text-muted mt-2 fst-italic",
                f"Autoevaluación media: $mediaConducta%.1f/5 · dato privado, nunca visible en el perfil público ni en informes de captación.")
            )
          )
        }
      },

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

          // ── BYPASS RATE HISTÓRICO POR TEMPORADA ───────────────────────────
          {
            val bpEvo = DatabaseManager.getBypassRateEvolution()
            if (bpEvo.size >= 2) {
              val bpAniosJson = bpEvo.map(r => s"'${r("anio")}'").mkString("[",",","]")
              val bpMediaJson = bpEvo.map(r => f"${r("bpMedia").asInstanceOf[Double]}%.2f").mkString("[",",","]")
              val bpEficJson  = bpEvo.map(r => f"${r("bpEfic").asInstanceOf[Double]*100}%.1f").mkString("[",",","]")
              frag(
                div(cls:="card bg-dark border-success shadow mb-3",
                  div(cls:="card-header d-flex justify-content-between align-items-center",
                    span(cls:="text-success fw-bold small", "Bypass Rate | Evolución histórica por temporada"),
                    span(cls:="badge bg-success bg-opacity-25 text-success xx-small", "FASE 6.5 ✓")
                  ),
                  div(cls:="card-body p-2",
                    div(cls:="row g-3",
                      div(cls:="col-md-8",
                        tag("canvas")(id:="chartBPEvo", style:="max-height:190px;")
                      ),
                      div(cls:="col-md-4",
                        div(cls:="table-responsive",
                          table(cls:="table table-dark table-sm xx-small mb-0",
                            thead(tr(th("Año"), th("Media/pj"), th("Efic.%"), th("PJ"), th(""))),
                            tbody(frag(bpEvo.zipWithIndex.map { case (r, idx) =>
                              val media = r("bpMedia").asInstanceOf[Double]
                              val efic  = r("bpEfic").asInstanceOf[Double] * 100
                              val pj    = r("pj").asInstanceOf[Int]
                              val trend = if (idx > 0) {
                                val prev = bpEvo(idx - 1)("bpMedia").asInstanceOf[Double]
                                if (media > prev + 0.1) "↑" else if (media < prev - 0.1) "↓" else "→"
                              } else "—"
                              val tc = trend match { case "↑" => "success"; case "↓" => "danger"; case _ => "secondary" }
                              tr(
                                td(cls:="text-muted", r("anio").toString),
                                td(cls:="text-success fw-bold", f"$media%.1f"),
                                td(cls:="text-info", f"$efic%.0f%%"),
                                td(cls:="text-muted", pj.toString),
                                td(cls:=s"text-$tc fw-bold", trend)
                              )
                            }: _*))
                          )
                        ),
                        div(cls:="xx-small text-muted mt-2 fst-italic",
                          "Líneas superadas por partido y eficiencia por temporada."
                        )
                      )
                    )
                  )
                ),
                script(raw(s"""
                  (function() {
                    var ctxBP = document.getElementById('chartBPEvo');
                    if (!ctxBP) return;
                    new Chart(ctxBP, {
                      type: 'bar',
                      data: {
                        labels: $bpAniosJson,
                        datasets: [
                          { label: 'Lineas/partido', data: $bpMediaJson,
                            backgroundColor: 'rgba(40,167,69,0.6)', borderColor: '#28a745', borderWidth: 1, yAxisID: 'y' },
                          { label: 'Eficiencia %', data: $bpEficJson, type: 'line',
                            borderColor: '#0dcaf0', borderWidth: 2, pointRadius: 5,
                            pointBackgroundColor: '#0dcaf0', tension: 0.3, yAxisID: 'y1' }
                        ]
                      },
                      options: {
                        responsive: true, maintainAspectRatio: false,
                        scales: {
                          y:  { ticks: { color: '#28a745' }, grid: { color: '#333' } },
                          y1: { position: 'right', ticks: { color: '#0dcaf0' }, grid: { display: false } },
                          x:  { ticks: { color: '#aaa' }, grid: { display: false } }
                        },
                        plugins: { legend: { labels: { color: '#fff', font: { size: 10 } } } }
                      }
                    });
                  })();
                """))
              )
            } else div()
          },

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
  def strikerClusteringPage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val clusters = DatabaseManager.getStrikerClusters(efectivo)

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
          seasonSelector(temporadasDb, efectivo, "/striker-clustering"),

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
  def scanningRatePage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val conn = DatabaseManager.getConnection()
    val (partidos, avgScan, avgNota, corrData) = try {
      val rs = conn.createStatement().executeQuery(
        "SELECT fecha, rival, nota, scanning_rate, goles_contra, scanning_efectivo " +
          s"FROM matches WHERE status='PLAYED' AND nota > 0 ${DatabaseManager.seasonFilter(efectivo)} " +
          "ORDER BY fecha DESC LIMIT 30")
      var rows = List[(String, String, Double, Int, Int, Int)]()
      while (rs.next()) rows = rows :+ (
        rs.getString("fecha").take(10),
        Option(rs.getString("rival")).getOrElse(""),
        rs.getDouble("nota"),
        rs.getInt("scanning_rate"),
        rs.getInt("goles_contra"),
        rs.getInt("scanning_efectivo")
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
    // BLOQUE G: efectividad del scanning — de los escaneos, cuantos encontraron compañero libre
    val sumScan      = partidos.filter(_._4 > 0).map(_._4).sum
    val sumEfectivo  = partidos.filter(_._4 > 0).map(_._6).sum
    val ratioEfectividad = if (sumScan > 0) sumEfectivo * 100.0 / sumScan else 0.0
    val efectividadJson = partidos.reverse.map { case (_, _, _, scan, _, efectivo) =>
      if (scan > 0) f"${efectivo * 100.0 / scan}%.0f" else "null"
    }.mkString("[", ",", "]")
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
          seasonSelector(temporadasDb, efectivo, "/scanning-rate"),

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

          // BLOQUE G: ratio de efectividad del scanning
          if (conDatos == 0) frag() else div(cls := "card bg-dark border-success shadow mb-3",
            div(cls := "card-body p-3 text-center",
              div(cls := "xx-small text-muted fw-bold", "EFECTIVIDAD DEL SCANNING"),
              div(cls := "display-5 fw-black text-success", f"$ratioEfectividad%.0f%%"),
              div(cls := "xx-small text-muted mt-1",
                "% de escaneos que terminaron en encontrar un compañero libre. Escanear mucho sin encontrar opciones vale menos que escanear lo justo y decidir bien — este ratio mide la calidad de la lectura, no solo la cantidad.")
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

          // BLOQUE G: frecuencia vs efectividad, dos ejes Y
          if (conDatos >= 3) div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              "Frecuencia vs Efectividad del scanning — últimos 30 partidos"),
            div(cls := "card-body p-3",
              div(style := "height:220px;",
                tag("canvas")(id := "scanEfectividadChart")
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
                    th(cls := "xx-small text-muted text-center", "EFECTIVOS"),
                    th(cls := "xx-small text-muted text-center", "GC"),
                    th(cls := "xx-small text-muted text-center", "NOTA")
                  )),
                  tbody(
                    frag(partidos.map { case (fecha, rival, nota, scan, gc, efectivo) =>
                      val notaCls = if (nota >= 7) "success" else if (nota >= 5) "warning" else "danger"
                      tr(
                        td(cls := "xx-small text-muted", fecha),
                        td(cls := "xx-small", rival),
                        td(cls := s"xx-small text-center fw-bold text-info",
                          if (scan > 0) scan.toString else "—"),
                        td(cls := s"xx-small text-center fw-bold text-success",
                          if (scan > 0) efectivo.toString else "—"),
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

              var ctxEf = document.getElementById('scanEfectividadChart');
              if (ctxEf) {
                new Chart(ctxEf.getContext('2d'), {
                  data: {
                    labels: $labelsJson,
                    datasets: [
                      { type: 'bar', label: 'Escaneos (frecuencia)', data: $scanJson,
                        backgroundColor: 'rgba(13,202,240,0.4)', borderColor: 'rgba(13,202,240,0.8)', yAxisID: 'yFrec' },
                      { type: 'line', label: '% Efectividad', data: $efectividadJson,
                        borderColor: 'rgba(40,167,69,0.9)', tension: 0.3, pointRadius: 4, spanGaps: true, yAxisID: 'yEfec' }
                    ]
                  },
                  options: {
                    responsive: true, maintainAspectRatio: false,
                    plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                    scales: {
                      x: { ticks: { color: '#888', font: { size: 10 } }, grid: { color: '#333' } },
                      yFrec: { position: 'left', beginAtZero: true, ticks: { color: '#0dcaf0', stepSize: 1 }, grid: { color: '#333' } },
                      yEfec: { position: 'right', min: 0, max: 100, ticks: { color: '#28a745' }, grid: { drawOnChartArea: false } }
                    }
                  }
                });
              }
            })();
          """))
        )
      )
    ))
  }

  // == PSxG DELTA ==============================================================
  @cask.get("/psxg-delta")
  def psxgDeltaPage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val d = DatabaseManager.getPSxGDeltaData(efectivo)

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
          seasonSelector(temporadasDb, efectivo, "/psxg-delta"),

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
  def redZonePage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val d = DatabaseManager.getRedZoneData(efectivo)

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
          seasonSelector(temporadasDb, efectivo, "/red-zone"),

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

          // BLOQUE I: firma de fatiga personal — solo con >=8 partidos con ACWR alto y rubrica completa
          {
            val firma = DatabaseManager.getFirmaFatiga()
            if (!firma("suficiente").asInstanceOf[Boolean]) frag()
            else {
              val comparativa = firma("comparativa").asInstanceOf[List[Map[String, Any]]]
              val dimensionPrincipal = firma("firmaFatiga").asInstanceOf[String]
              div(cls := "card bg-dark border-danger shadow mb-3",
                div(cls := "card-header text-danger fw-bold small", "🔬 FIRMA DE FATIGA PERSONAL"),
                div(cls := "card-body p-3",
                  div(cls := "small text-white mb-2", s"Cuando Héctor llega cargado, lo primero que falla es: ", strong(dimensionPrincipal)),
                  div(cls := "table-responsive",
                    table(cls := "table table-sm table-dark mb-0",
                      thead(tr(th("Dimensión"), th(cls:="text-center","Media normal"), th(cls:="text-center","Media bajo fatiga"), th(cls:="text-center","Diferencia"))),
                      tbody(
                        comparativa.map { c =>
                          tr(
                            td(c("dimension").asInstanceOf[String]),
                            td(cls:="text-center", f"${c("normal").asInstanceOf[Double]}%.1f"),
                            td(cls:="text-center", f"${c("cansado").asInstanceOf[Double]}%.1f"),
                            td(cls:="text-center", f"${c("diferencia").asInstanceOf[Double]}%.1f")
                          )
                        }
                      )
                    )
                  )
                )
              )
            }
          },

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

  // == MATCH CONTEXT ANALYTICS =================================================
  @cask.get("/match-context")
  def matchContextPage(request: cask.Request, temporadaId: Int = 0) = withAuth(request) {
    val temporadasDb = DatabaseManager.getTodasTemporadas()
    val efectivo = if (temporadaId > 0) temporadaId else DatabaseManager.getTemporadaActivaId()
    val d = DatabaseManager.getMatchContextData(efectivo)

    val porTipo     = d("porTipo").asInstanceOf[List[Map[String, Any]]]
    val porClima    = d("porClima").asInstanceOf[List[Map[String, Any]]]
    val porDuracion = d("porDuracion").asInstanceOf[List[Map[String, Any]]]
    val localNota   = d("localNota").asInstanceOf[Double]
    val localGC     = d("localGC").asInstanceOf[Double]
    val localPJ     = d("localPJ").asInstanceOf[Int]
    val localLimpias= d("localLimpias").asInstanceOf[Int]
    val visitNota   = d("visitNota").asInstanceOf[Double]
    val visitGC     = d("visitGC").asInstanceOf[Double]
    val visitPJ     = d("visitPJ").asInstanceOf[Int]
    val visitLimpias= d("visitLimpias").asInstanceOf[Int]
    val trendLabels = d("trendLabels").asInstanceOf[List[String]]
    val trendNotas  = d("trendNotas").asInstanceOf[List[Double]]
    val trendPJs    = d("trendPJs").asInstanceOf[List[Int]]
    val mejorCtx    = d("mejorCtx").asInstanceOf[Option[(String, Double, Int)]]
    val peorCtx     = d("peorCtx").asInstanceOf[Option[(String, Double, Int)]]
    val totalPJ     = d("totalPJ").asInstanceOf[Int]
    val notaGlobal  = d("notaGlobal").asInstanceOf[Double]
    val gcGlobal    = d("gcGlobal").asInstanceOf[Double]

    val notaGlobalStr = f"$notaGlobal%.1f"
    val gcGlobalStr   = f"$gcGlobal%.1f"

    // helpers de color
    def notaColor(n: Double): String =
      if (n >= notaGlobal + 0.5) "success"
      else if (n >= notaGlobal - 0.3) "warning"
      else "danger"
    def gcColor(gc: Double): String =
      if (gc <= gcGlobal - 0.3) "success"
      else if (gc <= gcGlobal + 0.3) "warning"
      else "danger"

    // icono clima
    def climaIcon(c: String): String = c.toLowerCase match {
      case s if s.contains("sol")    => "☀️"
      case s if s.contains("lluv")   => "🌧️"
      case s if s.contains("frio") || s.contains("frío") => "🥶"
      case s if s.contains("nub")    => "☁️"
      case s if s.contains("vient")  => "💨"
      case _                         => "🌤️"
    }

    // JSON para gráfico tendencia mensual
    val trendLabelsJson = trendLabels.map(l => s"'$l'").mkString("[", ",", "]")
    val trendNotasJson  = trendNotas.map(v => f"$v%.2f").mkString("[", ",", "]")
    val trendPJsJson    = trendPJs.map(_.toString).mkString("[", ",", "]")
    val globalLineJson  = trendLabels.map(_ => f"$notaGlobal%.2f").mkString("[", ",", "]")

    // Tabla genérica para los 3 bloques de contexto
    def contextTable(rows: List[Map[String, Any]], labelKey: String, labelIcon: String => String = identity) =
      if (rows.isEmpty)
        div(cls:="text-center text-muted small py-3", "Sin datos suficientes")
      else
        div(cls:="table-responsive",
          table(cls:="table table-dark table-sm table-hover mb-0 xx-small align-middle",
            thead(tr(
              th("Contexto"), th("PJ"), th("Nota ø"), th("GC ø"), th("Limpias"), th("vs media")
            )),
            tbody(
              frag(rows.map { r =>
                val nota   = r("nota").asInstanceOf[Double]
                val gc     = r("gc").asInstanceOf[Double]
                val pj     = r("pj").asInstanceOf[Int]
                val cs     = r("limpias").asInstanceOf[Int]
                val label  = labelIcon(r(labelKey).toString)
                val diff   = nota - notaGlobal
                val diffStr = (if (diff >= 0) "+" else "") + f"$diff%.1f"
                val nc     = notaColor(nota)
                val badge  = if (diff >= 0.5) "bg-success" else if (diff >= -0.3) "bg-warning text-dark" else "bg-danger"
                tr(
                  td(cls:="text-white fw-bold", label),
                  td(cls:="text-muted", pj.toString),
                  td(cls:=s"text-$nc fw-bold", f"$nota%.1f"),
                  td(cls:=s"text-${gcColor(gc)}", f"$gc%.1f"),
                  td(cls:="text-info", s"$cs / $pj"),
                  td(span(cls:=s"badge $badge", diffStr))
                )
              }: _*)
            )
          )
        )

    renderHtml(basePage("history",
      div(cls:="row justify-content-center",
        div(cls:="col-md-11 col-12",

          // Header
          div(cls:="d-flex justify-content-between align-items-center mb-3",
            div(
              h2(cls:="text-warning mb-0", "MATCH CONTEXT | Rendimiento por Entorno"),
              span(cls:="badge bg-dark border border-warning text-warning", "FASE 7")
            ),
            a(href:="/dashboard", cls:="btn btn-outline-secondary btn-sm fw-bold", "Dashboard")
          ),
          seasonSelector(temporadasDb, efectivo, "/match-context"),

          // Banner resumen
          if (totalPJ >= 3) div(cls:="row g-2 mb-3",
            frag(Seq(
              ("Total partidos", totalPJ.toString, "secondary"),
              ("Nota media global", notaGlobalStr, "warning"),
              ("GC medio global", gcGlobalStr, "danger"),
              ("Mejor entorno", mejorCtx.map(c => s"${c._1} (${f"${c._2}%.1f"})").getOrElse("—"), "success"),
              ("Peor entorno",  peorCtx.map(c => s"${c._1} (${f"${c._2}%.1f"})").getOrElse("—"), "danger")
            ).map { case (lbl, v, c) =>
              div(cls:="col",
                div(cls:=s"card bg-dark border-$c text-center p-2 h-100",
                  div(cls:=s"fw-bold text-$c", v),
                  div(cls:="xx-small text-muted", lbl)
                )
              )
            }: _*)
          ) else div(),

          // Fila 1: TIPO + CLIMA
          div(cls:="row g-3 mb-3",
            div(cls:="col-md-6",
              div(cls:="card bg-dark border-warning shadow h-100",
                div(cls:="card-header text-warning fw-bold small",
                  "🏆 Por tipo de partido"
                ),
                div(cls:="card-body p-2",
                  contextTable(porTipo, "tipo")
                )
              )
            ),
            div(cls:="col-md-6",
              div(cls:="card bg-dark border-info shadow h-100",
                div(cls:="card-header text-info fw-bold small",
                  "🌤️ Por condición climática"
                ),
                div(cls:="card-body p-2",
                  contextTable(porClima, "clima", climaIcon)
                )
              )
            )
          ),

          // Fila 2: LOCAL vs VISITANTE + DURACIÓN
          div(cls:="row g-3 mb-3",

            // Local vs Visitante
            div(cls:="col-md-5",
              div(cls:="card bg-dark border-success shadow h-100",
                div(cls:="card-header text-success fw-bold small", "🏟️ Local vs Visitante"),
                div(cls:="card-body p-3",
                  if (localPJ + visitPJ == 0)
                    div(cls:="text-center text-muted small py-3",
                      div(style:="font-size:32px; opacity:0.3", "🏟️"),
                      div(cls:="mt-2", "Indica si es local o visitante al registrar el partido"),
                      div(cls:="xx-small text-secondary mt-1",
                        "El selector LOCAL / VISITANTE está en el Match Center, junto al campo Estadio")
                    )
                  else frag(
                    div(cls:="row g-2 text-center",
                      div(cls:="col-6",
                        div(cls:="p-3 rounded h-100",
                          style:=s"border:2px solid #28a745; background:rgba(40,167,69,0.08);",
                          div(cls:="xx-small text-muted fw-bold mb-1", "LOCAL"),
                          if (localPJ == 0)
                            div(cls:="text-muted small py-2", "Sin datos")
                          else frag(
                            div(cls:=s"fw-black text-${notaColor(localNota)}",
                              style:="font-size:2rem;", f"$localNota%.1f"),
                            div(cls:="xx-small text-muted", "nota media"),
                            div(cls:="mt-2 xx-small",
                              span(cls:="text-muted", "GC: "),
                              span(cls:=s"text-${gcColor(localGC)} fw-bold", f"$localGC%.1f")
                            ),
                            div(cls:="xx-small text-info mt-1",
                              s"$localLimpias limpias / $localPJ PJ")
                          )
                        )
                      ),
                      div(cls:="col-6",
                        div(cls:="p-3 rounded h-100",
                          style:="border:2px solid #0dcaf0; background:rgba(13,202,240,0.08);",
                          div(cls:="xx-small text-muted fw-bold mb-1", "VISITANTE"),
                          if (visitPJ == 0)
                            div(cls:="text-muted small py-2", "Sin datos")
                          else frag(
                            div(cls:=s"fw-black text-${notaColor(visitNota)}",
                              style:="font-size:2rem;", f"$visitNota%.1f"),
                            div(cls:="xx-small text-muted", "nota media"),
                            div(cls:="mt-2 xx-small",
                              span(cls:="text-muted", "GC: "),
                              span(cls:=s"text-${gcColor(visitGC)} fw-bold", f"$visitGC%.1f")
                            ),
                            div(cls:="xx-small text-info mt-1",
                              s"$visitLimpias limpias / $visitPJ PJ")
                          )
                        )
                      )
                    ),
                    if (localPJ >= 2 && visitPJ >= 2) {
                      val diff = localNota - visitNota
                      val msg = if (diff > 0.5) "Rinde claramente mejor en casa."
                      else if (diff < -0.5) "Rinde mejor fuera de casa — inusual y positivo."
                      else "Rendimiento equilibrado local/visitante."
                      div(cls:="mt-3 p-2 rounded xx-small text-center",
                        style:="background:rgba(255,255,255,0.04);",
                        span(cls:="text-white fst-italic", msg)
                      )
                    } else div()
                  )
                )
              )
            ),

            // Duración
            div(cls:="col-md-7",
              div(cls:="card bg-dark border-secondary shadow h-100",
                div(cls:="card-header text-white fw-bold small", "⏱️ Por duración del partido"),
                div(cls:="card-body p-2",
                  contextTable(porDuracion, "franja")
                )
              )
            )
          ),

          // Fila 3: Tendencia mensual
          if (trendLabels.size >= 2)
            div(cls:="card bg-dark border-secondary shadow mb-3",
              div(cls:="card-header text-white fw-bold small",
                "📈 Tendencia mensual — últimos 12 meses"
              ),
              div(cls:="card-body p-2",
                tag("canvas")(id:="chartTrend", style:="max-height:220px;")
              )
            )
          else div(),

          // Nota de uso
          div(cls:="alert alert-dark border-secondary xx-small text-muted mt-2",
            "💡 Los contextos con menos de 2 partidos se excluyen del análisis comparativo. ",
            "Cuantos más partidos registres, más precisas serán las comparativas. ",
            "La columna 'vs media' compara cada entorno con tu nota global de temporada."
          ),

          // Script gráfico tendencia
          if (trendLabels.size >= 2) frag(
            script(src:="https://cdn.jsdelivr.net/npm/chart.js"),
            script(raw(s"""
              var ctxTrend = document.getElementById('chartTrend');
              if (ctxTrend) {
                new Chart(ctxTrend, {
                  type: 'line',
                  data: {
                    labels: $trendLabelsJson,
                    datasets: [
                      {
                        label: 'Nota media',
                        data: $trendNotasJson,
                        borderColor: '#ffc107',
                        backgroundColor: 'rgba(255,193,7,0.12)',
                        borderWidth: 2,
                        tension: 0.3,
                        pointRadius: 5,
                        pointBackgroundColor: '#ffc107',
                        yAxisID: 'y'
                      },
                      {
                        label: 'Media global (' + $notaGlobal.toFixed(1) + ')',
                        data: $globalLineJson,
                        borderColor: 'rgba(255,255,255,0.25)',
                        borderWidth: 1,
                        borderDash: [6,4],
                        pointRadius: 0,
                        yAxisID: 'y'
                      },
                      {
                        label: 'Partidos',
                        data: $trendPJsJson,
                        type: 'bar',
                        backgroundColor: 'rgba(13,202,240,0.15)',
                        borderColor: 'rgba(13,202,240,0.4)',
                        borderWidth: 1,
                        yAxisID: 'y1'
                      }
                    ]
                  },
                  options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    scales: {
                      y: {
                        min: 4, max: 10,
                        ticks: { color: '#aaa' },
                        grid: { color: '#333' }
                      },
                      y1: {
                        position: 'right',
                        ticks: { color: '#0dcaf0', stepSize: 1 },
                        grid: { display: false }
                      },
                      x: { ticks: { color: '#888' }, grid: { display: false } }
                    },
                    plugins: {
                      legend: { labels: { color: '#fff', font: { size: 10 } } },
                      tooltip: { mode: 'index', intersect: false }
                    }
                  }
                });
              }
            """))
          ) else span()
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
    // BLOQUE A: Goal Coverage Mapping — usa la proyeccion adulta ya calculada arriba para consistencia
    val gc = DatabaseManager.calcularGoalCoverage(d("alturaProyectada").asInstanceOf[Double])
    // BLOQUE E: Markov Career Pathing — None si hay menos de 2 temporadas cerradas
    val markov = DatabaseManager.calcularMarkovPathway()
    // BLOQUE RFFM: posicion real de Hector en la categoria — None si aun no hay >=10 equipos sincronizados
    val rffmPercentil = DatabaseManager.getPercentilRealHector()

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

          // BLOQUE A: GOAL COVERAGE MAPPING (geometria pura, sin IA) ──────────
          div(cls:="card bg-dark border-primary shadow mb-3",
            div(cls:="card-header text-primary fw-bold small", "📐 COBERTURA DE PORTERÍA"),
            div(cls:="card-body p-3",
              {
                val tallaCm = gc("tallaCm").asInstanceOf[Double]
                val enverCm = gc("envergaduraCm").asInstanceOf[Double]
                val alcanceCm = gc("alcanceVerticalCm").asInstanceOf[Double]
                val pctBase = gc("pctCoberturaBase").asInstanceOf[Double]
                val pctEstirada = gc("pctCoberturaEstirada").asInstanceOf[Double]
                val tallaAdultaCmGc = gc("tallaAdultaCm").asInstanceOf[Double]
                val pctAdulto = gc("pctCoberturaAdulto").asInstanceOf[Double]
                val pAncho = gc("porteriaAncho").asInstanceOf[Double]
                val pAlto = gc("porteriaAlto").asInstanceOf[Double]

                // Geometria SVG: viewBox 500x200 -> 100px = 1m
                val anchoEstiradaM = math.min((enverCm / 100.0) + (enverCm / 200.0), pAncho)
                val anchoBaseM = math.min(enverCm / 100.0, pAncho)
                val altoM = math.min(alcanceCm / 100.0, pAlto)
                val wEstiradaPx = anchoEstiradaM * 100
                val wBasePx = anchoBaseM * 100
                val hPx = altoM * 100
                val xEstirada = (500 - wEstiradaPx) / 2
                val xBase = (500 - wBasePx) / 2
                val yTop = 200 - hPx

                div(
                  div(cls:="text-center mb-3",
                    tag("svg")(attr("viewBox") := "0 0 500 200", attr("width") := "100%", style := "max-width:500px; background:#0f172a; border-radius:8px;",
                      // Marco porteria
                      tag("rect")(attr("x") := "4", attr("y") := "4", attr("width") := "492", attr("height") := "192",
                        attr("fill") := "none", attr("stroke") := "white", attr("stroke-width") := "4"),
                      // Cobertura con estirada (azul claro)
                      tag("rect")(attr("x") := xEstirada.toString, attr("y") := yTop.toString,
                        attr("width") := wEstiradaPx.toString, attr("height") := hPx.toString,
                        attr("fill") := "rgba(13,202,240,0.35)"),
                      // Cobertura base (azul mas intenso)
                      tag("rect")(attr("x") := xBase.toString, attr("y") := yTop.toString,
                        attr("width") := wBasePx.toString, attr("height") := hPx.toString,
                        attr("fill") := "rgba(13,110,253,0.55)"),
                      // Figura estilizada del portero (centro)
                      tag("circle")(attr("cx") := "250", attr("cy") := (200 - hPx * 0.55).toString, attr("r") := "9", attr("fill") := "#ffc107"),
                      tag("line")(attr("x1") := "250", attr("y1") := (200 - hPx * 0.55 + 9).toString, attr("x2") := "250", attr("y2") := "196",
                        attr("stroke") := "#ffc107", attr("stroke-width") := "4"),
                      // Linea de suelo
                      tag("line")(attr("x1") := "0", attr("y1") := "198", attr("x2") := "500", attr("y2") := "198",
                        attr("stroke") := "#475569", attr("stroke-width") := "2")
                    )
                  ),
                  div(cls:="row g-2 text-center mb-3",
                    div(cls:="col-4",
                      div(cls:="card bg-dark border-primary h-100", div(cls:="card-body p-2",
                        div(cls:="xx-small text-muted", "Cobertura base"),
                        div(cls:="fw-bold text-primary fs-5", f"$pctBase%.0f%%")))),
                    div(cls:="col-4",
                      div(cls:="card bg-dark border-info h-100", div(cls:="card-body p-2",
                        div(cls:="xx-small text-muted", "Con estirada"),
                        div(cls:="fw-bold text-info fs-5", f"$pctEstirada%.0f%%")))),
                    div(cls:="col-4",
                      div(cls:="card bg-dark border-warning h-100", div(cls:="card-body p-2",
                        div(cls:="xx-small text-muted", "Adulta proyectada"),
                        div(cls:="fw-bold text-warning fs-5", f"$pctAdulto%.0f%%"))))
                  ),
                  div(cls:="small text-light",
                    f"Con su altura actual de ${tallaCm.toInt}cm y una envergadura de ${enverCm.toInt}cm, Héctor cubre el $pctBase%.0f%% de la portería sin moverse. Con estirada lateral cubre el $pctEstirada%.0f%%. A su altura adulta proyectada de ${tallaAdultaCmGc.toInt}cm, cubrirá el $pctAdulto%.0f%%."),
                  div(cls:="xx-small text-muted mt-2 fst-italic",
                    s"Portería Fútbol 7 Prebenjamín: ${pAncho.toInt}m × ${pAlto.toInt}m (reglamento RFFM)")
                )
              }
            )
          ),

          // MODULO ARQUETIPO: linea junto al Markov con la proyeccion a talla adulta
          {
            val arq = DatabaseManager.calcularArquetipoPortero()
            if (!arq("activo").asInstanceOf[Boolean]) div()
            else {
              val desc = DatabaseManager.arquetipoDescripcion(arq("dominante").asInstanceOf[String])
              div(cls := "card bg-dark border-secondary shadow mb-3 p-3",
                div(cls := "xx-small text-white",
                  s"🎭 Arquetipo proyectado a la talla adulta: dado su perfil ${desc("nombre")} y su talla proyectada de ${alturaProyStr}cm, encaja especialmente en ${desc("sistema_ideal")}")
              )
            }
          },

          // MODULO LA VOZ DEL PORTERO: linea de caritas junto al arco completo, si hay 6+ meses de historial
          {
            val hist = DatabaseManager.getVozPorteroHistorial()
            if (hist.size < 6) div()
            else {
              val linea = hist.reverse.map(h => DatabaseManager.caritaEmoji(h("motivacionCarita").asInstanceOf[Int])).mkString(" ")
              div(cls := "card bg-dark border-secondary shadow mb-3 p-3",
                div(cls := "xx-small text-white", s"🎤 Motivación declarada a lo largo del tiempo: $linea")
              )
            }
          },

          // BLOQUE E: RUTA DE CARRERA (MARKOV) ────────────────────────────────
          div(cls:="card bg-dark border-info shadow mb-3",
            div(cls:="card-header text-info fw-bold small", "🗺️ RUTA DE CARRERA (Markov)"),
            div(cls:="card-body p-3",
              markov match {
                case None => div(cls:="text-center text-muted small py-3",
                  "🗺️ Este módulo se activará cuando haya 2 temporadas completas registradas.")
                case Some(m) =>
                  val estados = m("estados").asInstanceOf[List[String]]
                  val idxActual = m("estadoActualIdx").asInstanceOf[Int]
                  val estadoActual = m("estadoActual").asInstanceOf[String]
                  val siguienteEstado = m("siguienteEstado").asInstanceOf[String]
                  val prob2Temp = m("probabilidad2Temp").asInstanceOf[Int]
                  val velocidad = m("velocidadMejora").asInstanceOf[Double]
                  val temporadasHasta = m("temporadasHastaSiguiente").asInstanceOf[Option[Double]]
                  val temporadaEstimada = m("temporadaEstimada").asInstanceOf[String]
                  val nTemp = m("nTemporadas").asInstanceOf[Int]

                  div(
                    // Linea de estados horizontales
                    div(cls:="d-flex justify-content-between align-items-center mb-3 flex-wrap gap-1",
                      estados.zipWithIndex.map { case (est, i) =>
                        frag(
                          span(cls:=s"badge ${if (i == idxActual) "bg-info text-dark" else "bg-dark border border-secondary text-muted"} small",
                            style:=(if (i == idxActual) "font-size:11px; padding:6px 10px;" else "font-size:10px;"), est),
                          if (i < estados.size - 1) span(cls:="text-secondary mx-1", "→") else frag()
                        )
                      }
                    ),
                    div(cls:="row g-2 text-center mb-2",
                      div(cls:="col-6",
                        div(cls:="card bg-dark border-info h-100", div(cls:="card-body p-2",
                          div(cls:="xx-small text-muted", if (siguienteEstado.nonEmpty) s"Prob. alcanzar $siguienteEstado en 2 temporadas" else "Ya en el nivel máximo"),
                          div(cls:="fw-bold text-info fs-5", if (siguienteEstado.nonEmpty) s"$prob2Temp%" else "—")))),
                      div(cls:="col-6",
                        div(cls:="card bg-dark border-warning h-100", div(cls:="card-body p-2",
                          div(cls:="xx-small text-muted", "Velocidad de mejora"),
                          div(cls:="fw-bold text-warning fs-5", f"${if (velocidad>=0) "+" else ""}$velocidad%.1f pts/temp"))))
                    ),
                    if (temporadasHasta.nonEmpty) div(cls:="xx-small text-muted mb-2",
                      f"Temporadas estimadas hasta $siguienteEstado: ${temporadasHasta.get}%.1f temporadas")
                    else div(),
                    div(cls:="small text-light mt-2",
                      s"Basado en $nTemp temporadas de datos reales." +
                      (if (siguienteEstado.nonEmpty && temporadaEstimada.nonEmpty)
                        s" Con el ritmo actual, Héctor podría alcanzar el nivel $siguienteEstado en la temporada $temporadaEstimada."
                       else if (siguienteEstado.isEmpty) " Héctor ya se encuentra en el nivel formativo más alto del modelo." else "")
                    )
                  )
              }
            )
          ),

          // BLOQUE L: ARCO COMPLETO DE CARRERA — linea de tiempo con hitos formativos
          {
            val arco = DatabaseManager.getArcoCompletoData()
            val edadActualArco = arco("edadActual").asInstanceOf[Int]
            val categorias = arco("categorias").asInstanceOf[List[(String, Int)]]
            val tieneMarkov = arco("tieneMarkov").asInstanceOf[Boolean]
            val edadProyAcademiaPrimera = arco("edadProyectadaAcademiaPrimera").asInstanceOf[Option[Int]]

            val edadMin = 6.0; val edadMax = 20.0
            def xDe(edad: Double): Double = 30 + (edad - edadMin) / (edadMax - edadMin) * 540

            val puntosHtml = categorias.map { case (nombre, edadInicio) =>
              val x = xDe(edadInicio.toDouble)
              s"""<circle cx="$x" cy="60" r="5" fill="#334155" stroke="#94a3b8" stroke-width="1.5"/>
                  <text x="$x" y="82" text-anchor="middle" font-size="10" fill="#94a3b8">$nombre</text>
                  <text x="$x" y="94" text-anchor="middle" font-size="9" fill="#64748b">$edadInicio años</text>"""
            }.mkString("\n")

            val xHoy = xDe(edadActualArco.toDouble)
            val hoyHtml =
              s"""<line x1="$xHoy" y1="15" x2="$xHoy" y2="60" stroke="#facc15" stroke-width="2" stroke-dasharray="3,2"/>
                  <circle cx="$xHoy" cy="60" r="7" fill="#facc15" stroke="#1e293b" stroke-width="2"/>
                  <text x="$xHoy" y="12" text-anchor="middle" font-size="10" font-weight="bold" fill="#facc15">ESTÁS AQUÍ</text>"""

            val proyeccionHtml = if (tieneMarkov && edadProyAcademiaPrimera.nonEmpty) {
              val xProy = xDe(math.min(edadMax, edadProyAcademiaPrimera.get.toDouble))
              s"""<line x1="$xProy" y1="60" x2="$xProy" y2="105" stroke="#0dcaf0" stroke-width="2" stroke-dasharray="3,2"/>
                  <circle cx="$xProy" cy="60" r="6" fill="#0dcaf0"/>
                  <text x="$xProy" y="118" text-anchor="middle" font-size="9" fill="#0dcaf0">Academia Primera (proy.)</text>"""
            } else ""

            val textoProyeccion: Modifier = if (tieneMarkov && edadProyAcademiaPrimera.nonEmpty)
              div(cls := "small text-light mt-2 text-center", s"A este ritmo, Héctor podría estar en nivel Academia Primera a los ${edadProyAcademiaPrimera.get} años.")
            else if (!tieneMarkov)
              div(cls := "xx-small text-muted mt-2 text-center", "Las proyecciones se activarán con 2 temporadas completas registradas.")
            else div()

            div(cls := "card bg-dark border-secondary shadow mb-3",
              div(cls := "card-header text-white fw-bold small", "🗺️ ARCO COMPLETO DE CARRERA"),
              div(cls := "card-body p-3",
                raw(s"""<svg viewBox="0 0 600 130" style="width:100%; height:auto;">
                  <line x1="30" y1="60" x2="570" y2="60" stroke="#334155" stroke-width="2"/>
                  $puntosHtml
                  $hoyHtml
                  $proyeccionHtml
                </svg>"""),
                textoProyeccion
              )
            )
          },

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

          // BLOQUE RFFM: posicion real de Hector en la categoria Prebenjamin F7
          rffmPercentil match {
            case None => div()
            case Some(p) =>
              val mediaGc = p("mediaGcHector").asInstanceOf[Double]
              val percentil = p("percentilGC").asInstanceOf[Int]
              val totalEquipos = p("totalEquipos").asInstanceOf[Int]
              val totalPartidos = p("totalPartidos").asInstanceOf[Int]
              val fuente = p("fuenteDatos").asInstanceOf[String]
              div(cls := "card bg-dark border-info shadow mb-3",
                div(cls := "card-header text-info fw-bold small", "📊 POSICIÓN EN LA CATEGORÍA"),
                div(cls := "card-body p-3",
                  div(cls := "small text-white mb-2",
                    f"Héctor encaja $mediaGc%.1f goles/partido de media. El $percentil%% de los equipos de Prebenjamín F7 Madrid encajan más."),
                  div(cls := "progress mb-2", style := "height:14px;",
                    div(cls := "progress-bar bg-info fw-bold", style := s"width:$percentil%;", s"P$percentil")
                  ),
                  div(cls := "xx-small text-muted", s"Datos de $totalEquipos equipos · $totalPartidos partidos · $fuente")
                )
              )
          },

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

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — NLP SCOUTING AGGREGATOR
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.get("/scouting/nlp")
  def scoutingNlpPage(request: cask.Request) = withAuth(request) {
    val reports = DatabaseManager.getScoutReports()

    def proyColor(p: String): String = p match {
      case "ELITE"     => "danger"
      case "PRIMERA"   => "warning"
      case "SEGUNDA"   => "info"
      case "REGIONAL"  => "secondary"
      case _           => "secondary"
    }
    def recColor(r: String): String = r match {
      case "FICHAR_YA"       => "success"
      case "SEGUIMIENTO_6M"  => "warning"
      case "SEGUIMIENTO_12M" => "info"
      case "DESCARTAR"       => "danger"
      case _                 => "secondary"
    }
    def recLabel(r: String): String = r match {
      case "FICHAR_YA"       => "✅ FICHAR YA"
      case "SEGUIMIENTO_6M"  => "👁 SEGUIM. 6M"
      case "SEGUIMIENTO_12M" => "👁 SEGUIM. 12M"
      case "DESCARTAR"       => "❌ DESCARTAR"
      case _                 => r
    }

    val pageContent = basePage("scouting-nlp", div(
      p(cls := "text-muted small mb-4",
        "Pega el texto de cualquier informe de ojeador — Gemini extrae automáticamente valoraciones, proyección y recomendación."),

      // ── Formulario ──────────────────────────────────────────────────────────
      div(cls := "card bg-dark border-primary p-3 mb-4",
        div(cls := "fw-bold text-primary small text-uppercase mb-3", "📝 Nuevo Informe"),
        div(cls := "row g-2 mb-2",
          div(cls := "col-md-4",
            label(cls := "form-label text-muted small", "Ojeador (opcional)"),
            input(`type` := "text", id := "inp-ojeador", cls := "form-control bg-dark text-white border-secondary",
              placeholder := "Nombre del ojeador")
          ),
          div(cls := "col-md-4",
            label(cls := "form-label text-muted small", "Club origen (opcional)"),
            input(`type` := "text", id := "inp-club", cls := "form-control bg-dark text-white border-secondary",
              placeholder := "Club o academia")
          ),
          div(cls := "col-md-4",
            label(cls := "form-label text-muted small", "Fecha del informe"),
            input(`type` := "date", id := "inp-fecha", cls := "form-control bg-dark text-white border-secondary")
          )
        ),
        div(cls := "mb-2",
          label(cls := "form-label text-muted small", "Texto del informe *"),
          textarea(id := "inp-texto", cls := "form-control bg-dark text-white border-secondary",
            rows := "8",
            placeholder := "Pega aquí el texto completo del informe de scouting...")
        ),
        button(id := "btn-procesar", cls := "btn btn-primary fw-bold",
          onclick := "procesarInforme()",
          "⚡ Procesar con IA"
        ),
        div(id := "nlp-loading", cls := "d-none mt-3",
          div(cls := "d-flex align-items-center gap-2 text-warning",
            div(cls := "spinner-border spinner-border-sm"),
            span("Analizando informe con Gemini...")
          )
        ),
        div(id := "nlp-result", cls := "d-none mt-3")
      ),

      // ── Historial de informes ───────────────────────────────────────────────
      if (reports.nonEmpty)
        div(
          h6(cls := "text-muted text-uppercase small mb-3", s"📋 Historial — ${reports.size} informes"),
          div(cls := "row g-3",
            frag(reports.map { r =>
              val global = r("global").asInstanceOf[Int]
              val proy   = r("proyeccion").asInstanceOf[String]
              val rec    = r("recomendacion").asInstanceOf[String]
              val pc     = proyColor(proy)
              val rc     = recColor(rec)
              div(cls := "col-md-6",
                div(cls := s"card bg-dark border-$rc h-100",
                  div(cls := "card-body p-3",
                    div(cls := "d-flex justify-content-between align-items-start mb-2",
                      div(
                        div(cls := "fw-bold text-white small", {
                          val s = r("ojeador").asInstanceOf[String].take(30)
                          if (s.nonEmpty) s else "Ojeador anónimo"
                        }),
                        div(cls := "xx-small text-muted", {
                          val s = r("club").asInstanceOf[String].take(25)
                          (if (s.nonEmpty) s"$s · " else "") +
                            r("fecha").asInstanceOf[String]
                        }),
                      ),
                      div(cls := "text-end",
                        div(cls := s"badge bg-$pc mb-1", proy),
                        br(),
                        div(cls := s"badge bg-$rc", recLabel(rec))
                      )
                    ),
                    // Radar de 5 atributos
                    div(cls := "d-flex gap-1 mb-2 flex-wrap",
                      frag(Seq(
                        ("TEC", r("tec").asInstanceOf[Int]),
                        ("TAC", r("tac").asInstanceOf[Int]),
                        ("FIS", r("fis").asInstanceOf[Int]),
                        ("MEN", r("men").asInstanceOf[Int]),
                        ("DIS", r("dis").asInstanceOf[Int])
                      ).map { case (lbl, val0) =>
                        val barColor = if (val0 >= 8) "success" else if (val0 >= 6) "warning" else "secondary"
                        div(cls := "text-center", style := "min-width:42px;",
                          div(cls := s"small fw-bold text-$barColor", s"$val0"),
                          div(cls := "progress mb-1", style := "height:6px;",
                            div(cls := s"progress-bar bg-$barColor", style := s"width:${val0 * 10}%")
                          ),
                          div(cls := "xx-small text-muted", lbl)
                        )
                      }: _*)
                    ),
                    div(cls := "d-flex align-items-center gap-2 mb-2",
                      div(cls := "text-muted xx-small", "GLOBAL:"),
                      div(cls := "progress flex-grow-1", style := "height:8px;",
                        div(cls := s"progress-bar bg-${if (global >= 8) "success" else if (global >= 6) "warning" else "secondary"}",
                          style := s"width:${global * 10}%")
                      ),
                      div(cls := s"fw-bold small text-${if (global >= 8) "success" else if (global >= 6) "warning" else "secondary"}",
                        s"$global/10")
                    ),
                    if (r("resumen").asInstanceOf[String].nonEmpty)
                      p(cls := "small text-muted mb-1 fst-italic",
                        raw(r("resumen").asInstanceOf[String].take(200) + "..."))
                    else span(),
                    if (r("fortalezas").asInstanceOf[String].nonEmpty)
                      div(cls := "xx-small",
                        span(cls := "text-success me-1", "✚"),
                        span(cls := "text-muted", r("fortalezas").asInstanceOf[String].take(120))
                      )
                    else span()
                  )
                )
              )
            }: _*)
          )
        )
      else
        div(cls := "alert alert-secondary text-center",
          "No hay informes procesados todavía. Pega el primero arriba."),

      // ── JS ──────────────────────────────────────────────────────────────────
      script(raw("""
        // Fecha por defecto = hoy
        document.getElementById('inp-fecha').value = new Date().toISOString().split('T')[0];

        async function procesarInforme() {
          const texto  = document.getElementById('inp-texto').value.trim();
          const ojeador = document.getElementById('inp-ojeador').value.trim();
          const club    = document.getElementById('inp-club').value.trim();
          const fecha   = document.getElementById('inp-fecha').value;
          if (!texto || texto.length < 30) {
            alert('El texto del informe es demasiado corto. Pega el informe completo.');
            return;
          }
          document.getElementById('btn-procesar').disabled = true;
          document.getElementById('nlp-loading').classList.remove('d-none');
          document.getElementById('nlp-result').classList.add('d-none');

          const params = new URLSearchParams();
          params.append('texto', texto);
          params.append('ojeador', ojeador);
          params.append('club', club);
          params.append('fecha', fecha);

          try {
            const res = await fetch('/scouting/nlp/process', {
              method: 'POST',
              headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
              body: params.toString()
            });
            const json = await res.json();
            if (json.ok) {
              const rc = json.rec === 'FICHAR_YA' ? 'success' :
                         json.rec === 'SEGUIMIENTO_6M' ? 'warning' :
                         json.rec === 'SEGUIMIENTO_12M' ? 'info' : 'danger';
              const pc = json.proy === 'ELITE' ? 'danger' :
                         json.proy === 'PRIMERA' ? 'warning' :
                         json.proy === 'SEGUNDA' ? 'info' : 'secondary';
              const dimHtml = [
                ['Técnico', json.tec], ['Táctico', json.tac], ['Físico', json.fis],
                ['Mental', json.men], ['Distribución', json.dis]
              ].map(([l, v]) => {
                const c = v >= 8 ? 'success' : v >= 6 ? 'warning' : 'secondary';
                return '<div class="col-6 col-md"><div class="card bg-dark border-secondary text-center p-2">' +
                       '<div class="h4 fw-black text-' + c + '">' + v + '/10</div>' +
                       '<div class="xx-small text-muted">' + l + '</div></div></div>';
              }).join('');
              document.getElementById('nlp-result').innerHTML =
                '<div class="card bg-dark border-success p-3">' +
                '<div class="d-flex gap-2 mb-3 flex-wrap">' +
                '<span class="badge bg-' + pc + ' fs-6">' + json.proy + '</span>' +
                '<span class="badge bg-' + rc + ' fs-6">' + json.rec.replace(/_/g,' ') + '</span>' +
                '<span class="badge bg-secondary fs-6">Global: ' + json.global + '/10</span></div>' +
                '<div class="row g-2 mb-3">' + dimHtml + '</div>' +
                '<p class="small text-muted fst-italic mb-2">' + json.resumen + '</p>' +
                '<div class="small"><span class="text-success me-1">✚</span><span class="text-muted">' + json.fort + '</span></div>' +
                '<div class="small mt-1"><span class="text-warning me-1">△</span><span class="text-muted">' + json.areas + '</span></div>' +
                '<hr class="border-secondary"><a href="/scouting/nlp" class="btn btn-sm btn-outline-success">Ver en historial</a></div>';
              document.getElementById('nlp-result').classList.remove('d-none');
              document.getElementById('inp-texto').value = '';
            } else {
              document.getElementById('nlp-result').innerHTML =
                '<div class="alert alert-danger">Error al procesar: ' + (json.error || 'desconocido') + '</div>';
              document.getElementById('nlp-result').classList.remove('d-none');
            }
          } catch(e) {
            document.getElementById('nlp-result').innerHTML =
              '<div class="alert alert-danger">Error de red: ' + e.message + '</div>';
            document.getElementById('nlp-result').classList.remove('d-none');
          }
          document.getElementById('btn-procesar').disabled = false;
          document.getElementById('nlp-loading').classList.add('d-none');
        }
      """))
    )
    )
    renderHtml(pageContent)
  }

  @cask.postForm("/scouting/nlp/process")
  def scoutingNlpProcess(
                          request: cask.Request,
                          texto: String, ojeador: String, club: String, fecha: String
                        ) = withAuth(request) {
    try {
      val result = DatabaseManager.processScoutReport(texto, ojeador, club, fecha)
      val json = ujson.Obj(
        "ok"     -> ujson.True,
        "tec"    -> result("nivel_tecnico").asInstanceOf[Int],
        "tac"    -> result("nivel_tactico").asInstanceOf[Int],
        "fis"    -> result("nivel_fisico").asInstanceOf[Int],
        "men"    -> result("nivel_mental").asInstanceOf[Int],
        "dis"    -> result("nivel_distribucion").asInstanceOf[Int],
        "global" -> result("nivel_global").asInstanceOf[Int],
        "proy"   -> result("proyeccion").asInstanceOf[String],
        "rec"    -> result("recomendacion").asInstanceOf[String],
        "fort"   -> result("fortalezas").asInstanceOf[String],
        "areas"  -> result("areas_mejora").asInstanceOf[String],
        "resumen"-> result("resumen_ia").asInstanceOf[String]
      )
      cask.Response(json.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
    } catch {
      case e: Exception =>
        val json = ujson.Obj("ok" -> ujson.False, "error" -> e.getMessage)
        cask.Response(json.render().getBytes("UTF-8"), statusCode = 500, headers = Seq("Content-Type" -> "application/json"))
    }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — PERIODIZACION NUTRICIONAL REACTIVA
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.get("/nutrition")
  def nutritionPage(request: cask.Request) = withAuth(request) {
    nutritionRender(request, forceRefresh = false)
  }

  @cask.get("/nutrition/refresh")
  def nutritionRefreshPage(request: cask.Request) = withAuth(request) {
    nutritionRender(request, forceRefresh = true)
  }

  private def nutritionRender(request: cask.Request, forceRefresh: Boolean) = {
    val d        = DatabaseManager.getNutritionPlan(forceRefresh)
    val plan     = d("plan").asInstanceOf[String]
    val acwr     = d("acwr").asInstanceOf[Double]
    val rpe      = d("rpe").asInstanceOf[Double]
    val nota     = d("nota").asInstanceOf[Double]
    val faseStr  = d("faseStr").asInstanceOf[String]
    val altura   = d("altura").asInstanceOf[Double]
    val peso     = d("peso").asInstanceOf[Double]
    val cached   = d("cached").asInstanceOf[Boolean]

    val acwrColor = if (acwr > 1.5) "danger" else if (acwr > 1.2) "warning" else if (acwr > 0.8) "success" else "info"
    val acwrLabel = if (acwr > 1.5) "CARGA ALTA" else if (acwr > 1.2) "CARGA ELEVADA" else if (acwr > 0.8) "ÓPTIMO" else "DESCARGA"
    val rpeColor  = if (rpe > 7.5) "danger" else if (rpe > 5.5) "warning" else "success"

    val pageContent = basePage("nutrition", div(
      // Header
      div(cls := "d-flex justify-content-between align-items-start mb-4 flex-wrap gap-2",
        div(
          h4(cls := "fw-black text-white mb-1", "🥗 Periodización Nutricional Reactiva"),
          p(cls := "text-muted small mb-0",
            "Plan semanal generado por IA en función de tu carga real de entrenamiento y rendimiento.")
        ),
        a(href := "/nutrition/refresh", cls := "btn btn-outline-warning btn-sm fw-bold",
          "🔄 Regenerar plan")
      ),

      // KPIs contextuales
      div(cls := "row g-2 mb-4",
        div(cls := "col-6 col-md-3",
          div(cls := s"card bg-dark border-$acwrColor text-center p-3",
            div(cls := s"h4 fw-black text-$acwrColor", f"$acwr%.2f"),
            div(cls := "small text-muted", "ACWR"),
            div(cls := s"badge bg-$acwrColor mt-1", acwrLabel)
          )
        ),
        div(cls := "col-6 col-md-3",
          div(cls := s"card bg-dark border-$rpeColor text-center p-3",
            div(cls := s"h4 fw-black text-$rpeColor", f"$rpe%.1f"),
            div(cls := "small text-muted", "RPE media 7d"),
            div(cls := s"badge bg-$rpeColor mt-1", if (rpe > 7.5) "ALTA INTENSIDAD" else if (rpe > 5.5) "MODERADO" else "SUAVE")
          )
        ),
        div(cls := "col-6 col-md-3",
          div(cls := "card bg-dark border-secondary text-center p-3",
            div(cls := "h4 fw-black text-white", f"$nota%.0f"),
            div(cls := "small text-muted", "Nota último partido"),
            div(cls := s"badge bg-${if (nota >= 70) "success" else if (nota >= 50) "warning" else "danger"} mt-1",
              if (nota >= 70) "BUEN NIVEL" else if (nota >= 50) "NORMAL" else "BAJO")
          )
        ),
        div(cls := "col-6 col-md-3",
          div(cls := "card bg-dark border-secondary text-center p-3",
            div(cls := "h4 fw-black text-white", f"$peso%.1f kg"),
            div(cls := "small text-muted", s"Peso / ${altura.toInt} cm"),
            div(cls := "badge bg-secondary mt-1",
              f"IMC ${peso / math.pow(altura / 100.0, 2)}%.1f")
          )
        )
      ),

      // Badge de cache
      if (cached)
        div(cls := "alert alert-secondary small d-flex align-items-center gap-2 mb-3",
          span("ℹ️"),
          span("Mostrando plan de esta semana en caché. Haz clic en ",
            strong("Regenerar plan"), " para obtener uno nuevo con los datos actuales.")
        )
      else span(),

      // Plan IA
      div(cls := "card bg-dark border-secondary p-4 mb-4",
        div(cls := "text-muted small mb-2",
          span(cls := "me-2", "⚡ Generado por Gemini 2.0 Flash"),
          span(cls := "text-muted", s"· $faseStr")
        ),
        div(cls := "text-white nutrition-plan",
          raw(plan)
        )
      ),

      // Nota metodológica
      div(cls := "alert alert-secondary small",
        raw("""<strong>Nota:</strong> Las recomendaciones nutricionales se generan automáticamente
        en función de tu ACWR, RPE e historial de rendimiento. Consulta siempre con un nutricionista
        deportivo antes de realizar cambios significativos en tu dieta. El plan se cachea 6 días —
        usa "Regenerar" si cambias tu actividad sustancialmente.""")
      )
    )
    )
    renderHtml(pageContent)
  }

  // ── MODULO 7: INFORME DE CAPTACION EXPORTABLE (PRINT / PDF) ─────────────
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A2 — SUBIDA Y ANALISIS DE VIDEO REAL CON GEMINI VISION
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.post("/video/analyze-real/:matchId")
  def analyzeVideoRealAction(request: cask.Request, matchId: Int) = withAuth(request) {
    val contentType = request.exchange.getRequestHeaders.getFirst("Content-Type")
    val bodyBytes = request.data.readAllBytes()
    val maxBytes = 1.8d * 1024 * 1024 * 1024 // 1.8GB

    if (bodyBytes.length.toDouble > maxBytes) {
      val json = ujson.Obj("status" -> "error",
        "error" -> "El vídeo es demasiado grande. Sube solo el fragmento donde aparece Héctor (menos de 15 minutos) para reducir el tamaño.")
      cask.Response(json.render().getBytes("UTF-8"), statusCode = 413, headers = Seq("Content-Type" -> "application/json"))
    } else {
      val fields = parseMultipart(bodyBytes, contentType)
      val videoFieldOpt = fields.get("video").filter(_.data.nonEmpty)
      videoFieldOpt match {
        case None =>
          val json = ujson.Obj("status" -> "error", "error" -> "No se ha recibido ningún vídeo.")
          cask.Response(json.render().getBytes("UTF-8"), statusCode = 400, headers = Seq("Content-Type" -> "application/json"))
        case Some(videoField) =>
          val filenameLower = videoField.filename.getOrElse("video.mp4").toLowerCase
          val mediaType =
            if (filenameLower.endsWith(".webm")) "video/webm"
            else if (filenameLower.endsWith(".mov")) "video/quicktime"
            else "video/mp4"

          val base64Data = java.util.Base64.getEncoder.encodeToString(videoField.data)

          new Thread(new Runnable {
            def run(): Unit = {
              try { DatabaseManager.analyzeVideoReal(matchId, base64Data, mediaType) }
              catch { case _: Exception => () }
            }
          }).start()

          val json = ujson.Obj("status" -> "processing")
          cask.Response(json.render().getBytes("UTF-8"), statusCode = 202, headers = Seq("Content-Type" -> "application/json"))
      }
    }
  }

  // Lectura desde BD unicamente — nunca llama a Gemini
  @cask.get("/video/analyze-status/:matchId")
  def videoAnalyzeStatusAction(matchId: Int) = {
    val status = DatabaseManager.getVideoAnalysisStatus(matchId)
    val json = status.get("status") match {
      case Some("done") => ujson.Obj(
        "status"   -> "done",
        "analisis" -> status("analisis").asInstanceOf[String],
        "fecha"    -> status("fecha").asInstanceOf[String]
      )
      case _ => ujson.Obj("status" -> "pending")
    }
    cask.Response(json.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — SUBIDA Y ANALISIS DE VIDEO REAL EN ENTRENAMIENTOS CON GEMINI VISION
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.post("/video/analyze-training/:trainingId")
  def analyzeVideoTrainingAction(request: cask.Request, trainingId: Int) = withAuth(request) {
    val contentType = request.exchange.getRequestHeaders.getFirst("Content-Type")
    val bodyBytes = request.data.readAllBytes()
    val maxBytes = 1.8d * 1024 * 1024 * 1024 // 1.8GB

    if (bodyBytes.length.toDouble > maxBytes) {
      val json = ujson.Obj("status" -> "error",
        "error" -> "El vídeo es demasiado grande. Sube solo el fragmento donde aparece Héctor (menos de 15 minutos) para reducir el tamaño.")
      cask.Response(json.render().getBytes("UTF-8"), statusCode = 413, headers = Seq("Content-Type" -> "application/json"))
    } else {
      val fields = parseMultipart(bodyBytes, contentType)
      val videoFieldOpt = fields.get("video").filter(_.data.nonEmpty)
      videoFieldOpt match {
        case None =>
          val json = ujson.Obj("status" -> "error", "error" -> "No se ha recibido ningún vídeo.")
          cask.Response(json.render().getBytes("UTF-8"), statusCode = 400, headers = Seq("Content-Type" -> "application/json"))
        case Some(videoField) =>
          val filenameLower = videoField.filename.getOrElse("video.mp4").toLowerCase
          val mediaType =
            if (filenameLower.endsWith(".webm")) "video/webm"
            else if (filenameLower.endsWith(".mov")) "video/quicktime"
            else "video/mp4"

          val base64Data = java.util.Base64.getEncoder.encodeToString(videoField.data)

          new Thread(new Runnable {
            def run(): Unit = {
              try { DatabaseManager.analyzeVideoTraining(trainingId, base64Data, mediaType) }
              catch { case _: Exception => () }
            }
          }).start()

          val json = ujson.Obj("status" -> "processing")
          cask.Response(json.render().getBytes("UTF-8"), statusCode = 202, headers = Seq("Content-Type" -> "application/json"))
      }
    }
  }

  // Lectura desde BD unicamente — nunca llama a Gemini
  @cask.get("/video/training-status/:trainingId")
  def videoTrainingStatusAction(trainingId: Int) = {
    val status = DatabaseManager.getVideoAnalysisStatusTraining(trainingId)
    val json = status.get("status") match {
      case Some("done") => ujson.Obj(
        "status"   -> "done",
        "analisis" -> status("analisis").asInstanceOf[String],
        "fecha"    -> status("fecha").asInstanceOf[String]
      )
      case _ => ujson.Obj("status" -> "pending")
    }
    cask.Response(json.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A4 — HISTORIAL DE ANALISIS DE VIDEO
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.get("/video-history")
  def videoHistoryPage(request: cask.Request) = withAuth(request) {
    val hist = DatabaseManager.getVideoAnalysisHistoryAll()
    val evolCached = DatabaseManager.getVideoEvolutionAnalysisCached()

    def notaTecFmt(h: Map[String, Any]): String =
      h("notaTecnica").asInstanceOf[Option[Double]].map(n => f"$n%.1f").getOrElse("—")

    val rows = if (hist.isEmpty)
      tr(td(attr("colspan") := "4", cls := "text-center text-muted", "Sin análisis de vídeo todavía."))
    else
      frag(hist.map { h =>
        val notaTxt: String = notaTecFmt(h)
        val esPartido = h("tipoVideo").asInstanceOf[String] == "Partido"
        val badge = if (esPartido) span(cls := "badge bg-warning text-dark", "🏟️ Partido") else span(cls := "badge bg-info text-dark", "🏃 Entreno")
        tr(
          td(badge),
          td(fixEncoding(h("label").asInstanceOf[String])),
          td(h("fecha").asInstanceOf[String]),
          td(cls := "text-center", notaTxt)
        )
      }: _*)

    val labelsJs = hist.map(h => s""""${h("fecha").asInstanceOf[String]}"""").mkString("[", ",", "]")
    val notasMatchJs = hist.map(h =>
      if (h("tipoVideo").asInstanceOf[String] == "Partido") h("notaTecnica").asInstanceOf[Option[Double]].getOrElse(0.0).toString else "null"
    ).mkString("[", ",", "]")
    val notasTrainingJs = hist.map(h =>
      if (h("tipoVideo").asInstanceOf[String] == "Entreno") h("notaTecnica").asInstanceOf[Option[Double]].getOrElse(0.0).toString else "null"
    ).mkString("[", ",", "]")

    val evolSection = evolCached match {
      case Some(a) => div(cls := "card bg-dark border-info shadow mb-3",
        div(cls := "card-header text-info fw-bold small", "🧠 Evolución técnica IA"),
        div(cls := "card-body text-light small", style := "white-space:pre-wrap;", fixEncoding(a)))
      case None => div()
    }

    val disabledAttr: Modifier = if (hist.size < 2) attr("disabled") := "disabled" else frag()

    val content = basePage("video-history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h4(cls := "text-white fw-black mb-0", "🎬 Historial de Vídeo IA"),
            a(href := "/history", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Historial")
          ),
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "Evolución de la nota técnica (🏟️ Partido / 🏃 Entreno)"),
            div(cls := "card-body", tag("canvas")(id := "chartVideoEvol", style := "max-height:220px;"))
          ),
          div(cls := "d-grid mb-3",
            form(action := "/video-history/evolucion", method := "post",
              button(tpe := "submit", cls := "btn btn-info fw-bold w-100", disabledAttr, "🧠 Evolución técnica IA")
            )
          ),
          evolSection,
          div(cls := "card bg-dark border-secondary shadow",
            div(cls := "card-header text-white fw-bold small", "Análisis de vídeo (partidos y entrenamientos)"),
            table(cls := "table table-dark table-sm mb-0",
              thead(tr(th("Tipo"), th("Rival / Foco"), th("Fecha"), th(cls := "text-center", "Nota técnica"))),
              tbody(rows)
            )
          )
        )
      ),
      script(src := "https://cdn.jsdelivr.net/npm/chart.js"),
      script(raw(s"""
        var ctxVE = document.getElementById('chartVideoEvol');
        if (ctxVE) {
          new Chart(ctxVE, {
            type: 'line',
            data: { labels: $labelsJs, datasets: [
              { label: '🏟️ Partido', data: $notasMatchJs, borderColor: '#d4af37', backgroundColor: 'rgba(212,175,55,0.15)', borderWidth:2, pointRadius:4, fill:false, tension:0.3, spanGaps:true },
              { label: '🏃 Entreno', data: $notasTrainingJs, borderColor: '#0dcaf0', backgroundColor: 'rgba(13,202,240,0.15)', borderWidth:2, pointRadius:4, fill:false, tension:0.3, spanGaps:true }
            ]},
            options: { responsive:true, plugins:{ legend:{ display:true, labels:{ color:'#ccc' } } }, scales:{ y:{ min:0, max:10 } } }
          });
        }
      """))
    )
    renderHtml(content)
  }

  @cask.post("/video-history/evolucion")
  def videoHistoryEvolucionAction(request: cask.Request) = withAuth(request) {
    DatabaseManager.generateVideoEvolutionAnalysis()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/video-history"))
  }

  // BLOQUE J: genera (con Gemini) la vista del ojeador externo — solo al pulsar boton, cache 30 dias
  @cask.post("/scouting-report/ojeador-externo")
  def generarOjeadorExternoAction(request: cask.Request) = withAuth(request) {
    DatabaseManager.generarOjeadorExternoNarrative()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/scouting-report"))
  }

  @cask.get("/scouting-report")
  def scoutingReportPage(request: cask.Request) = withAuth(request) {
    val card      = DatabaseManager.getLatestCardData()
    val edad      = DatabaseManager.calcularEdadExacta(card.fechaNacimiento)
    val seasons   = DatabaseManager.getCareerSummary()
    val matches   = DatabaseManager.getMatchesList()
    val evolution = DatabaseManager.getSeasonEvolution()
    val opps      = DatabaseManager.getOpportunities().take(3)
    val skills    = DatabaseManager.getGoalkeeperSkills()

    val pj = matches.size
    val notaMedia = if (pj > 0) matches.map(_.nota).sum / pj else 0.0
    def gfOf(m: MatchLog): Int = m.resultado.split("-").headOption.flatMap(_.trim.toIntOption).getOrElse(0)
    def gcOf(m: MatchLog): Int = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0)
    val cleanSheets = matches.count(gcOf(_) == 0)
    val pctCS       = if (pj > 0) cleanSheets * 100 / pj else 0
    val ganados     = matches.count(m => gfOf(m) > gcOf(m))
    val winRate     = if (pj > 0) ganados * 100 / pj else 0
    val minutosTotales = matches.map(_.minutos).sum
    val acute   = DatabaseManager.getWorkloads(7)
    val chronic = DatabaseManager.getWorkloads(28)
    val acwr    = StatsCalculator.calculateACWR(acute, chronic)

    val analisisIA = if (pj >= 3)
      DatabaseManager.getScoutingReportNarrative(edad, notaMedia, pctCS, winRate, acwr, pj)
    else "Se necesitan al menos 3 partidos registrados para generar el análisis de ojeador."

    val rae = DatabaseManager.getRaeAdjustedStats()
    val raeFactor     = rae("raeFactor").asInstanceOf[Double]
    val notaMediaRae  = rae("notaMediaRae").asInstanceOf[Double]
    val pctCSRae      = rae("pctCSRae").asInstanceOf[Int]
    val winRateRae    = rae("winRateRae").asInstanceOf[Int]

    val presion = DatabaseManager.getPresionPattern()
    val presionTotal = presion("total").asInstanceOf[Int]
    val presionDist  = presion("distribucion").asInstanceOf[List[Map[String, Any]]]

    val aniosJs  = evolution.map(e => s""""${e._1}"""").mkString("[", ",", "]")
    val mediasJs = evolution.map(e => f"${e._2}%.1f").mkString("[", ",", "]")

    val skillsByCategoria = skills.groupBy(_.categoria).map { case (cat, list) =>
      val pct = if (list.nonEmpty) list.count(_.conseguido) * 100 / list.size else 0
      (cat, pct)
    }

    val matchRows = matches.take(30).map { m =>
      val notaColor = if (m.nota >= 7) "#27ae60" else if (m.nota >= 5) "#e67e22" else "#c0392b"
      s"""<tr><td>${m.fecha}</td><td><b>${DatabaseManager.escHtml(DatabaseManager.fixEncoding(m.rival))}</b></td>
        <td style="text-align:center;">${m.resultado}</td>
        <td style="text-align:center; color:$notaColor; font-weight:bold;">${m.nota}</td></tr>"""
    }.mkString("")

    val seasonRows = seasons.map { s =>
      s"""<tr><td>${DatabaseManager.escHtml(s.categoria)}</td>
        <td style="text-align:center;">${s.partidosJugados}</td>
        <td style="text-align:center;">${s.golesContra}</td>
        <td style="text-align:center;">${s.mediaFinal}</td></tr>"""
    }.mkString("")

    val oppRows = if (opps.isEmpty) "<tr><td colspan=\"3\">Sin oportunidades registradas</td></tr>" else
      opps.map { o =>
        s"""<tr><td>${o.fecha.take(10)}</td>
          <td>${DatabaseManager.escHtml(o.tipo)} — ${DatabaseManager.escHtml(DatabaseManager.fixEncoding(o.clubOEntidad))}</td>
          <td>${DatabaseManager.escHtml(DatabaseManager.fixEncoding(o.resultado))}</td></tr>"""
      }.mkString("")

    val skillsBoxes = skillsByCategoria.map { case (cat, pct) =>
      s"""<div class="attr-box"><div class="av" style="font-size:18px;color:#d4af37;">$pct%</div><div class="al">${DatabaseManager.escHtml(cat)}</div></div>"""
    }.mkString("")

    // ── BLOQUE A5: ultimo analisis de video IA — solo lectura de BD ────────
    val videoIaSectionHtml = matches.headOption.flatMap { ultimo =>
      val status = DatabaseManager.getVideoAnalysisStatus(ultimo.id)
      status.get("status") match {
        case Some("done") =>
          val secciones = DatabaseManager.parseVideoAnalysisSections(status("analisis").asInstanceOf[String])
          val fuertes = secciones.getOrElse("PUNTOS FUERTES", "")
          val notaTec = secciones.getOrElse("NOTA TÉCNICA GLOBAL", "")
          Some(s"""
<p class="section-title">🎬 ÚLTIMO ANÁLISIS TÉCNICO DE VÍDEO IA</p>
<div class="narrative" style="font-size:12px;">
  <b>vs ${DatabaseManager.escHtml(DatabaseManager.fixEncoding(ultimo.rival))} (${ultimo.fecha})</b><br/><br/>
  <b>Puntos fuertes:</b> ${DatabaseManager.escHtml(fuertes)}<br/><br/>
  <b>Nota técnica global:</b> ${DatabaseManager.escHtml(notaTec)}
</div>""")
        case _ => None
      }
    }.getOrElse("")

    // ── BLOQUE B (cognicion): perfil cognitivo — solo lectura/calculo de BD ─
    val cognitivoTests = DatabaseManager.getCognitivoTests()
    val cognitivoSectionHtml = cognitivoTests.lastOption.map { t =>
      val indiceActual: Double = t("indice").asInstanceOf[Double]
      val evolRows = cognitivoTests.map { tt =>
        s"""<tr><td>${tt("fecha").asInstanceOf[String]}</td><td style="text-align:center;">${f"${tt("indice").asInstanceOf[Double]}%.0f"}</td></tr>"""
      }.mkString("")
      s"""
<p class="section-title">🧠 PERFIL COGNITIVO</p>
<div class="stats-grid" style="grid-template-columns:repeat(1,1fr); margin-bottom:12px;">
  <div class="stat-card"><div class="value">${f"$indiceActual%.0f"}</div><div class="label">Índice de cognición anticipatoria actual</div></div>
</div>
<table>
  <thead><tr><th>Fecha</th><th>Índice</th></tr></thead>
  <tbody>$evolRows</tbody>
</table>"""
    }.getOrElse("")

    // ── BLOQUE B6: plan de desarrollo individual activo — solo lectura de BD ─
    val idpSectionHtml = DatabaseManager.getActiveIdpTemporada().map { temp =>
      val objetivos = DatabaseManager.getIdpObjetivos(temp("id").asInstanceOf[Int])
      val objetivosRows = objetivos.map { o =>
        s"""<tr><td>${DatabaseManager.escHtml(o("dimension").asInstanceOf[String])}</td>
          <td>${DatabaseManager.escHtml(o("objetivo").asInstanceOf[String])}</td>
          <td style="text-align:center;">${o("progresoPct")}%</td></tr>"""
      }.mkString("")
      s"""
<p class="section-title">🗺️ PLAN DE DESARROLLO INDIVIDUAL — TEMPORADA ${DatabaseManager.escHtml(temp("temporada").asInstanceOf[String])}</p>
<table>
  <thead><tr><th>Dimensión</th><th>Objetivo</th><th>Progreso</th></tr></thead>
  <tbody>$objetivosRows</tbody>
</table>"""
    }.getOrElse("")

    // BLOQUE A: Goal Coverage Mapping — dato diferenciador (geometria pura, sin IA)
    val gcReport = DatabaseManager.calcularGoalCoverage()
    val gcReportPctBase = gcReport("pctCoberturaEstirada").asInstanceOf[Double]
    val gcReportPctAdulto = gcReport("pctCoberturaAdulto").asInstanceOf[Double]
    val goalCoverageHtml =
      s"""<div class="narrative" style="font-size:12px;">📐 Cobertura de portería actual: ${f"$gcReportPctBase%.0f"}% · Proyección adulta: ${f"$gcReportPctAdulto%.0f"}%</div>"""

    // BLOQUE E: estado Markov en el informe de captacion (None si <2 temporadas cerradas)
    val markovHtml = DatabaseManager.calcularMarkovPathway() match {
      case Some(m) =>
        val estadoActual = m("estadoActual").asInstanceOf[String]
        val siguienteEstado = m("siguienteEstado").asInstanceOf[String]
        val prob2Temp = m("probabilidad2Temp").asInstanceOf[Int]
        if (siguienteEstado.nonEmpty)
          s"""<div class="narrative" style="font-size:12px;">🗺️ Trayectoria proyectada: $estadoActual → $siguienteEstado con $prob2Temp% de probabilidad en 2 temporadas.</div>"""
        else
          s"""<div class="narrative" style="font-size:12px;">🗺️ Trayectoria proyectada: $estadoActual — nivel formativo máximo del modelo.</div>"""
      case None => ""
    }

    // BLOQUE B: indice de consistencia (temporada activa, minimo 8 partidos)
    val consistenciaHtml = {
      val vol = DatabaseManager.getVolatilityIndex(DatabaseManager.getTemporadaActivaId())
      if (!vol("suficiente").asInstanceOf[Boolean]) ""
      else s"""<div class="narrative" style="font-size:12px;">📊 Índice de Consistencia: ${vol("emoji")} ${vol("etiqueta")} (σ=${f"${vol("desviacion").asInstanceOf[Double]}%.2f"})</div>"""
    }

    // BLOQUE P: zona de la porteria mas castigada (temporada activa, minimo 10 goles con zona)
    val zonaVulnerableHtml = {
      val h = DatabaseManager.getGoalHeatmap6Zonas(DatabaseManager.getTemporadaActivaId())
      if (!h("suficiente").asInstanceOf[Boolean]) ""
      else {
        val z = h("zonaMax").asInstanceOf[String]
        val n = h("zonas").asInstanceOf[Map[String, Int]](z); val total = h("total").asInstanceOf[Int]
        s"""<div class="narrative" style="font-size:12px;">🥅 Zona más vulnerable: ${DatabaseManager.zonasPorteria6.toMap.apply(z)} ($n goles, ${n * 100 / total}% del total)</div>"""
      }
    }

    // BLOQUE F: correccion del paso negativo, cuando hay datos suficientes
    val pasoNegativoHtml = {
      val pn = DatabaseManager.getPasoNegativoTrend()
      if (!pn("suficiente").asInstanceOf[Boolean]) "" else {
        val tendenciaTxt = pn("tendencia").asInstanceOf[String] match {
          case "CORRIGIENDO" => "mejorando (menos partidos con paso negativo con el tiempo)"
          case "ESTABLE_O_PEOR" => "estable o empeorando — recomendable trabajarlo con el entrenador"
          case _ => "sin tendencia clara todavía"
        }
        s"""<div class="narrative" style="font-size:12px;">📉 Corrección del Paso Negativo: $tendenciaTxt</div>"""
      }
    }

    // BLOQUE RFFM: percentil real de Hector vs la categoria (None si aun no hay >=10 equipos sincronizados)
    val rffmHtml = DatabaseManager.getPercentilRealHector() match {
      case Some(p) =>
        val percentil = p("percentilGC").asInstanceOf[Int]
        val totalEquipos = p("totalEquipos").asInstanceOf[Int]
        s"""<div class="narrative" style="font-size:12px;">📊 Rendimiento vs categoría: percentil $percentil en GC/partido sobre $totalEquipos equipos de Prebenjamín F7 Madrid (RFFM)</div>"""
      case None => ""
    }

    // BLOQUE N: tabla de habilidades con su nivel de automatismo
    val automatismoTablaHtml = {
      val skills = DatabaseManager.getGoalkeeperSkills().filter(_.conseguido)
      if (skills.isEmpty) "" else {
        def nivelLabel(n: Option[String]): String = n match {
          case Some("INSTINTIVO") => "🟢 Instintivo"; case Some("AUTOMATICO") => "🔵 Automático"
          case Some("CONSCIENTE") => "🟡 Consciente"; case _ => "—"
        }
        val filas = skills.map(s => s"""<tr><td>${DatabaseManager.escHtml(s.habilidad)}</td><td>${DatabaseManager.escHtml(s.categoria)}</td><td style="text-align:center;">${nivelLabel(s.nivelAutomatismo)}</td></tr>""").mkString("")
        s"""<p class="section-title">🧤 NIVEL DE AUTOMATISMO DE LAS HABILIDADES</p>
            <table><thead><tr><th>Habilidad</th><th>Categoría</th><th style="text-align:center;">Nivel</th></tr></thead><tbody>$filas</tbody></table>"""
      }
    }

    // BLOQUE S: regulacion emocional bajo presion — patron dominante
    val regulacionHtml = {
      val re = DatabaseManager.getRegulacionEmocional()
      if (!re("suficiente").asInstanceOf[Boolean]) "" else {
        val patron = re("patronDominante").asInstanceOf[String]
        s"""<div class="narrative" style="font-size:12px;">🧠 Regulación emocional bajo presión: $patron</div>"""
      }
    }

    // MODULO LA VOZ DEL PORTERO: SQL puro, sin Gemini. NUNCA incluir respuesta_error — es privada.
    val vozPorteroHtml = {
      val hist = DatabaseManager.getVozPorteroHistorial()
      if (hist.isEmpty) "" else {
        val ultimo = hist.head
        val carita = ultimo("motivacionCarita").asInstanceOf[Int]
        val anteriores = hist.drop(1).take(3).map(_("motivacionCarita").asInstanceOf[Int])
        val tendenciaTxt =
          if (anteriores.isEmpty) "→ estable"
          else {
            val media = anteriores.sum.toDouble / anteriores.size
            if (carita > media + 0.3) "↑ en aumento" else if (carita < media - 0.3) "↓ en descenso" else "→ estable"
          }
        val aprendizaje = ultimo("respuestaAprendizaje").asInstanceOf[String]
        val extracto = aprendizaje.split("(?<=[.!?])\\s+").take(2).mkString(" ")
        s"""<p class="section-title">🎤 LA VOZ DEL PORTERO</p>
            <div class="narrative" style="font-size:12px;">
              Motivación intrínseca declarada: $carita/5<br/>
              Tendencia: $tendenciaTxt<br/>
              Última declaración sobre el aprendizaje: "${DatabaseManager.escHtml(extracto)}"
            </div>"""
      }
    }

    // MODULO ARQUETIPO: seccion en el informe de captacion — SQL puro, sin Gemini
    val arquetipoHtml = {
      val arq = DatabaseManager.calcularArquetipoPortero()
      if (!arq("activo").asInstanceOf[Boolean]) "" else {
        val descDom = DatabaseManager.arquetipoDescripcion(arq("dominante").asInstanceOf[String])
        val descSec = DatabaseManager.arquetipoDescripcion(arq("secundario").asInstanceOf[String])
        val domPct = arq("dominantePct").asInstanceOf[Int]; val secPct = arq("secundarioPct").asInstanceOf[Int]
        s"""<p class="section-title">🎭 ARQUETIPO EN DESARROLLO</p>
            <div class="narrative" style="font-size:12px;">
              ARQUETIPO DOMINANTE: ${DatabaseManager.escHtml(descDom("nombre"))} ($domPct%)<br/>
              ARQUETIPO SECUNDARIO: ${DatabaseManager.escHtml(descSec("nombre"))} ($secPct%)<br/>
              SISTEMA IDEAL: ${DatabaseManager.escHtml(descDom("sistema_ideal"))}<br/>
              REFERENTES: ${DatabaseManager.escHtml(descDom("referentes"))}
            </div>"""
      }
    }

    // BLOQUE K: Club Readiness Score — SQL puro, sin Gemini
    val readinessHtml = {
      val r = DatabaseManager.getClubReadinessScore()
      val readiness = r("readiness").asInstanceOf[Int]
      val interpretacion = r("interpretacion").asInstanceOf[String]
      s"""<div class="narrative" style="font-size:12px;">🎯 Club Readiness Score: $readiness/100 — $interpretacion</div>"""
    }

    // BLOQUE J: perspectiva del ojeador externo — solo lectura de cache (30 dias), nunca llama a Gemini aqui
    val ojeadorExternoHtml = DatabaseManager.getOjeadorExternoCache() match {
      case Some(texto) =>
        s"""<p class="section-title">👁️ VISTA DEL OJEADOR EXTERNO</p><div class="narrative">${DatabaseManager.escHtml(texto)}</div>"""
      case None =>
        """<div class="no-print" style="text-align:center; margin:16px 0;">
             <form action="/scouting-report/ojeador-externo" method="post">
               <button type="submit" class="print-btn" style="background:#555; color:#fff;">👁️ Vista del ojeador externo</button>
             </form>
           </div>"""
    }

    val presionRows = if (presionDist.isEmpty)
      "<tr><td colspan=\"2\">Sin datos suficientes de comportamiento bajo presión</td></tr>"
    else presionDist.map { d =>
      s"""<tr><td>${DatabaseManager.escHtml(d("label").asInstanceOf[String])}</td><td style="text-align:center;">${d("pct")}%</td></tr>"""
    }.mkString("")

    val htmlStr = s"""<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="utf-8"/>
<title>Informe de Captación — ${DatabaseManager.escHtml(card.nombre)}</title>
<script src="https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"></script>
<style>
  @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@400;700&display=swap');
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: 'Oswald', sans-serif; color: #1a1a1a; background: #fff; padding: 20px; }
  .no-print { text-align:center; margin-bottom:24px; }
  .print-btn { background:#d4af37; color:#000; border:none; padding:12px 32px; font-size:16px; font-weight:700; border-radius:6px; cursor:pointer; letter-spacing:1px; }
  .header { display:flex; justify-content:space-between; align-items:center; border-bottom:3px solid #d4af37; padding-bottom:16px; margin-bottom:24px; }
  .header-title h1 { font-size:26px; color:#1a1a1a; letter-spacing:2px; }
  .header-title p { color:#666; font-size:13px; margin-top:4px; }
  .stats-grid { display:grid; grid-template-columns:repeat(6,1fr); gap:10px; margin-bottom:24px; }
  .stat-card { border:2px solid #e0e0e0; border-radius:8px; text-align:center; padding:10px; }
  .stat-card .value { font-size:22px; font-weight:700; color:#d4af37; }
  .stat-card .label { font-size:10px; color:#888; margin-top:4px; text-transform:uppercase; letter-spacing:0.5px; }
  .attrs-grid { display:grid; grid-template-columns:repeat(6,1fr); gap:8px; margin-bottom:24px; }
  .attr-box { border:1px solid #ddd; border-radius:6px; text-align:center; padding:10px 6px; }
  .attr-box .av { font-size:24px; font-weight:700; }
  .attr-box .al { font-size:10px; color:#888; }
  .charts-row { display:grid; grid-template-columns:1fr; gap:20px; margin-bottom:24px; }
  .chart-box { border:1px solid #e0e0e0; border-radius:8px; padding:16px; }
  .chart-box h3 { font-size:13px; color:#666; margin-bottom:12px; text-transform:uppercase; letter-spacing:0.5px; }
  table { width:100%; border-collapse:collapse; font-size:12px; margin-bottom:24px; }
  thead tr { background:#1a1a1a; color:white; }
  th,td { border:1px solid #e0e0e0; padding:7px 10px; }
  tbody tr:nth-child(even) { background:#f9f9f9; }
  .section-title { font-size:16px; font-weight:700; color:#1a1a1a; border-left:4px solid #d4af37; padding-left:10px; margin-bottom:12px; }
  .narrative { border:1px solid #e0e0e0; border-radius:8px; padding:16px; margin-bottom:24px; font-size:13px; line-height:1.7; text-align:justify; }
  .footer { margin-top:24px; text-align:center; color:#aaa; font-size:11px; border-top:1px solid #eee; padding-top:12px; }
  @media print {
    .no-print { display:none; }
    body { padding:10px; }
    .charts-row canvas { max-height:200px; }
  }
</style>
</head>
<body>
<div class="no-print">
  <button class="print-btn" onclick="window.print()">🖨️ Imprimir / Guardar PDF</button>
</div>

<div class="header">
  <div style="display:flex; align-items:center; gap:16px;">
    ${if (card.fotoUrl.nonEmpty) s"""<img src="${card.fotoUrl}" style="width:64px;height:64px;border-radius:50%;object-fit:cover;border:2px solid #d4af37;"/>""" else ""}
    <div class="header-title">
      <h1>${DatabaseManager.escHtml(card.nombre)}</h1>
      <p>$edad años · ${DatabaseManager.escHtml(card.clubNombre)} · ${DatabaseManager.escHtml(card.posicion)} · ${seasons.size} temporadas registradas</p>
    </div>
  </div>
  <div style="text-align:right;">
    <div style="font-size:36px; font-weight:700; color:#d4af37;">${card.media}</div>
    <div style="font-size:11px; color:#666;">MEDIA GLOBAL</div>
  </div>
</div>

<div class="stats-grid">
  <div class="stat-card"><div class="value">${f"$notaMedia%.1f"}</div><div class="label">Nota media</div></div>
  <div class="stat-card"><div class="value" style="color:#27ae60;">$pctCS%</div><div class="label">Clean sheets ($cleanSheets/$pj)</div></div>
  <div class="stat-card"><div class="value">$winRate%</div><div class="label">Win rate</div></div>
  <div class="stat-card"><div class="value">$minutosTotales</div><div class="label">Minutos totales</div></div>
  <div class="stat-card"><div class="value" style="color:${if (acwr > 1.5) "#c0392b" else "#27ae60"};">${f"$acwr%.2f"}</div><div class="label">ACWR actual</div></div>
  <div class="stat-card"><div class="value">$pj</div><div class="label">Partidos</div></div>
</div>

<p class="section-title">EFECTO DE EDAD RELATIVA (RAE)</p>
<table>
  <thead><tr><th>Métrica</th><th>Valor real</th><th>Valor RAE-ajustado</th></tr></thead>
  <tbody>
    <tr><td>Nota media</td><td style="text-align:center;">${f"$notaMedia%.1f"}</td><td style="text-align:center; color:#2980b9; font-weight:bold;">${f"$notaMediaRae%.1f"}</td></tr>
    <tr><td>Porterías a 0</td><td style="text-align:center;">$pctCS%</td><td style="text-align:center; color:#2980b9; font-weight:bold;">$pctCSRae%</td></tr>
    <tr><td>Win rate</td><td style="text-align:center;">$winRate%</td><td style="text-align:center; color:#2980b9; font-weight:bold;">$winRateRae%</td></tr>
  </tbody>
</table>
<div class="narrative" style="font-size:12px;">Nota metodológica: Las métricas de Héctor se presentan en valor real y valor ajustado por Efecto de Edad Relativa (RAE). Nacido en junio, compite contra jugadores con hasta 6 meses más de madurez biológica. El valor ajustado (factor ${f"$raeFactor%.2f"}) refleja su rendimiento normalizado contra un hipotético cohorte de iguales madurativos.</div>

<p class="section-title">ATRIBUTOS ACTUALES</p>
<div class="attrs-grid">
  <div class="attr-box"><div class="av" style="color:#3498db;">${card.div}</div><div class="al">DIV</div></div>
  <div class="attr-box"><div class="av" style="color:#9b59b6;">${card.han}</div><div class="al">HAN</div></div>
  <div class="attr-box"><div class="av" style="color:#e67e22;">${card.kic}</div><div class="al">KIC</div></div>
  <div class="attr-box"><div class="av" style="color:#e74c3c;">${card.ref}</div><div class="al">REF</div></div>
  <div class="attr-box"><div class="av" style="color:#2ecc71;">${card.spd}</div><div class="al">SPD</div></div>
  <div class="attr-box"><div class="av" style="color:#f1c40f;">${card.pos}</div><div class="al">POS</div></div>
</div>

<div class="charts-row">
  <div class="chart-box">
    <h3>Progresión de media por temporada</h3>
    <canvas id="chartEvol" height="120"></canvas>
  </div>
</div>

<p class="section-title">RESUMEN POR TEMPORADA</p>
<table>
  <thead><tr><th>Categoría</th><th>PJ</th><th>GC</th><th>Media</th></tr></thead>
  <tbody>$seasonRows</tbody>
</table>

<p class="section-title">ANÁLISIS DE OJEADOR (IA)</p>
<div class="narrative">${DatabaseManager.escHtml(analisisIA)}</div>
$goalCoverageHtml
$markovHtml
$consistenciaHtml
$zonaVulnerableHtml
$pasoNegativoHtml
$arquetipoHtml
$vozPorteroHtml
$rffmHtml
$readinessHtml
$automatismoTablaHtml
$regulacionHtml
$ojeadorExternoHtml

<p class="section-title">OPORTUNIDADES RECIENTES</p>
<table>
  <thead><tr><th>Fecha</th><th>Tipo / Entidad</th><th>Resultado</th></tr></thead>
  <tbody>$oppRows</tbody>
</table>

<p class="section-title">PERFIL PSICOLÓGICO BAJO PRESIÓN</p>
<table>
  <thead><tr><th>Comportamiento tras gol encajado</th><th>% de partidos</th></tr></thead>
  <tbody>$presionRows</tbody>
</table>
${if (presionTotal > 0 && presionTotal < 5) "<div class=\"narrative\" style=\"font-size:12px;\">Distribución basada en " + presionTotal + " partido(s) con goles encajados — se necesitan al menos 5 para un patrón robusto.</div>" else ""}

<p class="section-title">CHECKLIST DE HABILIDADES POR CATEGORÍA</p>
<div class="attrs-grid">$skillsBoxes</div>
$videoIaSectionHtml
$idpSectionHtml
$cognitivoSectionHtml

<p class="section-title">HISTORIAL DE PARTIDOS (ÚLTIMOS 30)</p>
<table>
  <thead><tr><th>Fecha</th><th>Rival</th><th>Res.</th><th>Nota</th></tr></thead>
  <tbody>$matchRows</tbody>
</table>

<div class="footer">
  Guardian Elite — Informe de Captación generado automáticamente — combina entrenamiento colectivo, academia específica de porteros y judo como complemento físico.
</div>

<script>
  const anios = $aniosJs;
  const medias = $mediasJs;
  if (anios.length > 0) {
    new Chart(document.getElementById('chartEvol'), {
      type: 'line',
      data: { labels: anios, datasets: [{ label: 'Nota', data: medias, borderColor: '#d4af37', backgroundColor: 'rgba(212,175,55,0.15)', borderWidth:2, pointRadius:4, fill:true, tension:0.3 }] },
      options: { responsive:true, plugins:{ legend:{ display:false } }, scales:{ y:{ min:0, max:100 } } }
    });
  }
</script>
</body>
</html>"""

    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  initialize()
}