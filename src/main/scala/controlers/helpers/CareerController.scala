import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object CareerController extends cask.Routes {

  @cask.get("/gear")
  def gearPage(request: cask.Request) = withAuth(request) {
    val items = DatabaseManager.getActiveGear()

    val gearList = if (items.isEmpty) {
      div(cls := "alert alert-secondary text-center", "Sin material.")
    } else {
      val rows = for (i <- items) yield {
        val pct   = if (i.maxUsos > 0) (i.usos.toDouble / i.maxUsos.toDouble * 100).toInt else 0
        val color = if (pct > 90) "bg-danger" else if (pct > 75) "bg-warning" else "bg-success"
        val imgTag = if (i.img.length > 50)
          img(src := i.img, style := "width:50px; height:50px; object-fit:cover; border-radius:50%; margin-right:10px;")
        else
          div(cls := "me-3", style := "font-size: 30px;", if (i.tipo == "Guantes") "🧤" else "⚽")

        div(cls := "col-12 mb-3",
          div(cls := "card bg-dark border-secondary shadow",
            div(cls := "card-body d-flex align-items-center",
              imgTag,
              div(cls := "flex-grow-1",
                h5(cls := "text-white mb-0", i.nombre),
                div(cls := "small text-muted mb-1 fw-bold", i.tipo),
                div(cls := "progress", style := "height: 10px;",
                  div(cls := s"progress-bar $color", style := s"width: $pct%")
                )
              ),
              div(cls := "ms-3 text-end",
                div(cls := "fw-bold text-white", s"${i.usos}/${i.maxUsos}"),
                div(style := "font-size:10px", "USOS")
              )
            )
          )
        )
      }
      div(cls := "row", rows)
    }

    val content = basePage("gear",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-warning mb-4 text-center", "MATERIAL"),
          gearList,
          div(cls := "card bg-secondary bg-opacity-10 border-secondary mt-4",
            div(cls := "card-body",
              h5(cls := "text-white mb-3", "Nuevo"),
              form(action := "/gear/add", method := "post",
                div(cls := "row",
                  div(cls := "col-6 mb-2",
                    input(tpe := "text", name := "nombre", cls := "form-control",
                      placeholder := "Nombre", required := true)
                  ),
                  div(cls := "col-6 mb-2",
                    select(name := "tipo", cls := "form-select",
                      option(value := "Guantes", "Guantes"),
                      option(value := "Botas",   "Botas")
                    )
                  ),
                  div(cls := "col-12 mb-2",
                    input(tpe := "number", name := "vida", cls := "form-control",
                      value := "30", placeholder := "Vida util")
                  ),
                  div(cls := "col-12 mb-2",
                    label("Foto"),
                    input(tpe := "file", cls := "form-control", onchange := "convertToBase64(this, 'gearImg')")
                  ),
                  input(tpe := "hidden", name := "img", id := "gearImg"),
                  div(cls := "col-12",
                    button(tpe := "submit", cls := "btn btn-warning w-100", "Anadir")
                  )
                )
              )
            )
          ),
          script(raw("""function convertToBase64(i,t){if(i.files&&i.files[0]){var r=new FileReader();r.onload=function(e){document.getElementById(t).value=e.target.result;};r.readAsDataURL(i.files[0]);}}"""))
        )
      )
    )
    renderHtml(content)
  }
  @cask.postForm("/gear/add")
  def addGear(request: cask.Request, nombre: String, tipo: String, vida: Int, img: String) = withAuth(request) {
    // 1. Guardamos el nuevo material
    DatabaseManager.addNewGear(nombre, tipo, vida, if(img != null) img else "")

    // 2. Redireccionamos a la pagina de material (Gear) de forma limpia
    renderRedirect("/gear")
  }

  def medicalSection(reports: List[MedicalReport]) = {
    div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "🏥 MEDICAL VAULT & PASAPORTE BIOLOGICO"),
      div(cls := "card-body p-3",
        // Formulario de Subida
        form(action := "/bio/medical/upload", method := "post", enctype := "multipart/form-data",
          div(cls:="row g-2 mb-3",
            div(cls:="col-7",
              label(cls:="xx-small text-muted text-uppercase", "Tipo de Informe"),
              select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
                option(value:="Pediatria", "Pediatria (Crecimiento)"),
                option(value:="Analitica", "Analitica de Sangre"),
                option(value:="Traumatologia", "Traumatologia / Fisio"),
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
            label(cls:="form-check-label small text-muted", `for`:="checkPrevio", "Informe previo al inicio en futbol")
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-danger fw-bold", "Subir y Analizar con IA"))
        ),
        hr(cls:="border-secondary"),
        // Lista de Informes Procesados
        div(cls:="medical-history",
          if(reports.isEmpty) p(cls:="text-center text-muted small", "No hay registros medicos aun.")
          else for(r <- reports) yield div(cls:="border-start border-danger border-3 ps-2 mb-3",
            div(cls:="d-flex justify-content-between",
              span(cls:="fw-bold small text-danger", r.tipo),
              span(cls:="xx-small text-muted", r.fecha)
            ),
            div(cls:="xx-small text-light fst-italic", strong("Diagnostico: "), r.diagnostico),
            div(cls:="xx-small text-info", strong("Rec. IA: "), r.recomendaciones)
          )
        )
      )
    )
  }

  // --- 5. BIO & EVALUACION (CORREGIDO MODO OSCURO) ---

  @cask.get("/career")
  def careerPage(request: cask.Request) = withAuth(request) {
    val c = DatabaseManager.getCareerSummary()
    val listRows = for (s <- c) yield tr(
      td(cls := "fw-bold text-primary small", s.categoria),
      td(img(src := s.fotoUrl, style := "height: 35px; width: 35px; border-radius: 50%; object-fit: cover; border: 2px solid #ddd;")),
      td(cls := "text-center fw-bold small", s.partidosJugados),
      td(cls := "text-center text-danger small", s.golesContra),
      td(cls := "text-center", span(cls := "badge bg-dark text-warning border border-warning", s.mediaFinal))
    )
    val content = div(cls := "row justify-content-center",
      div(cls := "col-md-10 col-12",
        div(cls := "d-flex flex-column justify-content-center align-items-center mb-4 text-center",
          h2(cls := "text-warning m-0 mb-2", "Trayectoria"),
          div(cls := "mb-3 w-100",
            a(href := "/career/legacy", cls := "btn btn-warning w-100 fw-bold", "⭐ MODO LEGADO (RPG)")
          ),
          raw(DatabaseManager.getLegendComparison()),
          div(cls := "card bg-secondary p-2 w-100 mt-3",
            form(action := "/career/new-season", method := "post", cls := "d-flex flex-column gap-2",
              div(
                label(cls := "form-label text-white small m-0 fw-bold", "Nueva Categoria:"),
                input(tpe := "text", name := "categoria", cls := "form-control form-control-sm fw-bold",
                  placeholder := "Ej: Benjamin A", required := true)
              ),
              button(tpe := "submit", cls := "btn btn-danger btn-sm fw-bold",
                onclick := "return confirm('Seguro?');", "Cerrar & Empezar")
            )
          )
        ),
        div(cls := "card shadow-sm border-0",
          div(cls := "card-body p-0 table-responsive",
            table(cls := "table table-hover tm-table mb-0",
              thead(tr(th("Cat"), th("Ficha"), th("PJ"), th("GC"), th("Media"))),
              tbody(listRows)
            )
          )
        )
      )
    )
    renderHtml(basePage("career", content))
  }

  @cask.postForm("/career/new-season")
  def newSeasonAction(categoria: String) = {
    val msg = DatabaseManager.startNewSeason(categoria)
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.title("Nueva Temp"), tags2.style(raw(getCss()))),
      body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';",
        h1("OK"), h2(msg),
        p(s"Etapa iniciada: $categoria"),
        div(style := "margin-top: 20px;",
          a(href := "/", cls := "btn btn-warning fw-bold", "Ir a Inicio")
        )
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.get("/penalties")
  def penaltiesPage(request: cask.Request) = withAuth(request) {
    val (byZone, byRival) = DatabaseManager.getPenaltyDetailedStats()
    val totalPen = byZone.values.map(_._1).sum
    val totalGol = byZone.values.map(v => v._1 - v._2).sum
    val pctGlobal = if (totalPen > 0) (totalGol * 100 / totalPen) else 0

    val zones = Seq("TL","TM","TR","ML","MM","MR","BL","BM","BR")
    val maxTotal = if (byZone.values.nonEmpty) byZone.values.map(_._1).max.toDouble else 1.0

    def zoneLabel(z: String) = z match {
      case "TL"=>"Arr Izq"; case "TM"=>"Arr Cen"; case "TR"=>"Arr Der"
      case "ML"=>"Med Izq"; case "MM"=>"Med Cen"; case "MR"=>"Med Der"
      case "BL"=>"Baj Izq"; case "BM"=>"Baj Cen"; case "BR"=>"Baj Der"
      case _ => z
    }

    val heatmapCells = for (z <- zones) yield {
      val (tot, par) = byZone.getOrElse(z, (0,0))
      val goles = tot - par
      val intensity = if (maxTotal > 0) tot / maxTotal else 0.0
      val bg = if (tot == 0) "rgba(255,255,255,0.04)"
               else if (intensity < 0.3) "rgba(220,53,69,0.25)"
               else if (intensity < 0.6) "rgba(220,53,69,0.55)"
               else "rgba(220,53,69,0.88)"
      val pctPar = if (tot > 0) (par * 100 / tot) else 0
      div(cls := "pen-heatmap-cell d-flex flex-column justify-content-center align-items-center",
        style := s"background:$bg; border:1px solid rgba(255,255,255,0.1);",
        attr("title") := s"${zoneLabel(z)}: $tot tiros | $goles goles | $pctPar% parado",
        if (tot > 0) Seq(
          div(cls := "fw-bold text-white", style := "font-size:16px;", tot.toString),
          div(cls := "xx-small", style := s"color:${if(pctPar >= 50) "#28a745" else "#dc3545"};", s"$pctPar% par")
        ) else Seq(div(cls := "text-muted", style := "opacity:0.25; font-size:14px;", "—"))
      )
    }

    val rivalRows = byRival.map { case (rival, tot, goles) =>
      val par = tot - goles
      val pct = if (tot > 0) (par * 100 / tot) else 0
      tr(
        td(cls := "fw-bold text-white", fixEncoding(rival)),
        td(cls := "text-center", tot.toString),
        td(cls := "text-center text-danger", goles.toString),
        td(cls := "text-center text-success", par.toString),
        td(cls := "text-center",
          div(cls := "progress", style := "height:8px; min-width:60px;",
            div(cls := s"progress-bar ${if(pct>=50)"bg-success"else"bg-danger"}",
              style := s"width:$pct%;")
          ),
          div(cls := "xx-small text-muted mt-1", s"$pct%")
        )
      )
    }

    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-10 col-12",
          h2(cls := "text-center text-danger mb-1", "LABORATORIO PENALTIS"),
          p(cls := "text-center text-muted small mb-4", s"$totalPen penaltis totales • $pctGlobal% de goles encajados"),

          // KPIs globales
          div(cls := "row g-2 mb-4",
            Seq(
              ("Total Penaltis", totalPen.toString, "secondary"),
              ("Goles Encajados", totalGol.toString, "danger"),
              ("Parados", (totalPen - totalGol).toString, "success"),
              ("% Parado", s"${100 - pctGlobal}%", if(100-pctGlobal >= 50) "success" else "warning")
            ).map { case (label, value, color) =>
              div(cls := "col-3",
                div(cls := s"card bg-dark border-$color text-center py-2",
                  div(cls := s"text-$color fw-bold", style := "font-size:22px;", value),
                  div(cls := "xx-small text-muted", label)
                )
              )
            }
          ),

          div(cls := "row g-3",
            // --- COLUMNA IZQUIERDA: heatmap + form ---
            div(cls := "col-md-5",
              // Heatmap porteria
              div(cls := "card bg-dark border-danger shadow mb-3",
                div(cls := "card-header text-danger fw-bold small text-center", "MAPA DE TIROS RIVALES"),
                div(cls := "card-body",
                  // Postes
                  div(cls := "d-flex justify-content-center mb-1",
                    div(style := "width:220px; height:5px; background:linear-gradient(90deg,#888,#ccc,#888); border-radius:3px;")
                  ),
                  div(cls := "d-flex justify-content-center",
                    div(cls := "d-flex align-items-stretch",
                      div(style := "width:5px; background:linear-gradient(180deg,#888,#ccc,#888); border-radius:3px; min-height:180px;"),
                      div(style := "display:grid; grid-template-columns:1fr 1fr 1fr; grid-template-rows:1fr 1fr 1fr; width:220px; min-height:180px; gap:2px; padding:2px;",
                        heatmapCells.toSeq
                      ),
                      div(style := "width:5px; background:linear-gradient(180deg,#888,#ccc,#888); border-radius:3px;")
                    )
                  ),
                  div(cls := "d-flex justify-content-center mt-1",
                    div(style := "width:220px; height:4px; background:rgba(255,255,255,0.12); border-radius:2px;")
                  ),
                  // Leyenda
                  div(cls := "d-flex justify-content-center align-items-center gap-2 mt-3",
                    span(cls := "xx-small text-muted", "Menos tiros"),
                    div(style := "width:80px; height:7px; border-radius:4px; background:linear-gradient(90deg,rgba(220,53,69,0.1),rgba(220,53,69,0.9));"),
                    span(cls := "xx-small text-muted", "Mas tiros")
                  )
                )
              ),

              // Registro
              div(cls := "card bg-dark border-secondary shadow",
                div(cls := "card-header text-white fw-bold small", "REGISTRAR PENALTI"),
                div(cls := "card-body",
                  form(action := "/penalties/save", method := "post",
                    div(cls := "mb-2",
                      input(tpe := "text", name := "rival", cls := "form-control form-control-sm fw-bold",
                        placeholder := "Rival (opcional)")
                    ),
                    div(cls := "row g-2 mb-2",
                      div(cls := "col-6",
                        label(cls := "xx-small text-muted fw-bold", "Zona del tiro"),
                        select(name := "zTiro", cls := "form-select form-select-sm fw-bold",
                          option(value:="TL","Arr Izq"), option(value:="TM","Arr Cen"), option(value:="TR","Arr Der"),
                          option(value:="ML","Med Izq"), option(value:="MM","Med Cen"), option(value:="MR","Med Der"),
                          option(value:="BL","Baj Izq"), option(value:="BM","Baj Cen"), option(value:="BR","Baj Der")
                        )
                      ),
                      div(cls := "col-6",
                        label(cls := "xx-small text-muted fw-bold", "Salto Hector"),
                        select(name := "zSalto", cls := "form-select form-select-sm fw-bold",
                          option(value:="L","Izquierda"), option(value:="C","Centro"), option(value:="R","Derecha")
                        )
                      )
                    ),
                    div(cls := "form-check mb-2",
                      input(cls := "form-check-input", tpe := "checkbox", name := "esGol", id := "golCheck"),
                      label(cls := "form-check-label text-white small fw-bold", attr("for") := "golCheck", "Fue Gol")
                    ),
                    button(tpe := "submit", cls := "btn btn-danger w-100 btn-sm fw-bold", "Registrar")
                  )
                )
              )
            ),

            // --- COLUMNA DERECHA: stats por zona + rival ---
            div(cls := "col-md-7",
              // Stats por zona
              div(cls := "card bg-dark border-secondary shadow mb-3",
                div(cls := "card-header text-white fw-bold small", "% PARADO POR ZONA"),
                div(cls := "card-body p-2",
                  div(style := "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px;",
                    zones.map { z =>
                      val (tot, par) = byZone.getOrElse(z, (0,0))
                      val pct = if (tot > 0) (par * 100 / tot) else 0
                      val color = if (pct >= 70) "success" else if (pct >= 40) "warning" else "danger"
                      div(cls := "text-center p-2 bg-secondary bg-opacity-10 rounded",
                        div(cls := "xx-small text-muted", zoneLabel(z)),
                        div(cls := s"fw-bold text-$color", if (tot > 0) s"$pct%" else "—"),
                        div(cls := "xx-small text-muted", if (tot > 0) s"$tot tiros" else "")
                      )
                    }
                  )
                )
              ),

              // Historial por rival
              if (byRival.nonEmpty) {
                div(cls := "card bg-dark border-secondary shadow",
                  div(cls := "card-header text-white fw-bold small", "HISTORIAL POR RIVAL"),
                  div(cls := "card-body p-0",
                    div(cls := "table-responsive",
                      table(cls := "table table-dark table-sm mb-0 small",
                        thead(tr(
                          th("Rival"), th(cls:="text-center","Tiros"),
                          th(cls:="text-center text-danger","Goles"), th(cls:="text-center text-success","Parados"),
                          th(cls:="text-center","% Parado")
                        )),
                        tbody(rivalRows)
                      )
                    )
                  )
                )
              } else {
                div(cls := "alert alert-secondary text-center small",
                  "Registra penaltis con nombre de rival para ver el historial")
              }
            )
          )
        )
      )
    )
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/penalties/save")
  def savePenalty(rival: String, zTiro: String, zSalto: String, esGol: Boolean) = {
    DatabaseManager.logPenalty(rival, zTiro, zSalto, esGol)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/penalties"))
  }

  // 8. PIZARRA TACTICA

  // /oracle -> digital-twin (version mejorada con algoritmo Tanner + PHV)
  @cask.get("/oracle")
  def oraclePage(request: cask.Request, hDad: String = "180", hMom: String = "170") = withAuth(request) {
    cask.Response("".getBytes("UTF-8"), statusCode = 302,
      headers = Seq("Location" -> s"/digital-twin?hPadre=$hDad&hMadre=$hMom"))
  }

  // /distribution -> moneyball (version Fase 6.5 completa)
  @cask.get("/distribution")
  def distributionPage() = {
    cask.Response("".getBytes("UTF-8"), statusCode = 302,
      headers = Seq("Location" -> "/moneyball"))
  }

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
          div(cls:="col-6", div(cls:="p-3 border border-secondary rounded text-center bg-secondary bg-opacity-10", h3("🛡"), h6("Muro"), small("Bonus por Porteria a Cero"))),
          div(cls:="col-6", div(cls:="p-3 border border-secondary rounded text-center bg-secondary bg-opacity-10", h3("🧤"), h6("Manos de Oro"), small("Bonus por Paradas")))
        ),

        div(cls:="alert alert-dark border-info mt-4 text-center",
          h5(cls:="text-info", "Sistema de Puntos"),
          ul(cls:="list-unstyled small text-start d-inline-block",
            li("- Partido Jugado: +50 XP"),
            li("- Porteria a Cero: +100 XP"),
            li("- Parada: +5 XP"),
            li("- Nota > 7.0: +100 XP (Bonus)")
          )
        )
      )
    ))
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.get("/career/evolucion")
  def evolucionPage(request: cask.Request) = withAuth(request) {
    val evolution = DatabaseManager.getSeasonEvolution()
    val seasons = DatabaseManager.getCareerSummary()

    val anios    = evolution.map(_._1)
    val medias   = evolution.map(e => f"${e._2}%.1f")
    val pjs      = evolution.map(_._3.toString)
    val gcs      = evolution.map(_._4.toString)
    val pcs      = evolution.map(_._5.toString)

    val aniosJs  = anios.map(a => s""""$a"""").mkString("[",",","]")
    val mediasJs = medias.mkString("[",",","]")
    val pjsJs    = pjs.mkString("[",",","]")
    val gcsJs    = gcs.mkString("[",",","]")
    val pcsJs    = pcs.mkString("[",",","]")

    val totalPj = evolution.map(_._3).sum
    val avgAll  = if (evolution.nonEmpty) evolution.map(_._2).sum / evolution.size else 0.0
    val totalGc = evolution.map(_._4).sum
    val totalPc = evolution.map(_._5).sum

    val seasonTable = seasons.map { s =>
      tr(
        td(cls:="fw-bold text-warning", s.categoria),
        td(cls:="text-center", s.partidosJugados.toString),
        td(cls:="text-center text-danger", s.golesContra.toString),
        td(cls:="text-center",
          span(cls:=s"badge ${if(s.mediaFinal>=70)"bg-success"else if(s.mediaFinal>=55)"bg-warning text-dark"else"bg-danger"}",
            s.mediaFinal.toString))
      )
    }

    val content = basePage("career",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h2(cls := "text-warning mb-0", "EVOLUCION HISTORICA"),
            a(href := "/career", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Carrera")
          ),

          // KPIs globales
          div(cls := "row g-2 mb-4",
            Seq(
              ("Temporadas", evolution.size.toString, "warning"),
              ("Partidos", totalPj.toString, "info"),
              (s"Media Global", f"$avgAll%.1f", if(avgAll>=70)"success"else"warning"),
              ("Goles Encajados", totalGc.toString, "danger"),
              ("Porterias a 0", totalPc.toString, "success")
            ).map { case (lbl, v, c) =>
              div(cls := "col",
                div(cls := s"card bg-dark border-$c text-center py-2",
                  div(cls := s"text-$c fw-bold fs-4", v),
                  div(cls := "xx-small text-muted", lbl)
                )
              )
            }
          ),

          if (evolution.isEmpty) {
            div(cls := "alert alert-secondary text-center py-5",
              div(style:="font-size:40px; opacity:0.3;","📈"),
              div(cls:="fw-bold mt-2","Sin datos suficientes para mostrar evolucion"),
              div(cls:="small text-muted mt-1","Registra partidos para ver tu progresion")
            )
          } else div(
            // Grafico principal
            div(cls := "card bg-dark border-warning shadow mb-4",
              div(cls := "card-header text-warning fw-bold small", "EVOLUCION DE NOTA MEDIA POR TEMPORADA"),
              div(cls := "card-body",
                div(style := "position:relative; height:280px;",
                  tag("canvas")(id := "chartEvolucion")
                )
              )
            ),

            div(cls := "row g-3",
              // Grafico barras GC
              div(cls := "col-md-6",
                div(cls := "card bg-dark border-danger shadow",
                  div(cls := "card-header text-danger fw-bold small", "GOLES ENCAJADOS POR TEMPORADA"),
                  div(cls := "card-body",
                    div(style := "position:relative; height:200px;",
                      tag("canvas")(id := "chartGoles")
                    )
                  )
                )
              ),
              // Tabla por temporada
              div(cls := "col-md-6",
                div(cls := "card bg-dark border-secondary shadow",
                  div(cls := "card-header text-white fw-bold small", "RESUMEN POR TEMPORADA"),
                  div(cls := "card-body p-0",
                    table(cls := "table table-dark table-sm mb-0 small",
                      thead(tr(th("Temp"), th(cls:="text-center","PJ"), th(cls:="text-center","GC"), th(cls:="text-center","Media"))),
                      tbody(seasonTable)
                    )
                  )
                )
              )
            ),

            script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
            script(raw(s"""
              const anios  = $aniosJs;
              const medias = $mediasJs;
              const gcs    = $gcsJs;
              const pcs    = $pcsJs;

              // Grafico linea: media
              new Chart(document.getElementById('chartEvolucion'), {
                type: 'line',
                data: {
                  labels: anios,
                  datasets: [{
                    label: 'Nota Media',
                    data: medias,
                    borderColor: '#d4af37',
                    backgroundColor: 'rgba(212,175,55,0.15)',
                    borderWidth: 3,
                    pointBackgroundColor: '#d4af37',
                    pointRadius: 6,
                    fill: true,
                    tension: 0.3
                  }]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc' } } },
                  scales: {
                    x: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { min: 0, max: 100, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                  }
                }
              });

              // Grafico barras: goles encajados
              new Chart(document.getElementById('chartGoles'), {
                type: 'bar',
                data: {
                  labels: anios,
                  datasets: [{
                    label: 'Goles encajados',
                    data: gcs,
                    backgroundColor: 'rgba(220,53,69,0.7)',
                    borderColor: '#dc3545',
                    borderWidth: 1,
                    borderRadius: 4
                  }]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc' } } },
                  scales: {
                    x: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                  }
                }
              });
            """))
          )
        )
      )
    )
    renderHtml(content)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — MARKET ESTIMATOR
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.get("/market-estimator")
  def marketEstimatorPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getMarketEstimatorData()

    val rawScore:      Double = d("rawScore").asInstanceOf[Double]
    val valorEstimado: Int    = d("valorEstimado").asInstanceOf[Int]
    val percentil:     Int    = d("percentil").asInstanceOf[Int]
    val nivelLabel:    String = d("nivelLabel").asInstanceOf[String]
    val nivelColor:    String = d("nivelColor").asInstanceOf[String]
    val notaMedia:     Double = d("notaMedia").asInstanceOf[Double]
    val spvEfic:       Double = d("spvEfic").asInstanceOf[Double]
    val bypassEfic:    Double = d("bypassEfic").asInstanceOf[Double]
    val psxgDelta:     Double = d("psxgDelta").asInstanceOf[Double]
    val winRate:       Double = d("winRate").asInstanceOf[Double]
    val bioFactor:     Double = d("bioFactor").asInstanceOf[Double]
    val edad:          Int    = d("edad").asInstanceOf[Int]
    val pj:            Int    = d("pj").asInstanceOf[Int]
    val limpias:       Int    = d("limpias").asInstanceOf[Int]
    val analisisIA:    String = d("analisisIA").asInstanceOf[String]
    val evoLabels: List[String] = d("evoLabels").asInstanceOf[List[String]]
    val evoScores: List[Double] = d("evoScores").asInstanceOf[List[Double]]
    val refs:      List[Int]    = d("refs").asInstanceOf[List[Int]]

    val valorStr   = if (valorEstimado >= 1000) s"${valorEstimado / 1000}K €" else s"$valorEstimado €"
    val psxgStr    = (if (psxgDelta >= 0) "+" else "") + f"$psxgDelta%.2f"
    val psxgColor  = if (psxgDelta >= 0.3) "success" else if (psxgDelta >= -0.3) "warning" else "danger"
    val rawScoreStr = f"$rawScore%.1f"

    // Barras de las 5 dimensiones (normalizadas 0-100)
    val dims = List(
      ("Nota Media",    math.min(100, ((notaMedia - 40.0) / 60.0 * 100).toInt), "primary"),
      ("SPV Score",     math.min(100, spvEfic.toInt),                           "info"),
      ("Bypass Efic.",  math.min(100, (bypassEfic * 100).toInt),                "warning"),
      ("PSxG+",         math.min(100, math.max(0, ((psxgDelta + 2.0) / 4.0 * 100).toInt)), "success"),
      ("Win Rate",      math.min(100, (winRate * 100).toInt),                   "danger")
    )

    val content = div(
      h4(cls := "fw-black text-white mb-4", "💰 Market Estimator"),

      // ── Valor principal ──
      div(cls := "row g-3 mb-4",
        div(cls := "col-md-4",
          div(cls := "card bg-dark border-warning text-center p-4",
            div(cls := "text-warning fw-bold small mb-1", "VALOR FORMATIVO ESTIMADO"),
            div(cls := "display-4 fw-black text-warning", valorStr),
            div(cls := "small text-muted mt-1", s"Basado en $pj partidos registrados")
          )
        ),
        div(cls := "col-md-4",
          div(cls := "card bg-dark border-secondary text-center p-4",
            div(cls := s"text-$nivelColor fw-bold small mb-1", "NIVEL FORMATIVO"),
            div(cls := s"h3 fw-black text-$nivelColor", nivelLabel),
            div(cls := "small text-muted mt-1", s"Percentil $percentil% — porteros academia")
          )
        ),
        div(cls := "col-md-4",
          div(cls := "card bg-dark border-secondary text-center p-4",
            div(cls := "text-muted fw-bold small mb-1", "PUNTUACION MODELO"),
            div(cls := "display-4 fw-black text-white", rawScoreStr),
            div(cls := "small text-muted mt-1", "/100 puntos")
          )
        )
      ),

      // ── Barra de percentil ──
      div(cls := "card bg-dark border-secondary p-3 mb-4",
        div(cls := "d-flex justify-content-between small text-muted mb-1",
          span("P10"), span("P25"), span("P50"), span("P75"), span("P90")
        ),
        div(cls := "progress mb-1", style := "height:22px;",
          div(cls := s"progress-bar bg-$nivelColor fw-bold",
            style := s"width:$percentil%",
            s"$percentil%")
        ),
        div(cls := "d-flex justify-content-between xx-small text-muted",
          span(s"${refs(0)}"), span(s"${refs(1)}"), span(s"${refs(2)}"),
          span(s"${refs(3)}"), span(s"${refs(4)}")
        ),
        div(cls := "xx-small text-muted mt-1 text-center",
          "Referencia: porteros de academia española del mismo grupo de edad")
      ),

      // ── KPIs de las 5 dimensiones ──
      div(cls := "card bg-dark border-secondary p-3 mb-4",
        div(cls := "fw-bold text-muted small text-uppercase mb-3",
          "Dimensiones del Valor"),
        div(
          frag(dims.map { case (label, score, color) =>
            div(cls := "mb-2",
              div(cls := "d-flex justify-content-between small mb-1",
                span(cls := "text-white fw-bold", label),
                span(cls := s"text-$color fw-bold", s"$score/100")
              ),
              div(cls := "progress", style := "height:10px; border-radius:5px;",
                div(cls := s"progress-bar bg-$color",
                  style := s"width:${score}%; border-radius:5px;")
              )
            )
          }: _*)
        )
      ),

      // ── Stats rápidas ──
      div(cls := "row g-2 mb-4",
        frag(Seq(
          (s"$edad años",      "Edad",             "secondary"),
          (f"$notaMedia%.1f",  "Nota media",        "primary"),
          (s"$limpias",        "Limpias",           "success"),
          (f"$bioFactor%.2f",  "Factor bio",        "warning"),
          (psxgStr,            "PSxG Delta",        psxgColor),
          (f"${winRate*100}%.0f%%", "Win Rate",     "info")
        ).map { case (v, l, c) =>
          div(cls := "col-6 col-md-2",
            div(cls := "card bg-dark border-secondary text-center p-2",
              div(cls := s"h5 fw-black text-$c mb-0", v),
              div(cls := "xx-small text-muted", l)
            )
          )
        }: _*)
      ),

      // ── Evolución por temporada ──
      if (evoLabels.nonEmpty)
        div(cls := "card bg-dark border-secondary p-3 mb-4",
          div(cls := "fw-bold text-muted small text-uppercase mb-3",
            "Evolución del Valor por Temporada"),
          div(style := "height:180px;",
            canvas(id := "chartEvoMarket")
          )
        )
      else div(),

      // ── Análisis IA ──
      div(cls := "card bg-dark border-warning p-3 mb-4",
        div(cls := "fw-bold text-warning small text-uppercase mb-2",
          "🤖 Informe de Captación IA"),
        div(cls := "text-white small", raw(analisisIA))
      ),

      // ── Nota metodológica ──
      div(cls := "alert alert-secondary small",
        raw("""<strong>Metodología:</strong> El valor se calcula mediante regresión lineal multivariable
        ponderada sobre 5 dimensiones (nota media 35%, SPV 20%, bypass rate 15%, PSxG delta 15%, win rate 10%,
        factor bio-banding 5%). Los percentiles se calculan contra una tabla de referencia calibrada
        para porteros de academia española por grupo de edad. Este modelo es orientativo — el valor
        real depende del mercado de transferencias, el interés de clubes concretos y factores no cuantificables.""")
      ),

      if (evoLabels.nonEmpty)
        script(raw(s"""
          new Chart(document.getElementById('chartEvoMarket'), {
            type: 'line',
            data: {
              labels: [${evoLabels.map(l => s"'$l'").mkString(",")}],
              datasets: [{
                label: 'Score de valor',
                data: [${evoScores.map(v => f"$v%.1f").mkString(",")}],
                borderColor: '#ffc107', backgroundColor: 'rgba(255,193,7,0.15)',
                tension: 0.3, fill: true, pointRadius: 5,
                pointBackgroundColor: '#ffc107'
              }]
            },
            options: {
              responsive: true, maintainAspectRatio: false,
              plugins: { legend: { display: false } },
              scales: {
                x: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                y: { min: 0, max: 100, ticks: { color: '#aaa' },
                     grid: { color: 'rgba(255,255,255,0.05)' } }
              }
            }
          });
        """))
      else span()
    )
    renderHtml(basePage("market-estimator", content))
  }

  // ── EFECTO MARIPOSA ──────────────────────────────────────────────────────
  @cask.get("/efecto-mariposa")
  def efectoMariposaPage(request: cask.Request) = withAuth(request) {
    val d    = DatabaseManager.getEfectoMariposa()
    val gear = DatabaseManager.getGearROI()

    if (!d.getOrElse("ok", false).asInstanceOf[Boolean]) {
      renderHtml(basePage("career", div(cls := "alert alert-secondary m-4", "Sin datos suficientes")))
    } else {
      val pj            = d("pj").asInstanceOf[Int]
      val cs            = d("cleanSheets").asInstanceOf[Int]
      val csWinRate     = d("csWinRate").asInstanceOf[Int]
      val nonCsWinRate  = d("nonCsWinRate").asInstanceOf[Int]
      val ganados       = d("ganados").asInstanceOf[Int]
      val empatados     = d("empatados").asInstanceOf[Int]
      val perdidos      = d("perdidos").asInstanceOf[Int]
      val notaMedia     = d("notaMedia").asInstanceOf[Double]
      val clutch        = d("clutchPoints").asInstanceOf[Int]
      val influence     = d("influenceData").asInstanceOf[List[Map[String, Any]]]

      val csRate        = if (pj > 0) (cs * 100 / pj) else 0
      val winRate       = if (pj > 0) (ganados * 100 / pj) else 0

      // Chart data for influence
      val gNotas  = influence.filter(_("res") == "G").map(m => f"${m("nota").asInstanceOf[Double]}%.1f").mkString("[",",","]")
      val eNotas  = influence.filter(_("res") == "E").map(m => f"${m("nota").asInstanceOf[Double]}%.1f").mkString("[",",","]")
      val pNotas  = influence.filter(_("res") == "P").map(m => f"${m("nota").asInstanceOf[Double]}%.1f").mkString("[",",","]")
      val gCnts   = influence.filter(_("res") == "G").map(_("cnt").asInstanceOf[Int].toString).mkString("[",",","]")
      val eCnts   = influence.filter(_("res") == "E").map(_("cnt").asInstanceOf[Int].toString).mkString("[",",","]")
      val pCnts   = influence.filter(_("res") == "P").map(_("cnt").asInstanceOf[Int].toString).mkString("[",",","]")

      val activeGripAlert = gear.filter(g =>
        g("activo").asInstanceOf[Boolean] && g("gripAlert").asInstanceOf[Boolean])

      val diff      = csWinRate - nonCsWinRate
      val diffColor  = if (diff > 0) "#20c997" else "#dc3545"
      val diffSign   = if (diff > 0) "+" else ""

      val content = basePage("career",
        div(cls := "container-fluid px-2",

          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(
              h4(cls := "text-white fw-black mb-0", "🦋 Efecto Mariposa"),
              span(cls := "text-muted small", "Tu impacto real en los resultados del equipo")
            ),
            a(href := "/career", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Carrera")
          ),

          // ── CLEAN SHEET IMPACT ──────────────────────────────────────────
          div(cls := "card bg-dark border-success shadow mb-3",
            div(cls := "card-header bg-success bg-opacity-10 border-success",
              span(cls := "text-success fw-bold small", "🛡️ IMPACTO CLEAN SHEET")
            ),
            div(cls := "card-body p-3",
              div(cls := "row g-3 text-center mb-3",
                div(cls := "col-4",
                  div(cls := "fw-black text-success", style := "font-size:2rem;", s"$csRate%"),
                  div(cls := "xx-small text-muted", "% Clean Sheets")
                ),
                div(cls := "col-4",
                  div(cls := "fw-black text-warning", style := "font-size:2rem;", s"$csWinRate%"),
                  div(cls := "xx-small text-muted", "Win rate CON CS")
                ),
                div(cls := "col-4",
                  div(cls := "fw-black text-danger", style := "font-size:2rem;", s"$nonCsWinRate%"),
                  div(cls := "xx-small text-muted", "Win rate SIN CS")
                )
              ),
              div(cls := "text-center p-2 rounded",
                style := s"background:${diffColor}18; border:1px solid ${diffColor}44;",
                div(cls := "fw-black", style := s"font-size:1.5rem; color:$diffColor;",
                  s"$diffSign$diff%"),
                div(cls := "xx-small text-muted",
                  "diferencial de win rate cuando mantienes la portería a cero")
              )
            )
          ),

          // ── CLUTCH POINTS ───────────────────────────────────────────────
          div(cls := "card bg-dark border-warning shadow mb-3",
            div(cls := "card-header bg-warning bg-opacity-10 border-warning",
              span(cls := "text-warning fw-bold small", "⚡ CLUTCH POINTS")
            ),
            div(cls := "card-body p-3",
              div(cls := "row g-3 text-center",
                div(cls := "col-4",
                  div(cls := "fw-black text-warning", style := "font-size:2.5rem;", clutch.toString),
                  div(cls := "xx-small text-muted", "Partidos clutch")
                ),
                div(cls := "col-4",
                  div(cls := "fw-black text-white", style := "font-size:2.5rem;", s"${clutch * 3}"),
                  div(cls := "xx-small text-muted", "Puntos salvados")
                ),
                div(cls := "col-4",
                  div(cls := "fw-black text-info", style := "font-size:2.5rem;", f"$notaMedia%.1f"),
                  div(cls := "xx-small text-muted", "Nota media global")
                )
              ),
              div(cls := "xx-small text-muted text-center mt-2 fst-italic",
                "Clutch = victoria por 1 gol con nota ≥ 7.5 — tu aportación fue decisiva"
              )
            )
          ),

          // ── GRÁFICO INFLUENCIA ──────────────────────────────────────────
          div(cls := "card bg-dark border-info shadow mb-3",
            div(cls := "card-header border-info text-info fw-bold small", "📊 NOTA vs RESULTADO"),
            div(cls := "card-body p-2",
              div(style := "height:220px;", canvas(id := "chartInfluencia")),
              div(cls := "d-flex justify-content-center gap-3 mt-2",
                frag(Seq(("#20c997","G","Victorias"), ("#ffc107","E","Empates"), ("#dc3545","P","Derrotas")).map {
                  case (c,_,lbl) => span(cls := "xx-small",
                    span(style := s"display:inline-block;width:10px;height:10px;background:$c;border-radius:2px;margin-right:4px;"),
                    lbl)
                }: _*)
              )
            )
          ),

          // ── GEAR ALERT ──────────────────────────────────────────────────
          if (activeGripAlert.nonEmpty)
            div(cls := "alert alert-warning border-warning shadow mb-3",
              div(cls := "fw-bold text-dark mb-1", "⚠️ ALERTA DE GRIP"),
              frag(activeGripAlert.map { g =>
                val gl = g("gripLoss").asInstanceOf[Int]
                div(cls := "small text-dark",
                  s"${g("nombre").asInstanceOf[String]}: ${gl}% de desgaste estimado — considera usar el par de reserva")
              }: _*)
            )
          else span(),

          // ── GEAR ROI ────────────────────────────────────────────────────
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header border-secondary text-warning fw-bold small", "🧤 ROI DE GUANTES"),
            div(cls := "card-body p-2",
              if (gear.isEmpty)
                div(cls := "text-muted small text-center py-2", "Sin guantes registrados")
              else
                div(cls := "table-responsive",
                  table(cls := "table table-dark table-sm mb-0 xx-small",
                    thead(tr(th("Guante"), th("Usos"), th("Desgaste"), th("Coste/PJ"), th("Nota ★"))),
                    tbody(frag(gear.map { g =>
                      val desg  = g("desgaste").asInstanceOf[Int]
                      val desgColor = if(desg>=80)"text-danger" else if(desg>=50)"text-warning" else "text-success"
                      val cpp   = g("costePorPartido").asInstanceOf[Double]
                      val nota  = g("notaMedia").asInstanceOf[Double]
                      tr(
                        td(cls := "fw-bold text-white", g("nombre").asInstanceOf[String].take(20)),
                        td(g("usos").asInstanceOf[Int].toString),
                        td(cls := desgColor, s"$desg%"),
                        td(if(cpp>0) f"€$cpp%.2f" else "—"),
                        td(cls := "text-warning", if(nota>0) f"$nota%.1f" else "—")
                      )
                    }: _*))
                  )
                )
            )
          ),

          script(src := "https://cdn.jsdelivr.net/npm/chart.js"),
          script(raw(s"""
            var ctx = document.getElementById('chartInfluencia');
            if (ctx) {
              new Chart(ctx, {
                type: 'scatter',
                data: {
                  datasets: [
                    { label: 'Victoria', data: $gNotas.map(function(n,i){return {x:n,y:$gCnts[i]||1};}),
                      backgroundColor: 'rgba(32,201,151,0.7)', pointRadius: 8 },
                    { label: 'Empate', data: $eNotas.map(function(n,i){return {x:n,y:$eCnts[i]||1};}),
                      backgroundColor: 'rgba(255,193,7,0.7)', pointRadius: 8 },
                    { label: 'Derrota', data: $pNotas.map(function(n,i){return {x:n,y:$pCnts[i]||1};}),
                      backgroundColor: 'rgba(220,53,69,0.7)', pointRadius: 8 }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { display: false } },
                  scales: {
                    x: { title: { display: true, text: 'Tu nota', color: '#888' },
                         min: 1, max: 10, ticks: { color: '#888' }, grid: { color: '#333' } },
                    y: { title: { display: true, text: 'Partidos', color: '#888' },
                         ticks: { color: '#888', stepSize: 1 }, grid: { color: '#333' } }
                  }
                }
              });
            }
          """))
        )
      )
      renderHtml(content)
    }
  }

  @cask.post("/gear/update-precio")
  def updateGearPrecio(request: cask.Request) = withAuth(request) {
    val body   = new String(request.data.readAllBytes(), "UTF-8")
    val params = body.split("&").map { p => val kv = p.split("=",2); java.net.URLDecoder.decode(kv(0),"UTF-8") -> (if(kv.length>1) java.net.URLDecoder.decode(kv(1),"UTF-8") else "") }.toMap
    val gearId = params.getOrElse("gearId","0").toIntOption.getOrElse(0)
    val precio = params.getOrElse("precio","0").toDoubleOption.getOrElse(0.0)
    DatabaseManager.updateGearPrecio(gearId, precio)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/efecto-mariposa"))
  }

  initialize()
}
