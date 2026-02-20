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
          div(cls := "me-3", style := "font-size: 30px;", if (i.tipo == "Guantes") "🧤" else "👟")

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
    // AQUÍ VA LA COMPARATIVA DE LEYENDAS
    raw(DatabaseManager.getLegendComparison()),
    div(cls:="card bg-secondary p-2 w-100 mt-3", form(action := "/career/new-season", method := "post", cls:="d-flex flex-column gap-2", div(label(cls:="form-label text-white small m-0 fw-bold", "Nueva Categoria:"), input(tpe := "text", name := "categoria", cls := "form-control form-control-sm fw-bold", placeholder := "Ej: Benjamin A", required := true)), button(tpe := "submit", cls := "btn btn-danger btn-sm fw-bold", onclick := "return confirm('Seguro?');", "Cerrar & Empezar")))), div(cls := "card shadow-sm border-0", div(cls := "card-body p-0 table-responsive", table(cls := "table table-hover tm-table mb-0", thead(tr(th("Cat"), th("Ficha"), th("PJ"), th("GC"), th("Media"))), tbody(listRows)))))); renderHtml(basePage("career", content)) }
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
    val stats = DatabaseManager.getPenaltyStats()

    val heatmapCells = for (z <- Seq("TL","TM","TR","ML","MM","MR","BL","BM","BR")) yield {
      val s     = stats.find(_.zona == z).getOrElse(PenaltyStat(z, 0, 0))
      val color = if (s.total > 0) "rgba(220, 53, 69, 0.6)" else "rgba(255,255,255,0.1)"
      div(cls := "goal-cell d-flex justify-content-center align-items-center flex-column",
        style := s"background-color:$color; border:1px solid #444;",
        span(cls := "fw-bold text-white", s.total.toString),
        span(cls := "xx-small text-light",
          if (s.total > 0) s"${(s.goles.toDouble / s.total * 100).toInt}% G" else "")
      )
    }

    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-center text-danger mb-4", "LABORATORIO PENALTIS"),

          // Formulario de registro
          div(cls := "card bg-dark border-danger shadow mb-4",
            div(cls := "card-header bg-danger text-white fw-bold", "REGISTRAR LANZAMIENTO"),
            div(cls := "card-body",
              form(action := "/penalties/save", method := "post",
                div(cls := "mb-3",
                  input(tpe := "text", name := "rival", cls := "form-control fw-bold",
                    placeholder := "Nombre Rival (Opcional)")
                ),
                div(cls := "row mb-3",
                  div(cls := "col-6",
                    label(cls := "small text-white fw-bold", "Zona Tiro"),
                    select(name := "zTiro", cls := "form-select fw-bold",
                      option(value := "TL", "Arriba Izq"), option(value := "TM", "Arriba Cen"),
                      option(value := "TR", "Arriba Der"), option(value := "ML", "Media Izq"),
                      option(value := "MM", "Media Cen"),  option(value := "MR", "Media Der"),
                      option(value := "BL", "Baja Izq"),   option(value := "BM", "Baja Cen"),
                      option(value := "BR", "Baja Der")
                    )
                  ),
                  div(cls := "col-6",
                    label(cls := "small text-warning fw-bold", "Salto Hector"),
                    select(name := "zSalto", cls := "form-select fw-bold",
                      option(value := "L", "Izquierda"),
                      option(value := "C", "Centro"),
                      option(value := "R", "Derecha")
                    )
                  )
                ),
                div(cls := "form-check mb-3",
                  input(cls := "form-check-input", tpe := "checkbox", name := "esGol", id := "golCheck"),
                  label(cls := "form-check-label text-white fw-bold", attr("for") := "golCheck", "Fue Gol")
                ),
                button(tpe := "submit", cls := "btn btn-danger w-100 fw-bold", "Registrar Penalti")
              )
            )
          ),

          // Mapa de calor
          div(cls := "card bg-dark border-secondary shadow",
            div(cls := "card-header text-center text-white", "MAPA DE CALOR (TIROS RIVALES)"),
            div(cls := "card-body d-flex justify-content-center",
              div(cls := "goal-grid-3x3", style := "width: 250px; height: 180px;",
                heatmapCells.toSeq
              )
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

  // 8. PIZARRA TÁCTICA

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

  initialize()
}
