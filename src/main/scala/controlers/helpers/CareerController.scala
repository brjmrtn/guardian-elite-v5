import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object CareerController extends cask.Routes {

  // Parseo manual de body application/x-www-form-urlencoded (mas fiable que @cask.postForm con fetch)
  private def parseBody(request: cask.Request): Map[String, String] = {
    val body = new String(request.data.readAllBytes(), "UTF-8")
    body.split("&").filter(_.nonEmpty).map { p =>
      val kv = p.split("=", 2)
      java.net.URLDecoder.decode(kv(0), "UTF-8") -> (if (kv.length > 1) java.net.URLDecoder.decode(kv(1), "UTF-8") else "")
    }.toMap
  }

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

  private def resilienceWidget() = {
    val resilience = DatabaseManager.getResilienceIndex()
    val resIndice   = resilience("indice").asInstanceOf[Double]
    val resPerfil   = resilience("perfil").asInstanceOf[String]
    val resRecom    = resilience("recomendacion").asInstanceOf[String]
    val resEventos  = resilience("eventos").asInstanceOf[List[Map[String, Any]]]

    if (resIndice <= 0) span()
    else {
      val resColor = if (resIndice < 5) "danger" else if (resIndice <= 7) "warning" else "success"
      val labelsJs  = resEventos.map(e => s""""${e("label").asInstanceOf[String]}"""").mkString("[", ",", "]")
      val antesJs   = resEventos.map(e => f"${e("antes").asInstanceOf[Double]}%.1f").mkString("[", ",", "]")
      val despuesJs = resEventos.map(e => f"${e("despues").asInstanceOf[Double]}%.1f").mkString("[", ",", "]")

      div(cls := "card bg-dark border-secondary shadow mb-4 w-100",
        div(cls := "card-header text-white fw-bold small text-center", "🧠 ÍNDICE DE RESILIENCIA MENTAL"),
        div(cls := "card-body p-3",
          div(cls := "d-flex align-items-center gap-3 mb-3",
            div(cls := s"badge bg-$resColor", style := "font-size:1.6rem; padding:10px 18px;", f"$resIndice%.0f/10"),
            div(cls := "text-white fw-bold small", resPerfil)
          ),
          if (resRecom.nonEmpty)
            div(cls := "alert alert-secondary small mb-3",
              strong("Recomendación para el padre: "), resRecom)
          else span(),
          if (resEventos.size >= 2) div(style := "height:200px;", canvas(id := "chartResilience")) else span()
        ),
        if (resEventos.size >= 2) frag(
          script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
          script(raw(s"""
            new Chart(document.getElementById('chartResilience'), {
              type: 'line',
              data: {
                labels: $labelsJs,
                datasets: [
                  { label: 'Antes del evento', data: $antesJs, borderColor: '#6c757d', backgroundColor: 'transparent', borderDash: [4,4], tension: 0.3, pointRadius: 3 },
                  { label: 'Después del evento', data: $despuesJs, borderColor: '#ffc107', backgroundColor: 'rgba(255,193,7,0.1)', fill: true, tension: 0.3, pointRadius: 3 }
                ]
              },
              options: {
                responsive: true, maintainAspectRatio: false,
                plugins: { legend: { labels: { color: '#ccc', font: { size: 10 } } } },
                scales: {
                  x: { ticks: { color: '#aaa', font: { size: 9 } }, grid: { color: 'rgba(255,255,255,0.05)' } },
                  y: { min: 0, max: 10, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                }
              }
            });
          """))
        ) else span()
      )
    }
  }

  // ── MODULO 2 (sesion actual): VELOCIDAD DE APRENDIZAJE ──────────────────
  private def learningVelocityWidget() = {
    val d = DatabaseManager.getLearningVelocityIndex()
    val suficiente = d("suficiente").asInstanceOf[Boolean]

    if (!suficiente) {
      div(cls := "card bg-dark border-secondary shadow mb-4 w-100",
        div(cls := "card-header text-white fw-bold small text-center", "⚡ VELOCIDAD DE APRENDIZAJE"),
        div(cls := "card-body text-center text-muted small py-4",
          "Registra las fechas de inicio de trabajo para calcular el índice")
      )
    } else {
      val indice       = d("indice").asInstanceOf[Double]
      val porHabilidad = d("porHabilidad").asInstanceOf[List[Map[String, Any]]]
      val analisisIA   = d("analisisIA").asInstanceOf[String]
      val color = if (indice < 60) "danger" else if (indice <= 100) "warning" else "success"

      val labelsJs = porHabilidad.map(h => s""""${h("habilidad").asInstanceOf[String].replace("\"", "")}"""").mkString("[", ",", "]")
      val diasJs   = porHabilidad.map(h => h("dias").asInstanceOf[Int].toString).mkString("[", ",", "]")

      div(cls := s"card bg-dark border-$color shadow mb-4 w-100",
        div(cls := "card-header text-white fw-bold small text-center", "⚡ VELOCIDAD DE APRENDIZAJE"),
        div(cls := "card-body p-3",
          div(cls := "text-center mb-3",
            div(cls := s"display-4 fw-bold text-$color", f"$indice%.0f"),
            div(cls := "xx-small text-muted", "ÍNDICE (100 = referencia · 30 días de media)")
          ),
          div(style := "height:180px;", canvas(id := "chartLearningVelocity")),
          if (analisisIA.nonEmpty)
            div(cls := "alert alert-secondary small mt-3 mb-0", style := "white-space:pre-wrap;", analisisIA)
          else span()
        ),
        script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
        script(raw(s"""
          new Chart(document.getElementById('chartLearningVelocity'), {
            type: 'bar',
            data: {
              labels: $labelsJs,
              datasets: [{ label: 'Días para consolidar', data: $diasJs, backgroundColor: 'rgba(212,175,55,0.7)', borderColor: '#d4af37', borderWidth: 1, borderRadius: 4 }]
            },
            options: {
              indexAxis: 'y',
              responsive: true, maintainAspectRatio: false,
              plugins: { legend: { display: false } },
              scales: {
                x: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                y: { ticks: { color: '#aaa', font: { size: 9 } }, grid: { color: 'rgba(255,255,255,0.05)' } }
              }
            }
          });
        """))
      )
    }
  }

  // ── MODULO 3 (sesion actual): PERFIL BAJO PRESION ────────────────────────
  private def presionWidget() = {
    val d = DatabaseManager.getPresionPattern()
    val total = d("total").asInstanceOf[Int]

    if (total == 0) {
      div(cls := "card bg-dark border-secondary shadow mb-4 w-100",
        div(cls := "card-header text-white fw-bold small text-center", "🧠 PERFIL BAJO PRESIÓN"),
        div(cls := "card-body text-center text-muted small py-4",
          "Sin partidos con goles encajados registrados todavía")
      )
    } else {
      val distribucion = d("distribucion").asInstanceOf[List[Map[String, Any]]]
      val masFrecuente = d("masFrecuente").asInstanceOf[String]
      val analisisIA   = d("analisisIA").asInstanceOf[String]

      val labelsJs = distribucion.map(x => s""""${x("label").asInstanceOf[String]}"""").mkString("[", ",", "]")
      val dataJs   = distribucion.map(x => x("pct").asInstanceOf[Int].toString).mkString("[", ",", "]")

      div(cls := "card bg-dark border-info shadow mb-4 w-100",
        div(cls := "card-header text-info fw-bold small text-center", "🧠 PERFIL BAJO PRESIÓN"),
        div(cls := "card-body p-3",
          div(cls := "text-center mb-3",
            div(cls := "h4 fw-bold text-info", masFrecuente),
            div(cls := "xx-small text-muted", "COMPORTAMIENTO MÁS FRECUENTE")
          ),
          div(style := "height:200px;", canvas(id := "chartPresion")),
          if (total < 5)
            div(cls := "alert alert-secondary small mt-3 mb-0", "Registra al menos 5 partidos con goles encajados para ver el patrón")
          else if (analisisIA.nonEmpty)
            div(cls := "alert alert-secondary small mt-3 mb-0", style := "white-space:pre-wrap;", analisisIA)
          else span()
        ),
        script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
        script(raw(s"""
          new Chart(document.getElementById('chartPresion'), {
            type: 'doughnut',
            data: {
              labels: $labelsJs,
              datasets: [{ data: $dataJs, backgroundColor: ['#20c997','#0dcaf0','#8b5cf6','#dc3545','#ffc107','#6c757d'] }]
            },
            options: {
              responsive: true, maintainAspectRatio: false,
              plugins: { legend: { position: 'bottom', labels: { color: '#ccc', font: { size: 9 } } } }
            }
          });
        """))
      )
    }
  }

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
          resilienceWidget(),
          learningVelocityWidget(),
          presionWidget(),
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
      ("Nota Media",    math.max(0, math.min(100, ((notaMedia * 10.0 - 40.0) / 60.0 * 100).toInt)), "primary"),
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

  // ── FOOTBAR (SENSOR GPS DE RENDIMIENTO) ─────────────────────────────────
  @cask.get("/footbar")
  def footbarPage(request: cask.Request) = withAuth(request) {
    val d          = DatabaseManager.getFootbarPageData()
    val rows        = d("rows").asInstanceOf[List[Map[String, Any]]]
    val totalSesiones = d("totalSesiones").asInstanceOf[Int]

    val content = if (totalSesiones < 3) {
      div(
        h4(cls := "fw-black text-white mb-4", "🦵 Footbar"),
        div(cls := "alert alert-secondary text-center",
          "Necesitas al menos 3 partidos con datos Footbar para ver patrones")
      )
    } else {
      val avgDistanciaKm  = d("avgDistanciaKm").asInstanceOf[Double]
      val maxSprintKmh    = d("maxSprintKmh").asInstanceOf[Double]
      val avgPases        = d("avgPases").asInstanceOf[Double]
      val correlacionNota = d("correlacionNota").asInstanceOf[Double]

      val scatterData = rows.map { r =>
        val dist = r("distanciaKm").asInstanceOf[Double]
        val nota = r("nota").asInstanceOf[Double]
        s"{x:$dist,y:$nota}"
      }.mkString("[", ",", "]")

      div(
        h4(cls := "fw-black text-white mb-4", "🦵 Footbar"),

        // ── KPIs ──
        div(cls := "row g-3 mb-4",
          Seq(
            ("Distancia media/partido", f"$avgDistanciaKm%.2f km", "primary"),
            ("Sprint máx histórico",    f"$maxSprintKmh%.1f km/h", "danger"),
            ("Pases medios/partido",    f"$avgPases%.0f",          "info"),
            ("Correlación dist↔nota",   f"$correlacionNota%.2f",   "warning")
          ).map { case (label, v, color) =>
            div(cls := "col-md-3 col-6",
              div(cls := s"card bg-dark border-$color text-center p-3",
                div(cls := s"text-$color fw-bold xx-small mb-1", label.toUpperCase),
                div(cls := "h3 fw-black text-white mb-0", v)
              )
            )
          }
        ),

        // ── Gráfico scatter distancia vs nota ──
        div(cls := "card bg-dark border-secondary p-3 mb-4",
          div(cls := "fw-bold text-muted small text-uppercase mb-3", "Distancia (km) vs. Nota"),
          div(style := "height:260px;", canvas(id := "chartFootbarScatter")),
          script(src := "https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"),
          script(raw(s"""
            new Chart(document.getElementById('chartFootbarScatter'), {
              type: 'scatter',
              data: { datasets: [{
                label: 'Partidos',
                data: $scatterData,
                backgroundColor: 'rgba(255,193,7,0.7)',
                pointRadius: 6
              }]},
              options: {
                responsive: true, maintainAspectRatio: false,
                plugins: { legend: { display: false } },
                scales: {
                  x: { title: { display: true, text: 'Distancia (km)', color: '#aaa' },
                       ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                  y: { title: { display: true, text: 'Nota', color: '#aaa' }, min: 0, max: 10,
                       ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                }
              }
            });
          """))
        ),

        // ── Tabla por partido ──
        div(cls := "card bg-dark border-secondary p-3 mb-4",
          div(cls := "fw-bold text-muted small text-uppercase mb-3", "Partidos con datos Footbar"),
          div(style := "overflow-x:auto;",
            table(cls := "table table-dark table-sm mb-0",
              thead(tr(
                th("Fecha"), th("Rival"), th("Nota"), th("Distancia"),
                th("Sprint máx"), th("Pases"), th("Disparos")
              )),
              tbody(
                frag(rows.map { r =>
                  tr(
                    td(r("fecha").asInstanceOf[String].take(10)),
                    td(fixEncoding(r("rival").asInstanceOf[String])),
                    td(f"${r("nota").asInstanceOf[Double]}%.1f"),
                    td(f"${r("distanciaKm").asInstanceOf[Double]}%.2f km"),
                    td(f"${r("sprintMaxKmh").asInstanceOf[Double]}%.1f km/h"),
                    td(r("pases").asInstanceOf[Int].toString),
                    td(r("disparos").asInstanceOf[Int].toString)
                  )
                }: _*)
              )
            )
          )
        )
      )
    }
    renderHtml(basePage("footbar", content))
  }

  // ── MODULO 2: CHECKLIST DE HABILIDADES DE PORTERO ───────────────────────
  private val skillCategoryOrder = Seq("Tecnica basica", "Juego con los pies", "Comportamiento en el area", "Mental")

  private def skillRow(s: GoalkeeperSkill) = {
    val equipoWarning = if (s.conseguido && s.contextoConseguido.contains("EQUIPO"))
      div(cls := "badge bg-warning text-dark xx-small mt-1", "⚠️ Confirmar en academia o partido")
    else span()

    div(cls := "d-flex align-items-start justify-content-between gap-2 py-2 border-bottom border-secondary",
      div(cls := "flex-fill",
        div(cls := "d-flex align-items-center gap-2",
          span(if (s.conseguido) "✅" else "⬜"),
          span(cls := (if (s.conseguido) "text-white fw-bold" else "text-muted"), s.habilidad)
        ),
        if (s.conseguido)
          div(cls := "xx-small text-muted",
            s"${s.fechaConseguido.getOrElse("")} · ${s.contextoConseguido.getOrElse("")}")
        else span(),
        equipoWarning,
        form(action := "/goalkeeper-skills/notes", method := "post", cls := "d-flex gap-1 mt-1",
          input(tpe := "hidden", name := "skillId", value := s.id.toString),
          input(tpe := "text", name := "notas", value := s.notas,
            cls := "form-control form-control-sm bg-dark text-white border-secondary xx-small",
            placeholder := "Notas..."),
          button(tpe := "submit", cls := "btn btn-sm btn-outline-secondary", "💾")
        )
      ),
      div(
        if (!s.conseguido)
          form(action := "/goalkeeper-skills/toggle", method := "post", cls := "d-flex flex-column gap-1", style := "min-width:170px;",
            input(tpe := "hidden", name := "skillId", value := s.id.toString),
            input(tpe := "hidden", name := "achieved", value := "true"),
            if (s.fechaInicioTrabajo.isEmpty)
              div(
                label(cls := "xx-small text-muted", "¿Cuándo empezaste a trabajarla? (opcional)"),
                input(tpe := "date", name := "fechaInicioTrabajo",
                  cls := "form-control form-control-sm bg-dark text-white border-secondary")
              )
            else span(),
            div(cls := "d-flex gap-1",
              select(name := "contexto", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                option(value := "ACADEMIA", "Academia"),
                option(value := "PARTIDO", "Partido"),
                option(value := "EQUIPO", "Equipo")
              ),
              button(tpe := "submit", cls := "btn btn-sm btn-success", "✔")
            )
          )
        else
          form(action := "/goalkeeper-skills/toggle", method := "post",
            input(tpe := "hidden", name := "skillId", value := s.id.toString),
            input(tpe := "hidden", name := "achieved", value := "false"),
            button(tpe := "submit", cls := "btn btn-sm btn-outline-danger", "✕")
          )
      )
    )
  }

  @cask.get("/goalkeeper-skills")
  def goalkeeperSkillsPage(request: cask.Request) = withAuth(request) {
    val skills = DatabaseManager.getGoalkeeperSkills()
    val total = skills.size
    val conseguidas = skills.count(_.conseguido)
    val pctGlobal = if (total > 0) conseguidas * 100 / total else 0
    val pctColor = if (pctGlobal >= 70) "success" else if (pctGlobal >= 40) "warning" else "danger"

    val porCategoria = skills.groupBy(_.categoria).toList.sortBy { case (cat, _) =>
      val idx = skillCategoryOrder.indexOf(cat)
      if (idx >= 0) idx else 999
    }

    // ── MODULO 3: VENTANAS ACTIVAS AHORA ────────────────────────────────────
    val cardData      = DatabaseManager.getLatestCardData()
    val edadActual     = DatabaseManager.calcularEdadExacta(cardData.fechaNacimiento)
    val activeWindows = DatabaseManager.getActiveWindows(edadActual)

    val ventanasWidget = if (activeWindows.isEmpty) div() else {
      div(cls := "card bg-dark border-info shadow mb-4",
        div(cls := "card-header border-info text-info fw-bold small text-center", "⏰ VENTANAS ACTIVAS AHORA"),
        div(cls := "card-body p-3",
          frag(activeWindows.map { w =>
            val ventana     = w("ventana").asInstanceOf[String]
            val descripcion = w("descripcion").asInstanceOf[String]
            val edadFin     = w("edadFin").asInstanceOf[Int]
            val urgente     = w("urgente").asInstanceOf[Boolean]
            val pendientes  = w("skillsPendientes").asInstanceOf[List[Map[String, Any]]]
            div(cls := "mb-3 pb-3 border-bottom border-secondary",
              div(cls := "d-flex justify-content-between align-items-start",
                div(
                  div(cls := "fw-bold text-white", ventana),
                  div(cls := "xx-small text-muted", s"$descripcion · cierra a los $edadFin años")
                ),
                if (urgente) span(cls := "badge bg-danger", "⚠️ URGENTE") else span()
              ),
              if (pendientes.isEmpty)
                div(cls := "xx-small text-success mt-1", "✅ Sin habilidades pendientes en esta ventana")
              else
                div(cls := "mt-2",
                  frag(pendientes.map { p =>
                    div(cls := "xx-small text-warning", s"⬜ ${p("habilidad").asInstanceOf[String]}")
                  }: _*)
                )
            )
          }: _*)
        )
      )
    }

    val content = basePage("goalkeeper-skills",
      div(cls := "row justify-content-center",
        div(cls := "col-md-9 col-12",
          h2(cls := "text-white mb-4 text-center", "🧤 Checklist de Habilidades"),

          ventanasWidget,

          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "d-flex justify-content-between small text-muted mb-1",
              span("PROGRESO GLOBAL"), span(s"$conseguidas/$total")
            ),
            div(cls := "progress", style := "height:20px;",
              div(cls := s"progress-bar bg-$pctColor fw-bold", style := s"width:$pctGlobal%;", s"$pctGlobal%")
            )
          ),

          frag(porCategoria.map { case (cat, catSkills) =>
            val catTotal = catSkills.size
            val catDone = catSkills.count(_.conseguido)
            val catPct = if (catTotal > 0) catDone * 100 / catTotal else 0
            div(cls := "card bg-dark border-secondary p-3 mb-3",
              div(cls := "d-flex justify-content-between align-items-center mb-2",
                span(cls := "fw-bold text-white", cat.toUpperCase),
                span(cls := "badge bg-secondary", s"$catPct%")
              ),
              div(cls := "progress mb-3", style := "height:6px;",
                div(cls := "progress-bar bg-info", style := s"width:$catPct%;")
              ),
              frag(catSkills.map(skillRow): _*)
            )
          }: _*)
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/goalkeeper-skills/toggle")
  def toggleSkill(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val skillId = p.getOrElse("skillId", "0").toIntOption.getOrElse(0)
    val achieved = p.getOrElse("achieved", "false") == "true"
    val contexto = p.getOrElse("contexto", "")
    val fechaInicioTrabajo = p.getOrElse("fechaInicioTrabajo", "")
    DatabaseManager.setSkillAchieved(skillId, achieved, contexto, fechaInicioTrabajo)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/goalkeeper-skills"))
  }

  @cask.post("/goalkeeper-skills/notes")
  def saveSkillNotes(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val skillId = p.getOrElse("skillId", "0").toIntOption.getOrElse(0)
    DatabaseManager.updateSkillNotes(skillId, p.getOrElse("notas", ""))
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/goalkeeper-skills"))
  }

  // ── MODULO 3: VISIBILIDAD Y OPORTUNIDADES ───────────────────────────────
  private def opportunityTypeColor(tipo: String): String = tipo match {
    case "TORNEO"   => "primary"
    case "PRUEBA"   => "warning"
    case "CONTACTO" => "info"
    case "OJEADOR"  => "success"
    case "CAMPAMENTO" => "secondary"
    case _          => "secondary"
  }

  @cask.get("/opportunities")
  def opportunitiesPage(request: cask.Request) = withAuth(request) {
    val opps = DatabaseManager.getOpportunities()
    val total = opps.size
    val pendientes = opps.count(o => o.seguimiento.nonEmpty && !o.seguimientoCompletado)
    val contacts = DatabaseManager.getContacts()
    val contactsById = contacts.map(c => c.id -> c).toMap

    val content = basePage("opportunities",
      div(cls := "row justify-content-center",
        div(cls := "col-md-9 col-12",
          h2(cls := "text-white mb-4 text-center", "🏆 Visibilidad y Oportunidades"),

          // KPIs
          div(cls := "row g-2 mb-4",
            div(cls := "col-6",
              div(cls := "card bg-dark border-secondary text-center py-3",
                div(cls := "text-white fw-bold", style := "font-size:28px;", total.toString),
                div(cls := "xx-small text-muted mt-1", "OPORTUNIDADES TOTALES")
              )
            ),
            div(cls := "col-6",
              div(cls := s"card bg-dark border-${if (pendientes > 0) "danger" else "secondary"} text-center py-3",
                div(cls := s"${if (pendientes > 0) "text-danger" else "text-white"} fw-bold", style := "font-size:28px;", pendientes.toString),
                div(cls := "xx-small text-muted mt-1", "SEGUIMIENTO PENDIENTE"),
                if (pendientes > 0) div(cls := "badge bg-danger mt-1", "⚠️ Requiere atención") else span()
              )
            )
          ),

          // Formulario nueva oportunidad
          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-white small text-uppercase mb-3", "➕ Nueva oportunidad"),
            form(action := "/opportunities/save", method := "post",
              div(cls := "row g-2 mb-2",
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "FECHA"),
                  input(tpe := "date", name := "fecha", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                    value := java.time.LocalDate.now().toString, required := true)
                ),
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "TIPO"),
                  select(name := "tipo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                    option(value := "TORNEO", "Torneo"),
                    option(value := "PRUEBA", "Prueba"),
                    option(value := "CONTACTO", "Contacto"),
                    option(value := "OJEADOR", "Ojeador"),
                    option(value := "CAMPAMENTO", "Campamento")
                  )
                )
              ),
              div(cls := "mb-2",
                label(cls := "xx-small text-muted fw-bold", "CLUB / ENTIDAD"),
                input(tpe := "text", name := "clubOEntidad", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Ej: Real Madrid Cantera")
              ),
              div(cls := "mb-2",
                label(cls := "xx-small text-muted fw-bold", "DESCRIPCIÓN"),
                textarea(name := "descripcion", cls := "form-control form-control-sm bg-dark text-white border-secondary", rows := "2")()
              ),
              div(cls := "mb-2",
                label(cls := "xx-small text-muted fw-bold", "RESULTADO (si ya se conoce)"),
                input(tpe := "text", name := "resultado", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Ej: Convocado a segunda fase")
              ),
              div(cls := "mb-3",
                label(cls := "xx-small text-muted fw-bold", "SEGUIMIENTO PENDIENTE (opcional)"),
                input(tpe := "text", name := "seguimiento", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Ej: Esperar respuesta del club en 2 semanas")
              ),
              div(cls := "mb-3",
                label(cls := "xx-small text-muted fw-bold", "CONTACTO ASOCIADO (opcional)"),
                select(name := "contactId", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                  option(value := "", "— Sin contacto —"),
                  frag(contacts.map(c => option(value := c.id.toString, fixEncoding(c.nombre))): _*)
                )
              ),
              button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "Guardar")
            )
          ),

          // Lista cronológica inversa
          if (opps.isEmpty)
            div(cls := "alert alert-secondary text-center", "Sin oportunidades registradas todavía")
          else
            frag(opps.map { o =>
              div(cls := "card bg-dark border-secondary p-3 mb-2",
                div(cls := "d-flex justify-content-between align-items-start mb-1",
                  div(
                    span(cls := s"badge bg-${opportunityTypeColor(o.tipo)} me-2", o.tipo),
                    span(cls := "fw-bold text-white", if (o.clubOEntidad.nonEmpty) fixEncoding(o.clubOEntidad) else "—")
                  ),
                  span(cls := "xx-small text-muted", o.fecha.take(10))
                ),
                if (o.descripcion.nonEmpty) div(cls := "small text-light mb-2", fixEncoding(o.descripcion)) else span(),
                o.contactId.flatMap(contactsById.get) match {
                  case Some(c) => div(cls := "xx-small text-info mb-2", "👥 ", a(href := s"/contacts/${c.id}", cls := "text-info", fixEncoding(c.nombre)))
                  case None    => span()
                },
                form(action := "/opportunities/resultado", method := "post", cls := "d-flex gap-1 mb-2",
                  input(tpe := "hidden", name := "id", value := o.id.toString),
                  input(tpe := "text", name := "resultado", value := o.resultado,
                    cls := "form-control form-control-sm bg-dark text-white border-secondary xx-small",
                    placeholder := "Resultado..."),
                  button(tpe := "submit", cls := "btn btn-sm btn-outline-secondary", "💾")
                ),
                if (o.seguimiento.nonEmpty)
                  div(cls := "d-flex justify-content-between align-items-center",
                    div(cls := "xx-small",
                      span(cls := "text-muted fw-bold", "Seguimiento: "),
                      span(cls := (if (o.seguimientoCompletado) "text-success" else "text-warning"), o.seguimiento)
                    ),
                    if (!o.seguimientoCompletado)
                      form(action := "/opportunities/complete", method := "post",
                        input(tpe := "hidden", name := "id", value := o.id.toString),
                        button(tpe := "submit", cls := "btn btn-sm btn-success", "✅ Seguimiento completado")
                      )
                    else div(cls := "badge bg-success", "✅ Completado")
                  )
                else span()
              )
            }: _*)
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/opportunities/save")
  def saveOpportunity(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val contactId = p.getOrElse("contactId", "").toIntOption
    DatabaseManager.saveOpportunity(
      p.getOrElse("fecha", ""), p.getOrElse("tipo", "OTRO"),
      p.getOrElse("descripcion", ""), p.getOrElse("clubOEntidad", ""),
      p.getOrElse("resultado", ""), p.getOrElse("seguimiento", ""), contactId
    )
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/opportunities"))
  }

  @cask.postForm("/opportunities/resultado")
  def updateOpportunityResultado(id: Int, resultado: String) = {
    DatabaseManager.updateOpportunityResultado(id, resultado)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/opportunities"))
  }

  @cask.postForm("/opportunities/complete")
  def completeOpportunitySeguimiento(id: Int) = {
    DatabaseManager.completeSeguimiento(id)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/opportunities"))
  }

  // ── MODULO 5: BENCHMARKING CONTRA PORTEROS DE SU EDAD ───────────────────
  @cask.get("/benchmark")
  def benchmarkPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getBenchmark()
    val sinDatos = d("sinDatos").asInstanceOf[Boolean]
    val raeFactor     = d("raeFactor").asInstanceOf[Double]
    val notaMediaReal = d("notaMediaReal").asInstanceOf[Double]
    val notaMediaRae  = d("notaMediaRae").asInstanceOf[Double]
    val pctCSReal     = d("pctCSReal").asInstanceOf[Int]
    val pctCSRae      = d("pctCSRae").asInstanceOf[Int]
    val winRateReal   = d("winRateReal").asInstanceOf[Int]
    val winRateRae    = d("winRateRae").asInstanceOf[Int]

    val raeTable = div(cls := "card bg-dark border-info shadow mb-3",
      div(cls := "card-header text-info fw-bold small", "⚖️ AJUSTE POR EDAD RELATIVA (RAE)"),
      div(cls := "card-body p-0",
        div(cls := "table-responsive",
          table(cls := "table table-dark table-sm mb-0 small",
            thead(tr(th("Métrica"), th(cls := "text-center", "Valor real"), th(cls := "text-center", "Valor RAE-ajustado"))),
            tbody(
              tr(td("Nota media"), td(cls := "text-center", f"$notaMediaReal%.1f"), td(cls := "text-center text-info fw-bold", f"$notaMediaRae%.1f")),
              tr(td("Porterías a 0"), td(cls := "text-center", s"$pctCSReal%"), td(cls := "text-center text-info fw-bold", s"$pctCSRae%")),
              tr(td("Win rate"), td(cls := "text-center", s"$winRateReal%"), td(cls := "text-center text-info fw-bold", s"$winRateRae%"))
            )
          )
        )
      ),
      div(cls := "card-body pt-0",
        div(cls := "xx-small text-muted fst-italic",
          f"Ajuste por Efecto de Edad Relativa (factor $raeFactor%.2f) — Héctor nació en junio. Los jugadores nacidos en enero-marzo tienen hasta 6 meses más de desarrollo físico y cognitivo a esta edad.")
      )
    )

    val content = basePage("benchmark",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-4",
            h2(cls := "text-white mb-0", "📊 Benchmark"),
            form(action := "/benchmark/refresh", method := "post",
              button(tpe := "submit", cls := "btn btn-outline-warning btn-sm fw-bold", "🔄 Actualizar benchmark"))
          ),
          if (sinDatos)
            div(cls := "alert alert-secondary text-center", "Necesitas al menos 3 partidos registrados para generar el benchmark")
          else frag(
            raeTable,
            div(cls := "card bg-dark border-primary shadow mb-3",
              div(cls := "card-header text-primary fw-bold small", "📈 PERCENTIL DE PROGRESIÓN"),
              div(cls := "card-body text-light small", d("percentil").asInstanceOf[String])
            ),
            div(cls := "card bg-dark border-warning shadow mb-3",
              div(cls := "card-header text-warning fw-bold small", "🎯 ÁREAS PRIORITARIAS"),
              div(cls := "card-body text-light small", d("areas").asInstanceOf[String])
            ),
            div(cls := "card bg-dark border-success shadow mb-3",
              div(cls := "card-header text-success fw-bold small", "⭐ REFERENCIA REAL"),
              div(cls := "card-body text-light small", d("referencia").asInstanceOf[String])
            )
          )
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/benchmark/refresh")
  def refreshBenchmark(request: cask.Request) = withAuth(request) {
    DatabaseManager.invalidateBenchmarkCache()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/benchmark"))
  }

  // ── MODULO 6: PERIODIZACION ANUAL ───────────────────────────────────────
  private def periodizationTipoLabel(tipo: String): String = tipo match {
    case "CARGA_ALTA"        => "Carga alta"
    case "DESCARGA"          => "Descarga"
    case "TORNEO_CLAVE"      => "Torneo clave"
    case "VENTANA_ACADEMIAS" => "Ventana academias"
    case "EVALUACION"        => "Evaluación"
    case "DESCANSO"          => "Descanso"
    case _                   => tipo
  }

  @cask.get("/periodization")
  def periodizationPage(request: cask.Request) = withAuth(request) {
    val blocks = DatabaseManager.getPeriodization()
    val today = java.time.LocalDate.now()
    val yearStart = java.time.LocalDate.of(today.getYear, 1, 1)
    val yearLen = java.time.LocalDate.of(today.getYear, 12, 31).toEpochDay - yearStart.toEpochDay + 1
    def pct(d: Long): Double = math.max(0.0, math.min(100.0, d.toDouble / yearLen * 100.0))
    val hoyPct = pct(today.toEpochDay - yearStart.toEpochDay)

    val activo = blocks.find { b =>
      try {
        val ini = java.time.LocalDate.parse(b.fechaInicio); val fin = java.time.LocalDate.parse(b.fechaFin)
        !today.isBefore(ini) && !today.isAfter(fin)
      } catch { case _: Exception => false }
    }

    val monthLabels = Seq("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

    val timelineBlocks = blocks.flatMap { b =>
      try {
        val ini = java.time.LocalDate.parse(b.fechaInicio); val fin = java.time.LocalDate.parse(b.fechaFin)
        val yearEnd = yearStart.plusDays(yearLen - 1)
        if (fin.isBefore(yearStart) || ini.isAfter(yearEnd)) None
        else {
          val startD = math.max(0L, ini.toEpochDay - yearStart.toEpochDay)
          val endD   = math.min(yearLen, fin.toEpochDay - yearStart.toEpochDay + 1)
          val left = pct(startD); val width = math.max(0.6, pct(endD) - pct(startD))
          Some(div(cls := "position-absolute top-0 h-100 rounded",
            style := s"left:$left%; width:$width%; background:${b.color}; opacity:0.85;",
            attr("title") := s"${fixEncoding(b.nombre)} (${periodizationTipoLabel(b.tipo)})"))
        }
      } catch { case _: Exception => None }
    }

    val content = basePage("periodization",
      div(cls := "row justify-content-center",
        div(cls := "col-md-9 col-12",
          h2(cls := "text-white mb-4 text-center", "📅 Periodización Anual"),

          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-muted small text-uppercase mb-2", s"Año ${today.getYear}"),
            div(cls := "position-relative", style := "height:50px; background:#1a1a1a; border-radius:6px; overflow:hidden; margin-top:16px;",
              frag(timelineBlocks: _*),
              div(cls := "position-absolute top-0 h-100", style := s"left:$hoyPct%; width:2px; background:#fff; z-index:5;"),
              div(cls := "position-absolute", style := s"left:$hoyPct%; top:-16px; transform:translateX(-50%); font-size:9px; color:#fff; font-weight:bold; white-space:nowrap;", "▼ HOY")
            ),
            div(cls := "d-flex justify-content-between xx-small text-muted mt-2",
              frag(monthLabels.map(m => span(m)): _*)
            )
          ),

          activo match {
            case Some(b) =>
              div(cls := "card bg-dark shadow mb-4", style := s"border-color:${b.color};",
                div(cls := "card-header fw-bold small", style := s"color:${b.color};", "PERÍODO ACTIVO"),
                div(cls := "card-body",
                  div(cls := "fw-bold text-white fs-5", fixEncoding(b.nombre)),
                  div(cls := "small text-muted mb-2", s"${periodizationTipoLabel(b.tipo)} · ${b.fechaInicio} → ${b.fechaFin}"),
                  if (b.notas.nonEmpty) div(cls := "small text-light fst-italic", fixEncoding(b.notas)) else span()
                )
              )
            case None =>
              div(cls := "alert alert-secondary text-center mb-4", "Sin período activo definido para hoy")
          },

          div(cls := "card bg-dark border-warning p-3 mb-4",
            form(action := "/periodization/generate", method := "post",
              button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", "🧠 Generar plan IA (próximos 6 meses)")
            ),
            div(cls := "xx-small text-muted mt-2 text-center", "Respeta la estructura semanal fija — no añade sesiones extra")
          ),

          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-white small text-uppercase mb-3", "➕ Añadir período"),
            form(action := "/periodization/save", method := "post",
              div(cls := "mb-2",
                input(tpe := "text", name := "nombre", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Nombre del período", required := true)
              ),
              div(cls := "row g-2 mb-2",
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "INICIO"),
                  input(tpe := "date", name := "fechaInicio", cls := "form-control form-control-sm bg-dark text-white border-secondary", required := true)
                ),
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "FIN"),
                  input(tpe := "date", name := "fechaFin", cls := "form-control form-control-sm bg-dark text-white border-secondary", required := true)
                )
              ),
              div(cls := "mb-2",
                label(cls := "xx-small text-muted fw-bold", "TIPO"),
                select(name := "tipo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                  option(value := "CARGA_ALTA", "Carga alta"),
                  option(value := "DESCARGA", "Descarga"),
                  option(value := "TORNEO_CLAVE", "Torneo clave"),
                  option(value := "VENTANA_ACADEMIAS", "Ventana academias"),
                  option(value := "EVALUACION", "Evaluación"),
                  option(value := "DESCANSO", "Descanso")
                )
              ),
              div(cls := "mb-3",
                textarea(name := "notas", cls := "form-control form-control-sm bg-dark text-white border-secondary", rows := "2", placeholder := "Notas / implicación...")()
              ),
              button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "Guardar período")
            )
          ),

          if (blocks.nonEmpty)
            div(cls := "card bg-dark border-secondary p-3",
              div(cls := "fw-bold text-white small text-uppercase mb-3", "Períodos definidos"),
              frag(blocks.map { b =>
                div(cls := "d-flex align-items-center gap-2 py-2 border-bottom border-secondary",
                  div(style := s"width:10px; height:10px; border-radius:50%; background:${b.color}; flex-shrink:0;"),
                  div(cls := "flex-fill",
                    div(cls := "text-white small fw-bold", fixEncoding(b.nombre)),
                    div(cls := "xx-small text-muted", s"${periodizationTipoLabel(b.tipo)} · ${b.fechaInicio} → ${b.fechaFin}")
                  )
                )
              }: _*)
            )
          else span()
        )
      )
    )
    renderHtml(content)
  }

  @cask.postForm("/periodization/save")
  def savePeriodizationBlock(nombre: String, fechaInicio: String, fechaFin: String, tipo: String, notas: String = "") = {
    DatabaseManager.savePeriodization(nombre, fechaInicio, fechaFin, tipo, notas)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/periodization"))
  }

  @cask.postForm("/periodization/generate")
  def generatePeriodizationAI() = {
    val plan = DatabaseManager.generatePeriodizationPlan()
    val content = basePage("periodization",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-warning mb-4 text-center", "🧠 Plan de Periodización IA"),
          div(cls := "card bg-dark border-warning p-3 mb-4",
            div(cls := "text-light small", style := "white-space:pre-wrap;", plan)
          ),
          a(href := "/periodization", cls := "btn btn-outline-secondary w-100 fw-bold", "← Volver")
        )
      )
    )
    renderHtml(content)
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

  // ── MODULO 1: MOTOR DE PREDICCION DE TECHO ──────────────────────────────
  private def techoTendenciaColor(t: String): String = t match {
    case "ACELERANDO" => "success"; case "DESACELERANDO" => "danger"; case _ => "warning"
  }

  @cask.get("/techo")
  def techoPage(request: cask.Request) = withAuth(request) {
    val d = DatabaseManager.getTechoPrediction()
    val semaforo    = d("semaforo").asInstanceOf[String]
    val atributos   = d("atributos").asInstanceOf[List[Map[String, Any]]]
    val topAtributo = d("topAtributo").asInstanceOf[String]
    val analisisIA  = d("analisisIA").asInstanceOf[String]
    val edad        = d("edad").asInstanceOf[Int]

    val (semColor, semLabel, semIcon) = semaforo match {
      case "VERDE" => ("success", "ACELERANDO SU MEJORA", "🟢")
      case "ROJO"  => ("danger", "DESACELERANDO", "🔴")
      case _       => ("warning", "RITMO ESTABLE", "🟡")
    }

    val rows = atributos.map { a =>
      val nombre    = a("nombre").asInstanceOf[String]
      val actual    = a("actual").asInstanceOf[Double]
      val proy10    = a("proy10").asInstanceOf[Double]
      val proy14    = a("proy14").asInstanceOf[Double]
      val tendencia = a("tendencia").asInstanceOf[String]
      val flecha    = a("flecha").asInstanceOf[String]
      tr(
        td(cls := "fw-bold text-white", nombre),
        td(cls := "text-center", f"$actual%.0f"),
        td(cls := "text-center text-info", f"$proy10%.0f"),
        td(cls := "text-center text-warning", f"$proy14%.0f"),
        td(cls := "text-center",
          span(cls := s"badge bg-${techoTendenciaColor(tendencia)}", s"$flecha $tendencia"),
          if (nombre == topAtributo) span(cls := "badge bg-dark border border-warning text-warning ms-1", "⭐") else span()
        )
      )
    }

    val content = basePage("techo",
      div(cls := "row justify-content-center",
        div(cls := "col-md-9 col-12",
          h2(cls := "text-white mb-4 text-center", "🎯 Motor de Predicción de Techo"),

          div(cls := s"card bg-dark border-$semColor shadow mb-4",
            div(cls := "card-body text-center py-4",
              div(style := "font-size:48px;", semIcon),
              div(cls := s"h3 fw-bold text-$semColor mt-2", semLabel),
              div(cls := "text-muted small mt-1", s"Héctor tiene $edad años · Basado en su evolución histórica de atributos")
            )
          ),

          if (atributos.isEmpty)
            div(cls := "alert alert-secondary text-center", "Necesitas al menos una temporada registrada para generar la predicción")
          else div(
            div(cls := "card bg-dark border-secondary shadow mb-4",
              div(cls := "card-header text-white fw-bold small", "PROYECCIÓN POR ATRIBUTO"),
              div(cls := "card-body p-0",
                div(cls := "table-responsive",
                  table(cls := "table table-dark table-sm mb-0",
                    thead(tr(th("Atributo"), th(cls := "text-center", "Actual"), th(cls := "text-center", "A los 10"), th(cls := "text-center", "A los 14"), th(cls := "text-center", "Tendencia"))),
                    tbody(rows)
                  )
                )
              )
            ),

            div(cls := "card bg-dark border-warning shadow mb-4",
              div(cls := "card-header text-warning fw-bold small", "🤖 ANÁLISIS IA"),
              div(cls := "card-body text-light small", style := "white-space:pre-wrap;", analisisIA)
            )
          ),

          form(action := "/techo/recalcular", method := "post",
            button(tpe := "submit", cls := "btn btn-outline-warning w-100 fw-bold", "🔄 Recalcular")
          )
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/techo/recalcular")
  def recalcularTecho(request: cask.Request) = withAuth(request) {
    DatabaseManager.invalidateTechoCache()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/techo"))
  }

  // ── MODULO 4: SIMULADOR DE ESCENARIOS "¿QUE PASA SI?" ───────────────────
  private def simulatorForm(deltaNota: Double, limpiasExtra: Int, sesionesExtra: Int, atributo: String, deltaAtributo: Int) =
    form(action := "/simulate", method := "post",
      div(cls := "mb-3",
        div(cls := "d-flex justify-content-between",
          label(cls := "xx-small text-muted fw-bold", "SI MI NOTA MEDIA SUBIERA"),
          span(cls := "xx-small text-warning fw-bold", f"+$deltaNota%.1f")
        ),
        input(tpe := "range", name := "deltaNota", cls := "form-range", min := "0", max := "2", step := "0.1", value := deltaNota.toString)
      ),
      div(cls := "mb-3",
        div(cls := "d-flex justify-content-between",
          label(cls := "xx-small text-muted fw-bold", "SI CONSIGUIERA X LIMPIAS MÁS"),
          span(cls := "xx-small text-warning fw-bold", s"+$limpiasExtra")
        ),
        input(tpe := "range", name := "limpiasExtra", cls := "form-range", min := "0", max := "10", step := "1", value := limpiasExtra.toString)
      ),
      div(cls := "mb-3",
        label(cls := "xx-small text-muted fw-bold", "SI AÑADIERA SESIONES EXTRA DE ACADEMIA AL MES"),
        select(name := "sesionesExtra", cls := "form-select form-select-sm bg-dark text-white border-secondary",
          Seq(0, 1, 2, 4).map(n => if (n == sesionesExtra) option(value := n.toString, attr("selected") := "selected", n.toString) else option(value := n.toString, n.toString))
        )
      ),
      div(cls := "row g-2 mb-3",
        div(cls := "col-7",
          label(cls := "xx-small text-muted fw-bold", "SI MEJORARA EL ATRIBUTO"),
          select(name := "atributo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
            Seq("DIV", "HAN", "KIC", "REF", "SPD", "POS").map(a => if (a == atributo) option(value := a, attr("selected") := "selected", a) else option(value := a, a))
          )
        ),
        div(cls := "col-5",
          label(cls := "xx-small text-muted fw-bold", "EN X PUNTOS"),
          select(name := "deltaAtributo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
            (0 to 10).map(n => if (n == deltaAtributo) option(value := n.toString, attr("selected") := "selected", n.toString) else option(value := n.toString, n.toString))
          )
        )
      ),
      button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", "🧠 Simular con IA")
    )

  private def simulatorResultBlocks(resultado: Map[String, String]) = {
    val ratingTxt: String    = resultado.getOrElse("rating", "")
    val percentilTxt: String = resultado.getOrElse("percentil", "")
    val plazoTxt: String     = resultado.getOrElse("plazo", "")
    div(
      div(cls := "card bg-dark border-primary shadow mb-3",
        div(cls := "card-header text-primary fw-bold small", "⭐ IMPACTO EN RATING FUT"),
        div(cls := "card-body text-light small", style := "white-space:pre-wrap;", ratingTxt)
      ),
      div(cls := "card bg-dark border-info shadow mb-3",
        div(cls := "card-header text-info fw-bold small", "📈 IMPACTO EN PERCENTIL"),
        div(cls := "card-body text-light small", style := "white-space:pre-wrap;", percentilTxt)
      ),
      div(cls := "card bg-dark border-success shadow mb-3",
        div(cls := "card-header text-success fw-bold small", "⏳ PLAZO PARA OBJETIVOS"),
        div(cls := "card-body text-light small", style := "white-space:pre-wrap;", plazoTxt)
      )
    )
  }

  @cask.get("/simulate")
  def simulatePage(request: cask.Request) = withAuth(request) {
    val content = basePage("simulate",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-white mb-4 text-center", "🔮 Simulador de Escenarios"),
          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-white small text-uppercase mb-3", "¿Qué pasa si...?"),
            simulatorForm(0.0, 0, 0, "DIV", 0)
          )
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/simulate")
  def simulateAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val deltaNota      = p.getOrElse("deltaNota", "0").toDoubleOption.getOrElse(0.0)
    val limpiasExtra   = p.getOrElse("limpiasExtra", "0").toIntOption.getOrElse(0)
    val sesionesExtra  = p.getOrElse("sesionesExtra", "0").toIntOption.getOrElse(0)
    val atributo       = p.getOrElse("atributo", "DIV")
    val deltaAtributo  = p.getOrElse("deltaAtributo", "0").toIntOption.getOrElse(0)

    val resultado = DatabaseManager.simulateScenario(deltaNota, limpiasExtra, sesionesExtra, atributo, deltaAtributo)

    val content = basePage("simulate",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-white mb-4 text-center", "🔮 Simulador de Escenarios"),
          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-white small text-uppercase mb-3", "¿Qué pasa si...?"),
            simulatorForm(deltaNota, limpiasExtra, sesionesExtra, atributo, deltaAtributo)
          ),
          div(cls := "alert alert-secondary small fst-italic mb-3", s"Hipótesis: ${resultado.getOrElse("hipotesis", "")}"),
          simulatorResultBlocks(resultado)
        )
      )
    )
    renderHtml(content)
  }

  // ── MODULO 5: DIARIO NARRATIVO AUTOMATICO DE TEMPORADA ──────────────────
  @cask.get("/diary")
  def diaryPage(request: cask.Request) = withAuth(request) {
    val entries = DatabaseManager.getSeasonDiaryEntries() // DESC por mes
    val today = java.time.LocalDate.now()
    val targetMonthDate = if (today.getDayOfMonth == today.lengthOfMonth()) today else today.minusMonths(1)
    val targetMonth = targetMonthDate.toString.take(7)
    val yaGenerado = entries.exists(_("mes").asInstanceOf[String] == targetMonth)

    val entriesHtml = entries.zipWithIndex.map { case (e, idx) =>
      val mes         = e("mes").asInstanceOf[String]
      val contenido    = e("contenido").asInstanceOf[String]
      val partidos     = e("partidosIncluidos").asInstanceOf[Int]
      val hitos        = e("hitosIncluidos").asInstanceOf[Int]
      val openMod: Modifier = if (idx == 0) attr("open") := "open" else frag()
      tag("details")(cls := "border-bottom border-secondary py-2", openMod,
        tag("summary")(cls := "text-warning fw-bold", style := "cursor:pointer;",
          s"${DatabaseManager.mesLabel(mes).capitalize} · $partidos partidos · $hitos hitos"
        ),
        div(cls := "text-light small mt-2", style := "white-space:pre-wrap; line-height:1.6;", contenido)
      )
    }

    val content = basePage("diary",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-4",
            h2(cls := "text-white mb-0", "📖 Diario de Temporada"),
            a(href := "/diary/export", cls := "btn btn-outline-secondary btn-sm fw-bold", target := "_blank", "🖨️ Exportar")
          ),
          if (!yaGenerado)
            form(action := "/diary/generate", method := "post", cls := "mb-4",
              input(tpe := "hidden", name := "mes", value := targetMonth),
              button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", s"📖 Generar entrada de ${DatabaseManager.mesLabel(targetMonth)}")
            )
          else div(),
          if (entries.isEmpty)
            div(cls := "alert alert-secondary text-center", "Todavía no hay entradas en el diario")
          else
            div(cls := "card bg-dark border-secondary p-3",
              frag(entriesHtml: _*)
            )
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/diary/generate")
  def generateDiaryEntry(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val mes = p.getOrElse("mes", java.time.LocalDate.now().toString.take(7))
    DatabaseManager.generateMonthlyDiary(mes)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/diary"))
  }

  @cask.get("/diary/export")
  def exportDiary(request: cask.Request) = withAuth(request) {
    val entries = DatabaseManager.getSeasonDiaryEntries().sortBy(_("mes").asInstanceOf[String]) // cronologico ASC
    val entradasHtml = entries.map { e =>
      val mes       = e("mes").asInstanceOf[String]
      val contenido = e("contenido").asInstanceOf[String]
      div(style := "margin-bottom:30px; page-break-inside:avoid;",
        h3(style := "color:#b8860b;", DatabaseManager.mesLabel(mes).capitalize),
        p(style := "white-space:pre-wrap; line-height:1.6;", contenido)
      )
    }
    val htmlStr = "<!DOCTYPE html>" + html(
      head(meta(charset := "utf-8"), tags2.title("Diario de Héctor — Temporada completa")),
      body(style := "font-family: Georgia, serif; max-width:800px; margin:40px auto; padding:0 20px; color:#222;",
        h1(style := "text-align:center;", "Diario de Héctor"),
        p(style := "text-align:center; color:#888;", "Crónica de su desarrollo como portero"),
        hr(),
        frag(entradasHtml: _*)
      )
    ).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  // ── MODULO 6: COMPARATIVA TEMPORAL ENTRE EDADES ─────────────────────────
  @cask.get("/temporal")
  def temporalPage(request: cask.Request) = withAuth(request) {
    val data = DatabaseManager.getTemporalComparison()
    val currentSeasonId = data.lastOption.map(_("id").asInstanceOf[Int]).getOrElse(-1)

    val labels   = data.map(d => fixEncoding(d("categoria").asInstanceOf[String]).replace("\"", ""))
    val labelsJs = labels.map(l => s""""$l"""").mkString("[", ",", "]")

    def seriesJs(key: String): String = data.map(d => f"${d(key).asInstanceOf[Double]}%.1f").mkString("[", ",", "]")
    val divJs = seriesJs("div"); val hanJs = seriesJs("han"); val kicJs = seriesJs("kic")
    val refJs = seriesJs("ref"); val spdJs = seriesJs("spd"); val posJs = seriesJs("pos")
    val notaJs = seriesJs("notaMedia"); val gcJs = seriesJs("gcMedia")
    val limpiasJs = data.map(d => d("limpias").asInstanceOf[Int].toString).mkString("[", ",", "]")

    val (masCrecido, menosCrecido): ((String, Double), (String, Double)) = if (data.size >= 2) {
      val first = data.head; val last = data.last
      val attrs = Seq("DIV" -> "div", "HAN" -> "han", "KIC" -> "kic", "REF" -> "ref", "SPD" -> "spd", "POS" -> "pos")
      val deltas = attrs.map { case (label, key) => (label, last(key).asInstanceOf[Double] - first(key).asInstanceOf[Double]) }
      (deltas.maxBy(_._2), deltas.minBy(_._2))
    } else (("—", 0.0), ("—", 0.0))
    val masCrecidoLbl   = f"${masCrecido._1}%s (+${masCrecido._2}%.0f)"
    val menosCrecidoLbl = f"${menosCrecido._1}%s (+${menosCrecido._2}%.0f)"

    val rows = data.map { d =>
      val isCurrent = d("id").asInstanceOf[Int] == currentSeasonId
      tr(cls := (if (isCurrent) "table-warning" else ""),
        td(cls := "fw-bold", fixEncoding(d("categoria").asInstanceOf[String])),
        td(cls := "text-center", f"${d("div").asInstanceOf[Double]}%.0f"),
        td(cls := "text-center", f"${d("han").asInstanceOf[Double]}%.0f"),
        td(cls := "text-center", f"${d("kic").asInstanceOf[Double]}%.0f"),
        td(cls := "text-center", f"${d("ref").asInstanceOf[Double]}%.0f"),
        td(cls := "text-center", f"${d("spd").asInstanceOf[Double]}%.0f"),
        td(cls := "text-center", f"${d("pos").asInstanceOf[Double]}%.0f"),
        td(cls := "text-center", f"${d("notaMedia").asInstanceOf[Double]}%.1f"),
        td(cls := "text-center", d("limpias").asInstanceOf[Int].toString),
        td(cls := "text-center", d("partidos").asInstanceOf[Int].toString)
      )
    }

    val content = basePage("temporal",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          h2(cls := "text-white mb-4 text-center", "📈 Comparativa Temporal entre Edades"),

          if (data.isEmpty) div(cls := "alert alert-secondary text-center", "Sin temporadas registradas")
          else div(
            div(cls := "row g-2 mb-4",
              Seq(
                (masCrecidoLbl,   "MÁS HA CRECIDO",   "success"),
                (menosCrecidoLbl, "MENOS HA CRECIDO", "warning")
              ).map { case (v, lbl, c) =>
                div(cls := "col-6",
                  div(cls := s"card bg-dark border-$c text-center py-3",
                    div(cls := s"text-$c fw-bold fs-4", v),
                    div(cls := "xx-small text-muted mt-1", lbl)
                  )
                )
              }
            ),

            div(cls := "card bg-dark border-warning shadow mb-4",
              div(cls := "card-header text-warning fw-bold small", "EVOLUCIÓN DE ATRIBUTOS POR TEMPORADA"),
              div(cls := "card-body", div(style := "height:280px;", canvas(id := "chartAtributos")))
            ),

            div(cls := "card bg-dark border-info shadow mb-4",
              div(cls := "card-header text-info fw-bold small", "NOTA MEDIA / GOLES CONTRA / PORTERÍAS A 0"),
              div(cls := "card-body", div(style := "height:240px;", canvas(id := "chartRendimiento")))
            ),

            div(cls := "card bg-dark border-secondary shadow mb-4",
              div(cls := "card-header text-white fw-bold small", "TABLA COMPARATIVA"),
              div(cls := "card-body p-0",
                div(cls := "table-responsive",
                  table(cls := "table table-dark table-sm mb-0 small",
                    thead(tr(th("Temp"), th(cls := "text-center", "DIV"), th(cls := "text-center", "HAN"), th(cls := "text-center", "KIC"),
                      th(cls := "text-center", "REF"), th(cls := "text-center", "SPD"), th(cls := "text-center", "POS"),
                      th(cls := "text-center", "Nota"), th(cls := "text-center", "Limpias"), th(cls := "text-center", "PJ"))),
                    tbody(rows)
                  )
                )
              )
            ),

            script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
            script(raw(s"""
              new Chart(document.getElementById('chartAtributos'), {
                type: 'line',
                data: {
                  labels: $labelsJs,
                  datasets: [
                    { label: 'DIV', data: $divJs, borderColor: '#0dcaf0', tension: 0.3, fill: false },
                    { label: 'HAN', data: $hanJs, borderColor: '#ffc107', tension: 0.3, fill: false },
                    { label: 'KIC', data: $kicJs, borderColor: '#20c997', tension: 0.3, fill: false },
                    { label: 'REF', data: $refJs, borderColor: '#dc3545', tension: 0.3, fill: false },
                    { label: 'SPD', data: $spdJs, borderColor: '#8b5cf6', tension: 0.3, fill: false },
                    { label: 'POS', data: $posJs, borderColor: '#d4af37', tension: 0.3, fill: false }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 10 } } } },
                  scales: {
                    x: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { min: 0, max: 100, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                  }
                }
              });
              new Chart(document.getElementById('chartRendimiento'), {
                type: 'line',
                data: {
                  labels: $labelsJs,
                  datasets: [
                    { label: 'Nota media', data: $notaJs, borderColor: '#d4af37', backgroundColor: 'rgba(212,175,55,0.1)', tension: 0.3, fill: true, yAxisID: 'y' },
                    { label: 'GC media', data: $gcJs, borderColor: '#dc3545', tension: 0.3, fill: false, yAxisID: 'y' },
                    { label: 'Limpias', data: $limpiasJs, borderColor: '#20c997', tension: 0.3, fill: false, yAxisID: 'y1' }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 10 } } } },
                  scales: {
                    x: { ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { position: 'left', ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y1: { position: 'right', ticks: { color: '#aaa' }, grid: { display: false } }
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

  // ── MODULO 7: RED DE CONTACTOS (MINI CRM) ───────────────────────────────
  private def contactRolInfo(rol: String): (String, String) = rol match {
    case "OJEADOR"             => ("Ojeador", "#dc3545")
    case "ENTRENADOR_ACADEMIA" => ("Entrenador Academia", "#0d6efd")
    case "CLUB"                => ("Club", "#198754")
    case "PADRE_CONTACTO"      => ("Padre/Contacto", "#fd7e14")
    case "AGENTE"              => ("Agente", "#6f42c1")
    case _                     => ("Otro", "#6c757d")
  }

  private def importanciaBadgeCls(imp: String): String = imp match {
    case "ALTA" => "bg-danger"; case "MEDIA" => "bg-warning text-dark"; case _ => "bg-secondary"
  }

  @cask.get("/contacts")
  def contactsPage(request: cask.Request) = withAuth(request) {
    val contacts = DatabaseManager.getContacts()
    val total = contacts.size
    val altaCount = contacts.count(_.importancia == "ALTA")
    val ultimoContacto = contacts.flatMap(_.ultimaInteraccion).sorted.lastOption.getOrElse("—")

    val contactRows = contacts.map { c =>
      val (rolLabel, rolColor) = contactRolInfo(c.rol)
      val diasSinContacto = c.ultimaInteraccion.flatMap(f =>
        try Some(java.time.temporal.ChronoUnit.DAYS.between(java.time.LocalDate.parse(f), java.time.LocalDate.now()))
        catch { case _: Exception => None })
      val sinContacto = c.importancia == "ALTA" && (c.ultimaInteraccion.isEmpty || diasSinContacto.exists(_ > 30))

      div(cls := "card bg-dark border-secondary p-3 mb-2",
        div(cls := "d-flex justify-content-between align-items-start mb-1",
          div(
            a(href := s"/contacts/view/${c.id}", cls := "text-white fw-bold text-decoration-none", fixEncoding(c.nombre)),
            div(cls := "d-flex gap-1 mt-1 flex-wrap",
              span(cls := "badge", style := s"background:$rolColor;", rolLabel),
              span(cls := s"badge ${importanciaBadgeCls(c.importancia)}", c.importancia),
              if (sinContacto) span(cls := "badge bg-danger", "⚠️ Sin contacto") else span()
            )
          ),
          span(cls := "xx-small text-muted", c.ultimaInteraccion.map(_.take(10)).getOrElse("Sin interacción"): String)
        ),
        if (c.clubOEntidad.nonEmpty) div(cls := "xx-small text-muted mb-1", fixEncoding(c.clubOEntidad)) else span(),
        form(action := "/contacts/interaccion", method := "post", cls := "d-flex gap-1 mt-2",
          input(tpe := "hidden", name := "id", value := c.id.toString),
          input(tpe := "date", name := "fecha", cls := "form-control form-control-sm bg-dark text-white border-secondary",
            style := "max-width:140px;", value := java.time.LocalDate.now().toString),
          input(tpe := "text", name := "nota", cls := "form-control form-control-sm bg-dark text-white border-secondary",
            placeholder := "📅 Registrar interacción..."),
          button(tpe := "submit", cls := "btn btn-sm btn-outline-info", "💾")
        )
      )
    }

    val content = basePage("contacts",
      div(cls := "row justify-content-center",
        div(cls := "col-md-9 col-12",
          h2(cls := "text-white mb-4 text-center", "👥 Red de Contactos"),

          div(cls := "row g-2 mb-4",
            Seq(
              (total.toString, "TOTAL CONTACTOS", "secondary"),
              (altaCount.toString, "IMPORTANCIA ALTA", "danger"),
              (ultimoContacto.take(10), "ÚLTIMO CONTACTO", "info")
            ).map { case (v, lbl, c) =>
              div(cls := "col-4",
                div(cls := s"card bg-dark border-$c text-center py-3",
                  div(cls := s"text-$c fw-bold", style := "font-size:18px;", v),
                  div(cls := "xx-small text-muted mt-1", lbl)
                )
              )
            }
          ),

          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-white small text-uppercase mb-3", "➕ Nuevo contacto"),
            form(action := "/contacts/save", method := "post",
              div(cls := "mb-2",
                input(tpe := "text", name := "nombre", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Nombre", required := true)
              ),
              div(cls := "row g-2 mb-2",
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "ROL"),
                  select(name := "rol", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                    option(value := "ENTRENADOR_ACADEMIA", "Entrenador Academia"),
                    option(value := "OJEADOR", "Ojeador"),
                    option(value := "PADRE_CONTACTO", "Padre/Contacto"),
                    option(value := "CLUB", "Club"),
                    option(value := "AGENTE", "Agente"),
                    option(value := "OTRO", "Otro")
                  )
                ),
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "IMPORTANCIA"),
                  select(name := "importancia", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                    option(value := "ALTA", "Alta"),
                    option(value := "MEDIA", attr("selected") := "selected", "Media"),
                    option(value := "BAJA", "Baja")
                  )
                )
              ),
              div(cls := "mb-2",
                input(tpe := "text", name := "clubOEntidad", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Club / entidad")
              ),
              div(cls := "row g-2 mb-2",
                div(cls := "col-6", input(tpe := "text", name := "telefono", cls := "form-control form-control-sm bg-dark text-white border-secondary", placeholder := "Teléfono (opcional)")),
                div(cls := "col-6", input(tpe := "email", name := "email", cls := "form-control form-control-sm bg-dark text-white border-secondary", placeholder := "Email (opcional)"))
              ),
              div(cls := "mb-2",
                input(tpe := "text", name := "comoConocido", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "¿Cómo se conoció?")
              ),
              div(cls := "mb-3",
                textarea(name := "notas", cls := "form-control form-control-sm bg-dark text-white border-secondary", rows := "2", placeholder := "Notas")()
              ),
              button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "Guardar contacto")
            )
          ),

          if (contacts.isEmpty) div(cls := "alert alert-secondary text-center", "Sin contactos registrados todavía")
          else frag(contactRows: _*)
        )
      )
    )
    renderHtml(content)
  }

  @cask.get("/contacts/view/:id")
  def contactDetailPage(request: cask.Request, id: Int) = withAuth(request) {
    DatabaseManager.getContactById(id) match {
      case None => renderHtml(basePage("contacts", div(cls := "alert alert-secondary m-4", "Contacto no encontrado")))
      case Some(c) =>
        val (rolLabel, rolColor) = contactRolInfo(c.rol)
        val linkedOpps = DatabaseManager.getOpportunitiesByContact(id)
        val content = basePage("contacts",
          div(cls := "row justify-content-center",
            div(cls := "col-md-8 col-12",
              div(cls := "d-flex justify-content-between align-items-center mb-4",
                h2(cls := "text-white mb-0", fixEncoding(c.nombre)),
                a(href := "/contacts", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Contactos")
              ),
              div(cls := "d-flex gap-1 mb-3",
                span(cls := "badge", style := s"background:$rolColor;", rolLabel),
                span(cls := s"badge ${importanciaBadgeCls(c.importancia)}", c.importancia)
              ),
              div(cls := "card bg-dark border-secondary p-3 mb-3",
                if (c.clubOEntidad.nonEmpty) div(cls := "small text-white mb-1", strong("Entidad: "), fixEncoding(c.clubOEntidad)) else span(),
                if (c.telefono.nonEmpty) div(cls := "small text-white mb-1", strong("Teléfono: "), c.telefono) else span(),
                if (c.email.nonEmpty) div(cls := "small text-white mb-1", strong("Email: "), c.email) else span(),
                if (c.comoConocido.nonEmpty) div(cls := "small text-light mb-1", strong("Cómo se conoció: "), fixEncoding(c.comoConocido)) else span(),
                div(cls := "xx-small text-muted mt-2", s"Última interacción: ${c.ultimaInteraccion.getOrElse("Sin registrar")}")
              ),
              div(cls := "card bg-dark border-secondary p-3 mb-3",
                div(cls := "fw-bold text-white small text-uppercase mb-2", "Historial de interacciones"),
                if (c.notas.trim.isEmpty) div(cls := "text-muted small", "Sin interacciones registradas")
                else pre(cls := "text-light small", style := "white-space:pre-wrap; font-family:inherit;", c.notas)
              ),
              if (linkedOpps.nonEmpty)
                div(cls := "card bg-dark border-secondary p-3 mb-3",
                  div(cls := "fw-bold text-white small text-uppercase mb-2", "Oportunidades vinculadas"),
                  frag(linkedOpps.map { o =>
                    div(cls := "xx-small text-light border-bottom border-secondary py-1",
                      span(cls := "badge bg-secondary me-2", o.tipo), o.fecha.take(10),
                      if (o.descripcion.nonEmpty) s" — ${fixEncoding(o.descripcion)}" else ""
                    )
                  }: _*)
                )
              else div()
            )
          )
        )
        renderHtml(content)
    }
  }

  @cask.post("/contacts/save")
  def saveContactAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    DatabaseManager.saveContact(
      p.getOrElse("nombre", ""), p.getOrElse("rol", "OTRO"), p.getOrElse("clubOEntidad", ""),
      p.getOrElse("telefono", ""), p.getOrElse("email", ""), p.getOrElse("comoConocido", ""),
      p.getOrElse("notas", ""), p.getOrElse("importancia", "MEDIA")
    )
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/contacts"))
  }

  @cask.post("/contacts/interaccion")
  def registrarInteraccionAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val id = p.getOrElse("id", "0").toIntOption.getOrElse(0)
    DatabaseManager.registrarInteraccion(id, p.getOrElse("fecha", ""), p.getOrElse("nota", ""))
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/contacts"))
  }

  // ── MODULO 4 (sesion actual): MAPA DE VISIBILIDAD Y EVENTOS CLAVE ───────
  private def visibilidadNivelInfo(nivel: String): (String, String) = nivel match {
    case "ALTO"  => ("Alto", "#dc3545")
    case "MEDIO" => ("Medio", "#ffc107")
    case _       => ("Bajo", "#6c757d")
  }

  private def visibilidadTipoLabel(tipo: String): String = tipo match {
    case "TORNEO"        => "Torneo"
    case "LIGA_REGIONAL" => "Liga regional"
    case "CAMPUS"        => "Campus"
    case "PRUEBA_CLUB"   => "Prueba de club"
    case _                => "Otro"
  }

  @cask.get("/visibility")
  def visibilityPage(request: cask.Request) = withAuth(request) {
    val events = DatabaseManager.getVisibilityEvents()
    val ojeadoresContacts = DatabaseManager.getContacts().filter(_.rol == "OJEADOR")

    val altoCount            = events.count(_("nivelVisibilidad").asInstanceOf[String] == "ALTO")
    val participamosCount    = events.count(_("participamos").asInstanceOf[Boolean])
    val ojeadoresConfirmados = events.count(e => e("ojeadoresPresentes").asInstanceOf[Option[Boolean]].contains(true))

    val eventCards = events.map { e =>
      val id                 = e("id").asInstanceOf[Int]
      val nombre              = e("nombre").asInstanceOf[String]
      val fecha                = e("fecha").asInstanceOf[String]
      val tipo                = e("tipo").asInstanceOf[String]
      val organizador          = e("organizador").asInstanceOf[String]
      val nivel                = e("nivelVisibilidad").asInstanceOf[String]
      val participamos         = e("participamos").asInstanceOf[Boolean]
      val ojeadoresPresentes   = e("ojeadoresPresentes").asInstanceOf[Option[Boolean]]
      val contactoNombre       = e("contactoNombre").asInstanceOf[String]
      val (nivelLabel, nivelColor) = visibilidadNivelInfo(nivel)

      div(cls := "card bg-dark border-secondary p-3 mb-2",
        div(cls := "d-flex justify-content-between align-items-start mb-1",
          div(
            div(cls := "fw-bold text-white", fixEncoding(nombre)),
            div(cls := "xx-small text-muted", s"${visibilidadTipoLabel(tipo)}${if (organizador.nonEmpty) s" · $organizador" else ""}")
          ),
          div(cls := "text-end",
            span(cls := "badge mb-1", style := s"background:$nivelColor;", nivelLabel),
            div(cls := "xx-small text-muted", fecha)
          )
        ),
        form(action := "/visibility/participamos", method := "post", cls := "d-flex align-items-center gap-2 mt-2",
          input(tpe := "hidden", name := "id", value := id.toString),
          input(tpe := "hidden", name := "participamos", value := (!participamos).toString),
          button(tpe := "submit", cls := s"btn btn-sm ${if (participamos) "btn-success" else "btn-outline-secondary"} fw-bold",
            if (participamos) "✅ Participamos" else "¿Participamos?")
        ),
        if (participamos)
          div(cls := "mt-2",
            ojeadoresPresentes match {
              case Some(true) =>
                div(cls := "d-flex align-items-center gap-2",
                  span(cls := "badge bg-danger", "🔍 Hubo ojeadores"),
                  if (contactoNombre.nonEmpty) span(cls := "xx-small text-info", s"— ${fixEncoding(contactoNombre)}") else span()
                )
              case Some(false) => span(cls := "badge bg-secondary", "Sin ojeadores confirmados")
              case None =>
                form(action := "/visibility/ojeadores", method := "post", cls := "d-flex gap-1 align-items-center flex-wrap",
                  input(tpe := "hidden", name := "id", value := id.toString),
                  label(cls := "xx-small text-muted", "¿Hubo ojeadores?"),
                  select(name := "contactId", cls := "form-select form-select-sm bg-dark text-white border-secondary", style := "max-width:160px;",
                    option(value := "", "— Sin identificar —"),
                    frag(ojeadoresContacts.map(c => option(value := c.id.toString, fixEncoding(c.nombre))): _*)
                  ),
                  button(tpe := "submit", name := "hubo", value := "true", cls := "btn btn-sm btn-outline-danger", "Sí"),
                  button(tpe := "submit", name := "hubo", value := "false", cls := "btn btn-sm btn-outline-secondary", "No")
                )
            }
          )
        else span()
      )
    }

    val content = basePage("visibility",
      div(cls := "row justify-content-center",
        div(cls := "col-md-9 col-12",
          h2(cls := "text-white mb-4 text-center", "🗺️ Mapa de Visibilidad"),

          div(cls := "row g-2 mb-4",
            Seq(
              (altoCount.toString, "EVENTOS NIVEL ALTO", "danger"),
              (participamosCount.toString, "PARTICIPAMOS", "success"),
              (ojeadoresConfirmados.toString, "CON OJEADORES", "info")
            ).map { case (v, lbl, c) =>
              div(cls := "col-4",
                div(cls := s"card bg-dark border-$c text-center py-3",
                  div(cls := s"text-$c fw-bold fs-4", v),
                  div(cls := "xx-small text-muted mt-1", lbl)
                )
              )
            }
          ),

          div(cls := "card bg-dark border-warning p-3 mb-4",
            form(action := "/visibility/recomendar", method := "post",
              button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", "🧠 Recomendar próximos eventos")
            )
          ),

          div(cls := "card bg-dark border-secondary p-3 mb-4",
            div(cls := "fw-bold text-white small text-uppercase mb-3", "➕ Añadir evento"),
            form(action := "/visibility/save", method := "post",
              div(cls := "mb-2",
                input(tpe := "text", name := "nombre", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Nombre del evento", required := true)
              ),
              div(cls := "row g-2 mb-2",
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "FECHA"),
                  input(tpe := "date", name := "fecha", cls := "form-control form-control-sm bg-dark text-white border-secondary", required := true)
                ),
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "TIPO"),
                  select(name := "tipo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                    option(value := "TORNEO", "Torneo"),
                    option(value := "LIGA_REGIONAL", "Liga regional"),
                    option(value := "CAMPUS", "Campus"),
                    option(value := "PRUEBA_CLUB", "Prueba de club"),
                    option(value := "OTRO", "Otro")
                  )
                )
              ),
              div(cls := "mb-2",
                input(tpe := "text", name := "organizador", cls := "form-control form-control-sm bg-dark text-white border-secondary",
                  placeholder := "Organizador")
              ),
              div(cls := "mb-3",
                label(cls := "xx-small text-muted fw-bold", "NIVEL DE VISIBILIDAD ESTIMADO"),
                select(name := "nivelVisibilidad", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                  option(value := "ALTO", "Alto"),
                  option(value := "MEDIO", attr("selected") := "selected", "Medio"),
                  option(value := "BAJO", "Bajo")
                )
              ),
              button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "Guardar evento")
            )
          ),

          if (events.isEmpty) div(cls := "alert alert-secondary text-center", "Sin eventos registrados este año")
          else frag(eventCards: _*)
        )
      )
    )
    renderHtml(content)
  }

  @cask.post("/visibility/save")
  def saveVisibilityEventAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    DatabaseManager.saveVisibilityEvent(
      p.getOrElse("nombre", ""), p.getOrElse("fecha", java.time.LocalDate.now().toString),
      p.getOrElse("tipo", "OTRO"), p.getOrElse("organizador", ""), p.getOrElse("nivelVisibilidad", "MEDIO")
    )
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/visibility"))
  }

  @cask.post("/visibility/participamos")
  def toggleParticipamosAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val id = p.getOrElse("id", "0").toIntOption.getOrElse(0)
    val participamos = p.getOrElse("participamos", "false") == "true"
    DatabaseManager.updateVisibilityParticipamos(id, participamos)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/visibility"))
  }

  @cask.post("/visibility/ojeadores")
  def updateOjeadoresAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val id = p.getOrElse("id", "0").toIntOption.getOrElse(0)
    val hubo = p.getOrElse("hubo", "false") == "true"
    val contactId = p.getOrElse("contactId", "").toIntOption
    DatabaseManager.updateVisibilityOjeadores(id, hubo, contactId)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/visibility"))
  }

  @cask.post("/visibility/recomendar")
  def recommendVisibilityAction(request: cask.Request) = withAuth(request) {
    val recomendacion = DatabaseManager.recommendVisibilityEvents()
    val content = basePage("visibility",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-warning mb-4 text-center", "🧠 Recomendación de Eventos"),
          div(cls := "card bg-dark border-warning p-3 mb-4",
            div(cls := "text-light small", style := "white-space:pre-wrap;", recomendacion)
          ),
          a(href := "/visibility", cls := "btn btn-outline-secondary w-100 fw-bold", "← Volver")
        )
      )
    )
    renderHtml(content)
  }

  initialize()
}
