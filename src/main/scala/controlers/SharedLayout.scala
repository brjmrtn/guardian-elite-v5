import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
object SharedLayout {

  // --- Configuracion de seguridad (desde variables de entorno) ---
  val authUser          = sys.env.getOrElse("GUARDIAN_USER", "admin")
  val authPass          = sys.env.getOrElse("GUARDIAN_PASS", "hector2026")
  val sessionCookieName = "guardian_session"

  def withAuth(request: cask.Request)(block: => cask.Response[Array[Byte]]): cask.Response[Array[Byte]] = {
    val isAuthenticated = request.cookies.get(sessionCookieName).exists(_.value == "active")
    if (isAuthenticated) {
      block
    } else {
      // Guardamos la ruta actual para volver despues del login
      val currentPath = request.exchange.getRequestPath
      val red = cask.Redirect(s"/login?next=$currentPath")
      cask.Response(Array.empty[Byte], red.statusCode, red.headers ++ Seq("Cache-Control" -> "no-store, no-cache, must-revalidate"), red.cookies)
    }
  }

  def fixEncoding(s: String): String = { try { if (s.contains("A")) new String(s.getBytes("ISO-8859-1"), "UTF-8") else s } catch { case e: Exception => s } }

  // ==========================================

  def renderMatchRow(m: MatchLog) = {
    val pParts = m.resultado.split("-").map(s => try s.trim.toInt catch { case _:Exception => 0 })
    val colorClass = if(pParts.length >= 2) { if (pParts(0) > pParts(1)) "text-success" else if (pParts(0) == pParts(1)) "text-warning" else "text-danger" } else "text-muted"
    val extraBtn = if(m.video.nonEmpty) a(href:=m.video, target:="_blank", cls:="btn btn-sm btn-danger py-0 ms-1", style:="font-size:10px", "V") else span("")
    val waText = URLEncoder.encode(s"MATCH: ${m.rival} ${m.resultado}", "UTF-8").replace("+", "%20")
    val tipoBadge = if(m.tipo == "LIGA") span(cls:="badge bg-primary me-1", style:="font-size:9px", "LIGA") else if(m.tipo=="AMISTOSO") span(cls:="badge bg-secondary me-1", style:="font-size:9px", "AMIST") else span(cls:="badge bg-warning text-dark me-1", style:="font-size:9px", "TORNEO")
    val estadioInfo = if(m.estadio.nonEmpty && m.estadio != "-") span(cls:="d-block text-muted fst-italic", style:="font-size:10px", s"[estadio] ${fixEncoding(m.estadio)}") else span("")

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
              a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "[config]")
            )
          ),
          div(cls := "container main-content", pageContents), tags2.nav(cls := "bottom-nav", a(href:="/", cls:=s"nav-item ${if(activeLink=="home") "active" else ""}", div(cls:="nav-icon", "H"), span(cls:="nav-label", "Inicio")), a(href:="/match-center", cls:=s"nav-item ${if(activeLink=="match-center") "active" else ""}", div(cls:="nav-icon", "P"), span(cls:="nav-label", "Jugar")), a(href:="/bio", cls:=s"nav-item ${if(activeLink=="bio") "active" else ""}", div(cls:="nav-icon", "B"), span(cls:="nav-label", "Bio")), a(href:="/career/legacy", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon text-warning", "[star]"), span(cls:="nav-label text-warning", "Legado")), a(href:="/tactics", cls:=s"nav-item ${if(activeLink=="tactics") "active" else ""}", div(cls:="nav-icon", "[info]"), span(cls:="nav-label", "Pizarra")), a(href:="/career", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon", "T"), span(cls:="nav-label", "Trayect.")), a(href:="/history", cls:=s"nav-item ${if(activeLink=="history") "active" else ""}", div(cls:="nav-icon", "L"), span(cls:="nav-label", "Historial"))))
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

  // --- 1. EL ORACULO (Prediccion de Altura) ---
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
          h2(cls:="text-center text-info mb-4", "[oracle] EL ORACULO"),

          // Tarjeta Inteligencia
          div(cls:="card bg-dark border-info shadow mb-4",
            div(cls:="card-header bg-info text-dark fw-bold d-flex justify-content-between align-items-center",
              span("[IA] INTELIGENCIA DEPORTIVA"),
              span(cls:="badge bg-dark text-info", s"Edad: $edadActual anos")
            ),
            div(cls:="card-body", raw(bioInsights))
          ),

          // Tarjeta Grafico
          div(cls:="card bg-dark border-secondary shadow mb-4",
            div(cls:="card-header text-white small", "Evolucion Biometrica Historica"),
            div(cls:="card-body", style:="height: 300px; position: relative;",
              canvas(id:="growthChart")
            )
          ),

          // Tabla OMS
          div(cls:="card bg-dark border-secondary shadow mb-4",
            div(cls:="card-header text-muted small fw-bold text-uppercase", s"[stats] Referencia OMS para $edadActual anos"),
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

          // Prediccion Genetica
          div(cls:="card bg-dark text-white border-secondary shadow p-4",
            h4(cls:="text-center text-warning mb-3", "Prediccion Altura Final"),
            div(cls:="bg-secondary bg-opacity-10 p-3 rounded mb-3", raw(predictionHtml)),
            form(action:="/oracle", method:="get", cls:="mt-4 border-top border-secondary pt-3",
              div(cls:="row",
                div(cls:="col-6", label(cls:="small text-muted fw-bold", "Papa (cm)"), input(tpe:="number", name:="hDad", value:=hDad, cls:="form-control bg-dark text-white text-center")),
                div(cls:="col-6", label(cls:="small text-muted fw-bold", "Mama (cm)"), input(tpe:="number", name:="hMom", value:=hMom, cls:="form-control bg-dark text-white text-center"))
              ),
              div(cls:="d-grid mt-3", button(tpe:="submit", cls:="btn btn-outline-info fw-bold", "[sync] Recalcular"))
            )
          )
        )
      ),
      // 2. Script inyectado (Aseguramos que Chart.js este cargado en basePage)
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
  // --- 2. MONEYBALL (Distribucion Tactica) ---
  @cask.get("/distribution")
  def distributionPage() = {
    val tac = DatabaseManager.getTacticalStats()

    // Funcion auxiliar para calcular porcentajes seguros
    def pct(n: Double, d: Double): Int = if(d > 0) ((n/d)*100).toInt else 0

    val totG = if(tac("g_tot") > 0) tac("g_tot").toDouble else 1.0
    val (ga, gm, gr) = (pct(tac("g_alt"), totG), pct(tac("g_med"), totG), pct(tac("g_ras"), totG))
    val (gl, gc, gd) = (pct(tac("g_izq"), totG), pct(tac("g_cen"), totG), pct(tac("g_der"), totG))

    val totP = if(tac("p_tot") > 0) tac("p_tot").toDouble else 1.0
    val (pa, pm, pr) = (pct(tac("p_alt"), totP), pct(tac("p_med"), totP), pct(tac("p_ras"), totP))
    val (pl, pc, pd) = (pct(tac("p_izq"), totP), pct(tac("p_cen"), totP), pct(tac("p_der"), totP))

    // Celda tactica visual
    def tCell(label: String, p: Int, color: String) = div(
      cls:=s"flex-fill text-center p-3 border border-secondary $color",
      style:="color: #000; font-weight: 800;",
      div(style:="font-size:14px;", label),
      div(style:="font-size:24px;", s"$p%")
    )

    val content = basePage("bio", div(cls:="row justify-content-center",
      div(cls:="col-md-8 col-12",
        h2(cls:="text-center text-warning mb-4", "[stats] MONEYBALL TACTICS"),

        // Seccion Goles Encajados
        div(cls:="card bg-dark text-white border-danger shadow mb-4",
          div(cls:="card-header bg-danger text-white fw-bold text-center", "ZONA DE ENCAJE (Debilidades)"),
          div(cls:="card-body p-0",
            div(cls:="d-flex", tCell("ALTA", ga, "bg-danger bg-opacity-75"), tCell("MEDIA", gm, "bg-warning bg-opacity-75"), tCell("BAJA", gr, "bg-light bg-opacity-75")),
            div(cls:="d-flex", tCell("IZQ", gl, "bg-danger bg-opacity-75"), tCell("CEN", gc, "bg-warning bg-opacity-75"), tCell("DER", gd, "bg-danger bg-opacity-75"))
          )
        ),

        // Seccion Paradas
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
                    esPrevio: String = "false", // Cambiado a String
                    archivo: cask.FormValue) = {
    val isPrevio = esPrevio == "on"

    // 1. Extraemos los bytes directamente usando la interfaz de datos de Cask
    // Intentamos obtener los bytes y el nombre sin llamar a la clase interna .File
    val fileBytes = try {
      archivo.getClass.getMethod("data").invoke(archivo).asInstanceOf[Array[Byte]]
    } catch {
      case _: Exception => Array.empty[Byte]
    }

    val fileName = try {
      archivo.getClass.getMethod("name").invoke(archivo).asInstanceOf[String]
    } catch {
      case _: Exception => "documento.pdf"
    }

    if (fileBytes.nonEmpty) {
      // 2. Proceso para Gemini
      val base64Content = java.util.Base64.getEncoder.encodeToString(fileBytes)
      val mimeType = if (fileName.toLowerCase.endsWith(".pdf")) "application/pdf" else "image/jpeg"

      val medicalPrompt = s"Analiza este informe ($tipo) de Hector. Extrae DIAGNOSTICO y RECOMENDACION DEPORTIVA. Formato: DIAGNOSTICO: [texto] | RECOMENDACION: [texto]"

      val analisisIA = DatabaseManager.AIProvider.ask(medicalPrompt, Some((mimeType, base64Content)))
      val partes = analisisIA.split("\\|")
      val diag = partes.headOption.getOrElse("No detectado").replace("DIAGNOSTICO:", "").trim
      val rec = partes.lastOption.getOrElse("No detectado").replace("RECOMENDACION:", "").trim

      DatabaseManager.saveMedicalRecordFull(fecha, tipo, diag, rec, isPrevio)
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
        h2(cls:="text-center text-warning mb-4", "[star] MODO LEGADO"),

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
          div(cls:="col-6", div(cls:="p-3 border border-secondary rounded text-center bg-secondary bg-opacity-10", h3("[shield]"), h6("Muro"), small("Bonus por Porteria a Cero"))),
          div(cls:="col-6", div(cls:="p-3 border border-secondary rounded text-center bg-secondary bg-opacity-10", h3("[guantes]"), h6("Manos de Oro"), small("Bonus por Paradas")))
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

  // --- Helpers de respuesta HTTP ---
  def renderHtml(content: String): cask.Response[Array[Byte]] = {
    val html = doctype("html")(content).render
    cask.Response(html.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  def renderRedirect(url: String): cask.Response[Array[Byte]] = {
    cask.Response(Array.emptyByteArray, statusCode = 302, headers = Seq("Location" -> url))
  }

}