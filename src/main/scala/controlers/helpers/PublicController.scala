import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

// ─────────────────────────────────────────────────────────────────────────────
// BLOQUE C — URL PUBLICA CONTROLADA DE HECTOR (/hector)
// Ruta de solo lectura para terceros de confianza (ojeadores, entrenadores).
// Nunca expone datos medicos, wellness, audio-diarios ni notas privadas.
// ─────────────────────────────────────────────────────────────────────────────
object PublicController extends cask.Routes {

  private val publicCookieName = "guardian_public_read"

  private def parseBody(request: cask.Request): Map[String, String] = {
    val body = new String(request.data.readAllBytes(), "UTF-8")
    body.split("&").filter(_.nonEmpty).map { p =>
      val kv = p.split("=", 2)
      java.net.URLDecoder.decode(kv(0), "UTF-8") -> (if (kv.length > 1) java.net.URLDecoder.decode(kv(1), "UTF-8") else "")
    }.toMap
  }

  private def isPublicAuthenticated(request: cask.Request): Boolean =
    request.cookies.get(publicCookieName).map(_.value).contains("ok")

  @cask.get("/hector")
  def publicProfile(request: cask.Request, error: String = ""): cask.Response[Array[Byte]] = {
    val config = DatabaseManager.getPerfilPublicoConfig()
    val activo = config("activo").asInstanceOf[Boolean]
    if (!activo) {
      cask.Response("Not Found".getBytes("UTF-8"), statusCode = 404, headers = Seq("Content-Type" -> "text/plain; charset=utf-8"))
    } else if (!isPublicAuthenticated(request)) {
      renderLoginScreen(error)
    } else {
      DatabaseManager.registrarVisitaPublica()
      renderPublicProfile(config)
    }
  }

  @cask.post("/hector/login")
  def publicLogin(request: cask.Request): cask.Response[Array[Byte]] = {
    val p = parseBody(request)
    val pass = p.getOrElse("pass", "")
    if (DatabaseManager.validatePublicPassword(pass)) {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location"   -> "/hector",
        "Set-Cookie" -> s"$publicCookieName=ok; Path=/hector; SameSite=Lax; HttpOnly; Max-Age=86400"
      ))
    } else {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location" -> s"/hector?error=${java.net.URLEncoder.encode("Contraseña incorrecta", "UTF-8")}"
      ))
    }
  }

  private def renderLoginScreen(error: String): cask.Response[Array[Byte]] = {
    val pageHtml = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tags2.title("Guardian Elite — Perfil"),
        link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        link(rel := "stylesheet", href := "https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;700&display=swap"),
        tags2.style(raw("""
          body { background:#121212; color:#f0f0f0; font-family:'Oswald',sans-serif; }
          .card { background:#1e1e1e !important; border-color:#333 !important; }
          input.form-control { background:#2b2b2b !important; color:#fff !important; border-color:#444 !important; }
        """))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center", style := "min-height:100vh;",
          div(style := "width:340px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:48px;", "🛡"),
              h4(cls := "fw-black text-warning", "GUARDIAN ELITE"),
              span(cls := "text-muted small", "Perfil de Héctor")
            ),
            if (error.nonEmpty) div(cls := "alert alert-danger small p-2 mb-3", error) else span(),
            div(cls := "card p-4",
              form(action := "/hector/login", method := "post",
                div(cls := "mb-3",
                  input(tpe := "password", name := "pass", cls := "form-control", placeholder := "Contraseña", required := true)
                ),
                button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", "ENTRAR")
              )
            )
          )
        )
      )
    ).render
    cask.Response(pageHtml.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  private def renderPublicProfile(config: Map[String, Any]): cask.Response[Array[Byte]] = {
    val card = DatabaseManager.getLatestCardData()
    val edad = DatabaseManager.calcularEdadExacta(card.fechaNacimiento)

    val mostrarCarta      = config("mostrarCarta").asInstanceOf[Boolean]
    val mostrarProgresion = config("mostrarProgresion").asInstanceOf[Boolean]
    val mostrarVideoIa    = config("mostrarVideoIa").asInstanceOf[Boolean]
    val mostrarIdp        = config("mostrarIdp").asInstanceOf[Boolean]
    val mostrarInforme    = config("mostrarInforme").asInstanceOf[Boolean]
    val mostrarCognitivo  = config("mostrarCognitivo").asInstanceOf[Boolean]

    // --- Carta FUT ---
    val cartaSection: Modifier = if (mostrarCarta)
      div(cls := "d-flex justify-content-center mb-4",
        div(cls := "fut-card",
          div(cls := "left-info",
            div(cls := "rating", card.media), div(cls := "position", card.posicion),
            img(src := card.flagUrl, cls := "nation")),
          img(src := card.clubUrl, cls := "club-badge"),
          div(cls := "player-circle-container", img(src := card.fotoUrl, cls := "player-img")),
          div(cls := "name-container", div(cls := "player-name", card.nombre),
            div(style := "font-size:12px; margin-top:-5px; opacity:.9; font-weight:bold;", card.clubNombre),
            div(style := "font-size:9px; opacity:.75; font-weight:bold; letter-spacing:0.5px;", card.categoria)),
          div(cls := "stats-container", div(cls := "stats-grid",
            div(cls := "stat-item", span(cls := "stat-val", card.div), span(cls := "stat-label", "DIV")),
            div(cls := "stat-item", span(cls := "stat-val", card.kic), span(cls := "stat-label", "KIC")),
            div(cls := "stat-item", span(cls := "stat-val", card.spd), span(cls := "stat-label", "SPD")),
            div(cls := "stat-item", span(cls := "stat-val", card.han), span(cls := "stat-label", "HAN")),
            div(cls := "stat-item", span(cls := "stat-val", card.ref), span(cls := "stat-label", "REF")),
            div(cls := "stat-item", span(cls := "stat-val", card.pos), span(cls := "stat-label", "POS"))
          ))
        )
      )
    else div()

    // --- Progresion de rating por temporada ---
    val evolution = DatabaseManager.getSeasonEvolution()
    val aniosJs  = evolution.map(e => s""""${e._1}"""").mkString("[", ",", "]")
    val mediasJs = evolution.map(e => f"${e._2}%.1f").mkString("[", ",", "]")
    val progresionSection: Modifier = if (mostrarProgresion && evolution.nonEmpty)
      div(cls := "card bg-dark border-secondary shadow mb-3",
        div(cls := "card-header text-white fw-bold small", "📈 Progresión de rating por temporada"),
        div(cls := "card-body", tag("canvas")(id := "publicEvolChart", style := "max-height:200px;"))
      )
    else div()

    // --- Ultimos 3 analisis de video IA (solo puntos fuertes + nota, nunca mejoras) ---
    val videoHist = DatabaseManager.getVideoAnalysisHistory().reverse.take(3)
    val videoSection: Modifier = if (mostrarVideoIa && videoHist.nonEmpty)
      div(cls := "card bg-dark border-secondary shadow mb-3",
        div(cls := "card-header text-white fw-bold small", "🎬 Últimos análisis de vídeo IA"),
        div(cls := "card-body p-3",
          frag(videoHist.map { h =>
            val secciones = DatabaseManager.parseVideoAnalysisSections(h("analisis").asInstanceOf[String])
            val fuertes: String = secciones.getOrElse("PUNTOS FUERTES", "")
            val nota: String = secciones.getOrElse("NOTA TÉCNICA GLOBAL", "")
            div(cls := "mb-3 pb-2 border-bottom border-secondary",
              div(cls := "small fw-bold text-warning", s"vs ${DatabaseManager.fixEncoding(h("rival").asInstanceOf[String])} — ${h("fecha").asInstanceOf[String]}"),
              div(cls := "xx-small text-success mt-1", strong("Puntos fuertes: "), fuertes),
              div(cls := "xx-small text-info mt-1", strong("Nota técnica: "), nota)
            )
          }: _*)
        )
      )
    else div()

    // --- IDP activo ---
    val idpSection: Modifier = if (mostrarIdp) {
      DatabaseManager.getActiveIdpTemporada() match {
        case Some(temp) =>
          val temporadaId = temp("id").asInstanceOf[Int]
          val objetivos = DatabaseManager.getIdpObjetivos(temporadaId)
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", s"🗺️ Plan de Desarrollo Individual — ${temp("temporada").asInstanceOf[String]}"),
            div(cls := "card-body p-3",
              frag(objetivos.map { o =>
                val progreso: Int = o("progresoPct").asInstanceOf[Int]
                div(cls := "mb-2",
                  div(cls := "d-flex justify-content-between xx-small text-muted",
                    span(o("dimension").asInstanceOf[String]), span(s"$progreso%")),
                  div(cls := "progress", style := "height:8px;",
                    div(cls := "progress-bar bg-warning", style := s"width:$progreso%"))
                )
              }: _*)
            )
          )
        case None => div()
      }
    } else div()

    // --- Informe de captacion (resumen estadistico, sin llamadas a Gemini) ---
    val matches = DatabaseManager.getMatchesList()
    val pj = matches.size
    val notaMedia = if (pj > 0) matches.map(_.nota).sum / pj else 0.0
    def gcOf(m: MatchLog): Int = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0)
    val cleanSheets = matches.count(gcOf(_) == 0)
    val pctCS = if (pj > 0) cleanSheets * 100 / pj else 0
    val skills = DatabaseManager.getGoalkeeperSkills()
    val pctChecklist = if (skills.nonEmpty) skills.count(_.conseguido) * 100 / skills.size else 0

    val informeSection: Modifier = if (mostrarInforme)
      div(cls := "card bg-dark border-secondary shadow mb-3",
        div(cls := "card-header text-white fw-bold small", "📄 Informe de captación"),
        div(cls := "card-body p-3",
          div(cls := "row text-center",
            div(cls := "col-4",
              div(cls := "fw-bold text-warning", f"$notaMedia%.1f"), div(cls := "xx-small text-muted", "Nota media")),
            div(cls := "col-4",
              div(cls := "fw-bold text-success", s"$pctCS%"), div(cls := "xx-small text-muted", s"Clean sheets ($cleanSheets/$pj)")),
            div(cls := "col-4",
              div(cls := "fw-bold text-info", s"$pctChecklist%"), div(cls := "xx-small text-muted", "Checklist técnico"))
          )
        )
      )
    else div()

    // --- Indice cognitivo ---
    val cognitivoTests = DatabaseManager.getCognitivoTests()
    val cognitivoSection: Modifier = if (mostrarCognitivo) {
      cognitivoTests.lastOption match {
        case Some(t) =>
          val indice: Double = t("indice").asInstanceOf[Double]
          val interpretacion =
            if (indice < 40) "Desarrollo inicial"
            else if (indice < 65) "En progreso"
            else if (indice < 80) "Avanzado"
            else "Élite para su edad"
          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small", "🧠 Índice cognitivo"),
            div(cls := "card-body p-3 text-center",
              div(style := "font-size:32px; font-weight:900; color:#d4af37;", f"$indice%.0f"),
              div(cls := "text-muted small fw-bold text-uppercase", interpretacion)
            )
          )
        case None => div()
      }
    } else div()

    val pageHtml = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tags2.title(s"${card.nombre} — Guardian Elite"),
        link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        link(rel := "stylesheet", href := "https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;700&display=swap"),
        script(src := "https://cdn.jsdelivr.net/npm/chart.js"),
        tags2.style(raw(getCss() + """
          body { padding-bottom:30px; }
        """))
      ),
      body(
        div(cls := "container", style := "max-width:600px; padding-top:24px;",
          div(cls := "text-center mb-4",
            span(cls := "text-warning fw-black", "G"), span(cls := "fw-black", " GUARDIAN ELITE"),
            div(cls := "text-muted xx-small mt-1", s"Perfil público de ${card.nombre} · $edad años")
          ),
          cartaSection,
          progresionSection,
          videoSection,
          idpSection,
          informeSection,
          cognitivoSection,
          div(cls := "text-center text-muted xx-small mt-4",
            "Perfil generado con Guardian Elite · Datos actualizados en tiempo real")
        ),
        script(raw(s"""
          var ctxPE = document.getElementById('publicEvolChart');
          if (ctxPE) {
            new Chart(ctxPE, {
              type: 'line',
              data: { labels: $aniosJs, datasets: [{ label: 'Rating', data: $mediasJs, borderColor: '#d4af37', backgroundColor: 'rgba(212,175,55,0.15)', borderWidth:2, pointRadius:4, fill:true, tension:0.3 }] },
              options: { responsive:true, plugins:{ legend:{ display:false } } }
            });
          }
        """))
      )
    ).render
    cask.Response(pageHtml.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  initialize()
}
