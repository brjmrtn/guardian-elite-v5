import cask._
import scalatags.Text.all._
import scalatags.Text.tags2

// ─────────────────────────────────────────────────────────────────────────────
// GUARDIAN AMATEUR — Controller principal
// Todas las rutas bajo /am/*
// Auth por cookie independiente: am_session={userId}
// ─────────────────────────────────────────────────────────────────────────────
object AmateurController extends cask.Routes {

  val AM_COOKIE = "guardian_session"
  def amCookieValue(id: Int) = s"am:$id"

  // ── AUTH HELPERS ───────────────────────────────────────────────────────────
  // Cookie unificada guardian_session=am:{id} — gestionada por AuthController
  private def getAmUserId(request: cask.Request): Option[Int] =
    request.cookies.get("guardian_session").flatMap { c =>
      val v = c.value
      if (v.startsWith("am:")) scala.util.Try(v.drop(3).toInt).toOption
      else None
    }

  private def withAmAuth(request: cask.Request)(
    f: AmUser => cask.Response[Array[Byte]]
  ): cask.Response[Array[Byte]] = {
    getAmUserId(request).flatMap(AmateurDatabaseManager.getUserById) match {
      case Some(user) => f(user)
      case None =>
        cask.Response(
          Array.emptyByteArray,
          statusCode = 302,
          headers = Seq("Location" -> "/login")
        )
    }
  }

  // ── RENDER ─────────────────────────────────────────────────────────────────
  private def renderAm(
                        activeLink: String,
                        userName: String,
                        pageContent: scalatags.Text.Modifier
                      ): cask.Response[Array[Byte]] = {
    val page = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tags2.title("Guardian Amateur"),
        link(rel := "stylesheet",
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        tags2.style(raw("""
          * { box-sizing: border-box; }
          body {
            background: #f0f4f8;
            color: #1a202c;
            font-family: 'Segoe UI', sans-serif;
            padding-bottom: 90px;
            min-height: 100vh;
          }
          /* ── RESET INPUTS — sobreescribe CSS global Elite ── */
          body input, body select, body textarea,
          body .form-control, body .form-select {
            background-color: #ffffff !important;
            background: #ffffff !important;
            color: #1a202c !important;
            border: 1px solid #cbd5e0 !important;
            -webkit-text-fill-color: #1a202c !important;
            font-weight: 500 !important;
            border-radius: 8px !important;
          }
          body input:focus, body select:focus, body textarea:focus,
          body .form-control:focus, body .form-select:focus {
            background-color: #ffffff !important;
            color: #1a202c !important;
            -webkit-text-fill-color: #1a202c !important;
            border-color: #0d6efd !important;
            box-shadow: 0 0 0 3px rgba(13,110,253,0.15) !important;
            outline: none !important;
          }
          body input:-webkit-autofill,
          body input:-webkit-autofill:hover,
          body input:-webkit-autofill:focus {
            -webkit-box-shadow: 0 0 0px 1000px #ffffff inset !important;
            -webkit-text-fill-color: #1a202c !important;
          }
          body input::placeholder, body textarea::placeholder {
            color: #a0aec0 !important; opacity: 1 !important;
          }
          body option { background: #fff !important; color: #1a202c !important; }
          body input[type=range] { background: transparent !important; border: none !important; box-shadow: none !important; }
          body input[type=date]::-webkit-calendar-picker-indicator,
          body input[type=time]::-webkit-calendar-picker-indicator { filter: none !important; }
          body .form-check-input { background-color: #fff !important; border-color: #cbd5e0 !important; }
          body .form-check-input:checked { background-color: #0d6efd !important; border-color: #0d6efd !important; }
          body label { color: #4a5568 !important; }
          /* ── LAYOUT ── */
          .bottom-nav {
            position: fixed; bottom: 0; left: 0; right: 0;
            background: #ffffff; border-top: 1px solid #e2e8f0;
            display: flex; z-index: 1000; padding-bottom: env(safe-area-inset-bottom);
            box-shadow: 0 -2px 8px rgba(0,0,0,0.07);
          }
          .nav-item {
            flex: 1; text-align: center; padding: 8px 2px 6px;
            text-decoration: none; color: #a0aec0; font-size: 10px;
            display: flex; flex-direction: column; align-items: center;
          }
          .nav-item.active { color: #0d6efd; }
          .nav-item .nav-icon { font-size: 20px; display: block; margin-bottom: 2px; }
          .xx-small { font-size: 0.7rem; }
          .am-header {
            background: #ffffff;
            border-bottom: 1px solid #e2e8f0;
            padding: 10px 16px;
            display: flex; align-items: center; justify-content: space-between;
            margin-bottom: 16px;
            box-shadow: 0 1px 4px rgba(0,0,0,0.06);
          }
          .card-am {
            background: #ffffff;
            border: 1px solid #e2e8f0;
            border-radius: 12px;
            box-shadow: 0 1px 4px rgba(0,0,0,0.05);
          }
          .btn-goal-zone {
            width: 100%; aspect-ratio: 1; font-size: 11px; font-weight: 700;
            border: 2px solid #e2e8f0; background: #f7fafc; color: #718096;
            border-radius: 6px; cursor: pointer; transition: all 0.15s;
          }
          .btn-goal-zone.selected { background: #dc3545; color: white; border-color: #dc3545; }
          .btn-dir {
            width: 100%; padding: 16px 8px; font-weight: 700; font-size: 14px;
            border: 2px solid #e2e8f0; background: #f7fafc; color: #718096;
            border-radius: 8px; cursor: pointer; transition: all 0.15s;
          }
          .btn-dir.selected-tiro     { background: #dc3545; color: white; border-color: #dc3545; }
          .btn-dir.selected-estirada { background: #0d6efd; color: white; border-color: #0d6efd; }
          .nota-badge {
            width: 48px; height: 48px; border-radius: 50%;
            display: flex; align-items: center; justify-content: center;
            font-weight: 900; font-size: 16px;
          }
          .badge-green  { background: #c6f6d5; color: #276749; }
          .badge-yellow { background: #fefcbf; color: #744210; }
          .badge-red    { background: #fed7d7; color: #9b2c2c; }
          .text-muted   { color: #718096 !important; }
          .border-bottom { border-bottom-color: #e2e8f0 !important; }
          .cal-day {
            min-height: 56px; background: #f7fafc; border: 1px solid #e2e8f0;
            border-radius: 8px; padding: 4px 6px; font-size: 11px; color: #1a202c;
          }
          .cal-day.today        { border-color: #0d6efd; background: #ebf8ff; }
          .cal-day.has-match    { border-color: #38a169; background: #f0fff4; }
          .cal-day.has-schedule { border-color: #d69e2e; background: #fffff0; }
          .cal-day .day-num { font-weight: 700; font-size: 13px; color: #1a202c; }
          .cal-dot { width:8px; height:8px; border-radius:50%; display:inline-block; margin:1px; }
        """))
      ),
      body(
        // Header
        div(cls := "am-header",
          div(
            span(cls := "fw-black text-primary", style := "font-size:15px;", "🛡 GUARDIAN"),
            span(cls := "badge bg-primary ms-1", style := "font-size:9px;", "AMATEUR"),
            span(cls := "d-block xx-small text-muted", userName)
          ),
          a(href := "/profiles", cls := "btn btn-outline-secondary btn-sm xx-small", "Cambiar")
        ),

        // Contenido
        div(cls := "container-fluid px-3", pageContent),

        // Nav inferior
        tags2.nav(cls := "bottom-nav",
          a(href := "/am/dashboard",
            cls := s"nav-item ${if (activeLink == "home") "active" else ""}",
            span(cls := "nav-icon", "🏠"), span("Inicio")),
          a(href := "/am/match-center",
            cls := s"nav-item ${if (activeLink == "match") "active" else ""}",
            span(cls := "nav-icon", "⚽"), span("Partido")),
          a(href := "/am/calendar",
            cls := s"nav-item ${if (activeLink == "calendar") "active" else ""}",
            span(cls := "nav-icon", "📅"), span("Agenda")),
          a(href := "/am/penalties",
            cls := s"nav-item ${if (activeLink == "penalties") "active" else ""}",
            span(cls := "nav-icon", "🥅"), span("Penaltis")),
          a(href := "/am/gear",
            cls := s"nav-item ${if (activeLink == "gear") "active" else ""}",
            span(cls := "nav-icon", "🧤"), span("Guantes")),
          a(href := "/am/history",
            cls := s"nav-item ${if (activeLink == "history") "active" else ""}",
            span(cls := "nav-icon", "📊"), span("Historial")),
          a(href := "/am/progression",
            cls := s"nav-item ${if (activeLink == "progression") "active" else ""}",
            span(cls := "nav-icon", "📈"), span("Progreso")),
          a(href := "/am/mapa-goles",
            cls := s"nav-item ${if (activeLink == "goals") "active" else ""}",
            span(cls := "nav-icon", "🥅"), span("Mapa")),
          a(href := "/am/rivals",
            cls := s"nav-item ${if (activeLink == "rivals") "active" else ""}",
            span(cls := "nav-icon", "⚔️"), span("Rivales")),
          a(href := "/am/wellness",
            cls := s"nav-item ${if (activeLink == "wellness") "active" else ""}",
            span(cls := "nav-icon", "🧠"), span("Wellness"))
        ),

        script(src := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")
      )
    ).render

    cask.Response(
      page.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8")
    )
  }

  // ── LOGIN / REGISTER ───────────────────────────────────────────────────────
  @cask.get("/am/login")
  def loginPage(request: cask.Request, error: String = "") = {
    val page = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tags2.title("Guardian Amateur - Login"),
        link(rel := "stylesheet",
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        tags2.style(raw("body { background:#f0f4f8; color:#1a202c; } .card { background:#fff !important; border-color:#e2e8f0 !important; } input, select { background:#fff !important; color:#1a202c !important; border-color:#cbd5e0 !important; }"))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center",
          style := "min-height:100vh;",
          div(style := "width:340px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:48px;", "🛡"),
              h3(cls := "fw-black text-primary", "GUARDIAN AMATEUR"),
              span(cls := "text-muted small", "Tu rendimiento, registrado.")
            ),
            div(cls := "card bg-dark border-primary p-4 mb-3",
              h5(cls := "text-white fw-bold mb-3", "Iniciar sesión"),
              if (error.nonEmpty) div(cls := "alert alert-danger small p-2 mb-3", error) else span(),
              form(action := "/am/login", method := "post",
                div(cls := "mb-3",
                  label(cls := "text-muted small fw-bold", "USUARIO"),
                  input(tpe := "text", name := "username", cls := "form-control bg-dark text-white border-secondary mt-1", required := true, attr("autocomplete") := "username")
                ),
                div(cls := "mb-3",
                  label(cls := "text-muted small fw-bold", "CONTRASEÑA"),
                  input(tpe := "password", name := "password", cls := "form-control bg-dark text-white border-secondary mt-1", required := true)
                ),
                button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "ENTRAR")
              )
            ),
            div(cls := "card bg-dark border-secondary p-3 text-center",
              p(cls := "text-muted small mb-2", "¿Primera vez? Crea tu cuenta gratis"),
              a(href := "/am/register", cls := "btn btn-outline-secondary w-100 btn-sm", "Registrarse")
            )
          )
        )
      )
    ).render
    cask.Response(page.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.postForm("/am/login")
  def doLogin(request: cask.Request, username: String, password: String) = {
    AmateurDatabaseManager.authenticate(username, password) match {
      case Some(user) =>
        cask.Response(
          Array.emptyByteArray,
          statusCode = 302,
          headers = Seq(
            "Location"   -> "/am/dashboard",
            "Set-Cookie" -> s"guardian_session=${amCookieValue(user.id)}; Path=/; HttpOnly; SameSite=Lax; Max-Age=604800"
          )
        )
      case None =>
        cask.Response(
          Array.emptyByteArray,
          statusCode = 302,
          headers = Seq("Location" -> "/am/login?error=Usuario+o+contraseña+incorrectos")
        )
    }
  }

  @cask.get("/am/register")
  def registerPage(request: cask.Request, error: String = "") = {
    val page = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tags2.title("Guardian Amateur - Registro"),
        link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        tags2.style(raw("body { background:#f0f4f8; color:#1a202c; } .card { background:#fff !important; border-color:#e2e8f0 !important; } input, select { background:#fff !important; color:#1a202c !important; border-color:#cbd5e0 !important; }"))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center", style := "min-height:100vh;",
          div(style := "width:340px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:48px;", "🛡"),
              h3(cls := "fw-black text-primary", "Crear cuenta")
            ),
            div(cls := "card bg-dark border-primary p-4",
              if (error.nonEmpty) div(cls := "alert alert-danger small p-2 mb-3", error) else span(),
              form(action := "/am/register", method := "post",
                div(cls := "mb-3",
                  label(cls := "text-muted small fw-bold", "TU NOMBRE"),
                  input(tpe := "text", name := "nombre", cls := "form-control bg-dark text-white border-secondary mt-1", placeholder := "Ej: Carlos López", required := true)
                ),
                div(cls := "mb-3",
                  label(cls := "text-muted small fw-bold", "USUARIO"),
                  input(tpe := "text", name := "username", cls := "form-control bg-dark text-white border-secondary mt-1", placeholder := "sin espacios, sin tildes", required := true, attr("autocomplete") := "username")
                ),
                div(cls := "mb-3",
                  label(cls := "text-muted small fw-bold", "CONTRASEÑA"),
                  input(tpe := "password", name := "password", cls := "form-control bg-dark text-white border-secondary mt-1", required := true, attr("minlength") := "4")
                ),
                button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "CREAR CUENTA"),
                div(cls := "text-center mt-3",
                  a(href := "/am/login", cls := "text-muted small", "Ya tengo cuenta"))
              )
            )
          )
        )
      )
    ).render
    cask.Response(page.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.postForm("/am/register")
  def doRegister(request: cask.Request, nombre: String, username: String, password: String) = {
    if (username.trim.isEmpty || password.length < 4) {
      cask.Response(Array.emptyByteArray, 302,
        headers = Seq("Location" -> "/am/register?error=Usuario+y+contraseña+mínimo+4+caracteres"))
    } else {
      AmateurDatabaseManager.registerUser(username.trim.toLowerCase, password, nombre) match {
        case Right(id) =>
          cask.Response(Array.emptyByteArray, 302,
            headers = Seq(
              "Location"   -> "/am/dashboard",
              "Set-Cookie" -> s"guardian_session=${amCookieValue(id)}; Path=/; HttpOnly; SameSite=Lax; Max-Age=604800"
            ))
        case Left(err) =>
          cask.Response(Array.emptyByteArray, 302,
            headers = Seq("Location" -> s"/am/register?error=${java.net.URLEncoder.encode(err, "UTF-8")}"))
      }
    }
  }

  @cask.get("/am/logout")
  def doLogout(request: cask.Request) =
    cask.Response(Array.emptyByteArray, 302,
      headers = Seq(
        "Location"   -> "/profiles",
        "Set-Cookie" -> s"guardian_session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
      ))

  // ── DASHBOARD ──────────────────────────────────────────────────────────────
  @cask.get("/am/dashboard")
  def dashboardPage(request: cask.Request) = withAmAuth(request) { user =>
    val st       = AmateurDatabaseManager.getDashboardStats(user.id)
    val upcoming = AmateurDatabaseManager.getUpcomingSchedule(user.id, 1)
    val seasonInfo = AmateurDatabaseManager.getSeasonInfo(user.id)
    val currentSeason = seasonInfo("currentSeason").asInstanceOf[Int]
    val pastSeasons   = seasonInfo("pastSeasons").asInstanceOf[List[Map[String, String]]]
    val pj             = st("pj").asInstanceOf[Int]
    val notaMedia      = st("notaMedia").asInstanceOf[Double]
    val notaAjustada   = st("notaAjustada").asInstanceOf[Double]
    val gcMedia        = st("gcMedia").asInstanceOf[Double]
    val limpias        = st("limpias").asInstanceOf[Int]
    val ganados        = st("ganados").asInstanceOf[Int]
    val empatados      = st("empatados").asInstanceOf[Int]
    val perdidos       = st("perdidos").asInstanceOf[Int]
    val rachaLimpias   = st("rachaLimpias").asInstanceOf[Int]
    val ultimos        = st("ultimos").asInstanceOf[List[Map[String, String]]]

    def notaColor(n: Double) = if (n >= 7.0) "success" else if (n >= 5.0) "warning" else "danger"
    def notaBadgeCls(n: Double) = if (n >= 7.0) "badge-green" else if (n >= 5.0) "badge-yellow" else "badge-red"

    renderAm("home", user.nombre,
      div(
        // Bienvenida
        div(cls := "mb-3",
          h5(cls := "fw-black text-white mb-0", s"Hola, ${user.nombre} 👋"),
          span(cls := "text-muted small", if (pj == 0) "Registra tu primer partido para empezar."
          else s"$pj partidos registrados")
        ),

        if (pj == 0)
          div(cls := "card-am p-4 text-center mb-3",
            div(style := "font-size:52px; opacity:0.4", "⚽"),
            h5(cls := "text-muted mt-3", "Sin partidos aún"),
            p(cls := "text-secondary small", "Pulsa en «Partido» para registrar tu primera actuación."),
            a(href := "/am/match-center", cls := "btn btn-primary mt-2 fw-bold", "Registrar partido")
          )
        else frag(

          // Próximo partido programado — Widget con cuenta atrás
          upcoming.headOption.map { s =>
            val tipoColor = s.tipo match {
              case "LIGA"     => "#0d6efd"
              case "TORNEO"   => "#dc3545"
              case "CUP"      => "#6f42c1"
              case _          => "#20c997"
            }
            val tipoBadge = s.tipo match {
              case "LIGA"     => "LIGA"
              case "TORNEO"   => "TORNEO"
              case "CUP"      => "CUP"
              case _          => "AMISTOSO"
            }
            div(cls := "card-am p-3 mb-3",
              style := s"border-left: 4px solid $tipoColor;",
              div(cls := "d-flex justify-content-between align-items-start mb-1",
                div(cls := "xx-small fw-bold text-muted", "PROXIMO PARTIDO"),
                span(cls := "badge rounded-pill xx-small",
                  style := s"background:${tipoColor}22; color:$tipoColor;",
                  tipoBadge)
              ),
              div(cls := "fw-black mb-1", style := "font-size:1.15rem;", s.rival),
              div(cls := "xx-small text-muted mb-2",
                s.fecha,
                if (s.hora.nonEmpty) s" - ${s.hora}" else "",
                if (s.lugar.nonEmpty) s" - ${s.lugar}" else ""
              ),
              div(id := "countdown-widget", cls := "fw-bold text-center mb-2",
                style := s"font-size:1.05rem; color:$tipoColor;", "..."),
              div(cls := "row g-1",
                div(cls := "col-8",
                  a(href := "/am/match-center", cls := "btn btn-sm fw-bold w-100",
                    style := s"background:$tipoColor; color:#fff;", "Registrar partido")
                ),
                div(cls := "col-4",
                  a(href := "/am/calendar", cls := "btn btn-sm btn-outline-secondary fw-bold w-100", "Agenda")
                )
              ),
              script(raw(s"""
                (function() {
                  var target = new Date("${s.fecha}T${if (s.hora.nonEmpty && s.hora.length >= 5) s.hora else "10:00"}:00");
                  function update() {
                    var now = new Date(); var diff = target - now;
                    var el = document.getElementById('countdown-widget');
                    if (!el) return;
                    if (diff <= 0) { el.textContent = "HOY JUEGAS!"; return; }
                    var d = Math.floor(diff/86400000), h = Math.floor((diff%86400000)/3600000), m = Math.floor((diff%3600000)/60000);
                    if (d > 0) el.textContent = d+"d "+h+"h para el partido";
                    else if (h > 0) el.textContent = h+"h "+m+"m para el partido";
                    else el.textContent = m+" minutos para el partido";
                  }
                  update(); setInterval(update, 60000);
                })();
              """))
            )
          }.getOrElse(
            div(cls := "card-am p-3 mb-3 text-center",
              style := "border-style:dashed;",
              div(cls := "xx-small text-muted mt-1", "Sin partidos programados"),
              a(href := "/am/calendar/add", cls := "btn btn-outline-primary btn-sm mt-2 fw-bold",
                "+ Añadir a la agenda")
            )
          ),

          // KPIs principales
          div(cls := "row g-2 mb-3",
            div(cls := "col-6",
              div(cls := "card-am p-3 text-center",
                div(cls := s"fw-black text-${notaColor(notaMedia)}", style := "font-size:2.4rem;",
                  f"$notaMedia%.1f"),
                div(cls := "xx-small text-muted", "Nota media"),
                if (notaAjustada > notaMedia + 0.05)
                  div(cls := "xx-small text-success mt-1",
                    f"↑ $notaAjustada%.1f ajustada")
                else span()
              )
            ),
            div(cls := "col-6",
              div(cls := "card-am p-3 text-center",
                div(cls := "fw-black text-danger", style := "font-size:2.4rem;",
                  f"$gcMedia%.1f"),
                div(cls := "xx-small text-muted", "GC por partido")
              )
            ),
            div(cls := "col-4",
              div(cls := "card-am p-2 text-center",
                div(cls := "fw-bold text-success", style := "font-size:1.6rem;", limpias.toString),
                div(cls := "xx-small text-muted", "Limpias")
              )
            ),
            div(cls := "col-4",
              div(cls := "card-am p-2 text-center",
                div(cls := "fw-bold text-info", style := "font-size:1.6rem;", pj.toString),
                div(cls := "xx-small text-muted", "Partidos")
              )
            ),
            div(cls := "col-4",
              div(cls := "card-am p-2 text-center",
                div(cls := "fw-bold text-warning", style := "font-size:1.6rem;", rachaLimpias.toString),
                div(cls := "xx-small text-muted", "Racha 0 GC")
              )
            )
          ),

          // Resultados + win rate
          div(cls := "card-am p-3 mb-3",
            div(cls := "d-flex justify-content-around text-center",
              div(
                div(cls := "fw-black text-success", style := "font-size:1.8rem;", ganados.toString),
                div(cls := "xx-small text-muted", "Ganados")
              ),
              div(cls := "border-start border-secondary"),
              div(
                div(cls := "fw-black text-warning", style := "font-size:1.8rem;", empatados.toString),
                div(cls := "xx-small text-muted", "Empates")
              ),
              div(cls := "border-start border-secondary"),
              div(
                div(cls := "fw-black text-danger", style := "font-size:1.8rem;", perdidos.toString),
                div(cls := "xx-small text-muted", "Perdidos")
              ),
              div(cls := "border-start border-secondary"),
              {
                val wr = if (pj > 0) (ganados.toDouble / pj * 100).toInt else 0
                val wrColor = if (wr >= 60) "text-success" else if (wr >= 40) "text-warning" else "text-danger"
                div(
                  div(cls := s"fw-black $wrColor", style := "font-size:1.8rem;", s"$wr%"),
                  div(cls := "xx-small text-muted", "Win rate")
                )
              }
            )
          ),

          // Últimos partidos
          if (ultimos.nonEmpty)
            div(cls := "card-am p-3 mb-3",
              div(cls := "fw-bold small text-muted mb-2", "ÚLTIMOS PARTIDOS"),
              frag(ultimos.map { m =>
                val nota = m("nota").toDouble
                div(cls := "d-flex align-items-center gap-2 py-2",
                  style := "border-bottom:1px solid #1e1e1e;",
                  div(cls := s"nota-badge ${notaBadgeCls(nota)}", m("nota")),
                  div(cls := "flex-fill",
                    div(cls := "fw-bold small text-white", m("rival")),
                    div(cls := "xx-small text-muted", m("fecha"))
                  ),
                  div(cls := "fw-black text-white small", m("res"))
                )
              }: _*)
            )
          else span(),

          // Acceso rápido
          div(cls := "row g-2 mb-3",
            div(cls := "col-6",
              a(href := "/am/match-center", cls := "btn btn-primary w-100 fw-bold py-3",
                "⚽ Nuevo partido")),
            div(cls := "col-6",
              a(href := "/am/progression", cls := "btn btn-outline-info w-100 fw-bold py-3",
                "📈 Mi progreso"))
          ),

          // Temporada actual + historial
          div(cls := "card-am p-3 mb-3",
            div(cls := "d-flex justify-content-between align-items-center mb-2",
              div(
                div(cls := "xx-small fw-bold text-muted", "TEMPORADA ACTUAL"),
                div(cls := "fw-black text-white", style := "font-size:1.1rem;", s"Temporada $currentSeason")
              ),
              button(
                tpe := "button",
                cls := "btn btn-outline-warning btn-sm fw-bold",
                style := "font-size:11px;",
                attr("data-bs-toggle") := "modal",
                attr("data-bs-target") := "#modalEndSeason",
                "🏁 Finalizar temporada"
              )
            ),
            if (pastSeasons.nonEmpty)
              div(
                div(cls := "xx-small fw-bold text-muted mb-2", "TEMPORADAS ANTERIORES"),
                frag(pastSeasons.map { s =>
                  div(cls := "d-flex justify-content-between align-items-center py-1",
                    style := "border-bottom:1px solid #1e1e1e; font-size:11px;",
                    div(cls := "text-muted", s"T${s("num")} · ${s("ended").take(7)}"),
                    div(cls := "text-white",
                      span(cls := "text-success me-1", s"${s("g")}G"),
                      span(cls := "text-muted me-1", s"${s("e")}E"),
                      span(cls := "text-danger me-1", s"${s("p")}P"),
                      span(cls := "text-warning", s"★${s("nota")}")
                    )
                  )
                }: _*)
              )
            else span()
          ),

          // Modal confirmación finalizar temporada
          div(cls := "modal fade", id := "modalEndSeason",
            attr("tabindex") := "-1",
            div(cls := "modal-dialog modal-dialog-centered",
              div(cls := "modal-content bg-dark border-warning",
                div(cls := "modal-header border-warning",
                  h5(cls := "modal-title text-warning fw-black", "🏁 Finalizar temporada"),
                  button(tpe := "button", cls := "btn-close btn-close-white",
                    attr("data-bs-dismiss") := "modal")
                ),
                div(cls := "modal-body text-white",
                  p(s"¿Seguro que quieres cerrar la Temporada $currentSeason?"),
                  p(cls := "text-muted small",
                    "Se guardará un resumen de la temporada y todos los partidos nuevos contarán para la Temporada ",
                    strong(cls := "text-warning", s"${currentSeason + 1}"),
                    ". Los datos históricos se conservan.")
                ),
                div(cls := "modal-footer border-secondary",
                  button(tpe := "button", cls := "btn btn-secondary",
                    attr("data-bs-dismiss") := "modal", "Cancelar"),
                  a(href := "/am/end-season", cls := "btn btn-warning fw-bold",
                    "✅ Confirmar y nueva temporada")
                )
              )
            )
          )
        )
      )
    )
  }

  // ── MATCH CENTER ───────────────────────────────────────────────────────────
  @cask.get("/am/match-center")
  def matchCenterPage(request: cask.Request) = withAmAuth(request) { user =>
    renderAm("match", user.nombre,
      div(
        h5(cls := "fw-black text-white mb-3", "⚽ Registrar partido"),

        form(action := "/am/match/save", method := "post", id := "matchForm",

          // Selector de posición
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small fw-bold text-muted mb-2", "DE QUE JUGASTE?"),
            div(cls := "d-flex gap-2",
              div(cls := "flex-fill",
                input(tpe := "radio", name := "posicion_partido", id := "pos_portero",
                  value := "portero", checked := true, style := "display:none;",
                  attr("onchange") := "togglePosicion()"),
                label(cls := "btn btn-primary w-100 fw-bold", attr("for") := "pos_portero",
                  id := "lbl_portero", style := "font-size:13px;", "Portero")
              ),
              div(cls := "flex-fill",
                input(tpe := "radio", name := "posicion_partido", id := "pos_jugador",
                  value := "jugador", style := "display:none;",
                  attr("onchange") := "togglePosicion()"),
                label(cls := "btn btn-outline-secondary w-100 fw-bold", attr("for") := "pos_jugador",
                  id := "lbl_jugador", style := "font-size:13px;", "Jugador de campo")
              )
            ),
            div(id := "posicion_campo_div", style := "display:none;",
              div(cls := "mt-2",
                label(cls := "xx-small text-muted fw-bold", "POSICION EN CAMPO"),
                select(name := "posicion_campo", cls := "form-select mt-1",
                  option(value := "Delantero", "Delantero"),
                  option(value := "Centrocampista", "Centrocampista"),
                  option(value := "Extremo", "Extremo"),
                  option(value := "Defensa", "Defensa")
                )
              ),
              div(cls := "row g-2 mt-1",
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "GOLES MARCADOS"),
                  input(tpe := "number", name := "goles_marcados", value := "0",
                    cls := "form-control mt-1", attr("min") := "0", attr("max") := "20")
                ),
                div(cls := "col-6",
                  label(cls := "xx-small text-muted fw-bold", "ASISTENCIAS"),
                  input(tpe := "number", name := "asistencias", value := "0",
                    cls := "form-control mt-1", attr("min") := "0", attr("max") := "20")
                )
              )
            )
          ),

          // Rival y fecha
          div(cls := "card-am p-3 mb-3",
            div(cls := "row g-2",
              div(cls := "col-8",
                label(cls := "xx-small text-muted fw-bold", "RIVAL"),
                input(tpe := "text", name := "rival", cls := "form-control bg-dark text-white border-secondary mt-1", placeholder := "Nombre del equipo rival", required := true)
              ),
              div(cls := "col-4",
                label(cls := "xx-small text-muted fw-bold", "FECHA"),
                input(tpe := "date", name := "fecha", cls := "form-control bg-dark text-white border-secondary mt-1",
                  value := java.time.LocalDate.now().toString)
              )
            )
          ),

          // Resultado
          div(cls := "card-am p-3 mb-3", id := "gc_section",
            div(cls := "xx-small text-muted fw-bold mb-2", "RESULTADO"),
            div(cls := "row g-3 text-center",
              div(cls := "col-5",
                label(cls := "small text-success fw-bold", "A FAVOR"),
                input(tpe := "number", name := "gf", id := "inGF",
                  cls := "form-control text-center bg-success bg-opacity-25 text-white border-0 fw-black mt-1",
                  style := "font-size:2rem;", value := "0", min := "0", attr("inputmode") := "numeric")
              ),
              div(cls := "col-2 d-flex align-items-center justify-content-center",
                span(cls := "text-muted fw-bold", style := "font-size:1.5rem;", "−")
              ),
              div(cls := "col-5",
                label(cls := "small text-danger fw-bold", "EN CONTRA"),
                input(tpe := "number", name := "gc", id := "inGC",
                  cls := "form-control text-center bg-danger bg-opacity-25 text-white border-0 fw-black mt-1",
                  style := "font-size:2rem;", value := "0", min := "0",
                  attr("inputmode") := "numeric",
                  attr("oninput") := "syncGoalCount(this.value)")
              )
            )
          ),

          // Nota
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small text-muted fw-bold mb-2", "TU NOTA (1-10)"),
            div(cls := "d-flex align-items-center gap-3",
              input(tpe := "range", name := "nota", id := "notaSlider",
                cls := "form-range flex-fill", min := "1", max := "10", step := "0.5", value := "7",
                attr("oninput") := "document.getElementById('notaVal').textContent=this.value"),
              span(id := "notaVal", cls := "fw-black text-warning", style := "font-size:1.8rem; min-width:40px;", "7")
            )
          ),

          // Clima + Local/Visitante
          div(cls := "card-am p-3 mb-3",
            div(cls := "row g-2",
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "CLIMA"),
                select(name := "clima", cls := "form-select bg-dark text-white border-secondary mt-1",
                  option(value := "Sol", "☀️ Sol"),
                  option(value := "Nubes", "☁️ Nubes"),
                  option(value := "Lluvia", "🌧️ Lluvia"),
                  option(value := "Frio", "❄️ Frío"),
                  option(value := "Viento", "💨 Viento")
                )
              ),
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "ESTADIO"),
                input(tpe := "text", name := "estadio", cls := "form-control bg-dark text-white border-secondary mt-1",
                  placeholder := "Campo o pabellón")
              )
            ),
            div(cls := "mt-2",
              label(cls := "xx-small text-muted fw-bold", "¿LOCAL O VISITANTE?"),
              div(cls := "d-flex gap-2 mt-1",
                frag(Seq(("", "— Sin especificar"), ("true", "?? Local"), ("false", "✈️ Visitante")).map {
                  case (v, lbl) =>
                    label(cls := "flex-fill text-center border border-secondary rounded p-2 xx-small fw-bold",
                      style := "cursor:pointer; background:#1a1a1a;",
                      input(tpe := "radio", name := "esLocal", value := v, cls := "d-none",
                        if (v == "") attr("checked") := "checked" else span()),
                      span(lbl)
                    )
                }: _*)
              )
            )
          ),

          // Mapa de goles encajados
          div(cls := "card-am p-3 mb-3", id := "goalsSection",
            div(cls := "d-flex justify-content-between align-items-center mb-2",
              span(cls := "xx-small text-muted fw-bold", "GOLES ENCAJADOS — Zona y contexto"),
              span(id := "goalCounter", cls := "badge bg-danger", "0 goles")
            ),
            div(id := "goalsList"),
            div(cls := "text-center",
              button(tpe := "button", cls := "btn btn-outline-danger btn-sm mt-2 fw-bold",
                attr("onclick") := "addGoalRow()",
                "+ Añadir gol encajado")
            ),
            input(tpe := "hidden", name := "goalsData", id := "goalsData")
          ),

          // Video (opcional)
          div(cls := "card-am p-3 mb-3",
            label(cls := "xx-small text-muted fw-bold", "ENLACE DE VÍDEO (opcional)"),
            input(tpe := "url", name := "video", cls := "form-control bg-dark text-white border-secondary mt-1",
              placeholder := "https://youtube.com/...")
          ),

          // Notas
          div(cls := "card-am p-3 mb-3",
            label(cls := "xx-small text-muted fw-bold", "NOTAS DEL PARTIDO"),
            textarea(name := "notas", cls := "form-control bg-dark text-white border-secondary mt-1",
              rows := "3", placeholder := "Qué salió bien, qué mejorar...")()
          ),

          button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold py-3 mb-2",
            "GUARDAR PARTIDO")
        ),

        // JavaScript para el formulario
        script(raw("""
          var goalCount = 0;
          var goals = [];
          var zones = ['TL','TC','TR','ML','MC','MR','BL','BC','BR'];
          var zoneLabels = {
            'TL':'↖ Alto Izq','TC':'↑ Alto Cen','TR':'↗ Alto Der',
            'ML':'← Med Izq','MC':'· Centro','MR':'→ Med Der',
            'BL':'↙ Bajo Izq','BC':'↓ Bajo Cen','BR':'↘ Bajo Der'
          };

          function syncGoalCount(val) {
            document.getElementById('goalCounter').textContent = val + ' goles';
          }

          function addGoalRow() {
            goalCount++;
            var id = 'goal_' + goalCount;
            var html = '<div id="' + id + '" class="card bg-dark border-secondary p-2 mb-2 rounded">' +
              '<div class="d-flex justify-content-between align-items-center mb-2">' +
              '<span class="xx-small text-danger fw-bold">GOL ' + goalCount + '</span>' +
              '<button type="button" class="btn btn-outline-secondary btn-sm xx-small" onclick="removeGoal(\'' + id + '\')">✕</button>' +
              '</div>' +
              '<div class="xx-small text-muted fw-bold mb-1">Zona de portería:</div>' +
              '<div class="row g-1 mb-2" style="display:grid;grid-template-columns:repeat(3,1fr);gap:4px;">';

            zones.forEach(function(z) {
              html += '<button type="button" class="btn-goal-zone" id="' + id + '_zone_' + z + '"' +
                ' onclick="selectZone(\'' + id + '\',\'' + z + '\')">' + zoneLabels[z] + '</button>';
            });

            html += '</div>' +
              '<div class="xx-small text-muted fw-bold mb-1">Situación:</div>' +
              '<select id="' + id + '_sit" class="form-select form-select-sm bg-dark text-white border-secondary mb-2">' +
              '<option value="Remate">Remate</option>' +
              '<option value="1v1">1 vs 1</option>' +
              '<option value="2v1">2 vs 1</option>' +
              '<option value="Cabezazo">Cabezazo</option>' +
              '<option value="Tiro libre">Tiro libre</option>' +
              '<option value="Penalti">Penalti</option>' +
              '</select>' +
              '<div class="form-check">' +
              '<input class="form-check-input" type="checkbox" id="' + id + '_errdef">' +
              '<label class="form-check-label xx-small text-warning" for="' + id + '_errdef">' +
              '⚠️ Error defensivo del equipo (no cuenta para mi nota)' +
              '</label>' +
              '</div>' +
              '</div>';

            document.getElementById('goalsList').insertAdjacentHTML('beforeend', html);
            // Select center by default
            selectZone(id, 'MC');
            syncGoalCount(document.getElementById('inGC').value);
          }

          function selectZone(goalId, zone) {
            zones.forEach(function(z) {
              var btn = document.getElementById(goalId + '_zone_' + z);
              if (btn) btn.classList.toggle('selected', z === zone);
            });
          }

          function removeGoal(id) {
            var el = document.getElementById(id);
            if (el) el.remove();
          }

          function getSelectedZone(goalId) {
            var selected = 'MC';
            zones.forEach(function(z) {
              var btn = document.getElementById(goalId + '_zone_' + z);
              if (btn && btn.classList.contains('selected')) selected = z;
            });
            return selected;
          }

          // Serializar antes de enviar
          document.getElementById('matchForm').addEventListener('submit', function() {
            var rows = document.getElementById('goalsList').querySelectorAll('[id^="goal_"]');
            var data = [];
            rows.forEach(function(row) {
              var gid = row.id;
              data.push(
                getSelectedZone(gid) + '|' +
                document.getElementById(gid + '_sit').value + '|' +
                (document.getElementById(gid + '_errdef').checked ? '1' : '0')
              );
            });
            document.getElementById('goalsData').value = data.join(';');
          });

          // Toggle portero / jugador de campo
          function togglePosicion() {
            var esJugador = document.getElementById('pos_jugador').checked;
            document.getElementById('posicion_campo_div').style.display = esJugador ? 'block' : 'none';
            document.getElementById('goalsSection').style.display      = esJugador ? 'none'  : 'block';
            document.getElementById('lbl_portero').className = esJugador
              ? 'btn btn-outline-secondary w-100 fw-bold'
              : 'btn btn-primary w-100 fw-bold';
            document.getElementById('lbl_jugador').className = esJugador
              ? 'btn btn-primary w-100 fw-bold'
              : 'btn btn-outline-secondary w-100 fw-bold';
          }
          // Inicializar estado
          togglePosicion();

          // Radio buttons local/visitante visual
          document.querySelectorAll('input[name="esLocal"]').forEach(function(r) {
            r.addEventListener('change', function() {
              document.querySelectorAll('input[name="esLocal"]').forEach(function(x) {
                x.parentElement.style.background = '#1a1a1a';
                x.parentElement.style.color = '';
              });
              this.parentElement.style.background =
                this.value === 'true' ? 'rgba(40,167,69,0.2)' :
                this.value === 'false' ? 'rgba(13,202,240,0.2)' : '#1a1a1a';
            });
          });
        """))
      )
    )
  }

  @cask.post("/am/match/save")
  def saveMatch(request: cask.Request) = withAmAuth(request) { user =>
    val body = new String(request.data.readAllBytes(), "UTF-8")
    val params = body.split("&").map { pair =>
      val p = pair.split("=", 2)
      val k = java.net.URLDecoder.decode(p(0), "UTF-8")
      val v = if (p.length > 1) java.net.URLDecoder.decode(p(1), "UTF-8") else ""
      k -> v
    }.toMap

    def str(k: String) = params.getOrElse(k, "")
    def int(k: String) = try str(k).toInt catch { case _: Exception => 0 }
    def dbl(k: String) = try str(k).toDouble catch { case _: Exception => 0.0 }

    val esLocalOpt: Option[Boolean] = str("esLocal") match {
      case "true"  => Some(true)
      case "false" => Some(false)
      case _       => None
    }

    val posicionPartido = str("posicion_partido") match {
      case "jugador" => "jugador"
      case _         => "portero"
    }
    val posicionCampo   = str("posicion_campo")
    val golesMarcados   = int("goles_marcados")
    val asistencias     = int("asistencias")

    val matchId = AmateurDatabaseManager.logMatch(
      userId   = user.id,
      rival    = str("rival"),
      gf       = int("gf"),
      gc       = int("gc"),
      nota     = dbl("nota"),
      clima    = str("clima"),
      estadio  = str("estadio"),
      esLocal  = esLocalOpt,
      fecha    = str("fecha"),
      videoUrl = str("video"),
      notas    = str("notas"),
      posicionPartido = posicionPartido,
      posicionCampo   = posicionCampo,
      golesMarcados   = golesMarcados,
      asistencias     = asistencias
    )

    // Guardar goles
    val goalsData = str("goalsData")
    if (goalsData.nonEmpty && matchId > 0) {
      goalsData.split(";").foreach { row =>
        val parts = row.split("\\|")
        if (parts.length >= 3) {
          AmateurDatabaseManager.saveGoal(
            matchId        = matchId,
            zona           = parts(0),
            situacion      = parts(1),
            errorDefensivo = parts(2) == "1",
            minuto         = 0,
            notas          = ""
          )
        }
      }
    }

    cask.Response(Array.emptyByteArray, 302,
      headers = Seq("Location" -> "/am/dashboard"))
  }

  // ── HISTORIAL ──────────────────────────────────────────────────────────────
  @cask.get("/am/history")
  def historyPage(request: cask.Request) = withAmAuth(request) { user =>
    val matches = AmateurDatabaseManager.getMatches(user.id)

    def notaBadgeCls(n: Double) = if (n >= 7.0) "badge-green" else if (n >= 5.0) "badge-yellow" else "badge-red"
    def climaIcon(c: String) = c.toLowerCase match {
      case s if s.contains("sol")  => "☀️"
      case s if s.contains("lluv") => "??️"
      case s if s.contains("frio") => "??"
      case s if s.contains("vient")=> "??"
      case _                       => "☁️"
    }

    renderAm("history", user.nombre,
      div(
        div(cls := "d-flex justify-content-between align-items-center mb-3",
          h5(cls := "fw-black text-white mb-0", "?? Historial"),
          a(href := "/am/match-center", cls := "btn btn-primary btn-sm fw-bold", "+ Partido")
        ),

        if (matches.isEmpty)
          div(cls := "card-am p-4 text-center",
            div(style := "font-size:40px; opacity:0.3", "??"),
            p(cls := "text-muted mt-3", "Aún no has registrado ningún partido.")
          )
        else
          frag(matches.map { m =>
            val gcStr   = if (m.gc == 0) "✅" else m.gc.toString
            val locStr  = m.esLocal match { case Some(true) => "??" case Some(false) => "✈️" case None => "" }
            div(cls := "card-am p-3 mb-2",
              div(cls := "d-flex align-items-center gap-3",
                div(cls := s"nota-badge ${notaBadgeCls(m.nota)}", f"${m.nota}%.1f"),
                div(cls := "flex-fill",
                  div(cls := "fw-bold text-white small",
                    span(locStr, " "), m.rival),
                  div(cls := "xx-small text-muted",
                    s"${m.fecha}  ${climaIcon(m.clima)}")
                ),
                div(cls := "text-end",
                  div(cls := "fw-black text-white", s"${m.gf}—${m.gc}"),
                  div(cls := "xx-small text-muted", s"GC: $gcStr")
                )
              )
            )
          }: _*)
      )
    )
  }

  // ── PENALTIS ───────────────────────────────────────────────────────────────
  @cask.get("/am/penalties")
  def penaltiesPage(request: cask.Request) = withAmAuth(request) { user =>
    val penalties = AmateurDatabaseManager.getPenalties(user.id)
    val stats     = AmateurDatabaseManager.getPenaltyStats(user.id)

    val total          = stats("total").asInstanceOf[Int]
    val paradas        = stats("paradas").asInstanceOf[Int]
    val adivinados     = stats("adivinados").asInstanceOf[Int]
    val pctParada      = stats("pctParada").asInstanceOf[Int]
    val pctIntuicion   = stats("pctIntuicion").asInstanceOf[Int]
    val parConInt      = stats("paradasConIntuicion").asInstanceOf[Int]
    val tirIzq         = stats("tirIzq").asInstanceOf[Int]
    val tirCen         = stats("tirCen").asInstanceOf[Int]
    val tirDer         = stats("tirDer").asInstanceOf[Int]
    val estIzq         = stats("estIzq").asInstanceOf[Int]
    val estCen         = stats("estCen").asInstanceOf[Int]
    val estDer         = stats("estDer").asInstanceOf[Int]

    def pct(n: Int, d: Int) = if (d > 0) n * 100 / d else 0
    def barWidth(n: Int, d: Int) = s"${pct(n, d)}%"

    renderAm("penalties", user.nombre,
      div(
        h5(cls := "fw-black text-white mb-3", "?? Penaltis"),

        // Estadísticas
        if (total > 0) frag(
          div(cls := "row g-2 mb-3",
            div(cls := "col-4",
              div(cls := "card-am p-2 text-center",
                div(cls := "fw-black text-white", style := "font-size:1.8rem;", total.toString),
                div(cls := "xx-small text-muted", "Totales")
              )
            ),
            div(cls := "col-4",
              div(cls := "card-am p-2 text-center",
                div(cls := "fw-black text-success", style := "font-size:1.8rem;", s"$pctParada%"),
                div(cls := "xx-small text-muted", "Parados")
              )
            ),
            div(cls := "col-4",
              div(cls := "card-am p-2 text-center",
                div(cls := "fw-black text-warning", style := "font-size:1.8rem;", s"$pctIntuicion%"),
                div(cls := "xx-small text-muted", "Intuición")
              )
            )
          ),

          // Desglose intuición
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small text-muted fw-bold mb-2", "ANÁLISIS DE INTUICIÓN"),
            div(cls := "small text-white mb-1",
              s"Adivinaste el lado $adivinados de $total veces ($pctIntuicion%)"),
            div(cls := "progress mb-2", style := "height:8px;",
              div(cls := "progress-bar bg-warning", style := s"width:$pctIntuicion%;")
            ),
            if (parConInt > 0)
              div(cls := "xx-small text-success",
                s"✅ $parConInt paradas con intuición correcta (te tiraste al lado correcto Y la paraste)")
            else span(),
            div(cls := "xx-small text-muted mt-2 fst-italic",
              if (pctIntuicion >= 60) "?? Buena lectura de penaltis. Confía en tu instinto."
              else if (pctIntuicion >= 40) "?? Intuición media. Estudia las tendencias del tiro."
              else "?? Trabajo de análisis de tendencias recomendado."
            )
          ),

          // Tendencias
          div(cls := "row g-2 mb-3",
            div(cls := "col-6",
              div(cls := "card-am p-3",
                div(cls := "xx-small text-muted fw-bold mb-2", "DÓNDE TIRAN"),
                frag(Seq(("Izquierda", tirIzq), ("Centro", tirCen), ("Derecha", tirDer)).map { case (lbl, n) =>
                  div(cls := "mb-2",
                    div(cls := "d-flex justify-content-between xx-small mb-1",
                      span(cls := "text-white", lbl),
                      span(cls := "text-danger fw-bold", n.toString)
                    ),
                    div(cls := "progress", style := "height:6px;",
                      div(cls := "progress-bar bg-danger", style := s"width:${barWidth(n, total)};")
                    )
                  )
                }: _*)
              )
            ),
            div(cls := "col-6",
              div(cls := "card-am p-3",
                div(cls := "xx-small text-muted fw-bold mb-2", "DÓNDE TE TIRAS"),
                frag(Seq(("Izquierda", estIzq), ("Centro", estCen), ("Derecha", estDer)).map { case (lbl, n) =>
                  div(cls := "mb-2",
                    div(cls := "d-flex justify-content-between xx-small mb-1",
                      span(cls := "text-white", lbl),
                      span(cls := "text-primary fw-bold", n.toString)
                    ),
                    div(cls := "progress", style := "height:6px;",
                      div(cls := "progress-bar bg-primary", style := s"width:${barWidth(n, total)};")
                    )
                  )
                }: _*)
              )
            )
          )
        ) else div(),

        // Formulario registro
        div(cls := "card-am p-3 mb-3",
          div(cls := "fw-bold small text-white mb-3", "➕ Registrar penalti"),
          form(action := "/am/penalties/save", method := "post",

            div(cls := "row g-2 mb-3",
              div(cls := "col-8",
                label(cls := "xx-small text-muted fw-bold", "RIVAL"),
                input(tpe := "text", name := "rival", cls := "form-control bg-dark text-white border-secondary mt-1",
                  placeholder := "Nombre del tirador (opcional)")
              ),
              div(cls := "col-4",
                label(cls := "xx-small text-muted fw-bold", "FECHA"),
                input(tpe := "date", name := "fecha", cls := "form-control bg-dark text-white border-secondary mt-1",
                  value := java.time.LocalDate.now().toString)
              )
            ),

            // Dirección del tiro
            div(cls := "mb-3",
              div(cls := "xx-small text-muted fw-bold mb-2", "?? DIRECCIÓN DEL TIRO"),
              div(cls := "row g-2",
                frag(Seq("Izquierda", "Centro", "Derecha").map { d =>
                  div(cls := "col-4",
                    label(cls := "d-block",
                      input(tpe := "radio", name := "dirTiro", value := d, cls := "d-none", required := true),
                      div(cls := "btn-dir text-center", id := s"tiro_$d",
                        attr("onclick") := s"selectDir('tiro','$d')",
                        if (d == "Izquierda") "← Izq" else if (d == "Derecha") "Der →" else "● Cen"
                      )
                    )
                  )
                }: _*)
              )
            ),

            // Dirección de la estirada
            div(cls := "mb-3",
              div(cls := "xx-small text-muted fw-bold mb-2", "?? ¿DÓNDE TE TIRASTE?"),
              div(cls := "row g-2",
                frag(Seq("Izquierda", "Centro", "Derecha").map { d =>
                  div(cls := "col-4",
                    label(cls := "d-block",
                      input(tpe := "radio", name := "dirEstirada", value := d, cls := "d-none", required := true),
                      div(cls := "btn-dir text-center", id := s"est_$d",
                        attr("onclick") := s"selectDir('est','$d')",
                        if (d == "Izquierda") "← Izq" else if (d == "Derecha") "Der →" else "● Cen"
                      )
                    )
                  )
                }: _*)
              )
            ),

            // Resultado
            div(cls := "mb-3",
              div(cls := "xx-small text-muted fw-bold mb-2", "RESULTADO"),
              div(cls := "d-flex gap-2",
                frag(Seq(("true", "✅ Parada"), ("false", "❌ Gol")).map { case (v, lbl) =>
                  label(cls := "flex-fill text-center border border-secondary rounded p-2 xx-small fw-bold",
                    style := "cursor:pointer; background:#1a1a1a;",
                    input(tpe := "radio", name := "parada", value := v, cls := "d-none",
                      if (v == "false") attr("checked") := "checked" else span()),
                    span(lbl)
                  )
                }: _*)
              )
            ),

            button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "Guardar penalti")
          )
        ),

        // Historial de penaltis
        if (penalties.nonEmpty)
          div(cls := "card-am p-3",
            div(cls := "xx-small text-muted fw-bold mb-2", "HISTORIAL"),
            frag(penalties.take(20).map { p =>
              val intuicion = p.direccionTiro == p.direccionEstirada
              val icono = if (p.parada) "✅" else if (intuicion) "??" else "❌"
              div(cls := "d-flex align-items-center gap-2 py-2",
                style := "border-bottom:1px solid #1e1e1e;",
                span(style := "font-size:18px;", icono),
                div(cls := "flex-fill",
                  div(cls := "xx-small text-white",
                    if (p.rival.nonEmpty) p.rival else "Sin rival"),
                  div(cls := "xx-small text-muted", p.fecha)
                ),
                div(cls := "text-end xx-small",
                  div(cls := "text-danger", s"Tiro: ${p.direccionTiro}"),
                  div(cls := "text-primary", s"Estirada: ${p.direccionEstirada}")
                ),
                form(action := "/am/penalties/delete", method := "post", cls := "ms-1",
                  input(tpe := "hidden", name := "penaltyId", value := p.id.toString),
                  button(tpe := "submit", cls := "btn btn-outline-secondary btn-sm xx-small", "✕")
                )
              )
            }: _*)
          )
        else div(),

        script(raw("""
          function selectDir(group, dir) {
            ['Izquierda','Centro','Derecha'].forEach(function(d) {
              var el = document.getElementById(group + '_' + d);
              if (el) {
                el.classList.remove('selected-tiro', 'selected-estirada');
                if (d === dir) el.classList.add(group === 'tiro' ? 'selected-tiro' : 'selected-estirada');
              }
              // también marcar el radio
              var radio = document.querySelector('input[name="' + (group === 'tiro' ? 'dirTiro' : 'dirEstirada') + '"][value="' + d + '"]');
              if (radio) radio.checked = (d === dir);
            });
          }
          // visual para radio parada
          document.querySelectorAll('input[name="parada"]').forEach(function(r) {
            r.addEventListener('change', function() {
              document.querySelectorAll('input[name="parada"]').forEach(function(x) {
                x.parentElement.style.background = '#1a1a1a';
              });
              this.parentElement.style.background =
                this.value === 'true' ? 'rgba(40,167,69,0.2)' : 'rgba(220,53,69,0.2)';
            });
          });
        """))
      )
    )
  }

  @cask.postForm("/am/penalties/save")
  def savePenalty(request: cask.Request, rival: String = "", fecha: String = "",
                  dirTiro: String, dirEstirada: String, parada: String = "false") =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.savePenalty(
        userId       = user.id,
        fecha        = fecha,
        rival        = rival,
        dirTiro      = dirTiro,
        dirEstirada  = dirEstirada,
        parada       = parada == "true",
        matchId      = None,
        notas        = ""
      )
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/penalties"))
    }

  @cask.postForm("/am/penalties/delete")
  def deletePenalty(request: cask.Request, penaltyId: Int) =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.deletePenalty(penaltyId, user.id)
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/penalties"))
    }

  // ── GEAR ───────────────────────────────────────────────────────────────────
  @cask.get("/am/gear")
  def gearPage(request: cask.Request) = withAmAuth(request) { user =>
    val items = AmateurDatabaseManager.getGear(user.id)

    val tiposLatex = Seq("Garra", "Flat", "Roll Finger", "Negative Cut", "Hybrid", "Otro")
    val tiposCorte = Seq("Roll", "Flat", "Negative", "Hybrid", "Gunn Cut", "Otro")

    def estadoColor(usos: Int) =
      if (usos < 15) "success" else if (usos < 30) "warning" else "danger"
    def estadoLabel(usos: Int) =
      if (usos < 15) "Nuevo" else if (usos < 30) "En uso" else "Desgastado"

    renderAm("gear", user.nombre,
      div(
        h5(cls := "fw-black text-white mb-3", "?? Mis Guantes"),

        // Lista
        if (items.nonEmpty)
          div(cls := "mb-3",
            frag(items.map { g =>
              val c = estadoColor(g.partidosUsados)
              div(cls := s"card-am p-3 mb-2 border-start border-$c border-3",
                div(cls := "d-flex justify-content-between align-items-start",
                  div(
                    div(cls := "fw-bold text-white small", g.nombre),
                    if (g.marca.nonEmpty) div(cls := "xx-small text-muted", g.marca) else span(),
                    div(cls := "xx-small text-muted mt-1",
                      if (g.tipoLatex.nonEmpty) s"${g.tipoLatex}" else "",
                      if (g.corte.nonEmpty) s" · Corte: ${g.corte}" else ""
                    )
                  ),
                  div(cls := "text-end",
                    span(cls := s"badge bg-$c bg-opacity-25 text-$c small fw-bold",
                      s"${g.partidosUsados} PJ"),
                    div(cls := "xx-small text-muted mt-1", estadoLabel(g.partidosUsados)),
                    if (!g.activo)
                      div(cls := "badge bg-secondary xx-small mt-1", "Retirado")
                    else span()
                  )
                ),
                div(cls := "d-flex gap-2 mt-2",
                  form(action := "/am/gear/use", method := "post",
                    input(tpe := "hidden", name := "gearId", value := g.id.toString),
                    button(tpe := "submit", cls := "btn btn-outline-success btn-sm xx-small fw-bold",
                      "+ Uso")
                  ),
                  form(action := "/am/gear/toggle", method := "post",
                    input(tpe := "hidden", name := "gearId", value := g.id.toString),
                    button(tpe := "submit",
                      cls := s"btn btn-outline-secondary btn-sm xx-small",
                      if (g.activo) "Retirar" else "Activar")
                  )
                ),
                if (g.notas.nonEmpty)
                  div(cls := "xx-small text-muted mt-2 fst-italic", g.notas)
                else span()
              )
            }: _*)
          )
        else
          div(cls := "card-am p-4 text-center mb-3",
            div(style := "font-size:40px; opacity:0.3", "??"),
            p(cls := "text-muted mt-2 small", "Aún no has añadido guantes.")
          ),

        // Formulario añadir
        div(cls := "card-am p-3",
          div(cls := "fw-bold small text-white mb-3", "➕ Añadir guantes"),
          form(action := "/am/gear/save", method := "post",
            div(cls := "mb-2",
              label(cls := "xx-small text-muted fw-bold", "NOMBRE / MODELO"),
              input(tpe := "text", name := "nombre", cls := "form-control bg-dark text-white border-secondary mt-1",
                placeholder := "Ej: Reusch Attrakt Gold", required := true)
            ),
            div(cls := "row g-2 mb-2",
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "MARCA"),
                input(tpe := "text", name := "marca", cls := "form-control bg-dark text-white border-secondary mt-1",
                  placeholder := "Reusch, Puma...")
              ),
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "TIPO LÁTEX"),
                select(name := "tipoLatex", cls := "form-select bg-dark text-white border-secondary mt-1",
                  frag(tiposLatex.map(t => option(value := t, t)): _*)
                )
              )
            ),
            div(cls := "row g-2 mb-2",
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "CORTE"),
                select(name := "corte", cls := "form-select bg-dark text-white border-secondary mt-1",
                  frag(tiposCorte.map(t => option(value := t, t)): _*)
                )
              )
            ),
            div(cls := "mb-2",
              label(cls := "xx-small text-muted fw-bold", "NOTAS (opcional)"),
              input(tpe := "text", name := "notas", cls := "form-control bg-dark text-white border-secondary mt-1",
                placeholder := "Para lluvia, para hierba...")
            ),
            button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold mt-1", "Guardar guantes")
          )
        )
      )
    )
  }

  @cask.postForm("/am/gear/save")
  def saveGear(request: cask.Request, nombre: String, marca: String = "",
               tipoLatex: String = "", corte: String = "", notas: String = "") =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.saveGear(user.id, nombre, marca, tipoLatex, corte, notas)
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/gear"))
    }

  @cask.postForm("/am/gear/use")
  def gearUse(request: cask.Request, gearId: Int) =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.incrementGearUsage(gearId, user.id)
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/gear"))
    }

  @cask.postForm("/am/gear/toggle")
  def gearToggle(request: cask.Request, gearId: Int) =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.toggleGearActive(gearId, user.id)
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/gear"))
    }

  // ── AGENDA / CALENDARIO ─────────────────────────────────────────────────────
  @cask.get("/am/calendar")
  def calendarPage(request: cask.Request, synced: String = "") = withAmAuth(request) { user =>
    val (leagueUrl, teamName) = AmateurDatabaseManager.getLeagueConfig(user.id)
    val today    = java.time.LocalDate.now()
    val year     = today.getYear
    val month    = today.getMonthValue
    val firstDay = java.time.LocalDate.of(year, month, 1)
    val lastDay  = firstDay.plusMonths(1).minusDays(1)
    val fromStr  = firstDay.toString
    val toStr    = lastDay.toString
    val monthName = firstDay.getMonth.getDisplayName(
      java.time.format.TextStyle.FULL, new java.util.Locale("es"))
    val schedules   = AmateurDatabaseManager.getScheduleRange(user.id, fromStr, toStr)
    val upcoming    = AmateurDatabaseManager.getUpcomingSchedule(user.id, 5)
    val schedByDate = schedules.groupBy(_.fecha)
    val startDow    = firstDay.getDayOfWeek.getValue
    val blancos     = startDow - 1
    val daysInMonth = lastDay.getDayOfMonth
    val todayStr    = today.toString

    renderAm("calendar", user.nombre,
      div(
        if (synced.nonEmpty)
          div(cls := "alert alert-success alert-sm py-2 px-3 mb-2 small fw-bold",
            style := "font-size:12px;",
            synced)
        else span(),

        div(cls := "d-flex justify-content-between align-items-center mb-3",
          h5(cls := "fw-black mb-0", s"$monthName $year"),
          div(cls := "d-flex gap-2",
            a(href := "/am/calendar/add", cls := "btn btn-primary btn-sm fw-bold", "+ Partido"),
            if (leagueUrl.nonEmpty)
              a(href := "/am/calendar/sync", cls := "btn btn-success btn-sm fw-bold",
                style := "font-size:11px;", "🔄 Sync liga")
            else
              a(href := "/am/league-config", cls := "btn btn-outline-success btn-sm fw-bold",
                style := "font-size:11px;", "⚙️ Config liga"),
            a(href := "#", cls := "btn btn-outline-danger btn-sm fw-bold",
              style := "font-size:11px;",
              attr("onclick") := "if(confirm('¿Borrar todos los partidos pendientes de la agenda?')) window.location='/am/calendar/clear'",
              "🗑")
          )
        ),
        div(cls := "card-am p-2 mb-3",
          div(style := "display:grid; grid-template-columns: repeat(7,1fr); gap:3px;",
            frag(Seq("L","M","X","J","V","S","D").map(d =>
              div(cls := "text-center xx-small text-muted fw-bold py-1", d)
            ): _*),
            frag((1 to blancos).map(_ => div()): _*),
            frag((1 to daysInMonth).map { d =>
              val dateStr = f"$year-$month%02d-$d%02d"
              val hasSched = schedByDate.contains(dateStr)
              val isToday  = dateStr == todayStr
              val dayCls   = "cal-day" + (if (isToday) " today" else if (hasSched) " has-schedule" else "")
              div(cls := dayCls,
                div(cls := "day-num", d.toString),
                if (hasSched)
                  frag(schedByDate(dateStr).map(s =>
                    div(cls := "xx-small", style := "overflow:hidden;white-space:nowrap;text-overflow:ellipsis;",
                      s.rival)
                  ): _*)
                else span()
              )
            }: _*)
          )
        ),
        div(cls := "d-flex gap-3 mb-3 xx-small",
          span(span(cls := "cal-dot", style := "background:#38a169;"), " Jugado"),
          span(span(cls := "cal-dot", style := "background:#d69e2e;"), " Programado"),
          span(span(cls := "cal-dot", style := "background:#0d6efd;"), " Hoy")
        ),
        if (upcoming.nonEmpty)
          div(cls := "card-am p-3",
            div(cls := "fw-bold small text-muted mb-2", "PROXIMOS PARTIDOS"),
            frag(upcoming.map { s =>
              val tipoColor = s.tipo match {
                case "TORNEO" => "#dc3545"; case "CUP" => "#6f42c1"; case _ => "#0d6efd"
              }
              div(cls := "d-flex align-items-center gap-2 py-2",
                style := "border-bottom:1px solid #e2e8f0;",
                div(style := s"width:4px;height:36px;background:$tipoColor;border-radius:2px;"),
                div(cls := "flex-fill",
                  div(cls := "fw-bold small", s.rival),
                  div(cls := "xx-small text-muted",
                    s.fecha,
                    if (s.hora.nonEmpty) s" - ${s.hora}" else "",
                    if (s.lugar.nonEmpty) s" - ${s.lugar}" else "")
                ),
                form(action := "/am/calendar/delete", method := "post",
                  input(tpe := "hidden", name := "scheduleId", value := s.id.toString),
                  button(tpe := "submit", cls := "btn btn-outline-danger btn-sm",
                    style := "font-size:10px;", "X")
                )
              )
            }: _*)
          )
        else
          div(cls := "card-am p-3 text-center", style := "border-style:dashed;",
            div(cls := "text-muted small", "Sin partidos programados"),
            a(href := "/am/calendar/add", cls := "btn btn-outline-primary btn-sm mt-2", "+ Añadir")
          )
      )
    )
  }

  @cask.get("/am/calendar/add")
  def calendarAddPage(request: cask.Request) = withAmAuth(request) { user =>
    val today = java.time.LocalDate.now().toString
    renderAm("calendar", user.nombre,
      div(
        h5(cls := "fw-black mb-3", "Añadir partido a la agenda"),
        div(cls := "card-am p-3",
          form(action := "/am/calendar/save", method := "post",
            div(cls := "mb-3",
              label(cls := "xx-small text-muted fw-bold", "RIVAL"),
              input(tpe := "text", name := "rival", cls := "form-control mt-1",
                placeholder := "Equipo rival", required := true)
            ),
            div(cls := "row g-2 mb-3",
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "FECHA"),
                input(tpe := "date", name := "fecha", cls := "form-control mt-1",
                  value := today, required := true)
              ),
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "HORA"),
                input(tpe := "time", name := "hora", cls := "form-control mt-1")
              )
            ),
            div(cls := "mb-3",
              label(cls := "xx-small text-muted fw-bold", "LUGAR"),
              input(tpe := "text", name := "lugar", cls := "form-control mt-1", placeholder := "Campo / pabellon")
            ),
            div(cls := "mb-3",
              label(cls := "xx-small text-muted fw-bold", "TIPO"),
              select(name := "tipo", cls := "form-select mt-1",
                option(value := "LIGA", "Liga"),
                option(value := "TORNEO", "Torneo"),
                option(value := "CUP", "Copa"),
                option(value := "AMISTOSO", "Amistoso")
              )
            ),
            div(cls := "mb-3",
              label(cls := "xx-small text-muted fw-bold", "NOTAS"),
              textarea(name := "notas", cls := "form-control mt-1", rows := "2")
            ),
            button(tpe := "submit", cls := "btn btn-primary fw-bold w-100", "Guardar")
          )
        )
      )
    )
  }

  @cask.post("/am/calendar/save")
  def calendarSave(request: cask.Request) = withAmAuth(request) { user =>
    val body   = new String(request.data.readAllBytes(), "UTF-8")
    val params = body.split("&").map { pair =>
      val p = pair.split("=", 2)
      java.net.URLDecoder.decode(p(0), "UTF-8") -> (if (p.length > 1) java.net.URLDecoder.decode(p(1), "UTF-8") else "")
    }.toMap
    AmateurDatabaseManager.saveSchedule(
      user.id,
      params.getOrElse("rival", ""),
      params.getOrElse("fecha", java.time.LocalDate.now().toString),
      params.getOrElse("hora", ""),
      params.getOrElse("lugar", ""),
      params.getOrElse("tipo", "LIGA"),
      params.getOrElse("notas", "")
    )
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/calendar"))
  }

  @cask.post("/am/calendar/delete")
  def calendarDelete(request: cask.Request) = withAmAuth(request) { user =>
    val body   = new String(request.data.readAllBytes(), "UTF-8")
    val params = body.split("&").map { pair =>
      val p = pair.split("=", 2)
      java.net.URLDecoder.decode(p(0), "UTF-8") -> (if (p.length > 1) java.net.URLDecoder.decode(p(1), "UTF-8") else "")
    }.toMap
    val sid = params.getOrElse("scheduleId", "0").toIntOption.getOrElse(0)
    AmateurDatabaseManager.deleteSchedule(sid, user.id)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/calendar"))
  }

  // ── INFORME PDF ──────────────────────────────────────────────────────────────
  @cask.get("/am/report")
  def reportPage(request: cask.Request) = withAmAuth(request) { user =>
    val data          = AmateurDatabaseManager.getReportData(user.id)
    val pj            = data.getOrElse("pj", 0).asInstanceOf[Int]
    val pjP           = data.getOrElse("pjPortero", 0).asInstanceOf[Int]
    val pjJ           = data.getOrElse("pjJugador", 0).asInstanceOf[Int]
    val notaM         = data.getOrElse("notaMedia", 0.0).asInstanceOf[Double]
    val notaP         = data.getOrElse("notaPortero", 0.0).asInstanceOf[Double]
    val notaJ         = data.getOrElse("notaJugador", 0.0).asInstanceOf[Double]
    val gcMedia       = data.getOrElse("gcMedia", 0.0).asInstanceOf[Double]
    val limpias       = data.getOrElse("limpias", 0).asInstanceOf[Int]
    val ganados       = data.getOrElse("ganados", 0).asInstanceOf[Int]
    val empatados     = data.getOrElse("empatados", 0).asInstanceOf[Int]
    val perdidos      = data.getOrElse("perdidos", 0).asInstanceOf[Int]
    val golesMarcados = data.getOrElse("golesMarcados", 0).asInstanceOf[Int]
    val asistencias   = data.getOrElse("asistencias", 0).asInstanceOf[Int]
    val totalPen      = data.getOrElse("totalPen", 0).asInstanceOf[Int]
    val paradasPen    = data.getOrElse("paradasPen", 0).asInstanceOf[Int]
    val historial     = data.getOrElse("historial", List.empty).asInstanceOf[List[Map[String, String]]]
    val nextMatch     = data.getOrElse("nextMatch", None).asInstanceOf[Option[Map[String, String]]]
    val pctLimpias    = if (pjP > 0) (limpias * 100) / pjP else 0
    val pctParadas    = if (totalPen > 0) (paradasPen * 100) / totalPen else 0
    val today         = java.time.LocalDate.now().toString

    val histRows = historial.map { m =>
      val nota   = m.getOrElse("nota","5").toDoubleOption.getOrElse(5.0)
      val notaCls = if(nota>=7)"nota-green" else if(nota>=5)"nota-yellow" else "nota-red"
      val esP    = m.getOrElse("posicion","Portero").startsWith("Portero")
      s"""<tr>
        <td>${m.getOrElse("fecha","")}</td>
        <td style="font-weight:600">${m.getOrElse("rival","")}</td>
        <td style="text-align:center">${m.getOrElse("res","")}</td>
        <td><span class="${if(esP) "badge-portero" else "badge-jugador"}">${m.getOrElse("posicion","Portero")}</span></td>
        <td style="text-align:center"><span class="nota-pill $notaCls">${m.getOrElse("nota","")}</span></td>
        <td style="text-align:center">${if(esP) "&mdash;" else m.getOrElse("goles","0")}</td>
        <td style="text-align:center">${if(esP) "&mdash;" else m.getOrElse("asist","0")}</td>
      </tr>"""
    }.mkString("\n")

    val nextHtml = nextMatch.map { nm =>
      s"""<div class="mb-3 p-2" style="border-left:4px solid #f59e0b;background:#fffbeb;border-radius:4px;">
        <div style="font-size:9px;font-weight:700;color:#92400e;">PROXIMO &middot; ${nm.getOrElse("tipo","LIGA")}</div>
        <div style="font-weight:900;font-size:1rem;">${nm.getOrElse("rival","")}</div>
        <div style="font-size:10px;color:#718096;">${nm.getOrElse("fecha","")}${if(nm.getOrElse("hora","").nonEmpty) " &middot; " + nm("hora") else ""}</div>
      </div>"""
    }.getOrElse("")

    val rolSection = if (pjJ > 0)
      s"""<div class="section-title">Desglose por posicion</div>
      <div class="row g-2 mb-2">
        <div class="col-6"><div class="stat-box" style="border-color:#bfdbfe;">
          <div style="font-size:9px;font-weight:700;color:#1e40af;margin-bottom:6px;">PORTERO</div>
          <div class="d-flex justify-content-around">
            <div><div class="stat-val text-primary">$pjP</div><div class="stat-lbl">Partidos</div></div>
            <div><div class="stat-val text-primary">${f"$notaP%.1f"}</div><div class="stat-lbl">Nota</div></div>
            <div><div class="stat-val text-success">$limpias</div><div class="stat-lbl">Limpias</div></div>
          </div></div></div>
        <div class="col-6"><div class="stat-box" style="border-color:#ddd6fe;">
          <div style="font-size:9px;font-weight:700;color:#5b21b6;margin-bottom:6px;">JUGADOR</div>
          <div class="d-flex justify-content-around">
            <div><div class="stat-val" style="color:#5b21b6;">$pjJ</div><div class="stat-lbl">Partidos</div></div>
            <div><div class="stat-val" style="color:#5b21b6;">${f"$notaJ%.1f"}</div><div class="stat-lbl">Nota</div></div>
            <div><div class="stat-val" style="color:#5b21b6;">$golesMarcados</div><div class="stat-lbl">Goles</div></div>
            <div><div class="stat-val" style="color:#5b21b6;">$asistencias</div><div class="stat-lbl">Asist.</div></div>
          </div></div></div>
      </div>"""
    else ""

    val penSection = if (totalPen > 0)
      s"""<div class="section-title">Penaltis</div>
      <div class="row g-2 mb-2">
        <div class="col-3"><div class="stat-box"><div class="stat-val">$totalPen</div><div class="stat-lbl">Total</div></div></div>
        <div class="col-3"><div class="stat-box"><div class="stat-val text-success">$paradasPen</div><div class="stat-lbl">Parados</div></div></div>
        <div class="col-3"><div class="stat-box"><div class="stat-val text-danger">${totalPen-paradasPen}</div><div class="stat-lbl">Encajados</div></div></div>
        <div class="col-3"><div class="stat-box"><div class="stat-val">$pctParadas%</div><div class="stat-lbl">% parada</div></div></div>
      </div>"""
    else ""

    val html =
      s"""<!DOCTYPE html><html><head><meta charset="utf-8"/>
<title>Informe Guardian Amateur - ${user.nombre}</title>
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"/>
<style>
@page{size:A4;margin:16mm}
body{font-family:'Segoe UI',sans-serif;background:#fff;color:#1a202c;font-size:12px}
@media screen{body{max-width:800px;margin:0 auto;padding:20px}}
@media print{.no-print{display:none!important}}
.stat-box{border:1px solid #e2e8f0;border-radius:8px;padding:10px 8px;text-align:center}
.stat-val{font-size:1.4rem;font-weight:900;line-height:1}
.stat-lbl{font-size:9px;color:#718096;text-transform:uppercase;margin-top:2px}
.section-title{font-size:10px;font-weight:800;letter-spacing:.08em;color:#718096;text-transform:uppercase;border-bottom:2px solid #e2e8f0;padding-bottom:4px;margin-bottom:10px;margin-top:14px}
.badge-portero{background:#dbeafe;color:#1e40af;border-radius:4px;padding:1px 6px;font-size:9px;font-weight:700}
.badge-jugador{background:#ede9fe;color:#5b21b6;border-radius:4px;padding:1px 6px;font-size:9px;font-weight:700}
table{border-collapse:collapse;width:100%;font-size:10px}
th{background:#f8fafc;font-weight:700;color:#4a5568;padding:5px 8px;border:1px solid #e2e8f0;text-align:left}
td{padding:4px 8px;border:1px solid #e2e8f0}
tr:nth-child(even){background:#f8fafc}
.nota-pill{border-radius:50%;width:24px;height:24px;display:inline-flex;align-items:center;justify-content:center;font-weight:900;font-size:10px}
.nota-green{background:#d1fae5;color:#065f46}
.nota-yellow{background:#fef3c7;color:#92400e}
.nota-red{background:#fee2e2;color:#991b1b}
</style></head>
<body>
<div class="no-print mb-3 d-flex gap-2">
  <button class="btn btn-primary fw-bold" onclick="window.print()">Imprimir / Guardar PDF</button>
  <a href="/am/dashboard" class="btn btn-outline-secondary">&larr; Volver</a>
</div>
<div class="d-flex align-items-center justify-content-between mb-3 pb-2" style="border-bottom:3px solid #0d6efd;">
  <div>
    <div style="font-size:1.2rem;font-weight:900;">🛡 GUARDIAN AMATEUR</div>
    <div style="font-size:10px;color:#718096;">Informe de rendimiento &mdash; ${user.nombre}</div>
  </div>
  <div class="text-end">
    <div style="font-size:9px;color:#718096;">$today</div>
    <div style="font-size:9px;color:#718096;">$pj partidos totales</div>
  </div>
</div>
$nextHtml
<div class="section-title">Estadisticas globales</div>
<div class="row g-2 mb-2">
  <div class="col"><div class="stat-box"><div class="stat-val text-primary">${f"$notaM%.1f"}</div><div class="stat-lbl">Nota media</div></div></div>
  <div class="col"><div class="stat-box"><div class="stat-val text-danger">${f"$gcMedia%.1f"}</div><div class="stat-lbl">GC/partido</div></div></div>
  <div class="col"><div class="stat-box"><div class="stat-val text-success">$limpias ($pctLimpias%)</div><div class="stat-lbl">Limpias</div></div></div>
  <div class="col"><div class="stat-box"><div class="stat-val">$ganados</div><div class="stat-lbl">Ganados</div></div></div>
  <div class="col"><div class="stat-box"><div class="stat-val text-warning">$empatados</div><div class="stat-lbl">Empates</div></div></div>
  <div class="col"><div class="stat-box"><div class="stat-val text-danger">$perdidos</div><div class="stat-lbl">Perdidos</div></div></div>
</div>
$rolSection
$penSection
<div class="section-title">Ultimos ${historial.size} partidos</div>
<table><thead><tr><th>Fecha</th><th>Rival</th><th>Resultado</th><th>Posicion</th><th>Nota</th><th>Goles</th><th>Asist.</th></tr></thead>
<tbody>$histRows</tbody></table>
<div class="mt-3 text-center" style="font-size:9px;color:#a0aec0;border-top:1px solid #e2e8f0;padding-top:8px;">Guardian Amateur &copy; $today</div>
</body></html>"""

    cask.Response(html.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  // ── FINALIZAR TEMPORADA ──────────────────────────────────────────────────
  @cask.get("/am/end-season")
  def endSeasonRoute(request: cask.Request) = withAmAuth(request) { user =>
    AmateurDatabaseManager.endSeason(user.id)
    cask.Response(Array.emptyByteArray, 302,
      headers = Seq("Location" -> "/am/dashboard"))
  }

  // ── MAPA DE GOLES ─────────────────────────────────────────────────────────
  @cask.get("/am/mapa-goles")
  def mapaGolesPage(request: cask.Request, tipo: String = "", rival: String = "") = withAmAuth(request) { user =>
    val heatmap    = if (rival.nonEmpty) AmateurDatabaseManager.getGoalHeatmapByRival(user.id, rival)
    else               AmateurDatabaseManager.getGoalHeatmap(user.id, tipo)
    val rivales    = AmateurDatabaseManager.getRivalesConGoles(user.id)
    val totalGoles = heatmap.values.sum

    val maxVal = { val m = heatmap.values.max; if (m > 0) m.toDouble else 1.0 }

    def cellColor(count: Int): String = {
      val i = count / maxVal
      if (i == 0)        "rgba(255,255,255,0.04)"
      else if (i < 0.25) "rgba(220,53,69,0.20)"
      else if (i < 0.50) "rgba(220,53,69,0.45)"
      else if (i < 0.75) "rgba(220,53,69,0.70)"
      else               "rgba(220,53,69,0.92)"
    }

    def cellLabel(z: String) = z match {
      case "TL" => "Arr Izq"; case "TC" => "Arr Cen"; case "TR" => "Arr Der"
      case "ML" => "Med Izq"; case "MC" => "Med Cen"; case "MR" => "Med Der"
      case "BL" => "Baj Izq"; case "BC" => "Baj Cen"; case "BR" => "Baj Der"
      case _ => z
    }

    val zonaRows = Seq(Seq("TL","TC","TR"), Seq("ML","MC","MR"), Seq("BL","BC","BR"))

    def renderCell(zone: String) = {
      val count = heatmap.getOrElse(zone, 0)
      val pct   = if (totalGoles > 0) (count * 100.0 / totalGoles).toInt else 0
      div(
        style := s"background:${cellColor(count)}; border:1px solid rgba(255,255,255,0.08); display:flex; flex-direction:column; align-items:center; justify-content:center; min-height:65px;",
        attr("title") := s"${cellLabel(zone)}: $count goles ($pct%)",
        if (count > 0) frag(
          div(cls := "fw-bold text-white", style := "font-size:18px; line-height:1;", count.toString),
          div(cls := "xx-small text-light", style := "opacity:.7;", s"$pct%")
        ) else frag(
          div(cls := "text-muted", style := "font-size:16px; opacity:.25;", "–")
        )
      )
    }

    val golsAlto  = Seq("TL","TC","TR").map(heatmap.getOrElse(_, 0)).sum
    val golsMedio = Seq("ML","MC","MR").map(heatmap.getOrElse(_, 0)).sum
    val golsBajo  = Seq("BL","BC","BR").map(heatmap.getOrElse(_, 0)).sum
    val golsIzq   = Seq("TL","ML","BL").map(heatmap.getOrElse(_, 0)).sum
    val golsCen   = Seq("TC","MC","BC").map(heatmap.getOrElse(_, 0)).sum
    val golsDer   = Seq("TR","MR","BR").map(heatmap.getOrElse(_, 0)).sum

    // Punto ciego: zona con más goles
    val puntoCiego = if (totalGoles > 0) {
      val worst = heatmap.maxBy(_._2)
      val wpct  = (worst._2 * 100.0 / totalGoles).toInt
      Some(cellLabel(worst._1).toUpperCase -> wpct)
    } else None

    val tituloFiltro = if (rival.nonEmpty) s"vs ${rival.toUpperCase}"
    else if (tipo.nonEmpty) tipo
    else "Todos los partidos"

    renderAm("goals", user.nombre,
      div(

        div(cls := "mb-3 d-flex justify-content-between align-items-center",
          div(
            h5(cls := "fw-black text-white mb-0", "🥅 Mapa de Goles"),
            span(cls := "text-muted small", s"$tituloFiltro — $totalGoles goles registrados")
          ),
          a(href := "/am/history", cls := "btn btn-outline-secondary btn-sm fw-bold xx-small", "← Historial")
        ),

        // Filtros
        div(cls := "card-am p-3 mb-3",
          div(cls := "xx-small fw-bold text-muted mb-2", "FILTRAR POR TIPO"),
          div(cls := "d-flex gap-2 flex-wrap mb-2",
            a(href := "/am/mapa-goles",
              cls := s"btn btn-sm fw-bold ${if (tipo.isEmpty && rival.isEmpty) "btn-danger" else "btn-outline-secondary"}",
              "TODOS"),
            Seq("LIGA","TORNEO","CUP","AMISTOSO").map { t =>
              a(href := s"/am/mapa-goles?tipo=$t",
                cls := s"btn btn-sm fw-bold ${if (tipo == t) "btn-danger" else "btn-outline-secondary"}",
                t)
            }
          ),
          if (rivales.nonEmpty)
            div(
              div(cls := "xx-small fw-bold text-muted mb-1 mt-2", "FILTRAR POR RIVAL"),
              div(cls := "d-flex gap-1 flex-wrap",
                rivales.take(8).map { r =>
                  a(href := s"/am/mapa-goles?rival=${java.net.URLEncoder.encode(r, "UTF-8")}",
                    cls := s"btn btn-sm fw-bold ${if (rival.toLowerCase == r.toLowerCase) "btn-warning" else "btn-outline-secondary"}",
                    style := "font-size:10px;",
                    if (r.length > 12) r.take(12) + "…" else r)
                }
              )
            )
          else span()
        ),

        if (totalGoles == 0)
          div(cls := "card-am p-4 text-center",
            div(style := "font-size:48px; opacity:.3;", "🥅"),
            h5(cls := "text-muted mt-3", "Sin goles registrados"),
            p(cls := "text-secondary small", "Los goles encajados solo se registran cuando juegas de portero")
          )
        else frag(

          // Portería heatmap
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small fw-bold text-muted text-center mb-2", "PORTERÍA — Vista frontal"),
            // Poste superior
            div(style := "height:5px; background:linear-gradient(90deg,#666,#bbb,#666); border-radius:3px; margin-bottom:2px;"),
            div(cls := "d-flex align-items-stretch",
              // Poste izq
              div(style := "width:5px; background:linear-gradient(180deg,#666,#bbb,#666); border-radius:3px; flex-shrink:0;"),
              // Grid 3x3
              div(style := "flex:1; display:grid; grid-template-columns:1fr 1fr 1fr; grid-template-rows:1fr 1fr 1fr; gap:2px; padding:2px;",
                zonaRows.flatten.map(renderCell)
              ),
              // Poste der
              div(style := "width:5px; background:linear-gradient(180deg,#666,#bbb,#666); border-radius:3px; flex-shrink:0;")
            ),
            // Línea de fondo
            div(style := "height:4px; background:rgba(255,255,255,.12); border-radius:2px; margin-top:2px;"),
            // Leyenda
            div(cls := "d-flex justify-content-center align-items-center gap-2 mt-2",
              span(cls := "xx-small text-muted", "0"),
              div(style := "width:80px; height:6px; border-radius:3px; background:linear-gradient(90deg,rgba(220,53,69,.05),rgba(220,53,69,.9));"),
              span(cls := "xx-small text-muted", s"${maxVal.toInt}")
            )
          ),

          // Stats por altura y lado
          div(cls := "row g-2 mb-3",
            div(cls := "col-6",
              div(cls := "card-am p-2",
                div(cls := "xx-small fw-bold text-muted mb-2 text-center", "POR ALTURA"),
                Seq(("Alto", golsAlto, "#dc3545"), ("Medio", golsMedio, "#ffc107"), ("Bajo", golsBajo, "#0dcaf0")).map {
                  case (lbl, n, color) =>
                    val p = if (totalGoles > 0) (n * 100.0 / totalGoles).toInt else 0
                    div(cls := "mb-1",
                      div(cls := "d-flex justify-content-between xx-small mb-1",
                        span(cls := "text-white", lbl),
                        span(style := s"color:$color; font-weight:700;", s"$n ($p%)")
                      ),
                      div(style := "height:6px; background:rgba(255,255,255,.08); border-radius:3px;",
                        div(style := s"height:6px; width:$p%; background:$color; border-radius:3px;")
                      )
                    )
                }
              )
            ),
            div(cls := "col-6",
              div(cls := "card-am p-2",
                div(cls := "xx-small fw-bold text-muted mb-2 text-center", "POR LADO"),
                Seq(("Izq", golsIzq, "#dc3545"), ("Centro", golsCen, "#ffc107"), ("Der", golsDer, "#0dcaf0")).map {
                  case (lbl, n, color) =>
                    val p = if (totalGoles > 0) (n * 100.0 / totalGoles).toInt else 0
                    div(cls := "mb-1",
                      div(cls := "d-flex justify-content-between xx-small mb-1",
                        span(cls := "text-white", lbl),
                        span(style := s"color:$color; font-weight:700;", s"$n ($p%)")
                      ),
                      div(style := "height:6px; background:rgba(255,255,255,.08); border-radius:3px;",
                        div(style := s"height:6px; width:$p%; background:$color; border-radius:3px;")
                      )
                    )
                }
              )
            )
          ),

          // Punto ciego
          puntoCiego.map { case (zona, wpct) =>
            div(cls := "card-am p-3 text-center",
              style := "border-top: 3px solid #dc3545;",
              div(cls := "xx-small fw-bold text-muted mb-1", "⚠️ PUNTO CIEGO"),
              div(cls := "fw-black text-danger", style := "font-size:1.4rem;", zona),
              div(cls := "xx-small text-white", s"$wpct% de tus goles encajados"),
              div(cls := "xx-small text-muted mt-1", "Trabaja el posicionamiento en esta zona")
            )
          }.getOrElse(span())
        )
      )
    )
  }

  // ── RIVALES H2H ────────────────────────────────────────────────────────────
  @cask.get("/am/rivals")
  def rivalsPage(request: cask.Request) = withAmAuth(request) { user =>
    val rivales = AmateurDatabaseManager.getRivalesList(user.id)

    renderAm("rivals", user.nombre,
      div(
        div(cls := "mb-3",
          h5(cls := "fw-black text-white mb-0", "⚔️ Historial de Rivales"),
          span(cls := "text-muted small", s"${rivales.size} equipos enfrentados")
        ),

        if (rivales.isEmpty)
          div(cls := "card-am p-4 text-center",
            div(style := "font-size:48px; opacity:.3;", "⚔️"),
            h5(cls := "text-muted mt-3", "Sin rivales aún"),
            p(cls := "text-secondary small", "Registra partidos para ver el historial por rival")
          )
        else frag(
          rivales.map { r =>
            val pj   = r("pj").toInt
            val g    = r("g").toInt
            val e    = r("e").toInt
            val p    = r("p").toInt
            val res  = r("resultado")
            val (resColor, resBadge) = res match {
              case "W" => ("#20c997", "DOMINAS")
              case "L" => ("#dc3545", "PIERDES")
              case _   => ("#ffc107", "IGUALADO")
            }
            a(href := s"/am/rivals/${java.net.URLEncoder.encode(r("rival"), "UTF-8")}",
              style := "text-decoration:none;",
              div(cls := "card-am p-3 mb-2",
                style := s"border-left: 3px solid $resColor;",
                div(cls := "d-flex justify-content-between align-items-start",
                  div(
                    div(cls := "fw-bold text-white", style := "font-size:.95rem;", r("rival")),
                    div(cls := "xx-small text-muted mt-1",
                      s"${pj} partidos · última vez ${r("ultimo").take(7)}")
                  ),
                  span(cls := "badge xx-small fw-bold",
                    style := s"background:${resColor}22; color:$resColor; border:1px solid ${resColor}55;",
                    resBadge)
                ),
                div(cls := "d-flex gap-3 mt-2",
                  div(cls := "text-center",
                    div(cls := "fw-black text-success", style := "font-size:1.2rem;", g.toString),
                    div(cls := "xx-small text-muted", "G")
                  ),
                  div(cls := "text-center",
                    div(cls := "fw-black text-warning", style := "font-size:1.2rem;", e.toString),
                    div(cls := "xx-small text-muted", "E")
                  ),
                  div(cls := "text-center",
                    div(cls := "fw-black text-danger", style := "font-size:1.2rem;", p.toString),
                    div(cls := "xx-small text-muted", "P")
                  ),
                  div(cls := "border-start border-secondary mx-1"),
                  div(cls := "text-center",
                    div(cls := "fw-black text-white", style := "font-size:1.2rem;", r("nota")),
                    div(cls := "xx-small text-muted", "Nota")
                  ),
                  div(cls := "text-center",
                    div(cls := "fw-black text-danger", style := "font-size:1.2rem;", r("gc")),
                    div(cls := "xx-small text-muted", "GC")
                  )
                )
              )
            )
          }: _*
        )
      )
    )
  }

  @cask.get("/am/rivals/:rivalName")
  def rivalDetailPage(request: cask.Request, rivalName: String) = withAmAuth(request) { user =>
    val rival = java.net.URLDecoder.decode(rivalName, "UTF-8")
    val d     = AmateurDatabaseManager.getRivalDetail(user.id, rival)

    val pj      = d("pj").asInstanceOf[Int]
    val g       = d("g").asInstanceOf[Int]
    val e       = d("e").asInstanceOf[Int]
    val p       = d("p").asInstanceOf[Int]
    val nota    = d("notaMedia").asInstanceOf[Double]
    val gcMedia = d("gcMedia").asInstanceOf[Double]
    val gmTotal = d("gmTotal").asInstanceOf[Int]
    val aTotal  = d("aTotal").asInstanceOf[Int]
    val limpias = d("limpias").asInstanceOf[Int]
    val partidos = d("partidos").asInstanceOf[List[Map[String, String]]]
    val notasTac = d("notasTacticas").asInstanceOf[List[String]]

    val resGeneral = if (g > p) ("DOMINAS", "#20c997") else if (g < p) ("PIERDES", "#dc3545") else ("IGUALADO", "#ffc107")
    val notaColor  = if (nota >= 7) "#20c997" else if (nota >= 5) "#ffc107" else "#dc3545"

    renderAm("rivals", user.nombre,
      div(
        // Header
        div(cls := "d-flex justify-content-between align-items-center mb-3",
          div(
            div(cls := "xx-small text-muted fw-bold", "HISTORIAL VS"),
            h5(cls := "fw-black text-white mb-0", rival.toUpperCase)
          ),
          a(href := "/am/rivals", cls := "btn btn-outline-secondary btn-sm xx-small fw-bold", "← Rivales")
        ),

        // Banner resultado general
        div(cls := "card-am p-3 mb-3 text-center",
          style := s"border-top: 3px solid ${resGeneral._2};",
          div(cls := "fw-black", style := s"font-size:1.1rem; color:${resGeneral._2};", resGeneral._1),
          div(cls := "fw-bold text-white mt-1", s"$g G — $e E — $p P  ·  $pj partidos")
        ),

        // KPIs
        div(cls := "row g-2 mb-3",
          div(cls := "col-4",
            div(cls := "card-am p-2 text-center",
              div(cls := "fw-black", style := s"font-size:1.6rem; color:$notaColor;", f"$nota%.1f"),
              div(cls := "xx-small text-muted", "Nota media")
            )
          ),
          div(cls := "col-4",
            div(cls := "card-am p-2 text-center",
              div(cls := "fw-black text-danger", style := "font-size:1.6rem;", f"$gcMedia%.1f"),
              div(cls := "xx-small text-muted", "GC/partido")
            )
          ),
          div(cls := "col-4",
            div(cls := "card-am p-2 text-center",
              div(cls := "fw-black text-success", style := "font-size:1.6rem;", limpias.toString),
              div(cls := "xx-small text-muted", "Limpias")
            )
          ),
          if (gmTotal > 0 || aTotal > 0)
            frag(
              div(cls := "col-6",
                div(cls := "card-am p-2 text-center",
                  div(cls := "fw-black text-info", style := "font-size:1.6rem;", gmTotal.toString),
                  div(cls := "xx-small text-muted", "Goles marcados")
                )
              ),
              div(cls := "col-6",
                div(cls := "card-am p-2 text-center",
                  div(cls := "fw-black text-info", style := "font-size:1.6rem;", aTotal.toString),
                  div(cls := "xx-small text-muted", "Asistencias")
                )
              )
            )
          else span()
        ),

        // Notas tácticas de partidos anteriores
        if (notasTac.nonEmpty)
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small fw-bold text-muted mb-2", "📝 NOTAS DE PARTIDOS ANTERIORES"),
            frag(notasTac.map { nota =>
              div(cls := "d-flex gap-2 py-2",
                style := "border-bottom:1px solid #1e1e1e;",
                div(style := "width:4px; background:#ffc107; border-radius:2px; flex-shrink:0;"),
                div(cls := "small text-white", style := "font-size:11px; line-height:1.4;", nota)
              )
            }: _*)
          )
        else span(),

        // Historial de partidos
        div(cls := "card-am p-3",
          div(cls := "xx-small fw-bold text-muted mb-2", s"TODOS LOS PARTIDOS ($pj)"),
          frag(partidos.map { m =>
            val resColor = m("res") match {
              case "G" => "#20c997"; case "P" => "#dc3545"; case _ => "#ffc107"
            }
            val esPortero = m("posicion") == "portero"
            div(cls := "d-flex align-items-center gap-2 py-2",
              style := "border-bottom:1px solid #1e1e1e;",
              div(cls := "fw-bold text-white", style := s"font-size:.85rem; color:$resColor; min-width:18px;",
                m("res")),
              div(cls := "fw-bold text-white", style := "font-size:.85rem; min-width:32px;",
                m("score")),
              div(cls := "flex-fill",
                div(cls := "xx-small text-muted", m("fecha")),
                div(cls := "xx-small",
                  style := "color:#888;",
                  if (esPortero) "Portero" else s"${m("posicion").capitalize} · ${m("campo")}"
                )
              ),
              div(cls := "text-center",
                div(cls := "fw-bold text-white xx-small", s"★${m("nota")}"),
                div(cls := "xx-small text-muted", m("local"))
              )
            )
          }: _*)
        )
      )
    )
  }

  // ── WELLNESS PRE-PARTIDO ──────────────────────────────────────────────────
  @cask.get("/am/wellness")
  def wellnessPage(request: cask.Request) = withAmAuth(request) { user =>
    val corr        = AmateurDatabaseManager.getWellnessCorrelation(user.id)
    val rows        = corr("rows").asInstanceOf[List[Map[String, Any]]]
    val lastW       = corr("lastWellness").asInstanceOf[Option[Map[String, Any]]]
    val avgHigh     = corr("avgNotaHigh").asInstanceOf[Double]
    val avgLow      = corr("avgNotaLow").asInstanceOf[Double]
    val nHigh       = corr("nHighSleep").asInstanceOf[Int]
    val nLow        = corr("nLowSleep").asInstanceOf[Int]
    val today       = java.time.LocalDate.now().toString
    val checkedToday = lastW.exists(_("fecha").asInstanceOf[String] == today)

    // Gemini insights (solo si hay datos suficientes)
    val insights: List[String] = if (rows.size >= 3) {
      val raw = AmateurDatabaseManager.callGeminiWellness(rows)
      if (raw.nonEmpty) raw.split("\n").map(_.trim).filter(_.nonEmpty).toList else List.empty
    } else List.empty

    renderAm("wellness", user.nombre,
      div(
        div(cls := "mb-3",
          h5(cls := "fw-black text-white mb-0", "🧠 Wellness"),
          span(cls := "text-muted small", "Bienestar pre-partido y correlación con rendimiento")
        ),

        // Check-in card
        div(cls := "card-am p-3 mb-3",
          style := s"border-top: 3px solid ${if (checkedToday) "#20c997" else "#ffc107"};",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            div(cls := "fw-bold text-white", "📋 Check-in de hoy"),
            if (checkedToday)
              span(cls := "badge", style := "background:#20c99733; color:#20c997; font-size:10px;", "✓ Registrado")
            else
              span(cls := "badge", style := "background:#ffc10733; color:#ffc107; font-size:10px;", "Pendiente")
          ),
          div(id := "wellness-form",
            // Sueño
            div(cls := "mb-3",
              div(cls := "xx-small fw-bold text-muted mb-2", "🌙 HORAS DE SUEÑO"),
              div(cls := "d-flex gap-2 flex-wrap",
                Seq(4,5,6,7,8,9,10).map { h =>
                  div(cls := "text-center",
                    input(tpe := "radio", name := "sueno", id := s"s$h", value := h.toString,
                      style := "display:none;",
                      attr("onchange") := "updateWellness()"),
                    label(attr("for") := s"s$h",
                      cls := "btn btn-sm fw-bold",
                      id := s"lbl_s$h",
                      style := "min-width:36px; font-size:12px;",
                      s"${h}h")
                  )
                }
              )
            ),
            // Energía
            div(cls := "mb-3",
              div(cls := "xx-small fw-bold text-muted mb-2", "⚡ NIVEL DE ENERGÍA"),
              div(cls := "d-flex gap-2",
                Seq((1,"😴"),(2,"😪"),(3,"😐"),(4,"😊"),(5,"🔥")).map { case (v, emoji) =>
                  div(cls := "text-center flex-fill",
                    input(tpe := "radio", name := "energia", id := s"e$v", value := v.toString,
                      style := "display:none;",
                      attr("onchange") := "updateWellness()"),
                    label(attr("for") := s"e$v",
                      cls := "btn w-100 fw-bold",
                      id := s"lbl_e$v",
                      style := "font-size:16px; padding:8px 4px;",
                      emoji)
                  )
                }
              )
            ),
            // Ánimo
            div(cls := "mb-3",
              div(cls := "xx-small fw-bold text-muted mb-2", "💭 ESTADO ANÍMICO"),
              div(cls := "d-flex gap-2",
                Seq((1,"😤"),(2,"😕"),(3,"😐"),(4,"🙂"),(5,"🤩")).map { case (v, emoji) =>
                  div(cls := "text-center flex-fill",
                    input(tpe := "radio", name := "animo", id := s"a$v", value := v.toString,
                      style := "display:none;",
                      attr("onchange") := "updateWellness()"),
                    label(attr("for") := s"a$v",
                      cls := "btn w-100 fw-bold",
                      id := s"lbl_a$v",
                      style := "font-size:16px; padding:8px 4px;",
                      emoji)
                  )
                }
              )
            ),
            // Nota rápida
            div(cls := "mb-3",
              div(cls := "xx-small fw-bold text-muted mb-1", "📝 NOTA RÁPIDA (opcional)"),
              input(tpe := "text", id := "w-notas", cls := "form-control bg-dark text-white border-secondary",
                style := "font-size:13px;",
                placeholder := "ej. cansado del entreno, resfrío leve...")
            ),
            button(tpe := "button", id := "btn-wellness",
              cls := "btn btn-warning w-100 fw-bold",
              attr("onclick") := "guardarWellness()",
              "💾 Guardar check-in")
          )
        ),

        // Correlación sueño-nota (si hay datos)
        if (nHigh + nLow >= 3)
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small fw-bold text-muted mb-2", "📊 CORRELACIÓN SUEÑO → RENDIMIENTO"),
            div(cls := "row g-2 mb-2",
              div(cls := "col-6",
                div(cls := "card-am p-2 text-center",
                  style := "border-top:2px solid #20c997;",
                  div(cls := "fw-black text-success", style := "font-size:1.5rem;",
                    f"$avgHigh%.1f"),
                  div(cls := "xx-small text-muted", s"Nota · ≥7h sueño"),
                  div(cls := "xx-small text-muted", s"($nHigh partidos)")
                )
              ),
              div(cls := "col-6",
                div(cls := "card-am p-2 text-center",
                  style := "border-top:2px solid #dc3545;",
                  div(cls := "fw-black text-danger", style := "font-size:1.5rem;",
                    f"$avgLow%.1f"),
                  div(cls := "xx-small text-muted", s"Nota · <7h sueño"),
                  div(cls := "xx-small text-muted", s"($nLow partidos)")
                )
              )
            ),

            // Insights Gemini
            if (insights.nonEmpty)
              div(cls := "mt-2",
                div(cls := "xx-small fw-bold text-muted mb-2", "✨ PATRONES DETECTADOS POR IA"),
                frag(insights.map { insight =>
                  div(cls := "d-flex gap-2 py-2",
                    style := "border-bottom:1px solid #1e1e1e;",
                    div(style := "width:3px; background:#a78bfa; border-radius:2px; flex-shrink:0;"),
                    div(cls := "small text-white", style := "font-size:11px; line-height:1.5;", insight)
                  )
                }: _*)
              )
            else span()
          )
        else
          div(cls := "card-am p-3 mb-3 text-center",
            style := "border-style:dashed; opacity:.6;",
            div(cls := "xx-small text-muted", "Registra wellness en al menos 3 días de partido"),
            div(cls := "xx-small text-muted", "para ver la correlación con tu rendimiento")
          ),

        // Historial reciente
        if (rows.nonEmpty)
          div(cls := "card-am p-3",
            div(cls := "xx-small fw-bold text-muted mb-2", "HISTORIAL (días con partido)"),
            frag(rows.take(8).map { r =>
              val nota  = r("nota").asInstanceOf[Double]
              val nc    = if (nota >= 7) "#20c997" else if (nota >= 5) "#ffc107" else "#dc3545"
              val sueno = r("sueno").asInstanceOf[Int]
              val en    = r("energia").asInstanceOf[Int]
              val an    = r("animo").asInstanceOf[Int]
              div(cls := "d-flex align-items-center gap-2 py-2",
                style := "border-bottom:1px solid #1e1e1e;",
                div(cls := "xx-small text-muted", style := "min-width:55px;",
                  r("fecha").asInstanceOf[String].take(10)),
                div(cls := "flex-fill d-flex gap-2",
                  span(cls := "xx-small text-info", s"🌙${sueno}h"),
                  span(cls := "xx-small text-warning", s"⚡$en"),
                  span(cls := "xx-small text-info", s"💭$an")
                ),
                div(cls := "fw-bold xx-small", style := s"color:$nc;", f"★$nota%.1f")
              )
            }: _*)
          )
        else span(),

        // JS
        script(raw("""
          function updateWellness() {
            ['s4','s5','s6','s7','s8','s9','s10'].forEach(function(id) {
              var el = document.getElementById('lbl_' + id);
              var inp = document.getElementById(id);
              if (el && inp) el.className = inp.checked
                ? 'btn btn-sm fw-bold btn-warning'
                : 'btn btn-sm fw-bold btn-outline-secondary';
            });
            [1,2,3,4,5].forEach(function(v) {
              ['e','a'].forEach(function(prefix) {
                var el  = document.getElementById('lbl_' + prefix + v);
                var inp = document.getElementById(prefix + v);
                if (el && inp) el.className = inp.checked
                  ? 'btn w-100 fw-bold btn-warning'
                  : 'btn w-100 fw-bold btn-outline-secondary';
              });
            });
          }
          function guardarWellness() {
            var sueno   = document.querySelector('input[name="sueno"]:checked');
            var energia = document.querySelector('input[name="energia"]:checked');
            var animo   = document.querySelector('input[name="animo"]:checked');
            if (!sueno || !energia || !animo) {
              alert('Completa los tres campos antes de guardar.');
              return;
            }
            var params = new URLSearchParams();
            params.append('sueno',   sueno.value);
            params.append('energia', energia.value);
            params.append('animo',   animo.value);
            params.append('notas',   document.getElementById('w-notas').value);
            document.getElementById('btn-wellness').disabled = true;
            document.getElementById('btn-wellness').textContent = 'Guardando...';
            fetch('/am/wellness/save', { method:'POST', body: params,
              headers: {'Content-Type':'application/x-www-form-urlencoded'} })
              .then(function(r) { if (r.ok) window.location.reload(); })
              .catch(function() {
                document.getElementById('btn-wellness').disabled = false;
                document.getElementById('btn-wellness').textContent = '💾 Guardar check-in';
              });
          }
        """))
      )
    )
  }

  @cask.postForm("/am/wellness/save")
  def wellnessSave(request: cask.Request,
                   sueno: String, energia: String, animo: String, notas: String = "") =
    withAmAuth(request) { user =>
      val today = java.time.LocalDate.now().toString
      AmateurDatabaseManager.saveWellness(
        user.id, today,
        try sueno.toInt   catch { case _: Exception => 0 },
        try energia.toInt catch { case _: Exception => 0 },
        try animo.toInt   catch { case _: Exception => 0 },
        notas
      )
      cask.Response(Array.emptyByteArray, 200)
    }

  // ── NLP CALENDARIO ────────────────────────────────────────────────────────
  @cask.get("/am/calendar/nlp")
  def calendarNlpPage(request: cask.Request) = withAmAuth(request) { user =>
    renderAm("calendar", user.nombre,
      div(
        div(cls := "mb-3 d-flex justify-content-between align-items-center",
          div(
            h5(cls := "fw-black text-white mb-0", "🔍 Carga de Calendario IA"),
            span(cls := "text-muted small", "Pega el texto de tu liga — Gemini extrae los partidos automáticamente")
          ),
          a(href := "/am/calendar", cls := "btn btn-outline-secondary btn-sm xx-small fw-bold", "← Agenda")
        ),

        // Instrucciones
        div(cls := "card-am p-3 mb-3",
          style := "border-left: 3px solid #7c3aed;",
          div(cls := "xx-small fw-bold text-muted mb-2", "📋 CÓMO USAR"),
          div(cls := "small text-white", style := "line-height:1.6;",
            "1. Opción rápida: copia la URL de la página de calendario de tu liga y pégala arriba"),
          div(cls := "small text-white", style := "line-height:1.6;",
            "2. Si la web bloquea el acceso automático, copia el texto manualmente (Ctrl+A, Ctrl+C) y pégalo"),
          div(cls := "small text-white", style := "line-height:1.6;",
            "3. Escribe el nombre exacto de tu equipo y pulsa Procesar"),
          div(cls := "small text-warning mt-2", style := "font-size:11px;",
            "⚠️ Solo se añaden partidos futuros — los ya jugados y los duplicados se ignoran")
        ),

        // Formulario
        div(cls := "card-am p-3 mb-3",
          // Nombre del equipo
          div(cls := "mb-3",
            label(cls := "xx-small fw-bold text-muted", "NOMBRE DE TU EQUIPO *"),
            input(tpe := "text", id := "team-name",
              cls := "form-control bg-dark text-white border-secondary mt-1",
              placeholder := "ej. MiniFlow FC",
              style := "font-size:13px;")
          ),
          // URL — modo automático
          div(cls := "mb-2",
            label(cls := "xx-small fw-bold text-muted", "URL DE LA LIGA (recomendado)"),
            input(tpe := "url", id := "league-url",
              cls := "form-control bg-dark text-white border-secondary mt-1",
              placeholder := "https://ligaelitefutbol.com/calendario/...",
              style := "font-size:13px;",
              attr("oninput") := "toggleInputMode()")
          ),
          // Separador
          div(cls := "d-flex align-items-center gap-2 my-2",
            div(style := "flex:1; height:1px; background:rgba(255,255,255,.1);"),
            span(cls := "xx-small text-muted", "o si la web bloquea el acceso"),
            div(style := "flex:1; height:1px; background:rgba(255,255,255,.1);")
          ),
          // Texto manual — fallback
          div(id := "manual-section",
            label(cls := "xx-small fw-bold text-muted", "PEGA EL TEXTO MANUALMENTE"),
            textarea(id := "league-text",
              cls := "form-control bg-dark text-white border-secondary mt-1",
              rows := "6",
              style := "font-size:12px; font-family:monospace;",
              placeholder := "Ctrl+A en la web de la liga, Ctrl+C, y pega aquí...")
          ),
          // Botón
          div(cls := "d-flex gap-2 mt-3",
            button(tpe := "button", id := "btn-nlp",
              cls := "btn btn-primary fw-bold flex-fill",
              attr("onclick") := "procesarCalendario()",
              "🤖 Procesar con IA"),
            a(href := "/am/calendar",
              cls := "btn btn-outline-secondary fw-bold",
              "Cancelar")
          )
        ),

        // Loading
        div(id := "nlp-loading", cls := "card-am p-3 text-center d-none",
          div(cls := "text-muted small", "⏳ Analizando con Gemini..."),
          div(cls := "text-muted", style := "font-size:11px; margin-top:4px;",
            "Esto puede tardar 5-10 segundos")
        ),

        // Resultado
        div(id := "nlp-result", cls := "d-none"),

        script(raw("""
          function toggleInputMode() {
            var url = document.getElementById('league-url').value.trim();
            var manual = document.getElementById('manual-section');
            manual.style.opacity = url ? '0.4' : '1';
            manual.querySelector('textarea').required = !url;
          }

          function procesarCalendario() {
            var team = document.getElementById('team-name').value.trim();
            var url  = document.getElementById('league-url').value.trim();
            var text = document.getElementById('league-text').value.trim();
            if (!team) { alert('Escribe el nombre de tu equipo.'); return; }
            if (!url && text.length < 50) { alert('Introduce una URL o pega el texto de la liga.'); return; }

            document.getElementById('btn-nlp').disabled = true;
            document.getElementById('btn-nlp').textContent = url ? '⏳ Descargando página...' : '⏳ Procesando...';
            document.getElementById('nlp-loading').classList.remove('d-none');
            document.getElementById('nlp-result').classList.add('d-none');

            var params = new URLSearchParams();
            params.append('teamName', team);
            params.append('url', url);
            params.append('texto', text);

            fetch('/am/calendar/nlp/process', {
              method: 'POST',
              body: params,
              headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
            })
            .then(r => r.json())
            .then(function(json) {
              document.getElementById('btn-nlp').disabled = false;
              document.getElementById('btn-nlp').textContent = '🤖 Procesar con IA';
              document.getElementById('nlp-loading').classList.add('d-none');
              var res = document.getElementById('nlp-result');
              res.classList.remove('d-none');

              if (json.ok) {
                var amenazasHtml = '';
                if (json.amenazas && json.amenazas.length > 0) {
                  amenazasHtml = '<div class="xx-small fw-bold text-muted mt-3 mb-1">⚠️ AMENAZAS DEL PRÓXIMO RIVAL</div>' +
                    json.amenazas.map(function(a) {
                      return '<span class="badge me-1" style="background:#dc354533;color:#dc3545;border:1px solid #dc354555;font-size:10px;">' + a + '</span>';
                    }).join('');
                }
                var proximoHtml = json.proximo ? '<div class="xx-small text-muted mt-1">Próximo rival detectado: <strong class="text-white">' + json.proximo + '</strong></div>' : '';
                res.innerHTML =
                  '<div class="card-am p-3" style="border-top:3px solid #20c997;">' +
                  '<div class="fw-bold text-success mb-1">✅ ' + json.inserted + ' partidos añadidos a tu agenda</div>' +
                  (json.skipped > 0 ? '<div class="xx-small text-muted">' + json.skipped + ' ya existían y se ignoraron</div>' : '') +
                  proximoHtml + amenazasHtml +
                  '<a href="/am/calendar" class="btn btn-success btn-sm fw-bold mt-3 w-100">Ver agenda actualizada →</a>' +
                  '</div>';
              } else {
                res.innerHTML =
                  '<div class="card-am p-3" style="border-top:3px solid #dc3545;">' +
                  '<div class="text-danger fw-bold">❌ Error al procesar</div>' +
                  '<div class="xx-small text-muted mt-1">' + (json.error || 'Error desconocido') + '</div>' +
                  '</div>';
              }
            })
            .catch(function(e) {
              document.getElementById('btn-nlp').disabled = false;
              document.getElementById('btn-nlp').textContent = '🤖 Procesar con IA';
              document.getElementById('nlp-loading').classList.add('d-none');
              document.getElementById('nlp-result').classList.remove('d-none');
              document.getElementById('nlp-result').innerHTML =
                '<div class="card-am p-3" style="border-top:3px solid #dc3545;">' +
                '<div class="text-danger">Error de red: ' + e.message + '</div></div>';
            });
          }
        """))
      )
    )
  }

  @cask.postForm("/am/calendar/nlp/process")
  def calendarNlpProcess(request: cask.Request, teamName: String,
                         texto: String = "", url: String = "") =
    withAmAuth(request) { user =>
      val result = AmateurDatabaseManager.processCalendarNLP(user.id, texto, teamName, url)
      val json = ujson.Obj(
        "ok"       -> result.getOrElse("ok", false).asInstanceOf[Boolean],
        "inserted" -> result.getOrElse("inserted", 0).asInstanceOf[Int],
        "skipped"  -> result.getOrElse("skipped", 0).asInstanceOf[Int],
        "total"    -> result.getOrElse("total", 0).asInstanceOf[Int],
        "proximo"  -> result.getOrElse("proximo", "").asInstanceOf[String],
        "amenazas" -> ujson.Arr.from(
          result.getOrElse("amenazas", List.empty).asInstanceOf[List[String]].map(ujson.Str(_))
        ),
        "error"    -> result.getOrElse("error", "").asInstanceOf[String]
      )
      cask.Response(
        ujson.write(json).getBytes("UTF-8"),
        headers = Seq("Content-Type" -> "application/json")
      )
    }

  // ── CONFIG LIGA + SYNC ────────────────────────────────────────────────────
  @cask.get("/am/league-config")
  def leagueConfigPage(request: cask.Request) = withAmAuth(request) { user =>
    val (currentUrl, currentTeam) = AmateurDatabaseManager.getLeagueConfig(user.id)
    renderAm("calendar", user.nombre,
      div(
        div(cls := "mb-3 d-flex justify-content-between align-items-center",
          div(
            h5(cls := "fw-black text-white mb-0", "⚙️ Configuración de Liga"),
            span(cls := "text-muted small", "Sincronización automática del calendario")
          ),
          a(href := "/am/calendar", cls := "btn btn-outline-secondary btn-sm xx-small fw-bold", "← Agenda")
        ),

        div(cls := "card-am p-3 mb-3",
          style := "border-left: 3px solid #20c997;",
          div(cls := "xx-small fw-bold text-muted mb-2", "💡 CÓMO FUNCIONA"),
          div(cls := "small text-white", style := "line-height:1.6;",
            "Configura una vez la URL de tu liga y el nombre de tu equipo."),
          div(cls := "small text-white", style := "line-height:1.6;",
            "Después, con el botón 🔄 Sync en la agenda, el sistema descarga automáticamente los partidos pendientes."),
          div(cls := "small text-warning mt-2", style := "font-size:11px;",
            "⚠️ Solo añade partidos futuros — los ya jugados y duplicados se ignoran")
        ),

        div(cls := "card-am p-3",
          div(cls := "mb-3",
            label(cls := "xx-small fw-bold text-muted", "URL DEL CALENDARIO DE TU LIGA *"),
            input(tpe := "url", id := "cfg-url",
              cls := "form-control bg-dark text-white border-secondary mt-1",
              value := currentUrl,
              placeholder := "https://ligaelitefutbol.com/calendario/grupo-a",
              style := "font-size:13px;")
          ),
          div(cls := "mb-3",
            label(cls := "xx-small fw-bold text-muted", "NOMBRE EXACTO DE TU EQUIPO *"),
            input(tpe := "text", id := "cfg-team",
              cls := "form-control bg-dark text-white border-secondary mt-1",
              value := currentTeam,
              placeholder := "MiniFlow FC",
              style := "font-size:13px;")
          ),
          div(id := "cfg-status"),
          div(cls := "d-flex gap-2",
            button(tpe := "button", cls := "btn btn-success fw-bold flex-fill",
              attr("onclick") := "guardarConfig()",
              "💾 Guardar configuración"),
            a(href := "/am/calendar", cls := "btn btn-outline-secondary fw-bold",
              "Cancelar")
          )
        ),

        script(raw("""
          function guardarConfig() {
            var url  = document.getElementById('cfg-url').value.trim();
            var team = document.getElementById('cfg-team').value.trim();
            if (!url || !team) { alert('Completa los dos campos.'); return; }
            var params = new URLSearchParams();
            params.append('leagueUrl', url);
            params.append('teamName', team);
            fetch('/am/league-config/save', { method:'POST', body: params,
              headers: {'Content-Type':'application/x-www-form-urlencoded'} })
              .then(function(r) {
                if (r.ok) window.location.href = '/am/calendar?synced=✅ Configuración guardada';
              });
          }
        """))
      )
    )
  }

  @cask.postForm("/am/league-config/save")
  def leagueConfigSave(request: cask.Request, leagueUrl: String, teamName: String) =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.saveLeagueConfig(user.id, leagueUrl, teamName)
      cask.Response(Array.emptyByteArray, 200)
    }

  @cask.get("/am/calendar/sync")
  def calendarSync(request: cask.Request) = withAmAuth(request) { user =>
    val result = AmateurDatabaseManager.syncCalendarFromConfig(user.id)
    val msg = if (result.getOrElse("ok", false).asInstanceOf[Boolean]) {
      val ins = result.getOrElse("inserted", 0).asInstanceOf[Int]
      val ski = result.getOrElse("skipped", 0).asInstanceOf[Int]
      val ame = result.getOrElse("amenazas", List.empty).asInstanceOf[List[String]]
      val ameStr = if (ame.nonEmpty) s" · Amenazas: ${ame.mkString(", ")}" else ""
      s"✅ $ins partidos añadidos, $ski ya existían$ameStr"
    } else {
      s"❌ ${result.getOrElse("error", "Error desconocido")}"
    }
    cask.Response(Array.emptyByteArray, 302,
      headers = Seq("Location" -> s"/am/calendar?synced=${java.net.URLEncoder.encode(msg, "UTF-8")}"))
  }

  @cask.get("/am/calendar/clear")
  def calendarClear(request: cask.Request) = withAmAuth(request) { user =>
    val n = AmateurDatabaseManager.clearSchedule(user.id)
    cask.Response(Array.emptyByteArray, 302,
      headers = Seq("Location" -> s"/am/calendar?synced=${java.net.URLEncoder.encode(s"🗑 Agenda vaciada ($n entradas borradas)", "UTF-8")}"))
  }

  // Redirect /am → /am/dashboard
  @cask.get("/am")
  def amRoot(request: cask.Request) =
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/dashboard"))

  // ── PROGRESIÓN Y TENDENCIAS ────────────────────────────────────────────────
  @cask.get("/am/progression")
  def progressionPage(request: cask.Request) = withAmAuth(request) { user =>
    val d = AmateurDatabaseManager.getProgressionData(user.id)

    val totalPartidos  = d("totalPartidos").asInstanceOf[Int]
    val tendencia      = d("tendencia").asInstanceOf[String]
    val tendenciaDelta = d("tendenciaDelta").asInstanceOf[Double]
    val labels         = d("labels").asInstanceOf[List[String]]
    val notas          = d("notas").asInstanceOf[List[Double]]
    val gcList         = d("gcList").asInstanceOf[List[Int]]
    val resultados     = d("resultados").asInstanceOf[List[String]]
    val mesList        = d("mesList").asInstanceOf[List[Map[String, Any]]]
    val mejorPartido   = d("mejorPartido").asInstanceOf[Option[Map[String, String]]]
    val peorPartido    = d("peorPartido").asInstanceOf[Option[Map[String, String]]]
    val racha          = d("racha").asInstanceOf[List[String]]

    val tendenciaColor = tendencia match {
      case "MEJORANDO"    => "#20c997"
      case "BAJANDO"      => "#dc3545"
      case "ESTABLE"      => "#ffc107"
      case _              => "#6c757d"
    }
    val tendenciaIcon = tendencia match {
      case "MEJORANDO"    => "↑"
      case "BAJANDO"      => "↓"
      case "ESTABLE"      => "→"
      case _              => "—"
    }
    val tendenciaLabel = tendencia match {
      case "MEJORANDO"    => "Mejorando"
      case "BAJANDO"      => "Bajando"
      case "ESTABLE"      => "Estable"
      case _              => "Pocos datos"
    }

    val labelsJson   = labels.map(l => s""""$l"""").mkString("[", ",", "]")
    val notasJson    = notas.map(n => f"$n%.1f").mkString("[", ",", "]")
    val gcJson       = gcList.mkString("[", ",", "]")

    def mesLabel(m: String): String = {
      val parts = m.split("-")
      if (parts.length == 2) {
        val mes = parts(1).toIntOption.getOrElse(0)
        val meses = Array("", "Ene", "Feb", "Mar", "Abr", "May", "Jun",
          "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
        if (mes >= 1 && mes <= 12) s"${meses(mes)} ${parts(0).takeRight(2)}" else m
      } else m
    }

    renderAm("progression", user.nombre,
      div(
        // Header
        div(cls := "mb-3",
          h5(cls := "fw-black text-white mb-0", "📈 Progresión"),
          span(cls := "text-muted small", s"$totalPartidos partidos registrados")
        ),

        if (totalPartidos == 0)
          div(cls := "card-am p-4 text-center",
            div(style := "font-size:48px; opacity:0.4", "📊"),
            h5(cls := "text-muted mt-3", "Sin datos aún"),
            p(cls := "text-secondary small", "Registra partidos para ver tu evolución."),
            a(href := "/am/match-center", cls := "btn btn-primary mt-2 fw-bold", "Registrar partido")
          )
        else frag(

          // Tendencia principal
          div(cls := "card-am p-3 mb-3",
            div(cls := "d-flex align-items-center gap-3",
              div(style := s"font-size:2.8rem; color:$tendenciaColor; font-weight:900; line-height:1;",
                tendenciaIcon),
              div(
                div(cls := "fw-black text-white", style := "font-size:1.2rem;", tendenciaLabel),
                div(cls := "xx-small text-muted",
                  if (tendencia != "POCOS_DATOS")
                    s"${if (tendenciaDelta >= 0) "+" else ""}${f"$tendenciaDelta%.2f"} puntos vs. 5 partidos anteriores"
                  else "Necesitas al menos 6 partidos para calcular tendencia"
                )
              )
            )
          ),

          // Racha actual (últimos 10)
          if (racha.nonEmpty)
            div(cls := "card-am p-3 mb-3",
              div(cls := "fw-bold small text-muted mb-2", "FORMA RECIENTE"),
              div(cls := "d-flex gap-1 flex-wrap",
                frag(racha.reverse.map { r =>
                  val (bg, txt) = r match {
                    case "W" => ("#20c997", "G")
                    case "D" => ("#ffc107", "E")
                    case _   => ("#dc3545", "P")
                  }
                  span(style := s"background:$bg; color:#000; font-weight:900; font-size:11px; width:26px; height:26px; display:inline-flex; align-items:center; justify-content:center; border-radius:4px;",
                    txt)
                }: _*)
              )
            )
          else span(),

          // Gráfico evolución nota
          if (notas.size >= 2)
            div(cls := "card-am p-3 mb-3",
              div(cls := "fw-bold small text-muted mb-2", "EVOLUCIÓN DE NOTA"),
              div(style := "height:160px;",
                canvas(id := "chartNota")
              ),
              script(raw(s"""
                new Chart(document.getElementById('chartNota'), {
                  type: 'line',
                  data: {
                    labels: $labelsJson,
                    datasets: [{
                      label: 'Nota',
                      data: $notasJson,
                      borderColor: '#0d6efd',
                      backgroundColor: 'rgba(13,110,253,0.1)',
                      tension: 0.3,
                      fill: true,
                      pointRadius: ${if (notas.size > 20) "0" else "3"},
                      borderWidth: 2
                    }]
                  },
                  options: {
                    responsive: true, maintainAspectRatio: false,
                    plugins: { legend: { display: false } },
                    scales: {
                      y: { min: 0, max: 10, ticks: { color: '#888', stepSize: 2 }, grid: { color: 'rgba(255,255,255,0.05)' } },
                      x: { ticks: { color: '#888', maxTicksLimit: 8 }, grid: { display: false } }
                    }
                  }
                });
              """))
            )
          else span(),

          // Gráfico GC por partido
          if (gcList.size >= 2)
            div(cls := "card-am p-3 mb-3",
              div(cls := "fw-bold small text-muted mb-2", "GOLES ENCAJADOS POR PARTIDO"),
              div(style := "height:120px;",
                canvas(id := "chartGC")
              ),
              script(raw(s"""
                new Chart(document.getElementById('chartGC'), {
                  type: 'bar',
                  data: {
                    labels: $labelsJson,
                    datasets: [{
                      label: 'GC',
                      data: $gcJson,
                      backgroundColor: function(ctx) {
                        var v = ctx.raw;
                        return v === 0 ? '#20c997' : v <= 1 ? '#ffc107' : '#dc3545';
                      },
                      borderRadius: 3
                    }]
                  },
                  options: {
                    responsive: true, maintainAspectRatio: false,
                    plugins: { legend: { display: false } },
                    scales: {
                      y: { ticks: { color: '#888', stepSize: 1 }, grid: { color: 'rgba(255,255,255,0.05)' } },
                      x: { ticks: { color: '#888', maxTicksLimit: 8 }, grid: { display: false } }
                    }
                  }
                });
              """))
            )
          else span(),

          // Stats por mes
          if (mesList.nonEmpty)
            div(cls := "card-am p-3 mb-3",
              div(cls := "fw-bold small text-muted mb-2", "POR MES"),
              frag(mesList.map { mes =>
                val nota = mes("notaMedia").asInstanceOf[Double]
                val pj   = mes("pj").asInstanceOf[Int]
                val lim  = mes("limpias").asInstanceOf[Int]
                val gan  = mes("ganados").asInstanceOf[Int]
                val nc   = if (nota >= 7.0) "#20c997" else if (nota >= 5.0) "#ffc107" else "#dc3545"
                div(cls := "d-flex align-items-center gap-2 py-2",
                  style := "border-bottom:1px solid rgba(255,255,255,0.06);",
                  div(style := s"min-width:52px; font-size:11px; font-weight:700; color:$nc;",
                    mesLabel(mes("mes").asInstanceOf[String])),
                  div(cls := "flex-fill",
                    div(cls := "d-flex gap-2",
                      span(cls := "xx-small text-muted", s"$pj PJ"),
                      span(cls := "xx-small text-muted", s"$gan G"),
                      span(cls := "xx-small text-muted", s"$lim LP")
                    )
                  ),
                  div(style := s"font-size:1.3rem; font-weight:900; color:$nc;",
                    f"$nota%.1f")
                )
              }: _*)
            )
          else span(),

          // Mejor / peor partido
          div(cls := "row g-2 mb-3",
            mejorPartido.map { m =>
              div(cls := "col-6",
                div(cls := "card-am p-2 text-center",
                  style := "border-top: 3px solid #20c997;",
                  div(cls := "xx-small text-muted mb-1", "MEJOR"),
                  div(cls := "fw-black text-success", style := "font-size:1.5rem;", m("nota")),
                  div(cls := "xx-small text-white fw-bold", m("rival").take(14)),
                  div(cls := "xx-small text-muted", m("res"))
                )
              )
            }.getOrElse(span()),
            peorPartido.map { m =>
              div(cls := "col-6",
                div(cls := "card-am p-2 text-center",
                  style := "border-top: 3px solid #dc3545;",
                  div(cls := "xx-small text-muted mb-1", "PEOR"),
                  div(cls := "fw-black text-danger", style := "font-size:1.5rem;", m("nota")),
                  div(cls := "xx-small text-white fw-bold", m("rival").take(14)),
                  div(cls := "xx-small text-muted", m("res"))
                )
              )
            }.getOrElse(span())
          )
        ),

        script(src := "https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js")
      )
    )
  }

  initialize()
}