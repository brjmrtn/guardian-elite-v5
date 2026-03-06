import cask._
import scalatags.Text.all._
import scalatags.Text.tags2

// ─────────────────────────────────────────────────────────────────────────────
// GUARDIAN AMATEUR — Controller principal
// Todas las rutas bajo /am/*
// Auth por cookie independiente: am_session={userId}
// ─────────────────────────────────────────────────────────────────────────────
object AmateurController extends cask.Routes {

  private val AM_COOKIE = SharedLayout.sessionCookieName  // cookie unificada

  // ── AUTH HELPERS ───────────────────────────────────────────────────────────
  private def getAmUserId(request: cask.Request): Option[Int] = {
    val cookieVal = request.cookies.get(AM_COOKIE).map(_.value).getOrElse("")
    if (cookieVal.startsWith("am:"))
      scala.util.Try(cookieVal.drop(3).toInt).toOption
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
            background: #0d0d0d;
            color: #e0e0e0;
            font-family: 'Segoe UI', sans-serif;
            padding-bottom: 90px;
            min-height: 100vh;
          }
          /* ── RESET INPUTS: sobreescribir el CSS global del Elite ── */
          input, select, textarea,
          .form-control, .form-select {
            background-color: #1e1e1e !important;
            color: #ffffff !important;
            border: 1px solid #3a3a3a !important;
            font-weight: 500 !important;
          }
          input::placeholder, textarea::placeholder { color: #666 !important; opacity: 1; }
          option { background: #1e1e1e; color: #fff; }
          /* range track */
          input[type=range] { background: transparent !important; border: none !important; }
          /* date picker icon blanco en Chrome */
          input[type=date]::-webkit-calendar-picker-indicator { filter: invert(1); }
          /* checkbox */
          .form-check-input { background-color: #1e1e1e !important; border-color: #555 !important; }
          .form-check-input:checked { background-color: #0d6efd !important; border-color: #0d6efd !important; }

          .bottom-nav {
            position: fixed; bottom: 0; left: 0; right: 0;
            background: #111; border-top: 1px solid #2a2a2a;
            display: flex; z-index: 1000; padding-bottom: env(safe-area-inset-bottom);
          }
          .nav-item {
            flex: 1; text-align: center; padding: 8px 2px 6px;
            text-decoration: none; color: #666; font-size: 10px;
            display: flex; flex-direction: column; align-items: center;
          }
          .nav-item.active { color: #0d6efd; }
          .nav-item .nav-icon { font-size: 20px; display: block; margin-bottom: 2px; }
          .xx-small { font-size: 0.7rem; }
          .am-header {
            background: linear-gradient(135deg, #0d6efd22, #0d0d0d);
            border-bottom: 1px solid #1a3a6b;
            padding: 10px 16px;
            display: flex; align-items: center; justify-content: space-between;
            margin-bottom: 16px;
          }
          .btn-goal-zone {
            width: 100%; aspect-ratio: 1;
            font-size: 11px; font-weight: 700;
            border: 2px solid #333;
            background: #1a1a1a; color: #aaa;
            border-radius: 6px; cursor: pointer;
            transition: all 0.15s;
          }
          .btn-goal-zone.selected { background: #dc3545; color: white; border-color: #dc3545; }
          .btn-dir { width: 100%; padding: 16px 8px; font-weight: 700;
            font-size: 14px; border: 2px solid #333; background: #1a1a1a;
            color: #aaa; border-radius: 8px; cursor: pointer; transition: all 0.15s; }
          .btn-dir.selected-tiro     { background: #dc3545; color: white; border-color: #dc3545; }
          .btn-dir.selected-estirada { background: #0d6efd; color: white; border-color: #0d6efd; }
          .card-am { background: #141414; border: 1px solid #222; border-radius: 12px; }
          .nota-badge {
            width: 48px; height: 48px; border-radius: 50%;
            display: flex; align-items: center; justify-content: center;
            font-weight: 900; font-size: 16px;
          }
          .badge-green  { background: rgba(40,167,69,0.2);  color: #28a745; }
          .badge-yellow { background: rgba(255,193,7,0.2);  color: #ffc107; }
          .badge-red    { background: rgba(220,53,69,0.2);  color: #dc3545; }
          /* calendario */
          .cal-day {
            min-height: 56px; background: #141414; border: 1px solid #222;
            border-radius: 8px; padding: 4px 6px; font-size: 11px;
          }
          .cal-day.today { border-color: #0d6efd; }
          .cal-day.has-match { border-color: #28a745; background: #0d200f; }
          .cal-day.has-schedule { border-color: #ffc107; background: #1e1500; }
          .cal-day .day-num { font-weight: 700; font-size: 13px; }
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
          div(cls := "d-flex gap-2 align-items-center",
            a(href := "/profiles",
              cls := "btn btn-outline-warning btn-sm xx-small fw-bold",
              "👤 Cambiar"),
            a(href := "/logout", cls := "btn btn-outline-secondary btn-sm xx-small", "Salir")
          )
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
            span(cls := "nav-icon", "📋"), span("Historial"))
        ),

        script(src := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")
      )
    ).render

    cask.Response(
      page.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8")
    )
  }

  // ── REGISTRO AMATEUR (accesible desde /am/register) ───────────────────────
  // El login y logout se gestionan desde AuthController (login unificado)

  @cask.get("/am/register")
  def registerPage(request: cask.Request, error: String = "") = {
    val page = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tags2.title("Guardian Amateur - Registro"),
        link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        tags2.style(raw("body { background:#0d0d0d; color:#e0e0e0; }"))
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
              "Set-Cookie" -> s"${SharedLayout.sessionCookieName}=am:$id; Path=/; HttpOnly; SameSite=Lax; Max-Age=604800"
            ))
        case Left(err) =>
          cask.Response(Array.emptyByteArray, 302,
            headers = Seq("Location" -> s"/am/register?error=${java.net.URLEncoder.encode(err, "UTF-8")}"))
      }
    }
  }

  @cask.get("/am/logout")
  def doAmLogout(request: cask.Request) =
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location"   -> "/login",
      "Set-Cookie" -> s"${SharedLayout.sessionCookieName}=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
    ))

  // ── DASHBOARD ──────────────────────────────────────────────────────────────
  @cask.get("/am/dashboard")
  def dashboardPage(request: cask.Request) = withAmAuth(request) { user =>
    val st = AmateurDatabaseManager.getDashboardStats(user.id)
    val upcoming   = AmateurDatabaseManager.getUpcomingSchedule(user.id, 1)
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

          // Próximo partido programado
          upcoming.headOption.map { s =>
            div(cls := "card-am p-3 mb-3",
              style := "border-color:#ffc107;",
              div(cls := "xx-small text-warning fw-bold mb-1", "PRÓXIMO PARTIDO"),
              div(cls := "fw-black text-white", s.rival),
              div(cls := "xx-small text-muted",
                s"📅 ${s.fecha}",
                if (s.hora.nonEmpty) s" · ⏰ ${s.hora}" else "",
                if (s.lugar.nonEmpty) s" · 📍 ${s.lugar}" else ""
              ),
              a(href := s"/am/match-center", cls := "btn btn-warning btn-sm fw-bold mt-2 w-100",
                "⚽ Registrar este partido")
            )
          }.getOrElse(div()),

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

          // Resultados
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
              )
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
          div(cls := "row g-2",
            div(cls := "col-6",
              a(href := "/am/match-center", cls := "btn btn-primary w-100 fw-bold py-3",
                "⚽ Nuevo partido")),
            div(cls := "col-6",
              a(href := "/am/penalties", cls := "btn btn-outline-info w-100 fw-bold py-3",
                "🥅 Penalti"))
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
          div(cls := "card-am p-3 mb-3",
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
                  option(value := "Frio", "🥶 Frío"),
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
                frag(Seq(("", "— Sin especificar"), ("true", "🏠 Local"), ("false", "✈️ Visitante")).map {
                  case (v, lbl) =>
                    label(cls := "flex-fill text-center border border-secondary rounded p-2 xx-small fw-bold",
                      style := "cursor:pointer; background:#1a1a1a;",
                      input(tpe := "radio", name := "esLocal", value := v, cls := "d-none",
                        if (v == "") checked := true else span()),
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
      notas    = str("notas")
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
      case s if s.contains("lluv") => "🌧️"
      case s if s.contains("frio") => "🥶"
      case s if s.contains("vient")=> "💨"
      case _                       => "☁️"
    }

    renderAm("history", user.nombre,
      div(
        div(cls := "d-flex justify-content-between align-items-center mb-3",
          h5(cls := "fw-black text-white mb-0", "📋 Historial"),
          a(href := "/am/match-center", cls := "btn btn-primary btn-sm fw-bold", "+ Partido")
        ),

        if (matches.isEmpty)
          div(cls := "card-am p-4 text-center",
            div(style := "font-size:40px; opacity:0.3", "📋"),
            p(cls := "text-muted mt-3", "Aún no has registrado ningún partido.")
          )
        else
          frag(matches.map { m =>
            val gcStr   = if (m.gc == 0) "✅" else m.gc.toString
            val locStr  = m.esLocal match { case Some(true) => "🏠" case Some(false) => "✈️" case None => "" }
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
        h5(cls := "fw-black text-white mb-3", "🥅 Penaltis"),

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
              if (pctIntuicion >= 60) "🔥 Buena lectura de penaltis. Confía en tu instinto."
              else if (pctIntuicion >= 40) "📊 Intuición media. Estudia las tendencias del tiro."
              else "📉 Trabajo de análisis de tendencias recomendado."
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
              div(cls := "xx-small text-muted fw-bold mb-2", "🎯 DIRECCIÓN DEL TIRO"),
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
              div(cls := "xx-small text-muted fw-bold mb-2", "🧤 ¿DÓNDE TE TIRASTE?"),
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
                      if (v == "false") checked := true else span()),
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
              val icono = if (p.parada) "✅" else if (intuicion) "🤔" else "❌"
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
        h5(cls := "fw-black text-white mb-3", "🧤 Mis Guantes"),

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
            div(style := "font-size:40px; opacity:0.3", "🧤"),
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

  // ── CALENDARIO ─────────────────────────────────────────────────────────────
  @cask.get("/am/calendar")
  def calendarPage(request: cask.Request) = withAmAuth(request) { user =>
    import java.time.{LocalDate, YearMonth}
    import java.time.format.DateTimeFormatter

    val today      = LocalDate.now()
    val ym         = YearMonth.of(today.getYear, today.getMonthValue)
    val firstDay   = ym.atDay(1)
    val lastDay    = ym.atEndOfMonth()
    val fromStr    = firstDay.toString
    val toStr      = lastDay.toString

    val schedules  = AmateurDatabaseManager.getScheduleRange(user.id, fromStr, toStr)
    val matches    = AmateurDatabaseManager.getMatches(user.id)
      .filter(m => m.fecha >= fromStr && m.fecha <= toStr)
    val upcoming   = AmateurDatabaseManager.getUpcomingSchedule(user.id, 5)

    // Mapas fecha → eventos
    val schedByDate = schedules.groupBy(_.fecha)
    val matchByDate = matches.groupBy(_.fecha)

    val monthNames = Seq("","Enero","Febrero","Marzo","Abril","Mayo","Junio",
                         "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
    val dayNames   = Seq("L","M","X","J","V","S","D")
    val monthLabel = s"${monthNames(today.getMonthValue)} ${today.getYear}"

    // Primer día de semana (1=Lun ... 7=Dom)
    val startDow = firstDay.getDayOfWeek.getValue  // 1-7
    val blancos  = startDow - 1  // celdas vacías al inicio
    val totalDays = ym.lengthOfMonth()

    val tipoColor = Map("LIGA"->"#0d6efd","TORNEO"->"#6f42c1","AMISTOSO"->"#20c997","CUP"->"#fd7e14")

    renderAm("calendar", user.nombre,
      div(
        // Cabecera mes
        div(cls := "d-flex justify-content-between align-items-center mb-3",
          h5(cls := "fw-black text-white mb-0", s"📅 $monthLabel"),
          a(href := "/am/calendar/add", cls := "btn btn-primary btn-sm fw-bold", "+ Partido")
        ),

        // Próximos partidos
        if (upcoming.nonEmpty)
          div(cls := "card-am p-3 mb-3",
            div(cls := "xx-small text-muted fw-bold mb-2", "PRÓXIMOS PARTIDOS"),
            frag(upcoming.map { s =>
              val col = tipoColor.getOrElse(s.tipo, "#0d6efd")
              div(cls := "d-flex align-items-center gap-2 py-2",
                style := "border-bottom:1px solid #1e1e1e;",
                div(style := s"width:4px; height:36px; border-radius:2px; background:$col; flex-shrink:0;"),
                div(cls := "flex-fill",
                  div(cls := "fw-bold small text-white", s.rival),
                  div(cls := "xx-small text-muted",
                    s"${s.fecha}${if(s.hora.nonEmpty) " · "+s.hora else ""}${if(s.lugar.nonEmpty) " · "+s.lugar else ""}")
                ),
                span(cls := "badge xx-small", style := s"background:${col}33; color:$col;", s.tipo),
                form(action := "/am/calendar/delete", method := "post",
                  input(tpe := "hidden", name := "scheduleId", value := s.id.toString),
                  button(tpe := "submit", cls := "btn btn-outline-secondary btn-sm", style := "padding:2px 6px; font-size:11px;", "✕")
                )
              )
            }: _*)
          )
        else div(),

        // Grid del mes
        div(cls := "card-am p-3 mb-3",
          // Cabeceras días
          div(cls := "row g-1 mb-1",
            frag(dayNames.map { d =>
              div(cls := "col",
                div(cls := "text-center xx-small text-muted fw-bold", d))
            }: _*)
          ),
          // Celdas — 7 columnas con CSS grid
          div(style := "display:grid; grid-template-columns:repeat(7,1fr); gap:4px;",
            // Blancos iniciales
            frag((1 to blancos).map(_ => div()): _*),
            // Días del mes
            frag((1 to totalDays).map { d =>
              val dateStr  = LocalDate.of(today.getYear, today.getMonthValue, d).toString
              val isToday  = d == today.getDayOfMonth
              val hasSched = schedByDate.contains(dateStr)
              val hasMatch = matchByDate.contains(dateStr)
              val cls0 = "cal-day" +
                (if (isToday) " today" else "") +
                (if (hasMatch) " has-match" else if (hasSched) " has-schedule" else "")
              div(cls := cls0,
                div(cls := s"day-num ${if(isToday) "text-primary" else "text-white"}", d.toString),
                if (hasMatch)
                  frag(matchByDate(dateStr).map { m =>
                    div(cls := "xx-small text-success", style := "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                      s"✅ ${m.rival}")
                  }: _*)
                else if (hasSched)
                  frag(schedByDate(dateStr).map { s =>
                    val col = tipoColor.getOrElse(s.tipo, "#ffc107")
                    div(cls := "xx-small", style := s"color:$col; white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                      s"⚽ ${s.rival}")
                  }: _*)
                else div()
              )
            }: _*)
          )
        ),

        // Leyenda
        div(cls := "d-flex gap-3 xx-small text-muted mb-3",
          div(span(cls := "cal-dot", style := "background:#28a745;"), " Jugado"),
          div(span(cls := "cal-dot", style := "background:#ffc107;"), " Programado"),
          div(span(cls := "cal-dot", style := s"background:#0d6efd;"), " Hoy")
        )
      )
    )
  }

  @cask.get("/am/calendar/add")
  def calendarAddPage(request: cask.Request) = withAmAuth(request) { user =>
    renderAm("calendar", user.nombre,
      div(
        h5(cls := "fw-black text-white mb-3", "📅 Añadir partido"),
        div(cls := "card-am p-3",
          form(action := "/am/calendar/save", method := "post",
            div(cls := "mb-3",
              label(cls := "xx-small text-muted fw-bold", "RIVAL"),
              input(tpe := "text", name := "rival",
                cls := "form-control mt-1",
                placeholder := "Nombre del equipo rival", required := true)
            ),
            div(cls := "row g-2 mb-3",
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "FECHA"),
                input(tpe := "date", name := "fecha", cls := "form-control mt-1",
                  value := java.time.LocalDate.now().toString, required := true)
              ),
              div(cls := "col-6",
                label(cls := "xx-small text-muted fw-bold", "HORA (opcional)"),
                input(tpe := "time", name := "hora", cls := "form-control mt-1")
              )
            ),
            div(cls := "mb-3",
              label(cls := "xx-small text-muted fw-bold", "LUGAR / CAMPO"),
              input(tpe := "text", name := "lugar", cls := "form-control mt-1",
                placeholder := "Ej: Campo Municipal Norte")
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
              label(cls := "xx-small text-muted fw-bold", "NOTAS (opcional)"),
              input(tpe := "text", name := "notas", cls := "form-control mt-1",
                placeholder := "Árbitro, vestuario, instrucciones del míster...")
            ),
            button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold py-3", "GUARDAR"),
            div(cls := "text-center mt-3",
              a(href := "/am/calendar", cls := "text-muted small", "← Volver al calendario"))
          )
        )
      )
    )
  }

  @cask.postForm("/am/calendar/save")
  def saveSchedule(request: cask.Request, rival: String, fecha: String,
                   hora: String = "", lugar: String = "",
                   tipo: String = "LIGA", notas: String = "") =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.saveSchedule(user.id, rival, fecha, hora, lugar, tipo, notas)
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/calendar"))
    }

  @cask.postForm("/am/calendar/delete")
  def deleteSchedule(request: cask.Request, scheduleId: Int) =
    withAmAuth(request) { user =>
      AmateurDatabaseManager.deleteSchedule(scheduleId, user.id)
      cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/calendar"))
    }

  // Redirect /am → /am/dashboard
  @cask.get("/am")
  def amRoot(request: cask.Request) =
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/am/dashboard"))

  initialize()
}
