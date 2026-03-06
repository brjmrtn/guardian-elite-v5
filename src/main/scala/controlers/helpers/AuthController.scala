import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object AuthController extends cask.Routes {

  // ── LOGIN UNIFICADO ────────────────────────────────────────────────────────
  // Valida: (1) credenciales Elite desde env vars, (2) usuarios Amateur en BD.
  // Cookie resultante:
  //   guardian_session=elite     → modo Elite
  //   guardian_session=am:{id}   → modo Amateur, usuario {id}

  @cask.get("/login")
  def loginPage(request: cask.Request, error: String = "", next: String = "/"): cask.Response[Array[Byte]] = {
    val cookieVal = request.cookies.get(sessionCookieName).map(_.value).getOrElse("")
    if (cookieVal == "elite" || cookieVal == "active") return renderRedirect("/profiles")
    if (cookieVal.startsWith("am:"))                   return renderRedirect("/profiles")

    val pageHtml: String = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tag("title")("Guardian — Login"),
        link(rel := "stylesheet",
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        style(raw("""
          body { background: #0d0d0d; color: #e0e0e0; min-height: 100vh; }
        """))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center",
          style := "min-height:100vh;",
          div(style := "width:360px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:52px;", "🛡"),
              h3(cls := "fw-black mb-0",
                span(cls := "text-warning", "GUARDIAN"),
                span(cls := "text-white", " SYSTEM")
              ),
              span(cls := "text-muted small", "Introduce tus credenciales")
            ),
            if (error.nonEmpty) div(cls := "alert alert-danger small p-2 mb-3", error) else span(),
            div(cls := "card bg-dark border-secondary p-4 mb-3",
              form(action := "/login", method := "post",
                input(tpe := "hidden", name := "next", value := next),
                div(cls := "mb-2",
                  input(tpe := "text", name := "user",
                    cls := "form-control bg-dark text-white border-secondary",
                    placeholder := "Usuario", required := true,
                    attr("autocomplete") := "username")
                ),
                div(cls := "mb-3",
                  input(tpe := "password", name := "pass",
                    cls := "form-control bg-dark text-white border-secondary",
                    placeholder := "Contraseña", required := true)
                ),
                button(tpe := "submit",
                  cls := "btn btn-warning w-100 fw-black text-dark", "ENTRAR")
              )
            ),
            div(cls := "text-center",
              span(cls := "text-muted small", "¿Primera vez como portero? "),
              a(href := "/am/register", cls := "text-primary small fw-bold",
                "Crear cuenta →")
            )
          )
        )
      )
    ).render
    cask.Response(pageHtml.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.postForm("/login")
  def doPostLogin(request: cask.Request, user: String, pass: String, next: String = "/") = {
    if (user.trim == authUser && pass == authPass) {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location"   -> "/profiles",
        "Set-Cookie" -> s"$sessionCookieName=elite; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
      ))
    } else {
      AmateurDatabaseManager.authenticate(user, pass) match {
        case Some(amUser) =>
          cask.Response(Array.emptyByteArray, 302, headers = Seq(
            "Location"   -> "/profiles",
            "Set-Cookie" -> s"$sessionCookieName=am:${amUser.id}; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
          ))
        case None =>
          cask.Response(Array.emptyByteArray, 302, headers = Seq(
            "Location" -> s"/login?error=${java.net.URLEncoder.encode("Usuario o contraseña incorrectos", "UTF-8")}&next=$next"
          ))
      }
    }
  }

  // ── PÁGINA DE PERFILES ─────────────────────────────────────────────────────
  @cask.get("/profiles")
  def profilesPage(request: cask.Request): cask.Response[Array[Byte]] = {
    val cookieVal = request.cookies.get(sessionCookieName).map(_.value).getOrElse("")
    val isElite   = cookieVal == "elite" || cookieVal == "active"
    val amIdOpt   = if (cookieVal.startsWith("am:"))
                      scala.util.Try(cookieVal.drop(3).toInt).toOption
                    else None
    if (!isElite && amIdOpt.isEmpty) return renderRedirect("/login")

    val amUsers = AmateurDatabaseManager.listUsers()

    val pageHtml: String = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tag("title")("Guardian — Perfiles"),
        link(rel := "stylesheet",
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        style(raw("""
          body { background: #0d0d0d; color: #e0e0e0; min-height: 100vh; }
          .profile-card {
            display: flex; align-items: center; gap: 14px;
            background: #141414; border: 2px solid #222; border-radius: 14px;
            padding: 14px 18px; color: white; text-decoration: none;
            width: 100%; margin-bottom: 10px; transition: all 0.15s;
          }
          .profile-card:hover { color: white; background: #1a1a2e; border-color: #0d6efd; }
          .profile-card.elite-card:hover { border-color: #ffc107; background: #1e1800; }
          .profile-card.active-profile { border-color: #0d6efd; background: #0d1a2e; }
          .profile-card.elite-card.active-profile { border-color: #ffc107; background: #1e1800; }
          .avatar { width: 48px; height: 48px; border-radius: 50%;
            display: flex; align-items: center; justify-content: center;
            font-size: 22px; flex-shrink: 0; }
          .av-elite { background: rgba(255,193,7,0.15); }
          .av-am    { background: rgba(13,110,253,0.15); }
          .badge-on { font-size: 10px; padding: 2px 8px; border-radius: 10px; }
        """))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center",
          style := "min-height:100vh; padding: 24px 0;",
          div(style := "width:380px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:44px;", "👤"),
              h4(cls := "fw-black text-white mb-1", "Seleccionar perfil"),
              span(cls := "text-muted small", "¿Con quién quieres entrar?")
            ),

            // Elite
            a(href := "/switch/elite",
              cls := s"profile-card elite-card ${if (isElite) "active-profile" else ""}",
              div(cls := "avatar av-elite", "🛡"),
              div(cls := "flex-fill",
                div(cls := "fw-black text-warning", style := "font-size:15px;", "GUARDIAN ELITE"),
                div(cls := "text-muted", style := "font-size:11px;", "Perfil completo · Héctor")
              ),
              if (isElite)
                span(cls := "badge bg-warning text-dark badge-on", "activo")
              else
                span(cls := "text-muted", style := "font-size:20px;", "›")
            ),

            if (amUsers.nonEmpty)
              div(cls := "text-muted small text-center",
                style := "padding: 8px 0; border-top: 1px solid #222; margin: 4px 0 10px;",
                "Porteros Amateur")
            else span(),

            frag(amUsers.map { u =>
              val isActive = amIdOpt.contains(u.id)
              a(href := s"/switch/am/${u.id}",
                cls := s"profile-card ${if (isActive) "active-profile" else ""}",
                div(cls := "avatar av-am", "⚽"),
                div(cls := "flex-fill",
                  div(cls := "fw-bold text-white", style := "font-size:14px;", u.nombre),
                  div(cls := "text-muted", style := "font-size:11px;", s"@${u.username}")
                ),
                if (isActive)
                  span(cls := "badge bg-primary badge-on", "activo")
                else
                  span(cls := "text-muted", style := "font-size:20px;", "›")
              )
            }: _*),

            div(cls := "text-center mt-3 mb-4",
              a(href := "/am/register", cls := "btn btn-outline-secondary btn-sm",
                "+ Añadir portero amateur")
            ),
            div(cls := "text-center",
              a(href := "/logout", cls := "text-danger small", "Cerrar sesión")
            )
          )
        )
      )
    ).render

    cask.Response(pageHtml.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  // ── SWITCH DE PERFIL ───────────────────────────────────────────────────────
  @cask.get("/switch/elite")
  def switchToElite(request: cask.Request): cask.Response[Array[Byte]] = {
    val cookieVal = request.cookies.get(sessionCookieName).map(_.value).getOrElse("")
    if (cookieVal == "elite" || cookieVal == "active") return renderRedirect("/")
    if (cookieVal.startsWith("am:"))                   return renderRedirect("/reauth/elite")
    renderRedirect("/login")
  }

  @cask.get("/switch/am/:userId")
  def switchToAm(request: cask.Request, userId: Int): cask.Response[Array[Byte]] = {
    val cookieVal = request.cookies.get(sessionCookieName).map(_.value).getOrElse("")
    val isElite   = cookieVal == "elite" || cookieVal == "active"
    val isAmSelf  = cookieVal == s"am:$userId"
    if (isAmSelf) {
      renderRedirect("/am/dashboard")
    } else if (isElite) {
      // Elite puede entrar en cualquier cuenta Amateur sin contraseña
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location"   -> "/am/dashboard",
        "Set-Cookie" -> s"$sessionCookieName=am:$userId; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
      ))
    } else {
      renderRedirect(s"/reauth/am/$userId")
    }
  }

  // Reautenticación Elite (cuando viene de una cuenta Amateur)
  @cask.get("/reauth/elite")
  def reauthElitePage(request: cask.Request, error: String = ""): cask.Response[Array[Byte]] = {
    val pageHtml: String = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tag("title")("Acceso Elite"),
        link(rel := "stylesheet",
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        style(raw("body { background:#0d0d0d; color:#e0e0e0; }"))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center",
          style := "min-height:100vh;",
          div(style := "width:320px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:44px;", "🛡"),
              h5(cls := "fw-black text-warning", "Acceso a Guardian Elite"),
              p(cls := "text-muted small", "Contraseña de administrador")
            ),
            if (error.nonEmpty) div(cls := "alert alert-danger small p-2 mb-3", error) else span(),
            div(cls := "card bg-dark border-warning p-4",
              form(action := "/reauth/elite", method := "post",
                div(cls := "mb-3",
                  input(tpe := "password", name := "pass",
                    cls := "form-control bg-dark text-white border-secondary",
                    placeholder := "Contraseña Elite", required := true,
                    attr("autofocus") := "true")
                ),
                button(tpe := "submit", cls := "btn btn-warning w-100 fw-black text-dark", "ENTRAR"),
                div(cls := "text-center mt-3",
                  a(href := "/profiles", cls := "text-muted small", "← Volver"))
              )
            )
          )
        )
      )
    ).render
    cask.Response(pageHtml.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.postForm("/reauth/elite")
  def doReauthElite(request: cask.Request, pass: String) = {
    if (pass == authPass) {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location"   -> "/",
        "Set-Cookie" -> s"$sessionCookieName=elite; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
      ))
    } else {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location" -> s"/reauth/elite?error=${java.net.URLEncoder.encode("Contraseña incorrecta", "UTF-8")}"
      ))
    }
  }

  // Reautenticación para cambiar entre cuentas Amateur distintas
  @cask.get("/reauth/am/:userId")
  def reauthAmPage(request: cask.Request, userId: Int, error: String = ""): cask.Response[Array[Byte]] = {
    val nombre = AmateurDatabaseManager.getUserById(userId).map(_.nombre).getOrElse("portero")
    val pageHtml: String = "<!DOCTYPE html>" + html(lang := "es",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tag("title")(s"Entrar como $nombre"),
        link(rel := "stylesheet",
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
        style(raw("body { background:#0d0d0d; color:#e0e0e0; }"))
      ),
      body(
        div(cls := "container d-flex justify-content-center align-items-center",
          style := "min-height:100vh;",
          div(style := "width:320px;",
            div(cls := "text-center mb-4",
              div(style := "font-size:44px;", "⚽"),
              h5(cls := "fw-black text-primary", s"Entrar como $nombre"),
              p(cls := "text-muted small", "Introduce tu contraseña")
            ),
            if (error.nonEmpty) div(cls := "alert alert-danger small p-2 mb-3", error) else span(),
            div(cls := "card bg-dark border-primary p-4",
              form(action := s"/reauth/am/$userId", method := "post",
                div(cls := "mb-3",
                  input(tpe := "password", name := "pass",
                    cls := "form-control bg-dark text-white border-secondary",
                    placeholder := "Contraseña", required := true,
                    attr("autofocus") := "true")
                ),
                button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "ENTRAR"),
                div(cls := "text-center mt-3",
                  a(href := "/profiles", cls := "text-muted small", "← Volver"))
              )
            )
          )
        )
      )
    ).render
    cask.Response(pageHtml.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.postForm("/reauth/am/:userId")
  def doReauthAm(request: cask.Request, userId: Int, pass: String) = {
    if (AmateurDatabaseManager.checkPassword(userId, pass)) {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location"   -> "/am/dashboard",
        "Set-Cookie" -> s"$sessionCookieName=am:$userId; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
      ))
    } else {
      cask.Response(Array.emptyByteArray, 302, headers = Seq(
        "Location" -> s"/reauth/am/$userId?error=${java.net.URLEncoder.encode("Contraseña incorrecta", "UTF-8")}"
      ))
    }
  }

  // ── LOGOUT ─────────────────────────────────────────────────────────────────
  @cask.get("/logout")
  def doLogout(request: cask.Request) =
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location"   -> "/login",
      "Set-Cookie" -> s"$sessionCookieName=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
    ))

  initialize()
}
