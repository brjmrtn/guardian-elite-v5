import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object AuthController extends cask.Routes {

  @cask.get("/login")
  def loginPage(error: Boolean = false, next: String = "/") = { // 1. Aceptamos el parámetro 'next'
    val content = basePage("login", div(cls:="container d-flex justify-content-center align-items-center", style:="height: 80vh",
      div(cls:="card bg-dark border-info p-4 shadow", style:="width: 350px",
        h3(cls:="text-info text-center mb-4", "🛡️ GUARDIAN LOGIN"),
        if(error) div(cls:="alert alert-danger small p-2", "Credenciales incorrectas") else "",
        form(action:="/login", method:="post",
          // 2. AÑADIMOS ESTA LÍNEA (Campo oculto para no perder la ruta):
          input(tpe:="hidden", name:="next", value:=next),

          div(cls:="mb-3",
            label(cls:="text-muted small fw-bold", "USUARIO"),
            input(tpe:="text", name:="user", cls:="form-control bg-dark text-white border-secondary", required:=true)
          ),
          div(cls:="mb-3",
            label(cls:="text-muted small fw-bold", "CONTRASEÑA"),
            input(tpe:="password", name:="pass", cls:="form-control bg-dark text-white border-secondary", required:=true)
          ),
          button(tpe:="submit", cls:="btn btn-info w-100 fw-bold", "ENTRAR AL SISTEMA")
        )
      )
    ))
    renderHtml(content)
  }

  @cask.postForm("/login")
  def doPostLogin(user: String, pass: String, next: String ="/") = {
    if (user == authUser && pass == authPass) {
      cask.Response(
        data = "Redirigiendo...",
        statusCode = 302,
        headers = Seq(
          "Location" -> next,
          // Aumentamos a 24h (86400) y añadimos SameSite=Lax para evitar bloqueos del navegador
          "Set-Cookie" -> s"$sessionCookieName=active; Path=/; SameSite=Lax; HttpOnly; Max-Age=86400"
        )
      )
    } else {
      cask.Redirect("/login?error=true")
    }
  }

  @cask.get("/logout")
  def doLogout() = {
    cask.Response(
      data = "",
      statusCode = 302,
      headers = Seq(
        "Location" -> "/login",
        "Set-Cookie" -> s"$sessionCookieName=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
      )
    )
  }

  initialize()
}