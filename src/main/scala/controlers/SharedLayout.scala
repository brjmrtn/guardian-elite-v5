import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

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

  def renderRedirect(url: String): cask.Response[Array[Byte]] =
    cask.Response(Array.empty[Byte], statusCode = 302,
      headers = Seq("Location" -> url, "Cache-Control" -> "no-store"))

  def renderMatchRow(m: MatchLog) = {
    val notaCls = if (m.nota >= 7) "table-success" else if (m.nota >= 5) "table-warning" else "table-danger"
    tr(cls := notaCls,
      td(m.fecha.take(10)),
      td(m.rival),
      td(m.resultado),
      td(cls := "text-center", m.paradas.toString),
      td(cls := "text-center fw-bold", m.nota.toString)
    )
  }

  // --- BASE PAGE ---
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
              span(id:="themeToggle", onclick:="toggleTheme()", style:="cursor:pointer; font-size:20px; user-select:none;", "☀️"),
              a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "⚙️")
            )
          ),
          div(cls := "container main-content", pageContents), tags2.nav(cls := "bottom-nav", a(href:="/", cls:=s"nav-item ${if(activeLink=="home") "active" else ""}", div(cls:="nav-icon", "H"), span(cls:="nav-label", "Inicio")), a(href:="/match-center", cls:=s"nav-item ${if(activeLink=="match-center") "active" else ""}", div(cls:="nav-icon", "P"), span(cls:="nav-label", "Jugar")), a(href:="/bio", cls:=s"nav-item ${if(activeLink=="bio") "active" else ""}", div(cls:="nav-icon", "B"), span(cls:="nav-label", "Bio")), a(href:="/bio/guantes", cls:=s"nav-item ${if(activeLink=="guantes") "active" else ""}", div(cls:="nav-icon", "🧤"), span(cls:="nav-label", "Guantes")), a(href:="/career/legacy", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon text-warning", "⭐"), span(cls:="nav-label text-warning", "Legado")), a(href:="/tactics", cls:=s"nav-item ${if(activeLink=="tactics") "active" else ""}", div(cls:="nav-icon", "ℹ️"), span(cls:="nav-label", "Pizarra")), a(href:="/career", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon", "T"), span(cls:="nav-label", "Trayect.")), a(href:="/tournament/bracket", cls:=s"nav-item ${if(activeLink=="bracket") "active" else ""}", div(cls:="nav-icon", "🏆"), span(cls:="nav-label", "Torneo")), a(href:="/history", cls:=s"nav-item ${if(activeLink=="history") "active" else ""}", div(cls:="nav-icon", "L"), span(cls:="nav-label", "Historial")), a(href:="/lesiones", cls:=s"nav-item ${if(activeLink=="lesiones") "active" else ""}", div(cls:="nav-icon", "🩹"), span(cls:="nav-label", "Lesiones")), a(href:="/flash-cards", cls:=s"nav-item ${if(activeLink=="flash-cards") "active" else ""}", div(cls:="nav-icon", "📋"), span(cls:="nav-label", "PrePartido")), a(href:="/gk-influence", cls:=s"nav-item ${if(activeLink=="gk-influence") "active" else ""}", div(cls:="nav-icon", "📡"), span(cls:="nav-label", "Influencia")), a(href:="/biomecanica", cls:=s"nav-item ${if(activeLink=="biomecanica") "active" else ""}", div(cls:="nav-icon", "🎯"), span(cls:="nav-label", "Biomecanica")), a(href:="/emocional", cls:=s"nav-item ${if(activeLink=="emocional") "active" else ""}", div(cls:="nav-icon", "🧠"), span(cls:="nav-label", "Emocional")), a(href:="/digital-twin", cls:=s"nav-item ${if(activeLink=="digital-twin") "active" else ""}", div(cls:="nav-icon", "🔮"), span(cls:="nav-label", "Twin")), a(href:="/moneyball", cls:=s"nav-item ${if(activeLink=="moneyball") "active" else ""}", div(cls:="nav-icon", "$"), span(cls:="nav-label", "Moneyball")), a(href:="/cognitive-reset", cls:=s"nav-item ${if(activeLink=="cognitive-reset") "active" else ""}", div(cls:="nav-icon", "🧩"), span(cls:="nav-label", "Reset")), a(href:="/red-zone", cls:=s"nav-item ${if(activeLink=="red-zone") "active" else ""}", div(cls:="nav-icon", "🔴"), span(cls:="nav-label", "RedZone")), a(href:="/psxg-delta", cls:=s"nav-item ${if(activeLink=="psxg-delta") "active" else ""}", div(cls:="nav-icon", "xG"), span(cls:="nav-label", "PSxG")), a(href:="/dojo", cls:=s"nav-item ${if(activeLink=="dojo") "active" else ""}", div(cls:="nav-icon", "🧠"), span(cls:="nav-label", "Dojo")), a(href:="/striker-clustering", cls:=s"nav-item ${if(activeLink=="striker-clustering") "active" else ""}", div(cls:="nav-icon", "⚔️"), span(cls:="nav-label", "Rivales")), a(href:="/scanning-rate", cls:=s"nav-item ${if(activeLink=="scanning-rate") "active" else ""}", div(cls:="nav-icon", "👁️"), span(cls:="nav-label", "Scanning"))))
        ,script(raw("""
        (function(){
          var t=localStorage.getItem('guardian_theme')||'dark';
          if(t==='light'){document.body.classList.add('light-mode');var btn=document.getElementById('themeToggle');if(btn)btn.textContent='🌙';}
        })();
        function toggleTheme(){
          var isLight=document.body.classList.toggle('light-mode');
          localStorage.setItem('guardian_theme', isLight?'light':'dark');
          var btn=document.getElementById('themeToggle');
          if(btn)btn.textContent=isLight?'🌙':'☀️';
        }
      """))
      ).render
  }

  def getCss() = """
    :root {
      --bg-main: #121212; --bg-card: #1e1e1e; --bg-nav: #1a1a1a;
      --text-main: #f0f0f0; --text-muted: #aaa; --border-col: #333;
      --input-bg: #2b2b2b; --input-color: #fff;
    }
    body.light-mode {
      --bg-main: #f5f5f5; --bg-card: #ffffff; --bg-nav: #ffffff;
      --text-main: #111; --text-muted: #666; --border-col: #ddd;
      --input-bg: #fff; --input-color: #111;
    }
    body { background-color: var(--bg-main); color: var(--text-main); font-family: 'Oswald', sans-serif; padding-bottom: 80px; margin: 0; font-weight: 500; transition: background 0.3s, color 0.3s; }
    body.light-mode .card, body.light-mode .bg-dark { background-color: #ffffff !important; color: #111 !important; }
    body.light-mode .text-muted { color: #666 !important; }
    body.light-mode .app-header, body.light-mode .bottom-nav { background: #ffffff !important; border-color: #ddd !important; }
    body.light-mode .nav-item { color: #555 !important; }
    body.light-mode .nav-item.active { color: #d4af37 !important; }
    body.light-mode .tm-table { background-color: #f9f9f9; }
    body.light-mode table.table-dark { --bs-table-bg: #f9f9f9; --bs-table-color: #111; }
    body.light-mode input, body.light-mode select, body.light-mode textarea,
    body.light-mode .form-control, body.light-mode .form-select {
      background-color: #fff !important; color: #111 !important; border-color: #ccc !important;
    }
    .theme-toggle-btn { position:fixed; top:12px; right:12px; z-index:2000; background:rgba(0,0,0,0.3); border:1px solid #444; border-radius:50%; width:36px; height:36px; display:flex; align-items:center; justify-content:center; cursor:pointer; font-size:18px; transition:all 0.2s; }
    .theme-toggle-btn:hover { background:rgba(212,175,55,0.3); }

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

/* VIDEOTECA */
.playlist-item { cursor: pointer; border: 1px solid transparent; transition: all 0.2s; background: rgba(255,255,255,0.03); }
.playlist-item:hover { background: rgba(212,175,55,0.1); border-color: rgba(212,175,55,0.3); }
.playlist-item.active { background: rgba(212,175,55,0.15); border-color: #d4af37 !important; }

/* PENALTIS HEATMAP */
.pen-heatmap-cell { min-height:60px; border-radius:3px; transition:transform 0.15s, box-shadow 0.15s; }
.pen-heatmap-cell:hover { transform:scale(1.08); box-shadow:0 0 10px rgba(220,53,69,0.6); z-index:2; position:relative; }

/* BRACKET TORNEO */
.bracket-match { transition: transform 0.15s; }
.bracket-match:hover { transform: translateX(3px); }

/* MAPA DE GOLES */
.goal-heatmap-cell {
  min-height: 70px;
  border-radius: 4px;
  transition: transform 0.15s, box-shadow 0.15s;
}
.goal-heatmap-cell:hover {
  transform: scale(1.05);
  box-shadow: 0 0 12px rgba(220,53,69,0.5);
  z-index: 2;
  position: relative;
}
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
  // /oracle -> redirect a /digital-twin (manejado tambien en CareerController)
  // Pagina completa eliminada para evitar duplicacion

  // /distribution -> redirect a /moneyball (manejado tambien en CareerController)
  // Pagina completa eliminada para evitar duplicacion

  def renderHtml(content: String, headers: Seq[(String, String)] = Nil): cask.Response[Array[Byte]] =
    cask.Response(content.getBytes("UTF-8"),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8") ++ headers)

  def redirect(url: String): cask.Response[Array[Byte]] =
    cask.Response(
      data       = Array.emptyByteArray,
      statusCode = 302,
      headers    = Seq("Location" -> url),
      cookies    = Seq.empty
    )

}