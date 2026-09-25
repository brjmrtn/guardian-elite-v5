import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object SharedLayout {

  // --- Configuracion de seguridad (desde variables de entorno) ---
  val authUser          = sys.env.getOrElse("GUARDIAN_USER", "")
  val authPass          = sys.env.getOrElse("GUARDIAN_PASS", "")
  val sessionCookieName = "guardian_session"

  def withAuth(request: cask.Request)(block: => cask.Response[Array[Byte]]): cask.Response[Array[Byte]] = {
    val cookieVal = request.cookies.get(sessionCookieName).map(_.value).getOrElse("")
    // Acepta "elite" (nuevo) o "active" (retrocompat)
    val isAuthenticated = cookieVal == "elite" || cookieVal == "active"
    if (isAuthenticated) {
      block
    } else {
      val currentPath = request.exchange.getRequestPath
      val red = cask.Redirect(s"/login?next=$currentPath")
      cask.Response(Array.empty[Byte], red.statusCode, red.headers ++ Seq("Cache-Control" -> "no-store, no-cache, must-revalidate"), red.cookies)
    }
  }

  // BLOQUE H1: solo re-encodea si detecta secuencias concretas de corrupcion UTF-8 mal
  // interpretada como ISO-8859-1 — la condicion anterior (contains("A")) corrompia
  // cualquier texto correcto que tuviera una A mayuscula.
  def fixEncoding(s: String): String = {
    if (s == null || s.isEmpty) return s
    try {
      val indicadores = Seq("Ã±", "Ã¡", "Ã©", "Ã³", "Ã", "Ãº", "Ã¼", "Ã ", "Ã¨", "Ã¬", "Ã²", "Ã€")
      if (indicadores.exists(s.contains)) new String(s.getBytes("ISO-8859-1"), "UTF-8")
      else s
    } catch { case _: Exception => s }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B2 — SELECTOR DE TEMPORADA (paginas estacionales, Elite exclusivamente)
  // ─────────────────────────────────────────────────────────────────────────────
  def seasonSelector(temporadas: List[Map[String,Any]], seleccionadaId: Int, urlBase: String): Modifier =
    div(cls := "d-flex align-items-center gap-2 mb-3",
      span(cls := "text-muted small fw-bold", "📅 Temporada:"),
      select(
        cls := "form-select form-select-sm bg-dark text-white border-secondary",
        style := "width:auto; font-size:11px;",
        onchange := s"window.location='$urlBase?temporadaId='+this.value",
        frag(temporadas.map { t =>
          val id = t("id").asInstanceOf[Int]
          val nombre = t("nombre").asInstanceOf[String]
          val activa = t("activa").asInstanceOf[Boolean]
          val label = if (activa) s"$nombre (actual)" else nombre
          if (id == seleccionadaId)
            option(value := id.toString, attr("selected") := "selected", label)
          else
            option(value := id.toString, label)
        }: _*)
      )
    )

  def renderRedirect(url: String): cask.Response[Array[Byte]] =
    cask.Response(Array.empty[Byte], statusCode = 302,
      headers = Seq("Location" -> url, "Cache-Control" -> "no-store"))

  private def zScoreBadge(z: Double): Modifier = {
    val (icono, texto, color) =
      if (z > 1.5) ("⭐", "Muy por encima de lo esperado en estas condiciones", "#20c997")
      else if (z >= 0.5) ("↑", "Por encima de su media en este contexto", "#0dcaf0")
      else if (z >= -0.5) ("→", "Dentro de su media habitual", "#6c757d")
      else ("↓", "Por debajo de su media en este contexto", "#dc3545")
    span(cls := "badge d-block mt-1", style := s"background:$color; font-size:8px; white-space:normal;",
      attr("title") := texto, s"$icono ${f"$z%.1f"}σ")
  }

  // BLOQUE B6: readOnly oculta el lapiz de edicion cuando se ve una temporada archivada (cerrada)
  /** Cuerpo application/x-www-form-urlencoded -> Map. Para endpoints nuevos (@cask.post con request). */
  def parseFormBody(request: cask.Request): Map[String, String] =
    new String(request.data.readAllBytes(), "UTF-8").split("&").filter(_.nonEmpty).map { p =>
      val kv = p.split("=", 2)
      java.net.URLDecoder.decode(kv(0), "UTF-8") -> (if (kv.length > 1) java.net.URLDecoder.decode(kv(1), "UTF-8") else "")
    }.toMap

  def renderMatchRow(m: MatchLog, zScoreOpt: Option[Double] = None, readOnly: Boolean = false,
                     sourceBadge: Option[String] = None) = {
    val notaCls = if (m.nota >= 7) "table-success" else if (m.nota >= 5) "table-warning" else "table-danger"
    val audioIcon = if (m.analisisVoz.nonEmpty) span(style := "color:#8b5cf6;", "🎙️") else span("🎤")
    // BLOQUE C: badge si el partido coincidio con un periodo de examenes escolares
    val examenesBadge: Modifier =
      if (DatabaseManager.esFechaDeExamenes(m.fecha)) span(cls := "badge bg-secondary xx-small d-block mt-1", "📚 Semana de exámenes") else frag()
    // BLOQUE D/S: registro minimo pendiente de completar / partido importado por CSV
    val origenBadge: Modifier = sourceBadge match {
      case Some("QUICK_PENDIENTE") => span(cls := "badge bg-warning text-dark xx-small d-block mt-1", "⚡ Datos pendientes")
      case Some("IMPORTADO")       => span(cls := "badge bg-info text-dark xx-small d-block mt-1", "📥 Importado")
      case _                       => frag()
    }
    tr(cls := notaCls,
      td(div(fixEncoding(m.rival)), div(cls := "xx-small text-muted", m.fecha.take(10)), examenesBadge, origenBadge),
      td(m.resultado),
      td(cls := "text-center fw-bold", m.nota.toString,
        m.cpi.map(c => span(cls := "xx-small text-info d-block", attr("title") := "El CPI ajusta la nota por la dificultad real del contexto: rival, condiciones físicas, clima y si jugó en casa o fuera.", f"CPI: $c%.1f")).getOrElse(frag()),
        zScoreOpt.map(zScoreBadge).getOrElse(frag())),
      td(cls := "text-end",
        if (readOnly) frag() else a(href := s"/match/edit/${m.id}", cls := "text-decoration-none me-2", "✏️"),
        if (m.paradas > 0) a(href := s"/history/paradas/${m.id}", cls := "text-decoration-none me-2", attr("title") := "Desglosar paradas", "📊") else frag(),
        a(href := s"/audio-diary/partido/${m.id}", cls := "text-decoration-none", audioIcon),
        a(href := s"/partido-card/${m.id}", target := "_blank", cls := "text-decoration-none ms-2", attr("title") := "Compartir", "📤")
      )
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO — ARQUETIPO DE PORTERO: barras compartidas (usado en /arquetipo y /hector)
  // ─────────────────────────────────────────────────────────────────────────────
  def arquetipoBarsWidget(arq: Map[String, Any]): Modifier = {
    val dominante = arq("dominante").asInstanceOf[String]
    val items = List(
      ("SWEEPER_KEEPER", "🔵 SWEEPER-KEEPER", arq("sweeper").asInstanceOf[Int], "info"),
      ("SHOT_STOPPER", "🔴 SHOT-STOPPER", arq("shotStopper").asInstanceOf[Int], "danger"),
      ("COMMANDING_KEEPER", "🟡 COMMANDING KEEPER", arq("commanding").asInstanceOf[Int], "warning"),
      ("MODERN_GUARDIAN", "🟢 MODERN GUARDIAN", arq("modern").asInstanceOf[Int], "success")
    ).sortBy(-_._3)
    div(
      items.map { case (key, label, pct, color) =>
        val esDominante = key == dominante
        div(cls := "mb-2",
          div(cls := "d-flex justify-content-between xx-small",
            span(cls := (if (esDominante) "fw-bold text-white" else "text-muted"), label + (if (esDominante) " (DOMINANTE)" else "")),
            span(cls := (if (esDominante) "fw-bold text-white" else "text-muted"), s"$pct%")
          ),
          div(cls := "progress", style := "height:10px;", div(cls := s"progress-bar bg-$color", style := s"width:$pct%;"))
        )
      }
    )
  }

  // BLOQUE Q: QR del perfil publico (QRCode.js desde cdnjs), mismo enlace que "Copiar enlace"
  def qrPerfilPublico(texto: String): Modifier = frag(
    div(cls := "text-center my-3",
      div(id := "qr-perfil", style := "display:inline-block; background:#fff; padding:10px; border-radius:10px;"),
      div(cls := "xx-small text-muted mt-2", texto)),
    script(src := "https://cdnjs.cloudflare.com/ajax/libs/qrcodejs/1.0.0/qrcode.min.js"),
    script(raw("(function(){ var el = document.getElementById('qr-perfil'); if (el && window.QRCode) new QRCode(el, { text: window.location.origin + '/hector', width: 200, height: 200 }); })();")))

  // BLOQUE C: badge de confianza estadistica encima de un modulo analitico. Discreto (gris) salvo
  // si es INSUFICIENTE: entonces el badge va en rojo y el modulo con borde rojo discontinuo.
  // visible=false (modulo oculto por falta de datos) no pinta nada.
  def badgeConfianza(tipo: String, n: Int, prefijo: String = ""): Modifier = {
    val c = DatabaseManager.getConfianzaModulo(tipo, n)
    val insuficiente = c("nivel") == "INSUFICIENTE"
    div(cls := "xx-small mb-1", attr("title") := s"Confianza: ${c("nivel")}",
      style := (if (insuficiente) s"color:#${c("color")}; font-weight:700;" else "color:#94a3b8;"),
      s"$prefijo${c("emoji")} ${c("texto")}")
  }

  def conConfianza(tipo: String, n: Int, visible: Boolean = true)(modulo: Modifier): Modifier =
    if (!visible) modulo
    else if (DatabaseManager.getConfianzaModulo(tipo, n)("nivel") == "INSUFICIENTE")
      div(cls := "mb-3", style := "border:2px dashed #dc2626; border-radius:12px; padding:6px;", badgeConfianza(tipo, n), modulo)
    else div(badgeConfianza(tipo, n), modulo)

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE I — NAVEGACION EN DOS NIVELES: 6 categorias + 3 favoritos (localStorage)
  // Movil: barra inferior con las categorias; al pulsar, el submenu se despliega hacia arriba.
  // Escritorio: menu lateral colapsable. Toda pagina nueva debe anadirse a una categoria.
  // ─────────────────────────────────────────────────────────────────────────────
  val navCategorias: Seq[(String, String, Seq[(String, String)])] = Seq(
    ("🏠", "HOY", Seq("Dashboard HOY" -> "/#hoy", "Dashboard COMPLETO" -> "/#completo")),
    ("⚽", "PARTIDO", Seq("Registrar partido" -> "/match-center", "Historial" -> "/history", "Timeline" -> "/career/timeline",
      "Flash-cards" -> "/flash-cards", "Torneo" -> "/tournament/bracket", "Pizarra" -> "/tactics", "Contexto del partido" -> "/match-context",
      "Rivales" -> "/striker-clustering", "Scouting NLP" -> "/scouting/nlp", "Vídeo IA" -> "/video-history")),
    ("💪", "HÉCTOR", Seq("Bio / Sueño" -> "/bio", "Crecimiento / PHV" -> "/bio-banding", "Tests físicos" -> "/physical-tests",
      "Movilidad" -> "/movilidad-tests", "Cognitivo" -> "/cognitivo", "Psico" -> "/psych", "Emocional" -> "/emocional",
      "Reset cognitivo" -> "/cognitive-reset", "Guantes" -> "/bio/guantes", "Lesiones" -> "/lesiones", "Nutrición" -> "/nutrition",
      "Skills" -> "/goalkeeper-skills", "Dojo" -> "/dojo", "Dojo entrenador" -> "/dojo/entrenador", "Diario" -> "/diary",
      "Periodización" -> "/periodization", "Footbar" -> "/footbar")),
    ("📊", "ANÁLISIS", Seq("Benchmark / RFMF" -> "/benchmark", "Correlaciones" -> "/correlaciones", "RedZone" -> "/red-zone",
      "PSxG" -> "/psxg-delta", "Biomecánica" -> "/biomecanica", "Arquetipo" -> "/arquetipo", "Voz del Portero" -> "/voz-portero",
      "Influencia" -> "/gk-influence", "Scanning" -> "/scanning-rate", "Evolución" -> "/temporal", "Simulador" -> "/simulate",
      "Moneyball" -> "/moneyball", "Efecto mariposa" -> "/efecto-mariposa")),
    ("🏆", "CARRERA", Seq("IDP / Objetivos" -> "/idp", "Legado / Hitos" -> "/career/legacy", "Trayectoria" -> "/career",
      "Digital Twin" -> "/digital-twin", "Visibilidad" -> "/visibility", "Contactos" -> "/contacts",
      "Informe captación" -> "/scouting-report", "Oportunidades" -> "/opportunities", "Pathway" -> "/pathway",
      "Techo" -> "/techo", "Mercado" -> "/market-estimator")),
    ("⚙️", "SISTEMA", Seq("Settings / Perfil público" -> "/settings", "Admin" -> "/admin", "Backups / Export" -> "/admin#backups"))
  )

  private def navegacion(): Modifier = {
    val itemsJs = ujson.write(ujson.Arr(navCategorias.flatMap(_._3).map { case (l, h) => ujson.Obj("h" -> h, "l" -> l) }: _*))
    def enlaces(items: Seq[(String, String)]): Modifier =
      frag(items.map { case (l, h) => a(href := h, cls := "gnav-link", attr("data-h") := h, l) }: _*)
    frag(
      // Escritorio: menu lateral
      tags2.nav(id := "gnavSide", cls := "gnav-side",
        div(cls := "gnav-side-title", span(cls := "text-warning", "G"), " GUARDIAN"),
        frag(navCategorias.zipWithIndex.map { case ((emoji, nombre, items), i) =>
          div(cls := "gnav-side-cat",
            button(tpe := "button", cls := "gnav-side-btn", attr("data-cat") := i.toString, onclick := s"gnavSideToggle($i)", s"$emoji $nombre"),
            div(id := s"gnavSideSub$i", cls := "gnav-side-sub", enlaces(items)))
        }: _*)),
      // Movil: submenus (hacia arriba) + favoritos + barra de categorias
      div(cls := "gnav-mobile",
        frag(navCategorias.zipWithIndex.map { case ((emoji, nombre, items), i) =>
          div(id := s"gnavSub$i", cls := "gnav-sub", div(cls := "gnav-sub-title", s"$emoji $nombre"), enlaces(items))
        }: _*),
        div(id := "gnavFavs", cls := "gnav-favs"),
        div(cls := "gnav-bar",
          frag(navCategorias.zipWithIndex.map { case ((emoji, nombre, _), i) =>
            button(tpe := "button", cls := "gnav-bar-btn", attr("data-cat") := i.toString, onclick := s"gnavOpen($i)",
              div(cls := "gnav-bar-icon", emoji), div(cls := "gnav-bar-label", nombre))
          }: _*))),
      script(raw(s"""
        var GNAV_ITEMS = $itemsJs;
        var GNAV_FAV_DEFAULT = [{h:'/#hoy',l:'HOY'},{h:'/match-center',l:'Registrar partido'},{h:'/bio',l:'Bio / Sueño'}];
        function gnavLS(k, v){ try { if (v === undefined) return localStorage.getItem(k); localStorage.setItem(k, v); } catch(e) { return null; } }
        function gnavFavs(){ try { var f = JSON.parse(gnavLS('guardian_favoritos')); if (Array.isArray(f)) return f; } catch(e) {} return GNAV_FAV_DEFAULT.slice(); }
        function gnavPath(h){ return h.split('#')[0]; }
        function gnavActual(){ return location.pathname; }
        function gnavRenderFavs(){
          var cont = document.getElementById('gnavFavs'); if (!cont) return;
          cont.innerHTML = '';
          gnavFavs().forEach(function(f){
            var a = document.createElement('a'); a.href = f.h; a.className = 'gnav-fav'; a.textContent = '⭐ ' + f.l;
            cont.appendChild(a);
          });
          var esFav = gnavFavs().some(function(f){ return gnavPath(f.h) === gnavActual(); });
          var star = document.getElementById('gnavStar'); if (star) { star.textContent = esFav ? '★' : '☆'; star.title = esFav ? 'Quitar de favoritos' : 'Fijar en favoritos (máx. 3)'; }
        }
        function gnavToggleFav(){
          var favs = gnavFavs(), actual = gnavActual();
          var idx = favs.findIndex(function(f){ return gnavPath(f.h) === actual; });
          if (idx >= 0) favs.splice(idx, 1);
          else {
            var item = GNAV_ITEMS.find(function(it){ return gnavPath(it.h) === actual; });
            favs.push({h: actual, l: item ? item.l : (document.querySelector('h2') ? document.querySelector('h2').textContent.trim().slice(0, 24) : actual)});
            while (favs.length > 3) favs.shift();
          }
          gnavLS('guardian_favoritos', JSON.stringify(favs));
          gnavRenderFavs();
        }
        function gnavOpen(i){
          document.querySelectorAll('.gnav-sub').forEach(function(p, j){ p.classList.toggle('open', j === i && !p.classList.contains('open')); });
        }
        document.addEventListener('click', function(e){
          if (!e.target.closest('.gnav-mobile')) document.querySelectorAll('.gnav-sub.open').forEach(function(p){ p.classList.remove('open'); });
        });
        function gnavSideToggle(i){ var s = document.getElementById('gnavSideSub' + i); if (s) s.classList.toggle('open'); }
        function gnavSidebar(){
          var abierto = !document.body.classList.contains('gnav-side-open');
          document.body.classList.toggle('gnav-side-open', abierto);
          gnavLS('guardian_sidebar', abierto ? '1' : '0');
        }
        (function(){
          if (gnavLS('guardian_sidebar') !== '0') document.body.classList.add('gnav-side-open');
          var actual = gnavActual();
          document.querySelectorAll('.gnav-link').forEach(function(a){
            if (gnavPath(a.getAttribute('data-h')) === actual && !(actual === '/' && a.getAttribute('data-h') === '/#completo')) {
              a.classList.add('active');
              var cat = a.closest('.gnav-sub, .gnav-side-sub');
              if (cat && cat.classList.contains('gnav-side-sub')) cat.classList.add('open');
              var i = cat ? cat.id.replace(/\\D/g, '') : null;
              if (i !== null) document.querySelectorAll('[data-cat="' + i + '"]').forEach(function(b){ b.classList.add('active'); });
            }
          });
          gnavRenderFavs();
        })();
      """))
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
            div(
              span(cls := "gnav-hamb", onclick := "gnavSidebar()", attr("title") := "Mostrar/ocultar menú", "☰ "),
              span(cls := "text-warning", "G"), " GUARDIAN ELITE",
              span(id := "gnavStar", onclick := "gnavToggleFav()", style := "cursor:pointer; color:#facc15; font-size:18px; margin-left:10px;", "☆")),
            div(cls:="d-flex align-items-center gap-3",
              a(href:="/profiles", style:="text-decoration:none; color:#ffc107; font-size:11px; font-weight:bold; border: 1px solid #ffc107; padding: 2px 8px; border-radius: 4px;", "👤 PERFIL"),
              a(href:="/logout", style:="text-decoration:none; color:#ff4d4d; font-size:11px; font-weight:bold; border: 1px solid #ff4d4d; padding: 2px 8px; border-radius: 4px;", "SALIR"),
              span(id:="themeToggle", onclick:="toggleTheme()", style:="cursor:pointer; font-size:20px; user-select:none;", "☀️"),
              a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "⚙️")
            )
          ),
          div(cls := "container main-content", pageContents), navegacion())
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
    /* BLOQUE I: navegacion en dos niveles */
    .gnav-side { display: none; }
    .gnav-hamb { display: none; cursor: pointer; color: #94a3b8; }
    .gnav-mobile { position: fixed; bottom: 0; left: 0; right: 0; z-index: 1000; }
    .gnav-bar { display: flex; background: #1a1a1a; border-top: 1px solid #333; box-shadow: 0 -2px 10px rgba(0,0,0,0.5); padding: 6px 0 8px; }
    .gnav-bar-btn { flex: 1; background: none; border: 0; color: #888; text-align: center; padding: 0; min-width: 0; }
    .gnav-bar-btn.active { color: #d4af37; }
    .gnav-bar-icon { font-size: 19px; line-height: 1.2; }
    .gnav-bar-label { font-size: 9px; font-weight: bold; letter-spacing: 0.5px; }
    .gnav-favs { display: flex; gap: 6px; padding: 4px 8px; background: #111; border-top: 1px solid #262626; overflow-x: auto; }
    .gnav-fav { font-size: 10px; color: #facc15; text-decoration: none; white-space: nowrap; border: 1px solid #3f3f1f; border-radius: 10px; padding: 2px 8px; }
    .gnav-sub { display: none; max-height: 55vh; overflow-y: auto; background: #151515; border-top: 2px solid #d4af37; padding: 8px 10px; }
    .gnav-sub.open { display: block; }
    .gnav-sub-title { font-size: 11px; color: #d4af37; font-weight: bold; letter-spacing: 1px; margin-bottom: 6px; }
    .gnav-link { display: block; padding: 7px 8px; color: #ddd; text-decoration: none; font-size: 13px; border-radius: 6px; }
    .gnav-link:hover, .gnav-link.active { background: #262626; color: #d4af37; }
    @media (max-width: 991.98px) { body { padding-bottom: 110px !important; } }
    @media (min-width: 992px) {
      .gnav-mobile { display: none; }
      .gnav-hamb { display: inline; }
      body { padding-bottom: 24px !important; }
      body.gnav-side-open { padding-left: 230px; }
      body.gnav-side-open .gnav-side { display: block; position: fixed; top: 0; left: 0; bottom: 0; width: 230px; overflow-y: auto;
        background: #151515; border-right: 1px solid #333; z-index: 1001; padding: 12px 8px; }
      .gnav-side-title { font-weight: bold; letter-spacing: 2px; color: #fff; padding: 6px 8px 12px; }
      .gnav-side-btn { width: 100%; text-align: left; background: none; border: 0; color: #bbb; font-weight: bold; font-size: 13px; padding: 8px; border-radius: 6px; }
      .gnav-side-btn.active { color: #d4af37; }
      .gnav-side-btn:hover { background: #222; }
      .gnav-side-sub { display: none; padding-left: 10px; }
      .gnav-side-sub.open { display: block; }
    }
    body.light-mode .gnav-bar, body.light-mode .gnav-sub, body.light-mode .gnav-favs, body.light-mode .gnav-side { background: #fff !important; border-color: #ddd !important; }
    body.light-mode .gnav-link { color: #333; }
    @keyframes pulseYellow {
      0%, 100% { border-left: 3px solid transparent; }
      50% { border-left: 3px solid #ffc107; }
    }
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
