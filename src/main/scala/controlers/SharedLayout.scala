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

  /**
   * Partido del historial como tarjeta compacta de una linea (fecha, rival, resultado y nota); al pulsarla se
   * despliega el detalle. Los data-* alimentan los filtros de la pagina.
   */
  def renderMatchCard(m: MatchLog, zScoreOpt: Option[Double] = None, readOnly: Boolean = false,
                      sourceBadge: Option[String] = None, detalle: Map[String, Any] = Map.empty): Modifier = {
    val (gf, gc) = m.resultado.split("-").map(_.trim.toIntOption) match {
      case Array(Some(a), Some(b)) => (Some(a), Some(b)); case _ => (None, None)
    }
    val res = (gf, gc) match { case (Some(a), Some(b)) if a > b => "V"; case (Some(a), Some(b)) if a < b => "D"; case (Some(_), Some(_)) => "E"; case _ => "" }
    val notaColor = if (m.nota >= 7) "#20c997" else if (m.nota >= 5) "#f59e0b" else "#ef4444"
    val pendiente = sourceBadge.contains("QUICK_PENDIENTE")
    val videoIA = detalle.get("videoIA").exists(_.asInstanceOf[Boolean])
    val audioIcon = if (m.analisisVoz.nonEmpty) span(style := "color:#8b5cf6;", "🎙️") else span("🎤")
    val rubrica = detalle.getOrElse("rubrica", Nil).asInstanceOf[List[Option[Int]]]
    val goles = detalle.getOrElse("goles", Nil).asInstanceOf[List[String]]
    tag("details")(cls := s"hcard${if (pendiente) " hcard-pendiente" else ""}",
      attr("data-res") := res, attr("data-pc0") := (if (gc.contains(0)) "1" else "0"), attr("data-video") := (if (videoIA || m.video.nonEmpty) "1" else "0"),
      tag("summary")(cls := "hcard-sum",
        span(cls := "hcard-fecha", m.fecha.take(10).drop(5).split("-").reverse.mkString("/")),
        span(cls := "hcard-rival", fixEncoding(m.rival)),
        span(cls := "hcard-res", m.resultado),
        span(cls := "hcard-nota", style := s"background:$notaColor;", f"${m.nota}%.1f")),
      div(cls := "hcard-body",
        div(cls := "d-flex flex-wrap gap-1 mb-2",
          sourceBadge match {
            case Some("QUICK_PENDIENTE") => span(cls := "badge bg-warning text-dark", "⚡ Datos pendientes")
            case Some("IMPORTADO") => span(cls := "badge bg-info text-dark", "📥 Importado")
            case _ => frag()
          },
          if (DatabaseManager.esFechaDeExamenes(m.fecha)) span(cls := "badge bg-secondary", "📚 Semana de exámenes") else frag(),
          m.cpi.map(c => span(cls := "badge bg-dark border border-info text-info", attr("title") := "El CPI ajusta la nota por la dificultad real del contexto: rival, condiciones físicas, clima y si jugó en casa o fuera.", f"CPI $c%.1f")).getOrElse(frag()),
          zScoreOpt.map(zScoreBadge).getOrElse(frag()),
          if (videoIA) span(cls := "badge bg-danger", "🎬 Vídeo IA") else frag()),
        if (rubrica.exists(_.isDefined))
          div(cls := "xx-small mb-1", span(cls := "text-muted", "Rúbrica: "),
            Seq("Pos", "Dec", "Pie", "Com", "Act").zip(rubrica).map { case (e, v) => s"$e ${v.map(_.toString).getOrElse("—")}" }.mkString(" · "))
        else div(cls := "xx-small text-muted mb-1", "Rúbrica sin completar"),
        div(cls := "xx-small mb-1", span(cls := "text-muted", "Paradas: "),
          s"${m.paradas} (${m.p1v1} en 1v1 · ${m.pAir} aéreas · ${m.pPie} con el pie) · ${m.minutos} min"),
        if (goles.nonEmpty) div(cls := "xx-small mb-1", span(cls := "text-muted", "Goles: "), goles.mkString(", ")) else frag(),
        if (m.notas.trim.nonEmpty) div(cls := "xx-small text-muted fst-italic mb-1", fixEncoding(m.notas).take(200)) else frag(),
        div(cls := "d-flex gap-3 mt-2",
          if (readOnly) frag() else a(href := s"/match/edit/${m.id}", cls := "text-decoration-none", "✏️ Editar"),
          if (m.paradas > 0) a(href := s"/history/paradas/${m.id}", cls := "text-decoration-none", "📊 Paradas") else frag(),
          a(href := s"/audio-diary/partido/${m.id}", cls := "text-decoration-none", audioIcon, " Audio"),
          a(href := s"/partido-card/${m.id}", target := "_blank", cls := "text-decoration-none", "📤 Compartir"))))
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
  // NAVEGACION: 6 categorias + 3 favoritos (localStorage)
  // Movil: barra inferior (categorias + favoritos); al pulsar una categoria se abre un panel hacia
  // arriba con sus paginas en cuadricula de 3 columnas. HOY va directo al dashboard.
  // Escritorio: menu lateral colapsable. Toda pagina nueva debe anadirse a una categoria.
  // ─────────────────────────────────────────────────────────────────────────────
  val navCategorias: Seq[(String, String, Seq[(String, String, String)])] = Seq(
    ("🏠", "HOY", Seq(("🏠", "Dashboard", "/"))),
    ("⚽", "PARTIDO", Seq(("⚡", "Registro rápido", "/match-center?quick=1"), ("📝", "Partido completo", "/match-center"),
      ("📋", "Historial", "/history"), ("🗓️", "Flash-cards", "/flash-cards"), ("🏆", "Torneo", "/tournament/bracket"),
      ("📈", "Evolución", "/temporal"), ("📊", "Contexto", "/match-context"), ("🦋", "Mariposa", "/efecto-mariposa"),
      ("🎯", "Mapa goles", "/mapa-goles"), ("⛳", "Penaltis", "/penalties"), ("🎬", "Vídeo IA", "/video-history"),
      ("🧩", "Pizarra", "/tactics"))),
    ("💪", "HÉCTOR", Seq(("📝", "Registro diario", "/bio"), ("💤", "Sueño", "/bio/sueno"), ("📈", "Carga ACWR", "/bio/carga"),
      ("😴", "Fatiga", "/bio/fatiga"), ("💪", "Tests físicos", "/physical-tests"), ("🧠", "Cognitivo", "/cognitivo"),
      ("🧠", "Psicológico", "/psych"), ("💚", "Emocional", "/emocional"), ("🤸", "Movilidad", "/movilidad-tests"),
      ("🥗", "Nutrición", "/nutrition"), ("🩹", "Lesiones", "/lesiones"), ("🧤", "Guantes", "/bio/guantes"),
      ("🦵", "Footbar", "/footbar"), ("👁️", "Scanning", "/scanning-rate"), ("🧬", "BioBand", "/bio-banding"))),
    ("📊", "ANÁLISIS", Seq(("📊", "Benchmark", "/benchmark"), ("🔬", "Correlaciones", "/correlaciones"), ("🔴", "RedZone", "/red-zone"),
      ("xG", "PSxG", "/psxg-delta"), ("🎯", "Biomecánica", "/biomecanica"), ("📡", "Influencia", "/gk-influence"),
      ("🎭", "Arquetipo", "/arquetipo"), ("⚔️", "Rivales", "/striker-clustering"), ("🔍", "Scouting", "/scouting/nlp"),
      ("🎯", "Techo", "/techo"), ("🔮", "Simulador", "/simulate"), ("🧠", "Dojo", "/dojo"), ("📋", "D.Entren", "/dojo/entrenador"))),
    ("🏆", "CARRERA", Seq(("🗺️", "IDP", "/idp"), ("⭐", "Legado", "/career/legacy"), ("🔮", "Twin 2035", "/digital-twin"),
      ("💰", "Mercado", "/market-estimator"), ("📄", "Informe", "/scouting-report"), ("🤖", "Skills", "/goalkeeper-skills"),
      ("🎤", "Voz portero", "/voz-portero"), ("📖", "Diario", "/diary"), ("📅", "Calendario", "/career/calendario"),
      ("📅", "Timeline", "/career/timeline"), ("🗺️", "Pathway", "/pathway"), ("👥", "Red", "/contacts"),
      ("🗺️", "Visibilidad", "/visibility"), ("🏆", "Oportunidades", "/opportunities"), ("🔬", "Longitudinal", "/career/longitudinal"))),
    ("⚙️", "SISTEMA", Seq(("⚙️", "Settings", "/settings"), ("🔧", "Admin", "/admin"), ("🧩", "Reset", "/cognitive-reset"),
      ("📅", "Periodización", "/periodization"), ("💵", "Moneyball", "/moneyball"), ("📊", "Trayectoria", "/career"),
      ("⚙️", "Distribución", "/distribution")))
  )

  private def navegacion(): Modifier = {
    val itemsJs = ujson.write(ujson.Arr(navCategorias.flatMap(_._3).map { case (e, l, h) => ujson.Obj("h" -> h, "l" -> l, "e" -> e) }: _*))
    frag(
      // Escritorio: menu lateral
      tags2.nav(id := "gnavSide", cls := "gnav-side",
        div(cls := "gnav-side-title", span(cls := "text-warning", "G"), " GUARDIAN"),
        frag(navCategorias.zipWithIndex.map { case ((emoji, nombre, items), i) =>
          div(cls := "gnav-side-cat",
            button(tpe := "button", cls := "gnav-side-btn", attr("data-cat") := i.toString, onclick := s"gnavSideToggle($i)", s"$emoji $nombre"),
            div(id := s"gnavSideSub$i", cls := "gnav-side-sub",
              frag(items.map { case (e, l, h) => a(href := h, cls := "gnav-link", attr("data-h") := h, s"$e $l") }: _*)))
        }: _*)),
      // Movil: panel de submenu (cuadricula) + barra inferior con categorias y favoritos
      div(cls := "gnav-mobile",
        div(id := "gnavPanel", cls := "gnav-panel", onclick := "if(event.target===this)gnavCerrar()",
          frag(navCategorias.zipWithIndex.map { case ((emoji, nombre, items), i) =>
            div(id := s"gnavSub$i", cls := "gnav-sub",
              div(cls := "gnav-sub-title", s"$emoji $nombre"),
              div(cls := "gnav-grid",
                frag(items.map { case (e, l, h) =>
                  a(href := h, cls := "gnav-tile gnav-link", attr("data-h") := h, div(cls := "gnav-tile-icon", e), div(cls := "gnav-tile-label", l))
                }: _*)))
          }: _*)),
        div(cls := "gnav-bar",
          frag(navCategorias.zipWithIndex.map { case ((emoji, nombre, _), i) =>
            button(tpe := "button", cls := "gnav-bar-btn", attr("data-cat") := i.toString,
              onclick := (if (i == 0) "location.href='/'" else s"gnavOpen($i)"),
              div(cls := "gnav-bar-icon", emoji), div(cls := "gnav-bar-label", nombre))
          }: _*),
          div(id := "gnavFavsBar", cls := "gnav-favs-bar"))),
      script(raw(s"""
        var GNAV_ITEMS = $itemsJs;
        var GNAV_FAV_DEFAULT = [{h:'/bio/sueno',l:'Sueño',e:'💤'},{h:'/match-center',l:'Partido',e:'📝'},{h:'/history',l:'Historial',e:'📋'}];
        function gnavLS(k, v){ try { if (v === undefined) return localStorage.getItem(k); localStorage.setItem(k, v); } catch(e) { return null; } }
        function gnavFavs(){ try { var f = JSON.parse(gnavLS('guardian_favoritos_v2')); if (Array.isArray(f)) return f; } catch(e) {} return GNAV_FAV_DEFAULT.slice(); }
        function gnavPath(h){ return h.split('#')[0].split('?')[0]; }
        function gnavActual(){ return location.pathname; }
        function gnavRenderFavs(){
          var cont = document.getElementById('gnavFavsBar'); if (!cont) return;
          cont.innerHTML = '';
          gnavFavs().forEach(function(f){
            var a = document.createElement('a'); a.href = f.h; a.className = 'gnav-fav' + (gnavPath(f.h) === gnavActual() ? ' active' : '');
            var i = document.createElement('div'); i.className = 'gnav-bar-icon'; i.textContent = f.e || '⭐';
            var l = document.createElement('div'); l.className = 'gnav-bar-label'; l.textContent = (f.l || '').split(' ')[0].slice(0, 9);
            a.appendChild(i); a.appendChild(l); cont.appendChild(a);
          });
          var esFav = gnavFavs().some(function(f){ return gnavPath(f.h) === gnavActual(); });
          var star = document.getElementById('gnavStar'); if (star) { star.textContent = esFav ? '★' : '☆'; star.title = esFav ? 'Quitar de favoritos' : 'Fijar en favoritos (máx. 3)'; }
        }
        function gnavToggleFav(){
          var favs = gnavFavs(), actual = gnavActual();
          var idx = favs.findIndex(function(f){ return gnavPath(f.h) === actual; });
          if (idx >= 0) favs.splice(idx, 1);
          else {
            var item = GNAV_ITEMS.find(function(it){ return it.h === actual; }) || GNAV_ITEMS.find(function(it){ return gnavPath(it.h) === actual; });
            favs.push({h: actual, l: item ? item.l : (document.querySelector('h2') ? document.querySelector('h2').textContent.trim().slice(0, 24) : actual), e: item ? item.e : '⭐'});
            while (favs.length > 3) favs.shift();
          }
          gnavLS('guardian_favoritos_v2', JSON.stringify(favs));
          gnavRenderFavs();
        }
        function gnavCerrar(){
          document.getElementById('gnavPanel').classList.remove('open');
          document.querySelectorAll('.gnav-sub').forEach(function(p){ p.classList.remove('open'); });
        }
        function gnavOpen(i){
          var sub = document.getElementById('gnavSub' + i);
          var yaAbierto = sub.classList.contains('open');
          gnavCerrar();
          if (!yaAbierto) { sub.classList.add('open'); document.getElementById('gnavPanel').classList.add('open'); }
        }
        document.addEventListener('click', function(e){ if (!e.target.closest('.gnav-mobile')) gnavCerrar(); });
        document.addEventListener('keydown', function(e){ if (e.key === 'Escape') gnavCerrar(); });
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
            if (gnavPath(a.getAttribute('data-h')) === actual) {
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

  // ─────────────────────────────────────────────────────────────────────────────
  // PATRONES COMUNES DE PAGINA: secciones colapsables, "sin datos aun" y pestanas
  // ─────────────────────────────────────────────────────────────────────────────
  /** Clave estable de una seccion: sin cifras (los contadores del titulo no cambian la clave). */
  private def claveSeccion(titulo: String): String = titulo.replaceAll("[0-9]+", "").replaceAll("\\s+", " ").trim.take(40)

  /**
   * Seccion colapsable. `abierta` es el estado por defecto; si el usuario la abre o la cierra, se recuerda
   * por pagina y titulo en localStorage.
   */
  def seccion(titulo: String, abierta: Boolean = false)(contenido: Modifier*): Modifier =
    div(cls := "guardian-section",
      button(tpe := "button", cls := "guardian-section-toggle", attr("data-key") := claveSeccion(titulo), onclick := "toggleSection(this)",
        span(titulo), span(cls := "toggle-icon", if (abierta) "▲" else "▼")),
      div(cls := s"guardian-section-body${if (abierta) "" else " collapsed"}", contenido))

  /** Marcador compacto para un modulo sin datos suficientes (en lugar de un panel vacio). */
  def sinDatos(titulo: String, detalle: String = ""): Modifier =
    div(cls := "guardian-sin-datos", attr("title") := detalle, span(s"📭 $titulo"), span(cls := "text-muted", " — sin datos aún"),
      if (detalle.nonEmpty) div(cls := "xx-small text-muted", detalle) else frag())

  /**
   * Pestanas. La activa se recuerda en localStorage; si la URL trae un #ancla que esta dentro de una
   * pestana, se abre esa pestana.
   */
  def pestanas(id: String, tabs: Seq[(String, Modifier)]): Modifier =
    div(cls := "guardian-tabs", attr("data-tabs") := id,
      div(cls := "guardian-tabs-nav",
        frag(tabs.zipWithIndex.map { case ((titulo, _), i) =>
          button(tpe := "button", cls := s"guardian-tab-btn${if (i == 0) " active" else ""}", onclick := s"mostrarPestana('$id', $i)", titulo)
        }: _*)),
      frag(tabs.zipWithIndex.map { case ((_, contenido), i) =>
        div(cls := "guardian-tab-body", attr("data-tab") := i.toString, style := (if (i == 0) "" else "display:none;"), contenido)
      }: _*))

  private val jsPatronesComunes = """
    function toggleSection(btn) {
      var body = btn.nextElementSibling;
      var abierta = body.classList.contains('collapsed');
      body.classList.toggle('collapsed', !abierta);
      btn.querySelector('.toggle-icon').textContent = abierta ? '▲' : '▼';
      try { localStorage.setItem('guardian_section_' + location.pathname + '_' + btn.getAttribute('data-key'), abierta ? '1' : '0'); } catch(e) {}
      // los graficos dibujados dentro de una seccion cerrada necesitan recalcular su tamano al abrirla
      if (abierta) window.dispatchEvent(new Event('resize'));
    }
    function mostrarPestana(id, i) {
      var cont = document.querySelector('[data-tabs="' + id + '"]'); if (!cont) return;
      cont.querySelectorAll('.guardian-tab-btn').forEach(function(b, j){ b.classList.toggle('active', j === i); });
      cont.querySelectorAll('.guardian-tab-body').forEach(function(b){ b.style.display = b.getAttribute('data-tab') == i ? '' : 'none'; });
      try { localStorage.setItem('guardian_tab_' + id, String(i)); } catch(e) {}
    }
    (function(){
      document.querySelectorAll('.guardian-section-toggle').forEach(function(btn) {
        try {
          var v = localStorage.getItem('guardian_section_' + location.pathname + '_' + btn.getAttribute('data-key'));
          if (v === null) return;
          var abierta = v === '1';
          btn.nextElementSibling.classList.toggle('collapsed', !abierta);
          btn.querySelector('.toggle-icon').textContent = abierta ? '▲' : '▼';
        } catch(e) {}
      });
      document.querySelectorAll('[data-tabs]').forEach(function(cont) {
        var id = cont.getAttribute('data-tabs'), i = null;
        if (location.hash) {
          var destino = document.getElementById(location.hash.slice(1));
          var body = destino ? destino.closest('.guardian-tab-body') : null;
          if (body && cont.contains(body)) i = parseInt(body.getAttribute('data-tab'));
        }
        if (i === null) { try { var g = localStorage.getItem('guardian_tab_' + id); if (g !== null) i = parseInt(g); } catch(e) {} }
        if (i !== null && cont.querySelector('[data-tab="' + i + '"]')) mostrarPestana(id, i);
        if (location.hash) { var d = document.getElementById(location.hash.slice(1)); if (d) setTimeout(function(){ d.scrollIntoView(); }, 50); }
      });
    })();
  """

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
            div(cls:="d-flex align-items-center gap-2",
              a(href:="/profiles", attr("title") := "Perfiles", style:="text-decoration:none; color:#ffc107; font-size:11px; font-weight:bold; border: 1px solid #ffc107; padding: 2px 8px; border-radius: 4px;", "👤", span(cls := "hdr-txt", " PERFIL")),
              a(href:="/logout", attr("title") := "Salir", style:="text-decoration:none; color:#ff4d4d; font-size:11px; font-weight:bold; border: 1px solid #ff4d4d; padding: 2px 8px; border-radius: 4px;", "⏻", span(cls := "hdr-txt", " SALIR")),
              span(id:="themeToggle", onclick:="toggleTheme()", style:="cursor:pointer; font-size:20px; user-select:none;", "☀️"),
              a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "⚙️")
            )
          ),
          div(cls := "container main-content", pageContents), navegacion(), script(raw(jsPatronesComunes)))
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
    /* NAVEGACION: barra inferior (categorias + favoritos), panel en cuadricula y menu lateral */
    .gnav-side { display: none; }
    .gnav-hamb { display: none; cursor: pointer; color: #94a3b8; }
    .gnav-mobile { position: fixed; bottom: 0; left: 0; right: 0; z-index: 1000; }
    .gnav-bar { display: flex; align-items: stretch; background: #1a1a1a; border-top: 1px solid #333; box-shadow: 0 -2px 10px rgba(0,0,0,0.5); padding: 6px 2px 8px; }
    .gnav-bar-btn, .gnav-fav { flex: 1; background: none; border: 0; color: #888; text-align: center; padding: 0; min-width: 0; text-decoration: none; }
    .gnav-bar-btn.active, .gnav-fav.active { color: #d4af37; }
    .gnav-bar-icon { font-size: 19px; line-height: 1.2; }
    .gnav-bar-label { font-size: 8.5px; font-weight: bold; letter-spacing: 0.3px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
    .gnav-favs-bar { display: flex; flex: 3; border-left: 1px solid #333; margin-left: 2px; }
    .gnav-fav { color: #facc15; }
    .gnav-panel { display: none; position: fixed; left: 0; right: 0; top: 0; bottom: 58px; background: rgba(0,0,0,0.35); }
    .gnav-panel.open { display: flex; align-items: flex-end; }
    .gnav-sub { display: none; width: 100%; max-height: 70vh; overflow-y: auto; padding: 12px 10px 14px;
      background: rgba(17,17,17,0.82); backdrop-filter: blur(10px); -webkit-backdrop-filter: blur(10px);
      border-top: 2px solid #d4af37; border-radius: 16px 16px 0 0; animation: gnavSube .18s ease-out; }
    .gnav-sub.open { display: block; }
    @keyframes gnavSube { from { transform: translateY(24px); opacity: 0; } to { transform: none; opacity: 1; } }
    .gnav-sub-title { font-size: 11px; color: #d4af37; font-weight: bold; letter-spacing: 1px; margin-bottom: 8px; }
    .gnav-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; }
    .gnav-tile { display: block; text-align: center; padding: 10px 4px; border-radius: 12px; background: rgba(255,255,255,0.06); color: #e5e5e5; text-decoration: none; }
    .gnav-tile.active { background: rgba(212,175,55,0.2); color: #d4af37; }
    .gnav-tile-icon { font-size: 22px; line-height: 1.2; }
    .gnav-tile-label { font-size: 11px; font-weight: 600; margin-top: 2px; }
    .gnav-link { color: #ddd; text-decoration: none; }
    .gnav-side .gnav-link { display: block; padding: 6px 8px; font-size: 13px; border-radius: 6px; }
    .gnav-side .gnav-link:hover, .gnav-side .gnav-link.active { background: #262626; color: #d4af37; }
    .hdr-txt { }
    @media (max-width: 991.98px) { body { padding-bottom: 76px !important; } }
    @media (max-width: 520px) { .hdr-txt { display: none; } .app-header { letter-spacing: 1px !important; font-size: 17px !important; } }
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
    body.light-mode .gnav-bar, body.light-mode .gnav-side { background: #fff !important; border-color: #ddd !important; }
    body.light-mode .gnav-sub { background: rgba(255,255,255,0.9); }
    body.light-mode .gnav-tile, body.light-mode .gnav-link { color: #333; }

    /* HISTORIAL: tarjetas de partido desplegables y filtros */
    .hcard { background: #1e293b; border: 1px solid #334155; border-radius: 10px; margin-bottom: 6px; }
    .hcard-pendiente { border-color: #facc15; background: rgba(250,204,21,0.08); }
    .hcard-sum { display: flex; align-items: center; gap: 10px; padding: 10px 12px; cursor: pointer; list-style: none; color: #e2e8f0; }
    .hcard-sum::-webkit-details-marker { display: none; }
    .hcard-fecha { font-size: 11px; color: #94a3b8; min-width: 38px; }
    .hcard-rival { flex: 1; font-weight: 700; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .hcard-res { font-weight: 700; min-width: 34px; text-align: center; }
    .hcard-nota { color: #111; font-weight: 900; border-radius: 6px; padding: 2px 8px; min-width: 42px; text-align: center; }
    .hcard-body { padding: 0 12px 10px; color: #e2e8f0; }
    .hchip { background: #1e293b; color: #94a3b8; border: 1px solid #334155; border-radius: 14px; padding: 4px 10px; font-size: 11px; font-weight: 700; white-space: nowrap; }
    .hchip.active { background: #d4af37; color: #111; border-color: #d4af37; }
    body.light-mode .hcard { background: #f8fafc; border-color: #cbd5e1; } body.light-mode .hcard-sum, body.light-mode .hcard-body { color: #1e293b; }

    /* PATRONES COMUNES: secciones colapsables, sin datos, pestanas */
    .guardian-section-toggle { width: 100%; text-align: left; background: #1e293b; color: #94a3b8; border: 1px solid #334155;
      padding: 10px 14px; border-radius: 8px; font-size: 12px; font-weight: 700; cursor: pointer;
      display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px; }
    .guardian-section-toggle:hover { background: #263548; color: #e2e8f0; }
    .guardian-section-body { margin-bottom: 12px; }
    .guardian-section-body.collapsed { display: none; }
    .toggle-icon { font-size: 10px; }
    .guardian-sin-datos { font-size: 12px; color: #94a3b8; border: 1px dashed #334155; border-radius: 8px; padding: 8px 12px; margin-bottom: 8px; }
    .guardian-tabs-nav { display: flex; gap: 6px; overflow-x: auto; margin-bottom: 12px; padding-bottom: 2px; scrollbar-width: none; }
    .guardian-tabs-nav::-webkit-scrollbar { display: none; }
    .guardian-tab-btn { flex: 1 0 auto; background: #1e293b; color: #94a3b8; border: 1px solid #334155; border-radius: 8px;
      padding: 8px 12px; font-size: 12px; font-weight: 700; white-space: nowrap; }
    .guardian-tab-btn.active { background: #d4af37; color: #111; border-color: #d4af37; }
    body.light-mode .guardian-section-toggle, body.light-mode .guardian-tab-btn { background: #f1f5f9; color: #334155; border-color: #cbd5e1; }
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
