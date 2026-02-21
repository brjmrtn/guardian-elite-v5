import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object DashboardController extends cask.Routes {

  @cask.get("/")
  def dashboard(request: cask.Request) = withAuth(request) {
    // 1. OBTENCION DE DATOS Y NUEVOS MOTORES (FASE 1)
    val techAlerts = DatabaseManager.getTechnicalAlerts() // Auditoria tecnica recurrente
    val weatherStats = DatabaseManager.getWeatherPerformance() // Correlacion nota vs clima
    val smartInsights = DatabaseManager.getSmartInsights()
    val card = DatabaseManager.getLatestCardData()
    val matches = DatabaseManager.getMatchesList()
    val chartData = DatabaseManager.getChartData()
    val aiMessage = DatabaseManager.getDeepAnalysis()
    val cognitiveInsight = DatabaseManager.getCognitiveInsight()

    val cognitiveWidget = div(cls:="card bg-dark border-info shadow mb-3",
      div(cls:="card-header border-info text-info fw-bold py-1 text-center small", "🧠 ANALISTA COGNITIVO"),
      div(cls:="card-body p-2",
        p(cls:="text-light small mb-0 text-center fw-bold", raw(cognitiveInsight))
      )
    )
    val tac = DatabaseManager.getTacticalStats()
    val objs = DatabaseManager.getSeasonObjectives()
    val upcoming = DatabaseManager.getUpcomingMatches().headOption
    val escudoData = upcoming.map(m => DatabaseManager.getCleanSheetPredictor(m.rival)).getOrElse(Map.empty[String,Any])
    val escudoProb = escudoData.getOrElse("prob", 0).asInstanceOf[Int]
    val escudoHoras = escudoData.getOrElse("horasSueno", 0.0).asInstanceOf[Double]
    val escudoAcwr  = escudoData.getOrElse("acwr", 1.0).asInstanceOf[Double]
    val escudoPcs   = escudoData.getOrElse("pcs", 0).asInstanceOf[Int]
    val escudoPj    = escudoData.getOrElse("pj", 0).asInstanceOf[Int]
    val (escudoColor, escudoLabel) = if (escudoProb >= 70) ("success", "ALTA")
    else if (escudoProb >= 45) ("warning", "MEDIA")
    else ("danger", "BAJA")
    val escudoWidget = if (upcoming.isEmpty) div() else {
      div(cls := "card bg-dark border-success shadow mb-3",
        div(cls := "card-header bg-success bg-opacity-10 border-success d-flex justify-content-between align-items-center py-2",
          span(cls := "text-success fw-bold small", "🛡️ ESCUDO CLEAN SHEET"),
          span(cls := s"badge bg-$escudoColor fw-bold", s"$escudoProb%")
        ),
        div(cls := "card-body p-3",
          div(cls := "d-flex align-items-center gap-3 mb-3",
            // Circulo probabilidad
            div(style := s"width:70px; height:70px; border-radius:50%; border:4px solid ${if(escudoColor=="success")"#28a745"else if(escudoColor=="warning")"#ffc107"else"#dc3545"}; display:flex; align-items:center; justify-content:center; flex-shrink:0;",
              div(style := s"font-size:20px; font-weight:700; color:${if(escudoColor=="success")"#28a745"else if(escudoColor=="warning")"#ffc107"else"#dc3545"};", s"$escudoProb%")
            ),
            div(
              div(cls := "fw-bold text-white", s"Probabilidad PORTERIAS A 0: $escudoLabel"),
              div(cls := "xx-small text-muted mt-1", s"vs ${upcoming.map(_.rival).getOrElse("")}")
            )
          ),
          div(cls := "row g-2",
            Seq(
              ("Historial cs", s"${if(escudoPj>0) escudoPcs else "—"}/${if(escudoPj>0) escudoPj else "—"}", if(escudoPj>0 && escudoPcs.toDouble/escudoPj>0.4)"success"else"secondary"),
      ("Sueno anoche", if(escudoHoras>0) f"${escudoHoras}%.1fh" else "—", if(escudoHoras>=8)"success"else if(escudoHoras>=6)"warning"else"secondary"),
      ("ACWR", if(escudoAcwr>0) f"${escudoAcwr}%.2f" else "—", if(escudoAcwr>1.5)"danger"else"success")
      ).map { case (lbl, v, c) =>
        div(cls := "col-4",
          div(cls := s"text-center p-1 rounded border border-$c bg-dark",
            div(cls := s"fw-bold text-$c small", v),
            div(cls := "xx-small text-muted", lbl)
          )
        )
      }
      )
      )
      )
    }

    // 2. CALCULOS DE TENDENCIAS Y XP
    val last5 = matches.take(5)
    val avgLast5 = if (last5.nonEmpty) last5.map(_.nota).sum / last5.length else 0.0
    val avgSeason = if (matches.nonEmpty) matches.map(_.nota).sum / matches.length else 0.0
    val trendDiff = avgLast5 - avgSeason
    val trendColor = if (trendDiff > 0) "text-success" else if (trendDiff < 0) "text-danger" else "text-muted"

    val radarData = s"""[${card.div}, ${card.han}, ${card.kic}, ${card.ref}, ${card.spd}, ${card.pos}]"""
    val rawMedia = (card.divRaw * 0.20) + (card.hanRaw * 0.20) + (card.kicRaw * 0.15) + (card.refRaw * 0.20) + (card.spdRaw * 0.05) + (card.posRaw * 0.20)
    val xpPercent = ((rawMedia - rawMedia.floor) * 100).toInt

    // 3. ESTADO FISICO (ACWR)
    val acute = DatabaseManager.getWorkloads(7)
    val chronic = DatabaseManager.getWorkloads(28)
    val acwr = StatsCalculator.calculateACWR(acute, chronic)
    val (acwrColor, acwrText) = if(acwr > 2.0) ("text-danger", "RIESGO ALTO")
    else if(acwr > 1.5) ("text-warning", "SOBRECARGA")
    else ("text-success", "OPTIMO")

    // 4. METODOS AUXILIARES Y CALCULOS TACTICOS
    // Definimos pct una sola vez como valor interno para evitar "ambiguous reference"
    val calculatePct = (n: Double, d: Double) => if(d > 0) ((n/d)*100).toInt else 0
    def tactCell(label: String, valPct: Int, colorBg: String) = div(cls:=s"flex-fill text-center p-1 border border-secondary $colorBg", style:="font-size: 10px; color: black; font-weight: 800;", div(label), div(s"$valPct%"))

    val totG = if(tac("g_tot") > 0) tac("g_tot").toDouble else 1.0
    val (ga, gm, gr) = (calculatePct(tac("g_alt"), totG), calculatePct(tac("g_med"), totG), calculatePct(tac("g_ras"), totG))
    val (gl, gc_tact, gd) = (calculatePct(tac("g_izq"), totG), calculatePct(tac("g_cen"), totG), calculatePct(tac("g_der"), totG))

    val totP = if(tac("p_tot") > 0) tac("p_tot").toDouble else 1.0
    val (pa, pm, pr) = (calculatePct(tac("p_alt"), totP), calculatePct(tac("p_med"), totP), calculatePct(tac("p_ras"), totP))
    val (pl, pc_tact, pd) = (calculatePct(tac("p_izq"), totP), calculatePct(tac("p_cen"), totP), calculatePct(tac("p_der"), totP))

    // --- WIDGETS DINAMICOS ---

    val nextMatchWidget = upcoming match {
      case None =>
        div(cls:="card bg-dark border-secondary shadow mb-3",
          div(cls:="card-body text-center py-4",
            div(style:="font-size:32px; opacity:0.3;", "📅"),
            div(cls:="text-muted small mt-2 fw-bold", "Sin partidos programados"),
            div(cls:="xx-small text-muted mt-1", "El calendario se cargara al inicio de la temporada")
          )
        )
      case Some(m) =>
        // Datos del rival del scouting
        val rivalInfo = DatabaseManager.getRivalInfo(m.rival)
        val (rivalMatches, rivalStats) = DatabaseManager.getRivalScouting(m.rival)
        val historial = if (rivalStats.getOrElse("pj", 0) > 0) {
          val pj = rivalStats("pj")
          val g  = rivalStats.getOrElse("ganados", 0)
          val p  = pj - g - rivalStats.getOrElse("empatados", 0)
          val e  = rivalStats.getOrElse("empatados", 0)
          s"$pj PJ | ${g}G ${e}E ${p}P"
        } else "Sin historial vs este rival"

        val tipoIcon = m.tipo match {
          case "TORNEO"  => "🏆"; case "COPA" => "🥇"; case _ => "⚽"
        }
        val tipoLabel = if (m.torneoNombre.nonEmpty) m.torneoNombre else m.tipo
        val faseLabel = if (m.fase.nonEmpty) s" | ${m.fase}" else ""
        val estadioLabel = if (m.estadio.nonEmpty) s"📍 ${fixEncoding(m.estadio)}" else "📍 Por confirmar"
        val estiloLabel = rivalInfo.map(r => s"Estilo: ${r.estilo}").getOrElse("")
        val clavesLabel = rivalInfo.map(r => fixEncoding(r.claves)).getOrElse("")

        div(cls:="card bg-dark border-warning shadow mb-3 overflow-hidden",
          // Header con tipo de partido
          div(cls:="card-header bg-warning bg-opacity-10 border-warning d-flex justify-content-between align-items-center py-2",
            span(cls:="text-warning fw-bold small", s"$tipoIcon PROXIMO PARTIDO — $tipoLabel$faseLabel"),
            span(cls:="badge bg-warning text-dark fw-bold", m.fecha)
          ),
          div(cls:="card-body p-3",
            // Rival + vs
            div(cls:="text-center mb-3",
              div(cls:="text-muted xx-small fw-bold mb-1", "RIVAL"),
              div(cls:="text-white fw-bold", style:="font-size:22px; letter-spacing:1px;", fixEncoding(m.rival).toUpperCase),
              div(cls:="text-muted small mt-1", estadioLabel)
            ),
            // Cuenta atras
            div(cls:="row g-2 mb-3 text-center",
              div(cls:="col-3", div(cls:="bg-secondary bg-opacity-25 rounded p-2",
                div(cls:="fw-bold text-warning fs-4", id:="cd-days", "—"),
                div(cls:="xx-small text-muted", "DIAS"))),
              div(cls:="col-3", div(cls:="bg-secondary bg-opacity-25 rounded p-2",
                div(cls:="fw-bold text-white fs-4", id:="cd-hours", "—"),
                div(cls:="xx-small text-muted", "HORAS"))),
              div(cls:="col-3", div(cls:="bg-secondary bg-opacity-25 rounded p-2",
                div(cls:="fw-bold text-white fs-4", id:="cd-mins", "—"),
                div(cls:="xx-small text-muted", "MIN"))),
              div(cls:="col-3", div(cls:="bg-secondary bg-opacity-25 rounded p-2",
                div(cls:="fw-bold text-white fs-4", id:="cd-secs", "—"),
                div(cls:="xx-small text-muted", "SEG")))
            ),
            // Scouting rapido si hay datos
            if (rivalInfo.isDefined || rivalStats.getOrElse("pj",0) > 0) {
              div(cls:="border-top border-secondary pt-2 mt-2",
                div(cls:="d-flex justify-content-between small",
                  div(cls:="text-muted fw-bold", historial),
                  if (estiloLabel.nonEmpty) div(cls:="badge bg-secondary", estiloLabel) else span()
                ),
                if (clavesLabel.nonEmpty)
                  div(cls:="xx-small text-muted mt-1 fst-italic", s"Claves: $clavesLabel")
                else span()
              )
            } else {
              div(cls:="xx-small text-muted text-center border-top border-secondary pt-2 mt-1 fst-italic",
                "Sin scouting previo — registralo en Historial > Scouting")
            },
            // Boton jugar
            div(cls:="d-grid mt-3",
              a(href:=s"/match-center?scheduleId=${m.id}", cls:="btn btn-warning fw-bold", "REGISTRAR PARTIDO →")
            )
          ),
          // Script cuenta atras
          script(raw(s"""
            (function() {
              var target = new Date("${m.fecha}T10:00:00");
              function tick() {
                var now = new Date();
                var diff = target - now;
                if (diff <= 0) {
                  document.getElementById('cd-days').textContent  = '0';
                  document.getElementById('cd-hours').textContent = '0';
                  document.getElementById('cd-mins').textContent  = '0';
                  document.getElementById('cd-secs').textContent  = '0';
                  return;
                }
                var d = Math.floor(diff / 86400000);
                var h = Math.floor((diff % 86400000) / 3600000);
                var m = Math.floor((diff % 3600000) / 60000);
                var s = Math.floor((diff % 60000) / 1000);
                document.getElementById('cd-days').textContent  = d;
                document.getElementById('cd-hours').textContent = String(h).padStart(2,'0');
                document.getElementById('cd-mins').textContent  = String(m).padStart(2,'0');
                document.getElementById('cd-secs').textContent  = String(s).padStart(2,'0');
              }
              tick();
              setInterval(tick, 1000);
            })();
          """))
        )
    }

    val techAuditorWidget = div(cls:="card bg-dark border-warning mb-3 shadow",
      div(cls:="card-header bg-warning text-dark small fw-bold text-center", "ℹ️ PLAN DE MEJORA (AUDITOR)"),
      div(cls:="card-body p-2",
        if(techAlerts.isEmpty) div(cls:="text-center p-2", span(cls:="text-success", "✅ Tecnica estable"), br, span(cls:="xx-small text-muted", "Sin fallos recurrentes detectados"))
        else ul(cls:="list-unstyled mb-0", for(alert <- techAlerts) yield li(cls:="border-bottom border-secondary py-1 small text-white", span(cls:="text-warning me-2", "⚡"), alert))
      )
    )

    val weatherPerformanceWidget = if(weatherStats.nonEmpty) {
      div(cls:="card bg-dark border-info shadow mb-3",
        div(cls:="card-header border-info text-info fw-bold py-1 text-uppercase text-center small", "🌤 RENDIMIENTO POR CLIMA"),
        div(cls:="card-body p-0",
          table(cls:="table table-dark table-sm mb-0 xx-small text-center",
            thead(tr(th("Clima"), th("Nota"), th("GC"))),
            tbody(for((clima, (nota, gc)) <- weatherStats.toSeq.take(3)) yield tr(td(clima), td(cls:="text-warning", f"$nota%1.1f"), td(cls:="text-danger", f"$gc%1.1f")))
          )
        )
      )
    } else div()

    // En GuardianServer.scala, dentro del dashboard:
    val cognitiveStatus = DatabaseManager.getCognitiveInsight()
    // Vault Medico
    val lastMedical = DatabaseManager.getLatestMedicalInsight() // Implementar consulta en DB

    val medicalAlertWidget = if(lastMedical.nonEmpty) {
      div(cls:="alert alert-danger border-danger shadow p-3 mb-3",
        div(cls:="d-flex align-items-center",
          span(style:="font-size: 24px; margin-right: 10px;", "🏥"),
          div(
            strong(cls:="text-danger", "ALERTA MEDICA"),
            div(cls:="small fw-bold", lastMedical)
          )
        )
      )
    } else div()


    // --- RENDERIZADO FINAL ---

    val content = basePage("home", div(cls := "row justify-content-center",
      div(cls := "col-md-5 mb-4",
        div(cls := "d-flex justify-content-center mobile-scale",
          div(cls := "fut-card",
            div(cls := "left-info", div(cls := "rating", card.media), div(cls := "position", card.posicion), img(src := card.flagUrl, cls := "nation")),
            img(src := card.clubUrl, cls := "club-badge"),
            div(cls := "player-circle-container", img(src := card.fotoUrl, cls := "player-img")),
            div(cls := "name-container", div(cls := "player-name", card.nombre), div(style:="font-size:12px; margin-top:-5px; opacity:0.9; font-weight:bold;", card.clubNombre)),
            div(cls := "stats-container", div(cls := "stats-grid",
              div(cls := "stat-item", span(cls:="stat-val", card.div), span(cls:="stat-label", "DIV")),
              div(cls := "stat-item", span(cls:="stat-val", card.kic), span(cls:="stat-label", "KIC")),
              div(cls := "stat-item", span(cls:="stat-val", card.spd), span(cls:="stat-label", "SPD")),
              div(cls := "stat-item", span(cls:="stat-val", card.han), span(cls:="stat-label", "HAN")),
              div(cls := "stat-item", span(cls:="stat-val", card.ref), span(cls:="stat-label", "REF")),
              div(cls := "stat-item", span(cls:="stat-val", card.pos), span(cls:="stat-label", "POS")))))),

        div(cls:="mt-3 mb-4 text-center",
          div(cls:="d-flex justify-content-between text-white xx-small px-4", span(s"Nivel ${card.media}"), span(s"${xpPercent}% XP"), span(s"Nivel ${card.media+1}")),
          div(cls:="progress mx-4", style:="height: 8px; background-color: #333;", div(cls:="progress-bar bg-warning", style:=s"width: $xpPercent%"))),

        techAuditorWidget,

        div(cls:="row g-2 mb-3",
          div(cls:="col-6", a(href:="/scouting", cls:="btn btn-outline-info w-100 shadow fw-bold d-flex flex-column align-items-center py-2", span(style:="font-size:20px;", "🔍"), span(style:="font-size:10px;", "SCOUTING"))),
          div(cls:="col-6", a(href:="/penalties", cls:="btn btn-outline-danger w-100 shadow fw-bold d-flex flex-column align-items-center py-2", span(style:="font-size:20px;", "⛳"), span(style:="font-size:10px;", "PENALTIS")))
        ),

        div(cls:="card bg-dark border-secondary shadow p-3 mb-3",
          div(cls:="d-flex justify-content-between align-items-center",
            div(h6(cls:="text-muted mb-0 small fw-bold", "RACHA (5)"), h3(cls:=s"mb-0 $trendColor fw-bold", f"$avgLast5%2.2f")),
            div(cls:="text-end", span(cls:="small text-muted fw-bold", "CARGA ACWR"), div(cls:=s"fw-bold $acwrColor", f"$acwr%1.2f")))
        )
      ),

      div(cls := "col-md-5",
        nextMatchWidget,
        escudoWidget,
        cognitiveWidget,
        div(cls := "alert alert-dark border-info shadow p-3 mb-3", div(cls:="d-flex align-items-center mb-2", span(style:="font-size: 24px; margin-right: 10px;", "🧠"), strong(cls:="text-info", "IA NEURO-SCOUT")), div(cls:="text-light small fst-italic lh-sm fw-bold", raw(aiMessage))),

        // Widget alertas + notificaciones
        div(cls := "card bg-dark border-warning shadow mb-3",
          div(cls := "card-header text-warning fw-bold small d-flex justify-content-between align-items-center",
            span("🔔 ALERTAS GUARDIAN"),
            button(id := "btnActivarNotif", cls := "btn btn-outline-warning btn-sm fw-bold xx-small",
              style := "font-size:10px; padding:2px 8px;",
              "ACTIVAR PUSH")
          ),
          div(cls := "card-body p-2",
            div(id := "alertasContainer",
              div(cls := "text-muted small text-center py-1 fw-bold", "Cargando alertas...")
            )
          )
        ),

        weatherPerformanceWidget,

        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header border-secondary text-warning fw-bold py-1 text-center small", "SCOUTING RADAR"), div(cls := "card-body p-1 d-flex justify-content-center", div(style:="width: 200px; height: 200px;", canvas(id := "radarChart")))),

        div(cls := "card bg-dark text-white border-danger shadow mb-3", div(cls := "card-header border-danger text-danger fw-bold py-1 text-center small", "🕵 INTELIGENCIA DE DATOS"), div(cls := "card-body p-2", raw(smartInsights))),

        div(cls:="row mt-3",
          div(cls:="col-6 pe-1", div(cls:="card bg-dark border-danger shadow p-1", h6(cls:="text-center text-danger mb-1 xx-small fw-bold", "GOLES RECIBIDOS"), div(cls:="d-flex mb-1", tactCell("A", ga, "bg-danger bg-opacity-75"), tactCell("M", gm, "bg-warning bg-opacity-75"), tactCell("B", gr, "bg-light bg-opacity-75")), div(cls:="d-flex", tactCell("I", gl, "bg-danger bg-opacity-75"), tactCell("C", gc_tact, "bg-warning bg-opacity-75"), tactCell("D", gd, "bg-danger bg-opacity-75")))),
          div(cls:="col-6 ps-1", div(cls:="card bg-dark border-success shadow p-1", h6(cls:="text-center text-success mb-1 xx-small fw-bold", "PARADAS"), div(cls:="d-flex mb-1", tactCell("A", pa, "bg-success bg-opacity-75"), tactCell("M", pm, "bg-info bg-opacity-75"), tactCell("B", pr, "bg-light bg-opacity-75")), div(cls:="d-flex", tactCell("I", pl, "bg-success bg-opacity-75"), tactCell("C", pc_tact, "bg-info bg-opacity-75"), tactCell("D", pd, "bg-success bg-opacity-75"))))))
    )
      , script(raw("""
      // ── NOTIFICACIONES PUSH GUARDIAN ELITE ──
      (function() {
        if (!('Notification' in window)) return;

        function solicitarPermiso() {
          Notification.requestPermission();
        }

        function lanzarNotif(titulo, cuerpo, tipo) {
          if (Notification.permission !== 'granted') return;
          var icono = tipo === 'danger' ? '🚨' : tipo === 'warning' ? '⚠️' : 'ℹ️';
          new Notification('GUARDIAN ELITE — ' + titulo, {
            body: icono + ' ' + cuerpo,
            icon: '/favicon.ico',
            tag: titulo
          });
        }

        // Solicitar permiso si aun no concedido
        if (Notification.permission === 'default') {
          setTimeout(solicitarPermiso, 2000);
        }

        // Mostrar boton de activar notifs si no hay permiso
        var btnNotif = document.getElementById('btnActivarNotif');
        if (btnNotif) {
          if (Notification.permission === 'granted') {
            btnNotif.style.display = 'none';
          } else {
            btnNotif.addEventListener('click', function() {
              Notification.requestPermission().then(function(p) {
                if (p === 'granted') btnNotif.style.display = 'none';
              });
            });
          }
        }

        // Lanzar alertas desde el servidor
        fetch('/api/alertas').then(function(r) { return r.json(); }).then(function(alertas) {
          var container = document.getElementById('alertasContainer');
          if (!container) return;
          if (alertas.length === 0) {
            container.innerHTML = '<div class="text-muted small text-center py-2 fw-bold">Sin alertas activas</div>';
            return;
          }
          alertas.forEach(function(a) {
            // Notif push
            lanzarNotif(a.titulo, a.mensaje, a.tipo);
            // Badge en pantalla
            var color = a.tipo === 'danger' ? '#dc3545' : a.tipo === 'warning' ? '#ffc107' : '#17a2b8';
            var textColor = a.tipo === 'warning' ? '#000' : '#fff';
            var icono = a.tipo === 'danger' ? '🚨' : a.tipo === 'warning' ? '⚠️' : '🔔';
            container.innerHTML += '<div style="background:' + color + '20; border-left:3px solid ' + color + '; padding:8px 10px; margin-bottom:6px; border-radius:4px;">' +
              '<div style="font-size:11px; font-weight:700; color:' + color + ';">' + icono + ' ' + a.titulo + '</div>' +
              '<div style="font-size:11px; color:#ccc; margin-top:2px;">' + a.mensaje + '</div>' +
              '</div>';
          });
        }).catch(function() {});
      })();
    """))
      , script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""const ctxRadar=document.getElementById('radarChart');if(ctxRadar){new Chart(ctxRadar,{type:'radar',data:{labels:['DIV','HAN','KIC','REF','SPD','POS'],datasets:[{data:$radarData,backgroundColor:'rgba(212,175,55,0.4)',borderColor:'#d4af37',borderWidth:2}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{r:{angleLines:{color:'#444'},grid:{color:'#444'},pointLabels:{color:'#fff',font:{size:10}},ticks:{display:false},suggestedMin:40,suggestedMax:90}}}});""")))
    renderHtml(content)
  }

  // --- 2. MATCH CENTER (JUGAR) - VERSION CORREGIDA 4.1 ---

  @cask.get("/api/alertas")
  def apiAlertas(request: cask.Request) = withAuth(request) {
    val alerts = DatabaseManager.getNotificationAlerts()
    val json = alerts.map { case (tipo, titulo, mensaje) =>
      s"""{"tipo":"$tipo","titulo":"${titulo.replace(""","")}","mensaje":"${mensaje.replace(""","")}"}"""
    }.mkString("[", ",", "]")
    cask.Response(json.getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json; charset=utf-8"))
  }

  initialize()
}
