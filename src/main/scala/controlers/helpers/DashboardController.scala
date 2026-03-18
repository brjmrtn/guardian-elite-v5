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

    val content = basePage("home",
      div(
        // ── HERO HEADER (dark) ─────────────────────────────────────────────
        div(style := "background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%); border-radius:16px; padding:20px; margin-bottom:20px;",
          div(cls := "d-flex justify-content-between align-items-start flex-wrap gap-3",

            // Carta FUT compacta a la izquierda
            div(cls := "d-flex justify-content-center mobile-scale",
              style := "transform: scale(0.72); transform-origin: top left; margin-bottom:-30px;",
              div(cls := "fut-card",
                div(cls := "left-info",
                  div(cls := "rating", card.media),
                  div(cls := "position", card.posicion),
                  img(src := card.flagUrl, cls := "nation")),
                img(src := card.clubUrl, cls := "club-badge"),
                div(cls := "player-circle-container",
                  img(src := card.fotoUrl, cls := "player-img")),
                div(cls := "name-container",
                  div(cls := "player-name", card.nombre),
                  div(style := "font-size:12px; margin-top:-5px; opacity:.9; font-weight:bold;",
                    card.clubNombre)),
                div(cls := "stats-container",
                  div(cls := "stats-grid",
                    div(cls := "stat-item", span(cls := "stat-val", card.div), span(cls := "stat-label", "DIV")),
                    div(cls := "stat-item", span(cls := "stat-val", card.kic), span(cls := "stat-label", "KIC")),
                    div(cls := "stat-item", span(cls := "stat-val", card.spd), span(cls := "stat-label", "SPD")),
                    div(cls := "stat-item", span(cls := "stat-val", card.han), span(cls := "stat-label", "HAN")),
                    div(cls := "stat-item", span(cls := "stat-val", card.ref), span(cls := "stat-label", "REF")),
                    div(cls := "stat-item", span(cls := "stat-val", card.pos), span(cls := "stat-label", "POS")))))),

            // Stats rápidos a la derecha de la carta
            div(cls := "flex-fill",
              style := "min-width:160px;",
              // XP bar
              div(style := "margin-bottom:12px;",
                div(cls := "d-flex justify-content-between",
                  style := "font-size:10px; color:#94a3b8; margin-bottom:4px;",
                  span(s"Nv ${card.media}"),
                  span(s"${xpPercent}% XP"),
                  span(s"Nv ${card.media+1}")
                ),
                div(style := "height:6px; background:#334155; border-radius:3px;",
                  div(style := s"height:6px; width:$xpPercent%; background:#d4af37; border-radius:3px;"))
              ),
              // KPIs rápidos
              div(cls := "row g-2",
                frag(Seq(
                  (f"$avgLast5%2.1f", "RACHA 5", if(trendDiff>0)"#20c997" else if(trendDiff<0)"#ef4444" else "#94a3b8"),
                  (f"$acwr%1.2f",     "ACWR",    if(acwr>1.5)"#ef4444" else if(acwr>1.2)"#f59e0b" else "#20c997"),
                  (if(matches.nonEmpty) f"${matches.head.nota}%.1f" else "—", "ÚLTIMO", "#d4af37"),
                  (if(matches.nonEmpty) matches.head.resultado else "—", "RESULT", "#94a3b8")
                ).map { case (v, lbl, color) =>
                  div(cls := "col-6",
                    div(style := "background:#1e293b; border:1px solid #334155; border-radius:8px; padding:8px; text-align:center;",
                      div(style := s"font-size:1.1rem; font-weight:900; color:$color; line-height:1;", v),
                      div(style := "font-size:9px; color:#64748b; margin-top:2px;", lbl)
                    )
                  )
                }: _*)
              )
            )
          )
        ),

        // ── CONTENT AREA (light) ──────────────────────────────────────────
        div(cls := "row g-3",

          // COLUMNA IZQUIERDA
          div(cls := "col-md-6",

            // Próximo partido — PRIORIDAD 1
            nextMatchWidget,

            // Clean Sheet Predictor
            escudoWidget,

            // Alertas
            div(cls := "card bg-white border-0 shadow-sm mb-3",
              style := "border-radius:12px; overflow:hidden;",
              div(style := "background:#0f172a; padding:10px 14px; display:flex; justify-content:space-between; align-items:center;",
                span(style := "font-size:12px; font-weight:800; color:#fbbf24;", "🔔 ALERTAS GUARDIAN"),
                button(id := "btnActivarNotif",
                  style := "font-size:10px; font-weight:700; color:#fbbf24; background:rgba(251,191,36,.15); border:1px solid rgba(251,191,36,.4); padding:2px 8px; border-radius:4px; cursor:pointer;",
                  "ACTIVAR PUSH")
              ),
              div(style := "padding:8px;",
                div(id := "alertasContainer",
                  div(style := "font-size:12px; color:#94a3b8; text-align:center; padding:8px;",
                    "Cargando alertas...")
                )
              )
            ),

            // Auditor técnico
            techAuditorWidget
          ),

          // COLUMNA DERECHA
          div(cls := "col-md-6",

            // IA Neuro-Scout — PRIORIDAD 2
            div(cls := "card bg-white border-0 shadow-sm mb-3",
              style := "border-radius:12px; overflow:hidden;",
              div(style := "background: linear-gradient(135deg, #1e293b, #0f172a); padding:10px 14px; display:flex; align-items:center; gap:8px;",
                span(style := "font-size:18px;", "🧠"),
                span(style := "font-size:12px; font-weight:800; color:#38bdf8;", "IA NEURO-SCOUT")
              ),
              div(style := "padding:14px;",
                div(style := "font-size:12px; color:#334155; line-height:1.7; font-style:italic; font-weight:600;",
                  raw(aiMessage))
              )
            ),

            // Cognitivo
            div(cls := "card bg-white border-0 shadow-sm mb-3",
              style := "border-radius:12px; overflow:hidden;",
              div(style := "background:#0c4a6e; padding:10px 14px;",
                span(style := "font-size:12px; font-weight:800; color:#7dd3fc;", "🧠 ANALISTA COGNITIVO")
              ),
              div(style := "padding:12px;",
                p(style := "font-size:12px; color:#334155; font-weight:600; margin:0; text-align:center;",
                  raw(cognitiveInsight))
              )
            ),

            // KPIs técnicos
            div(cls := "card bg-white border-0 shadow-sm mb-3",
              style := "border-radius:12px; padding:14px;",
              div(style := "font-size:10px; font-weight:800; color:#64748b; margin-bottom:10px;",
                "RENDIMIENTO TÉCNICO"),
              div(cls := "row g-2 mb-3",
                frag(Seq(
                  ("CLIMA",      weatherStats.headOption.map(w => w._1).getOrElse("—"),        "#0ea5e9"),
                  ("NOTA CLIMA", weatherStats.headOption.map(w => f"${w._2._1}%.1f").getOrElse("—"), "#0ea5e9"),
                  ("INTELIGENCIA", smartInsights.take(30) + "...",          "#8b5cf6"),
                  ("ACWR HOY",   f"$acwr%.2f",                             if(acwr>1.5)"#ef4444" else "#20c997")
                ).take(4).map { case (lbl, v, color) =>
                  div(cls := "col-6",
                    div(style := s"background:#f8fafc; border-left:3px solid $color; border-radius:6px; padding:8px;",
                      div(style := "font-size:9px; font-weight:700; color:#94a3b8;", lbl),
                      div(style := s"font-size:12px; font-weight:800; color:#1e293b; margin-top:2px;",
                        v)
                    )
                  )
                }: _*)
              )
            ),

            // Radar + heatmap táctico
            div(cls := "row g-2 mb-3",
              div(cls := "col-6",
                div(cls := "card bg-white border-0 shadow-sm",
                  style := "border-radius:12px; padding:10px;",
                  div(style := "font-size:9px; font-weight:800; color:#64748b; text-align:center; margin-bottom:6px;",
                    "RADAR"),
                  div(style := "width:100%; height:160px;",
                    canvas(id := "radarChart"))
                )
              ),
              div(cls := "col-6",
                div(cls := "card bg-white border-0 shadow-sm",
                  style := "border-radius:12px; padding:10px;",
                  div(style := "font-size:9px; font-weight:800; color:#64748b; text-align:center; margin-bottom:4px;",
                    "GOLES/PARADAS"),
                  div(style := "font-size:8px; color:#94a3b8; text-align:center; margin-bottom:4px;",
                    "GC recibidos"),
                  div(cls := "d-flex mb-1",
                    tactCell("A", ga, "bg-danger bg-opacity-75"),
                    tactCell("M", gm, "bg-warning bg-opacity-75"),
                    tactCell("B", gr, "bg-light bg-opacity-75")),
                  div(cls := "d-flex",
                    tactCell("I", gl, "bg-danger bg-opacity-75"),
                    tactCell("C", gc_tact, "bg-warning bg-opacity-75"),
                    tactCell("D", gd, "bg-danger bg-opacity-75")),
                  div(style := "font-size:8px; color:#94a3b8; text-align:center; margin:4px 0 2px;",
                    "Paradas"),
                  div(cls := "d-flex mb-1",
                    tactCell("A", pa, "bg-success bg-opacity-75"),
                    tactCell("M", pm, "bg-info bg-opacity-75"),
                    tactCell("B", pr, "bg-light bg-opacity-75")),
                  div(cls := "d-flex",
                    tactCell("I", pl, "bg-success bg-opacity-75"),
                    tactCell("C", pc_tact, "bg-info bg-opacity-75"),
                    tactCell("D", pd, "bg-success bg-opacity-75"))
                )
              )
            ),

            // Acceso rápido — PRIORIDAD 4
            div(cls := "row g-2",
              frag(Seq(
                ("/scouting",       "🔍", "Scouting",   "#0ea5e9"),
                ("/penalties",      "⛳", "Penaltis",   "#ef4444"),
                ("/mapa-goles",     "🥅", "Mapa goles", "#3b82f6"),
                ("/nutrition",      "🥗", "Nutrición",  "#20c997"),
                ("/digital-twin",   "🤖", "Twin",       "#8b5cf6"),
                ("/market-estimator","💰","Mercado",    "#f59e0b")
              ).map { case (url, icon, label, color) =>
                div(cls := "col-4",
                  a(href := url, style := "text-decoration:none;",
                    div(style := s"background:#fff; border:1px solid #e2e8f0; border-top:3px solid $color; border-radius:10px; padding:10px 6px; text-align:center;",
                      div(style := "font-size:18px;", icon),
                      div(style := s"font-size:9px; font-weight:800; color:$color; margin-top:3px;",
                        label)
                    )
                  )
                )
              }: _*)
            )
          )
        )
      )
    )
    renderHtml(content)
  }

  // --- 2. MATCH CENTER (JUGAR) - VERSION CORREGIDA 4.1 ---

  @cask.get("/api/alertas")
  def apiAlertas(request: cask.Request) = withAuth(request) {
    val alerts = DatabaseManager.getNotificationAlerts()
    def esc(s: String): String =
      s.replace("\\", "\\\\").replace("\"", "'").replace("\r\n", " ").replace("\n", " ").replace("\r", " ").replace("\t", " ")
    val json = alerts.map { case (tipo, titulo, mensaje) =>
      "{\"tipo\":\"" + esc(tipo) + "\",\"titulo\":\"" + esc(titulo) + "\",\"mensaje\":\"" + esc(mensaje) + "\"}"
    }.mkString("[", ",", "]")
    cask.Response(json.getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json; charset=utf-8"))
  }
  initialize()
}
