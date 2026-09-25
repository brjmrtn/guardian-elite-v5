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
    val smartInsightsText = smartInsights.replaceAll("<[^>]+>", "").trim
    val card = DatabaseManager.getLatestCardData()
    // BLOQUE B5: los KPIs del dashboard (nota media, PJ, racha) se calculan sobre la temporada activa
    val temporadaActivaId = DatabaseManager.getTemporadaActivaId()
    val temporadaActivaNombre = DatabaseManager.getTodasTemporadas()
      .find(_("id").asInstanceOf[Int] == temporadaActivaId)
      .map(_("nombre").asInstanceOf[String]).getOrElse("Temporada actual")
    val matches = DatabaseManager.getMatchesList(temporadaActivaId)
    val chartData = DatabaseManager.getChartData()
    val aiMessage = DatabaseManager.getDeepAnalysis()
    val cognitiveInsight = DatabaseManager.getCognitiveInsight()

    // ── BLOQUE F: RIESGO DE LESION COMPUESTO (SQL puro, sin Gemini) ───────────
    val riesgoLesion = DatabaseManager.calcularRiesgoLesion()
    val riesgoValor = riesgoLesion("riesgo").asInstanceOf[Double]
    val riesgoClasificacion = riesgoLesion("clasificacion").asInstanceOf[String]
    val riesgoSemaforo = riesgoLesion("semaforo").asInstanceOf[String]
    val riesgoFactores = riesgoLesion("factoresActivos").asInstanceOf[List[String]]
    val riesgoEsAltoOCritico = riesgoClasificacion == "ALTO" || riesgoClasificacion == "CRITICO"

    // BLOQUE G3: alerta Telegram de riesgo CRITICO, maximo una vez al dia, en background
    if (riesgoClasificacion == "CRITICO" && !DatabaseManager.yaAlertadoRiesgoCriticoHoy()) {
      DatabaseManager.marcarRiesgoCriticoAlertadoHoy()
      val factoresTxt = riesgoFactores.mkString(", ")
      new Thread(new Runnable {
        def run(): Unit = TelegramService.enviar(s"🔴 ALERTA GUARDIAN: Riesgo de lesión CRITICO. Factores: $factoresTxt")
      }).start()
    }

    // ── BLOQUE F2: alerta de maxima prioridad si el riesgo es CRITICO ─────────
    val riesgoCriticoAlert: Modifier =
      if (riesgoClasificacion == "CRITICO")
        div(cls := "alert alert-danger fw-bold shadow mb-3", style := "border-left:6px solid #dc3545;",
          s"🔴 RIESGO DE LESIÓN CRÍTICO — factores: ${riesgoFactores.mkString(", ")}")
      else div()

    val riesgoLesionWidget: Modifier = {
      val explicacion: Modifier =
        if (riesgoEsAltoOCritico) div(cls := "xx-small mt-1", style := "color:#fca5a5;",
          "Considera hablar con el entrenador sobre la carga de esta semana.")
        else div()
      div(cls := "card bg-dark border-secondary shadow-sm mb-3 p-2",
        attr("title") := riesgoFactores.mkString(", "),
        div(cls := "d-flex justify-content-between align-items-center",
          span(style := "font-size:11px; color:#94a3b8;", "🩹 RIESGO DE LESIÓN"),
          span(style := "font-size:16px; font-weight:900; color:#fff;", f"$riesgoSemaforo $riesgoValor%.1f — $riesgoClasificacion")
        ),
        explicacion,
        div(cls := "xx-small mt-1", style := "color:#64748b;", DatabaseManager.disclaimerACWR)
      )
    }

    // ── BLOQUE 5.6: DETECTOR DE DESGASTE SILENCIOSO (prioridad maxima) ────────
    val desgasteWidget: Modifier = DatabaseManager.detectarDesgasteSilencioso() match {
      case Some(msg) => div(cls := "alert alert-danger fw-bold shadow mb-3", style := "border-left:6px solid #dc3545;", msg)
      case None => div()
    }

    // ── BLOQUE O: ENFERMEDAD INCIPIENTE (FC sube + energia y animo bajan, SQL puro) ──
    val enfermedadWidget: Modifier = DatabaseManager.detectarEnfermedadIncipiente() match {
      case Some(msg) => div(cls := "alert alert-warning small p-2 mb-3", style := "border-left:6px solid #facc15;", msg)
      case None => div()
    }

    // ── BLOQUE B2: PENDIENTE DE REGISTRAR (estructura semanal, SQL puro) ──────
    val pendienteWidget: Modifier = {
      val pendientes = DatabaseManager.getSemanaIncompleta()
      if (pendientes.isEmpty) div()
      else div(cls := "card bg-secondary bg-opacity-25 border-secondary shadow-sm mb-3 p-2",
        div(cls := "xx-small text-muted fw-bold mb-1", "📋 PENDIENTE DE REGISTRAR"),
        div(cls := "xx-small text-light", pendientes.map(p => div(p)))
      )
    }

    // ── BLOQUE 2.7: ALERTAS DE TEMPORADA (SQL puro) ───────────────────────────
    val temporadaAlertWidget: Modifier = DatabaseManager.getTemporadaActivaInfo() match {
      case Some(t) if t("fechaFin").asInstanceOf[String].nonEmpty =>
        div(cls := "alert alert-warning small p-2 mb-3", s"📅 La temporada ${t("nombre")} está cerrada — ve a Admin para iniciar la nueva.")
      case Some(t) if t("fechaInicio").asInstanceOf[String].nonEmpty &&
        scala.util.Try(java.time.LocalDate.parse(t("fechaInicio").asInstanceOf[String])).toOption
          .exists(_.isBefore(java.time.LocalDate.now().minusMonths(11))) =>
        div(cls := "alert alert-warning small p-2 mb-3", "📅 La temporada lleva más de 11 meses activa. ¿Es momento de cerrarla?")
      case _ => div()
    }

    // ── BLOQUE 4.2: ULTIMA ACADEMIA (feedback del entrenador, ultimos 7 dias) ──
    val ultimaAcademiaWidget: Modifier = DatabaseManager.getUltimoFeedbackEntrenador() match {
      case Some(fb) => div(cls := "card bg-dark text-white border-info shadow-sm mb-3 p-3",
        div(style := "font-size:11px; color:#7dd3fc; letter-spacing:1px;", "🎓 ÚLTIMA ACADEMIA"),
        div(cls := "small mt-1", fb))
      case None => div()
    }

    // ── BLOQUE E3: TIP PARA EL ENTRENADOR DE ACADEMIA (video IA reciente, sin Gemini aqui) ──
    val academiaVideoTipWidget: Modifier = {
      val histVideo = DatabaseManager.getVideoAnalysisHistoryAll()
      val hayReciente = histVideo.lastOption.exists { h =>
        scala.util.Try(java.time.LocalDate.parse(h("fecha").asInstanceOf[String])).toOption
          .exists(_.isAfter(java.time.LocalDate.now().minusDays(30)))
      }
      if (!hayReciente) div()
      else DatabaseManager.getUltimoErrorRecurrente() match {
        case Some(error) => div(cls := "card bg-dark text-white border-warning shadow-sm mb-3 p-3",
          div(cls := "small", s"💡 Para comentar al entrenador de academia: la IA detectó en vídeo que $error. ¿Podría trabajarlo este domingo?"))
        case None => div()
      }
    }

    // ── BLOQUE 5.4: FOCO DE ESTA SEMANA (micro-objetivo) ──────────────────────
    val microObjetivo = DatabaseManager.getMicroObjetivoSemana()
    // sufijo: el widget aparece en las dos pestanas y los id del checkbox no pueden repetirse
    def microObjetivoCard(sufijo: String): Modifier = {
      val completado = microObjetivo("completado").asInstanceOf[Boolean]
      div(cls := "card bg-dark text-white border-info shadow-sm mb-3 p-3",
        div(cls := "d-flex justify-content-between align-items-start",
          div(
            div(style := "font-size:11px; color:#7dd3fc; letter-spacing:1px;", "🎯 FOCO DE ESTA SEMANA"),
            div(cls := "small fw-bold mt-1", microObjetivo("objetivo").asInstanceOf[String])
          )
        ),
        form(action := "/micro-objetivo/completar", method := "post", cls := "mt-2",
          div(cls := "form-check mb-2",
            input(cls := "form-check-input", tpe := "checkbox", name := "completado", id := s"microCompletado$sufijo",
              if (completado) attr("checked") := "checked" else frag()),
            label(`for` := s"microCompletado$sufijo", cls := "form-check-label xx-small", "Completado")
          ),
          input(tpe := "text", name := "resultado", cls := "form-control form-control-sm mb-2",
            placeholder := "Observación (opcional)", value := microObjetivo("resultado").asInstanceOf[String]),
          button(tpe := "submit", cls := "btn btn-sm btn-outline-info fw-bold", "Guardar")
        )
      )
    }

    val microObjetivoWidget: Modifier = microObjetivoCard("")

    // ── BLOQUE 5.5: PREPARACION SEMANAL (solo lectura de cache, nunca Gemini aqui) ─
    val preparacionWidget: Modifier = DatabaseManager.getPreparacionSemanalCache() match {
      case Some(texto) =>
        val partes = texto.split("/").map(_.trim)
        def parte(prefijo: String): String = partes.find(_.startsWith(prefijo)).map(_.drop(prefijo.length).trim).getOrElse("")
        div(cls := "row g-2 mb-3",
          div(cls := "col-4", div(cls := "card bg-secondary bg-opacity-25 border-0 p-2 h-100",
            div(cls := "xx-small text-warning fw-bold", "🚗 De camino al campo"),
            div(cls := "xx-small text-white mt-1", parte("CONSIGNA_COCHE:")))),
          div(cls := "col-4", div(cls := "card bg-secondary bg-opacity-25 border-0 p-2 h-100",
            div(cls := "xx-small text-warning fw-bold", "🏠 Entrenamiento invisible"),
            div(cls := "xx-small text-white mt-1", parte("DESCANSO_CASA:")))),
          div(cls := "col-4", div(cls := "card bg-secondary bg-opacity-25 border-0 p-2 h-100",
            div(cls := "xx-small text-warning fw-bold", "👁️ Tu ojo en la grada"),
            div(cls := "xx-small text-white mt-1", parte("FOCO_SABADO:"))))
        )
      case None =>
        div(cls := "d-grid mb-3",
          form(action := "/preparacion-semanal/generar", method := "post",
            button(tpe := "submit", cls := "btn btn-sm btn-outline-warning fw-bold w-100", "🧠 Generar preparación semanal")
          )
        )
    }

    // ── BLOQUE A3: INDICE DE FORMA DIARIO (solo SQL/matematicas, sin Gemini) ──
    val formaHoy = DatabaseManager.calcularFormaHoy()
    val esDiaDePartido = DatabaseManager.hayPartidoProximo()
    val formaWidget: Modifier = {
      val indice = formaHoy("indiceForma").asInstanceOf[Double]
      val semaforo = if (indice >= 7.5) "🟢" else if (indice >= 5.0) "🟡" else "🔴"
      val frase =
        if (indice >= 8.5) "Condiciones óptimas — partido ideal para rendir al máximo"
        else if (indice >= 7.0) "Buenas condiciones — rendimiento sólido esperado"
        else if (indice >= 5.0) "Condiciones normales — rendimiento estándar"
        else if (indice >= 3.0) "Señales de fatiga — considera hablar con el entrenador"
        else "Recuperación incompleta — vigilar durante el partido"

      val tieneFcHoy = formaHoy.get("tieneFcHoy").exists(_.asInstanceOf[Boolean])
      val fcScoreOpt = formaHoy.get("fcScore").flatMap(_.asInstanceOf[Option[Double]])

      val componentesBase = Seq(
        ("Sueño", formaHoy("suenoScore").asInstanceOf[Double], "#0dcaf0"),
        ("Energía", formaHoy("energiaScore").asInstanceOf[Double], "#20c997"),
        ("Ánimo", formaHoy("animoScore").asInstanceOf[Double], "#ffc107"),
        ("Carga", formaHoy("acwrScore").asInstanceOf[Double], "#fd7e14"),
        ("Descanso", formaHoy("descansoScore").asInstanceOf[Double], "#8b5cf6"),
        ("PHV", formaHoy("phvScore").asInstanceOf[Double], "#dc3545")
      )
      // BLOQUE A: mini-barra "FC reposo" adicional si hay dato de hoy
      val componentes = if (tieneFcHoy)
        componentesBase :+ (("FC reposo", fcScoreOpt.getOrElse(7.0), "#e83e8c"))
      else componentesBase
      val numCols = componentes.size
      val miniBars = componentes.map { case (label, valor, color) =>
        div(cls := "col",
          div(cls := "xx-small text-center text-muted", label),
          div(cls := "progress", style := "height:6px;",
            div(cls := "progress-bar", style := s"width:${valor * 10}%; background:$color;")
          ),
          div(cls := "xx-small text-center fw-bold", style := s"color:$color;", f"$valor%.1f")
        )
      }

      if (esDiaDePartido)
        div(style := "background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%); border-radius:16px; padding:16px; margin-bottom:16px; border:1px solid #334155;",
          div(cls := "d-flex justify-content-between align-items-center mb-2",
            div(
              div(style := "font-size:11px; color:#94a3b8; letter-spacing:1px;", "FORMA HOY"),
              div(style := "font-size:32px; font-weight:900; color:#fff;", f"$semaforo $indice%.1f")
            )
          ),
          div(cls := "small fw-bold mb-2", style := "color:#e2e8f0;", frase),
          div(cls := s"row row-cols-$numCols g-1", miniBars)
        )
      else
        div(style := "background:#1e293b; border-radius:12px; padding:10px 14px; margin-bottom:16px; border:1px solid #334155;",
          div(cls := "d-flex justify-content-between align-items-center",
            span(style := "font-size:11px; color:#94a3b8;", "FORMA HOY"),
            span(style := "font-size:18px; font-weight:900; color:#fff;", f"$semaforo $indice%.1f")
          ),
          div(cls := s"row row-cols-$numCols g-1 mt-1", miniBars)
        )
    }

    // ── BLOQUE G: FORMA PROYECTADA PARA EL SABADO (miercoles/jueves, SQL puro) ──
    val formaProyectadaWidget: Modifier = DatabaseManager.diasHastaPartidoSabado() match {
      case Some(dias) =>
        val pred = DatabaseManager.predecirFormaPartido(dias)
        if (!pred("disponible").asInstanceOf[Boolean]) div()
        else {
          val ind = pred("indice").asInstanceOf[Double]
          val sem = pred("semaforo").asInstanceOf[String]
          div(cls := "card bg-dark border-secondary shadow-sm mb-3 p-2",
            div(cls := "xx-small", style := "color:#e2e8f0;",
              f"📊 Forma proyectada para el sábado: $sem $ind%.1f ",
              span(cls := "text-muted", "(si el sueño y la carga son normales estos días)")),
            if (sem == "🔴") div(cls := "xx-small fw-bold mt-1", style := "color:#fca5a5;",
              "⚠️ Si el sueño no mejora estos días, Héctor llegará al partido con Índice de Forma en rojo.")
            else frag()
          )
        }
      case None => div()
    }

    // ── BLOQUE A6 (RFMF): RESULTADOS DETECTADOS PENDIENTES DE CONFIRMAR ──────
    val rfmfPendientes = DatabaseManager.getPartidosRFMFPendientes()
    val rfmfPendientesWidget: Modifier =
      if (rfmfPendientes.isEmpty) div()
      else div(rfmfPendientes.map { m =>
        val id = m("id").asInstanceOf[Int]
        val rival = m("rival").asInstanceOf[String]
        val gf = m("golesFavor").asInstanceOf[Int]
        val gc = m("golesContra").asInstanceOf[Int]
        div(cls := "card bg-dark border-warning shadow-sm mb-3 p-2",
          div(cls := "xx-small text-warning fw-bold", s"📋 RESULTADO DETECTADO en RFMF: vs $rival · $gf-$gc · ¿Confirmar?"),
          div(cls := "d-flex gap-2 mt-1",
            form(action := s"/match/rfmf-pendiente/$id/confirmar", method := "post", cls := "flex-fill",
              button(tpe := "submit", cls := "btn btn-sm btn-warning fw-bold w-100", "✅ Confirmar")),
            form(action := s"/match/rfmf-pendiente/$id/descartar", method := "post", cls := "flex-fill",
              button(tpe := "submit", cls := "btn btn-sm btn-outline-secondary fw-bold w-100", "✕ No es este"))
          )
        )
      })

    // ── MODULO LA VOZ DEL PORTERO: recordatorio mensual (SQL puro, sin Gemini) ─
    val vozPorteroWidget: Modifier =
      if (!DatabaseManager.debeRecordarVozPortero()) div()
      else div(cls := "card bg-dark border-warning shadow-sm mb-3 p-2",
        a(href := "/voz-portero", cls := "text-decoration-none xx-small text-warning fw-bold",
          "🎤 Este mes aún no has registrado La Voz del Portero — tarda 2 minutos.")
      )

    // ── MODULO ARQUETIPO: badge junto a los KPIs (SQL puro, sin Gemini) ───────
    val arquetipoWidget: Modifier = {
      val arq = DatabaseManager.calcularArquetipoPortero()
      if (!arq("activo").asInstanceOf[Boolean]) div()
      else {
        val desc = DatabaseManager.arquetipoDescripcion(arq("dominante").asInstanceOf[String])
        div(cls := "d-grid mb-3",
          a(href := "/arquetipo", cls := "btn btn-sm btn-outline-light fw-bold text-start",
            s"🎭 ${desc("emoji")} ${desc("nombre")} — ver arquetipo completo →")
        )
      }
    }

    // ── BLOQUE RFFM: BENCHMARK REAL VS CATEGORIA (SQL puro, sin Gemini) ───────
    val rffmWidget: Modifier = DatabaseManager.getPercentilRealHector() match {
      case Some(p) =>
        val totalEquipos = p("totalEquipos").asInstanceOf[Int]
        val mediaGc = p("mediaGcHector").asInstanceOf[Double]
        val percentil = p("percentilGC").asInstanceOf[Int]
        val pctLimpiasH = p("pctLimpiasHector").asInstanceOf[Double]
        val pctLimpiasCat = p("pctLimpiasCategoria").asInstanceOf[Double]
        div(cls := "card bg-dark border-secondary shadow-sm mb-3 p-2",
          div(cls := "xx-small text-muted fw-bold", "📊 VS CATEGORÍA (RFFM Madrid)"),
          div(cls := "xx-small text-white mt-1", f"GC/partido: $mediaGc%.1f · Percentil $percentil de $totalEquipos equipos"),
          div(cls := "xx-small text-white", f"Porterías a cero: $pctLimpiasH%.0f%% · Categoría: $pctLimpiasCat%.0f%%")
        )
      case None => div()
    }

    // ── BLOQUE I: SKILLS DETECTADAS EN EL FEEDBACK DE ACADEMIA (sin Gemini) ────
    val sugerenciasSkill = DatabaseManager.getSugerenciasSkillPendientes()
    val sugerenciasSkillWidget: Modifier =
      if (sugerenciasSkill.isEmpty) div()
      else div(sugerenciasSkill.map { s =>
        val skillId = s("skillId").asInstanceOf[Int]
        val habilidad = s("habilidad").asInstanceOf[String]
        div(cls := "card bg-dark border-info shadow-sm mb-3 p-2",
          div(cls := "xx-small text-info fw-bold", s"💡 El entrenador trabajó $habilidad — ¿actualizar el checklist?"),
          div(cls := "d-flex gap-2 mt-1",
            form(action := s"/skills/sugerencia/$skillId/confirmar", method := "post", cls := "flex-fill",
              button(tpe := "submit", cls := "btn btn-sm btn-info fw-bold w-100", "✅ Marcar conseguida")),
            form(action := s"/skills/sugerencia/$skillId/descartar", method := "post", cls := "flex-fill",
              button(tpe := "submit", cls := "btn btn-sm btn-outline-secondary fw-bold w-100", "✕ Aún no"))
          )
        )
      })

    // ── BLOQUE E: HITOS CONSEGUIDOS EN LOS ULTIMOS 7 DIAS (SQL puro, sin Gemini) ──
    val hitosRecientes = DatabaseManager.getHitosRecientes(7)
    val hitosWidget: Modifier =
      if (hitosRecientes.isEmpty) div()
      else div(cls := "card bg-dark border-warning shadow mb-3 p-3", style := "border-left:6px solid #d4af37;",
        hitosRecientes.map { h =>
          div(cls := "small fw-bold", style := "color:#facc15;", s"🏆 NUEVO HITO CONSEGUIDO: ${h("descripcion")}")
        }
      )

    // ── BLOQUE A: DEUDA DE SUENO ACUMULADA SEMANAL (SQL puro, sin Gemini) ─────
    val deudaSueno = formaHoy("deudaSueno").asInstanceOf[Map[String, Any]]
    val deudaNivel = deudaSueno("nivel").asInstanceOf[String]
    val deudaHorasDash = deudaSueno("deudaHoras").asInstanceOf[Double]
    val deudaMediaDash = deudaSueno("mediaDiaria").asInstanceOf[Double]
    val deudaSuenoWidget: Modifier = deudaNivel match {
      case "MODERADA" =>
        div(cls := "card bg-dark border-info shadow-sm mb-3 p-2",
          div(cls := "xx-small", style := "color:#e2e8f0;",
            raw(f"💤 Deuda de sueño esta semana: <strong>${deudaHorasDash}%.1fh</strong> — media de ${deudaMediaDash}%.1fh/noche")))
      case "ALTA" =>
        div(cls := "card bg-dark border-warning shadow-sm mb-3 p-2",
          div(cls := "xx-small", style := "color:#fde68a;",
            raw(f"⚠️ Deuda de sueño alta: <strong>${deudaHorasDash}%.1fh acumuladas</strong> (${deudaMediaDash}%.1fh/noche de media)")))
      case "CRITICA" =>
        div(cls := "alert alert-danger fw-bold shadow mb-3", style := "border-left:6px solid #dc3545;",
          raw(f"🔴 Deuda de sueño crítica: <strong>${deudaHorasDash}%.1fh acumuladas</strong> esta semana"))
      case _ => div()
    }

    // ── BLOQUE C: MODO DIA DE PARTIDO (SQL/cache, sin Gemini en el render) ────
    val diaPartidoBanner: Modifier = if (!esDiaDePartido) div() else {
      val indice = formaHoy("indiceForma").asInstanceOf[Double]
      val semaforo = if (indice >= 7.5) "🟢" else if (indice >= 5.0) "🟡" else "🔴"
      val tieneFcHoy = formaHoy.get("tieneFcHoy").exists(_.asInstanceOf[Boolean])
      val fcScoreOpt = formaHoy.get("fcScore").flatMap(_.asInstanceOf[Option[Double]])
      val fcLinea: Modifier = if (tieneFcHoy) {
        val fc = fcScoreOpt.getOrElse(7.0)
        val interp = if (fc >= 9.0) "excelente descanso" else if (fc >= 6.0) "descanso normal"
                     else if (fc >= 3.0) "FC reposo elevada — vigilar cansancio" else "FC reposo muy elevada — posible fatiga acumulada"
        div(cls := "xx-small", style := "color:#cbd5e1;", s"❤️ FC reposo hoy: $interp")
      } else div()

      // BLOQUE A: deuda de sueno en el modo dia de partido si es ALTA o CRITICA
      val deudaLinea: Modifier = if (deudaNivel == "ALTA" || deudaNivel == "CRITICA")
        div(cls := "xx-small fw-bold", style := "color:#fda4af;",
          f"💤 Deuda de sueño de la semana: ${deudaHorasDash}%.1fh — puede afectar concentración y reflejos.")
      else div()

      // BLOQUE F3: aviso de riesgo de lesion en el modo dia de partido
      val riesgoLinea: Modifier = if (riesgoEsAltoOCritico)
        div(cls := "xx-small fw-bold mt-1", style := "color:#fca5a5;",
          "⚠️ El riesgo de lesión hoy es ALTO — considera hablar con el entrenador sobre la intensidad del calentamiento.")
      else div()

      val info = DatabaseManager.getDiaPartidoInfo()
      val hayRival = info.getOrElse("hayRivalProgramado", false).asInstanceOf[Boolean]

      val rivalCard: Modifier = if (!hayRival) div() else {
        val rival = info("rival").asInstanceOf[String]
        val pj = info("pj").asInstanceOf[Int]
        val notaMedia = info("notaMedia").asInstanceOf[Double]
        val ultimo = info("ultimoResultado").asInstanceOf[String]
        val tipoPartido = info("tipoPartido").asInstanceOf[String]
        val torneoNombre = info("torneoNombre").asInstanceOf[String]
        val fase = info("fase").asInstanceOf[String]
        val estilo = info("estilo").asInstanceOf[Option[String]]
        val arquetipo = info("arquetipo").asInstanceOf[Option[String]]
        val torneoLinea: Modifier = if (tipoPartido == "TORNEO" && torneoNombre.nonEmpty)
          div(cls := "xx-small fw-bold", style := "color:#facc15;", s"🏆 ${torneoNombre.toUpperCase}${if (fase.nonEmpty) s" — $fase" else ""}")
        else div()
        div(cls := "mt-2 pt-2", style := "border-top:1px solid #334155;",
          div(style := "font-size:11px; color:#94a3b8; letter-spacing:1px;", "RIVAL DE HOY"),
          div(cls := "fw-bold", style := "color:#fff; font-size:18px;", rival),
          torneoLinea,
          if (pj > 0) div(cls := "xx-small", style := "color:#cbd5e1;",
            s"PJ $pj · nota media ${f"$notaMedia%.1f"}" + (if (ultimo.nonEmpty) s" · último resultado $ultimo" else ""))
          else div(cls := "xx-small text-muted", "Primer partido registrado contra este rival"),
          estilo.map(e => div(cls := "xx-small", style := "color:#cbd5e1;", s"Estilo: $e")).getOrElse(div()),
          arquetipo.map(a => div(cls := "xx-small", style := "color:#cbd5e1;", s"Tipo de delantero: $a")).getOrElse(div())
        )
      }

      val prepCache = DatabaseManager.getPreparacionSemanalCache()
      def parteCache(prefijo: String): String = prepCache.map { texto =>
        texto.split("/").map(_.trim).find(_.startsWith(prefijo)).map(_.drop(prefijo.length).trim).getOrElse("")
      }.getOrElse("")
      val consignaCoche = parteCache("CONSIGNA_COCHE:")
      val focoGrada = parteCache("FOCO_SABADO:")
      val comidaPrePartido = "Comida ligera y rica en carbohidratos 2-3h antes: pasta, arroz o plátano. Evita fritos y exceso de fibra."

      val tarjetas = Seq(
        ("🚗", "CONSIGNA COCHE", if (consignaCoche.nonEmpty) consignaCoche else "Genera la preparación semanal para ver una consigna personalizada."),
        ("🍽️", "COMIDA PRE-PARTIDO", comidaPrePartido),
        ("👁️", "FOCO EN LA GRADA", if (focoGrada.nonEmpty) focoGrada else "Genera la preparación semanal para ver el foco de hoy.")
      ).map { case (icono, titulo, texto) =>
        div(cls := "col-12 col-md-4",
          div(cls := "p-2 rounded h-100", style := "background:rgba(255,255,255,0.05);",
            div(cls := "xx-small fw-bold", style := "color:#facc15;", s"$icono $titulo"),
            div(cls := "xx-small mt-1", style := "color:#e2e8f0;", texto)
          )
        )
      }

      val microObjetivoLinea: Modifier = {
        val obj = microObjetivo("objetivo").asInstanceOf[String]
        if (obj.nonEmpty) div(cls := "xx-small mt-2", style := "color:#cbd5e1;", s"🎯 Micro-objetivo: $obj") else div()
      }

      // BLOQUE E: coincidencia con las condiciones de rendimiento pico historicas — SQL puro
      val condicionesPicoLinea: Modifier = DatabaseManager.getCoincidenciaConCondicionesPico() match {
        case Some(pct) => div(cls := "xx-small mt-2", style := "color:#cbd5e1;", s"🎯 Coincidencia con condiciones pico: $pct%")
        case None => div()
      }

      val botonRegistrar = hayRival match {
        case true =>
          val scheduleId = info("scheduleId").asInstanceOf[Int]
          a(href := s"/match-center?scheduleId=$scheduleId", cls := "btn btn-warning fw-bold w-100 mt-3", "⚽ REGISTRAR PARTIDO")
        case false =>
          a(href := "/match-center", cls := "btn btn-warning fw-bold w-100 mt-3", "⚽ REGISTRAR PARTIDO")
      }

      // ── BLOQUE B1: AUTOPERCEPCION DE HECTOR PRE-PARTIDO ───────────────────
      val autopercepcionHoy = DatabaseManager.getAutopercepcionTemporalHoy()
      val autopercepcionWidget: Modifier = div(cls := "mt-2 pt-2", style := "border-top:1px solid #334155;",
        div(cls := "xx-small fw-bold mb-1", style := "color:#facc15;", "🎯 ¿Cómo dice Héctor que se encuentra hoy? (pregúntale de camino al campo)"),
        div(cls := "d-flex gap-1",
          Seq((1, "😞"), (2, "😕"), (3, "😐"), (4, "🙂"), (5, "😃")).map { case (v, emoji) =>
            val activo = autopercepcionHoy.contains(v)
            button(tpe := "button", id := s"dashAutop$v",
              cls := s"btn btn-sm flex-fill ${if (activo) "btn-warning" else "btn-outline-light"}",
              onclick := s"registrarAutopercepcionDashboard($v)", s"$emoji $v")
          }
        )
      )

      div(id := "diaPartidoBanner",
        style := "background: linear-gradient(135deg, #451a03 0%, #1e293b 100%); border-radius:16px; padding:18px; margin-bottom:16px; border:1px solid #d4af37;",
        div(cls := "d-flex justify-content-between align-items-start",
          div(
            div(style := "font-size:13px; color:#facc15; letter-spacing:1px; font-weight:900;", "🏟️ HOY ES DÍA DE PARTIDO"),
            div(cls := "d-flex align-items-center gap-2 mt-1",
              span(style := "font-size:24px; font-weight:900; color:#fff;", f"$semaforo $indice%.1f"),
              span(cls := "xx-small text-muted", "ÍNDICE DE FORMA")
            ),
            fcLinea,
            deudaLinea,
            riesgoLinea
          ),
          span(style := "cursor:pointer; color:#94a3b8; font-size:18px;", onclick := "cerrarDiaPartidoBanner()", "✕")
        ),
        autopercepcionWidget,
        rivalCard,
        div(cls := "row g-2 mt-2", tarjetas),
        microObjetivoLinea,
        condicionesPicoLinea,
        botonRegistrar
      )
    }

    // BLOQUE C3: aviso post-partido si son las 15:00+ del dia de partido y aun no hay registro
    val postPartidoWidget: Modifier = if (!esDiaDePartido) div() else {
      val ahora = java.time.LocalTime.now()
      DatabaseManager.getPartidoHoyRegistrado() match {
        case None if ahora.isAfter(java.time.LocalTime.of(15, 0)) =>
          div(cls := "alert alert-warning small p-2 mb-3", style := "border-left:6px solid #fd7e14;",
            "⏳ ¿Ya terminó el partido? Regístralo antes de que se enfríe el recuerdo. ",
            a(href := "/match-center", cls := "fw-bold", "Registrar ahora →"))
        case Some(matchId) =>
          DatabaseManager.getGuiaConversacion(matchId) match {
            case Some(guia) if guia.nonEmpty =>
              val partes = guia.split("/").map(_.trim)
              def parte(p: String): String = partes.find(_.startsWith(p)).map(_.drop(p.length).trim).getOrElse("")
              div(cls := "card bg-dark border-success shadow-sm mb-3 p-3",
                div(cls := "xx-small fw-bold text-success mb-2", "💬 CLAVES PARA LA COMIDA DE HOY"),
                div(cls := "xx-small mb-1", strong("Resalta: "), parte("QUE_RESALTAR:")),
                div(cls := "xx-small mb-1", strong("No menciones: "), parte("QUE_CALLAR:")),
                div(cls := "xx-small", strong("Esta tarde: "), parte("ACCION_POSITIVA:"))
              )
            case _ => div()
          }
        case _ => div()
      }
    }

    // ── CONSEJOS IA CONSOLIDADOS (datos ya cargados arriba — sin llamadas extra) ─
    val eliteConsejos = scala.collection.mutable.ListBuffer[(String, String, String)]()

    // 1. IA Neuro-Scout — ya cargado en aiMessage
    try {
      if (aiMessage.nonEmpty) {
        val line = aiMessage.replaceAll("<[^>]+>","").split("\n").map(_.trim).filter(_.nonEmpty).headOption
        line.foreach { l => eliteConsejos += (("🧠", "Neuro-Scout", l)) }
      }
    } catch { case _: Exception => () }

    // 2. Analista cognitivo — ya cargado
    try {
      if (cognitiveInsight.nonEmpty) {
        val line = cognitiveInsight.replaceAll("<[^>]+>","").split("\n").map(_.trim).filter(_.nonEmpty).headOption
        line.foreach { l => eliteConsejos += (("🧩", "Cognitivo", l)) }
      }
    } catch { case _: Exception => () }

    // 3. Auditor técnico — ya cargado
    try { techAlerts.headOption.foreach { a => eliteConsejos += (("⚡", "Auditor", a)) } }
    catch { case _: Exception => () }

    // 4. Último audio — de matches ya cargados
    try {
      matches.find(_.analisisVoz.nonEmpty).foreach { m =>
        m.analisisVoz.split("\n").map(_.trim).filter(_.nonEmpty).headOption.foreach { l =>
          eliteConsejos += (("🎙️", s"Audio vs ${m.rival}", l))
        }
      }
    } catch { case _: Exception => () }

    // 5. Smart insights — ya cargado
    try {
      if (smartInsights.nonEmpty) {
        val line = smartInsights.replaceAll("<[^>]+>","").split("\n").map(_.trim).filter(_.nonEmpty).headOption
        line.foreach { l => eliteConsejos += (("📡", "Datos", l)) }
      }
    } catch { case _: Exception => () }

    // 6. Correlacion sueno-rendimiento — insight deterministico, sin llamada a Gemini en el render
    try {
      DatabaseManager.getSleepDashboardInsight().foreach { insight =>
        eliteConsejos += (("💤", "Sueño", insight))
      }
    } catch { case _: Exception => () }

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

    // B2: Detector de tendencia LOESS (solo activo con >=20 partidos totales)
    val loess = DatabaseManager.getTrendLOESS()
    val loessActivo = loess.getOrElse("activo", false).asInstanceOf[Boolean]

    // B4: Alertas estadisticas personalizadas
    val alertasPersonales = try DatabaseManager.getAlertasEstadisticasPersonales() catch { case _: Exception => List.empty[String] }

    val radarData = s"""[${card.div}, ${card.han}, ${card.kic}, ${card.ref}, ${card.spd}, ${card.pos}]"""
    val rawMedia = (card.divRaw * 0.20) + (card.hanRaw * 0.20) + (card.kicRaw * 0.15) + (card.refRaw * 0.20) + (card.spdRaw * 0.05) + (card.posRaw * 0.20)
    val xpPercent = ((rawMedia - rawMedia.floor) * 100).toInt

    // 3. ESTADO FISICO (ACWR) — FIX 2: evita mostrar un ratio disparado (ej. 4.00) cuando el
    // historico es insuficiente (<3 semanas con datos); en ese caso se comunica explicitamente.
    val acwrEstado = DatabaseManager.calcularACWRConEstado()
    val acwrInsuficiente = acwrEstado("status").asInstanceOf[String] == "INSUFICIENTE"
    val acwr = acwrEstado("acwr").asInstanceOf[Double]
    val acwrValStr = if (acwrInsuficiente) "—" else f"$acwr%.2f"
    // BLOQUE D: umbrales adaptados a la edad de Hector
    val umbrales = DatabaseManager.umbralesACWR()
    val (acwrColor, acwrText) =
      if (acwrInsuficiente) ("text-muted", "ACUMULANDO DATOS")
      else { val (_, color, etiqueta) = DatabaseManager.nivelACWR(acwr, umbrales); (s"text-$color", etiqueta) }
    def acwrHex(v: Double): String =
      if (v > umbrales.riesgo) "#ef4444" else if (v > umbrales.precaucion) "#f59e0b" else "#20c997"

    // ACWR GPS (Footbar): carga fisica objetiva medida por el sensor, no estimada
    val acwrGps = DatabaseManager.getFootbarACWR()
    val acwrGpsStr = if (acwrGps > 0) f"$acwrGps%.2f" else "—"
    val acwrGpsColor = if (acwrGps <= 0) "#94a3b8"
      else if (acwrGps > 1.5) "#ef4444" else if (acwrGps > 1.2) "#f59e0b" else "#20c997"

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

    // ── BLOQUE B5: ALERTAS DEL IDP (solo SQL, sin Gemini) ────────────────────
    val idpAlertWidget: Modifier = DatabaseManager.getActiveIdpTemporada() match {
      case Some(temp) =>
        val temporadaId = temp("id").asInstanceOf[Int]
        DatabaseManager.actualizarProgresoIDP(temporadaId)
        val objetivos = DatabaseManager.getIdpObjetivos(temporadaId)
        val fechaInicio = java.time.LocalDate.parse(temp("fechaInicio").asInstanceOf[String])
        val diasTranscurridos = java.time.temporal.ChronoUnit.DAYS.between(fechaInicio, java.time.LocalDate.now())

        val objetivoRetrasado = objetivos.find(o => o("progresoPct").asInstanceOf[Int] < 20 && diasTranscurridos > 60)
        val todosAvanzados = objetivos.nonEmpty && objetivos.forall(_("progresoPct").asInstanceOf[Int] >= 80)

        objetivoRetrasado match {
          case Some(o) =>
            val dim: String = o("dimension").asInstanceOf[String]
            val fechaLimite: String = o("fechaLimite").asInstanceOf[String]
            div(cls := "alert alert-warning border-warning shadow p-3 mb-3",
              div(cls := "d-flex align-items-center",
                span(style := "font-size: 20px; margin-right: 10px;", "⚠️"),
                div(cls := "small fw-bold", s"IDP: El objetivo $dim está por debajo del ritmo necesario para conseguirse antes del $fechaLimite.")
              )
            )
          case None if todosAvanzados =>
            div(cls := "alert alert-success border-success shadow p-3 mb-3",
              div(cls := "d-flex align-items-center",
                span(style := "font-size: 20px; margin-right: 10px;", "🌟"),
                div(cls := "small fw-bold", "IDP: Héctor está en camino de conseguir todos sus objetivos de temporada.")
              )
            )
          case _ => div()
        }
      case None => div()
    }


    // --- RENDERIZADO FINAL ---

    // ── MODULO 2: CONTEXTO AMBIENTAL CRUZADO ─────────────────────────────
    val contextPatterns = DatabaseManager.getContextPatterns().take(3)
    val contextPhrase   = DatabaseManager.getContextOptimoPhrase()
    val contextWidget = if (contextPatterns.isEmpty) div() else {
      div(cls := "card bg-white border-0 shadow-sm mb-3",
        style := "border-radius:12px; overflow:hidden;",
        div(style := "background:linear-gradient(135deg,#0c4a6e,#0f172a); padding:10px 14px;",
          span(style := "font-size:12px; font-weight:800; color:#7dd3fc;", "🌦 CONTEXTO ÓPTIMO")
        ),
        div(style := "padding:14px;",
          p(style := "font-size:12px; color:#334155; font-style:italic; font-weight:600; text-align:center; margin-bottom:12px;",
            contextPhrase),
          div(cls := "row g-2",
            frag(contextPatterns.map { c =>
              val nota  = c("notaMedia").asInstanceOf[Double]
              val color = if (nota >= 7) "#20c997" else if (nota >= 5) "#f59e0b" else "#ef4444"
              div(cls := "col-4",
                div(style := s"background:#f8fafc; border-top:3px solid $color; border-radius:8px; padding:8px; text-align:center;",
                  div(style := s"font-size:16px; font-weight:900; color:$color;", f"$nota%.1f"),
                  div(style := "font-size:9px; color:#64748b; margin-top:2px;", s"${c("clima")} · ${c("esLocal")}"),
                  div(style := "font-size:9px; color:#94a3b8;", s"${c("estadoDescanso")} · ${c("partidos")} PJ")
                )
              )
            }: _*)
          )
        )
      )
    }

    // ── BLOQUE F: FASE DE GUARDIAN (SQL puro) — modal propio, sin JS de Bootstrap ──
    val faseGuardian = DatabaseManager.getFaseGuardian()
    val faseGuardianModal: Modifier = {
      val siguiente = faseGuardian("siguiente").asInstanceOf[Option[Map[String, Any]]]
      val faltan = faseGuardian("faltan").asInstanceOf[List[String]]
      div(id := "faseGuardianModal", onclick := "if(event.target===this)this.style.display='none'",
        style := "display:none; position:fixed; inset:0; background:rgba(0,0,0,.7); z-index:2000; padding:16px; overflow:auto;",
        div(style := "max-width:460px; margin:60px auto; background:#0f172a; border:1px solid #334155; border-radius:14px; padding:18px; color:#e2e8f0;",
          div(cls := "d-flex justify-content-between align-items-start mb-2",
            div(cls := "fw-bold", s"${faseGuardian("emoji")} Fase ${faseGuardian("numero")} — ${faseGuardian("nombre")}"),
            span(style := "cursor:pointer; color:#94a3b8;", onclick := "document.getElementById('faseGuardianModal').style.display='none'", "✕")),
          div(cls := "small mb-2", faseGuardian("descripcion").toString),
          div(cls := "xx-small text-muted fw-bold", "MÓDULOS ACTIVOS"),
          ul(cls := "small mb-2", frag(faseGuardian("modulosActivos").asInstanceOf[List[String]].map(li(_)): _*)),
          siguiente match {
            case Some(sig) => frag(
              div(cls := "xx-small text-muted fw-bold", s"SIGUIENTE: ${sig("emoji")} FASE ${sig("numero")} — ${sig("nombre")}"),
              div(cls := "small mb-1", s"Se activará: ${sig("modulosActivos").asInstanceOf[List[String]].mkString(", ")}."),
              if (faltan.nonEmpty) div(cls := "xx-small", style := "color:#facc15;", s"Faltan: ${faltan.mkString(" · ")}.") else frag())
            case None => div(cls := "small text-success", "Fase máxima alcanzada.")
          },
          div(cls := "xx-small text-muted mt-2",
            s"Datos: ${faseGuardian("totalPartidos")} partidos · ${faseGuardian("totalSemanasSueno")} semanas con sueño · ${faseGuardian("totalTemporadas")} temporadas.")),
        script(raw("function abrirFaseGuardian(){ document.getElementById('faseGuardianModal').style.display='block'; }")))
    }

    // ── BLOQUE M: PESTANA "HOY" — solo lo accionable del dia (el resto sigue en COMPLETO) ──
    val hoyTab: Modifier = {
      val indice = formaHoy("indiceForma").asInstanceOf[Double]
      val semaforo = DatabaseManager.formaSemaforo(indice)
      val colorForma = if (indice >= 7.5) "#20c997" else if (indice >= 5.0) "#facc15" else "#ef4444"
      div(
        riesgoCriticoAlert,
        desgasteWidget,
        enfermedadWidget,
        div(cls := "text-center mb-3", style := "background:linear-gradient(135deg,#0f172a 0%,#1e293b 100%); border-radius:16px; padding:22px; border:1px solid #334155;",
          div(style := "font-size:12px; color:#94a3b8; letter-spacing:2px;", "ÍNDICE DE FORMA HOY"),
          div(style := s"font-size:64px; font-weight:900; color:$colorForma; line-height:1.1;", f"$semaforo $indice%.1f")),
        if (riesgoEsAltoOCritico) riesgoLesionWidget else frag(),
        microObjetivoCard("Hoy"),
        // BLOQUE H: rachas de registro — discreto, sin alarma si se rompe
        {
          val st = DatabaseManager.getStreakRegistro()
          val racha = st("streakSueno").asInstanceOf[Int]
          div(cls := "mb-3 xx-small", style := "color:#94a3b8; line-height:1.7;",
            div(s"🔥 Sueño: $racha ${if (racha == 1) "día seguido" else "días seguidos"}"),
            if (st("partidos").asInstanceOf[Int] > 0) div(s"⚽ Partidos: ${st("partidosConRubrica")}/${st("partidos")} con rúbrica completa esta temporada") else frag(),
            div(s"💤 Esta semana: ${st("diasSemana")}/7 días registrados"))
        },
        div(cls := "d-grid gap-2 mb-3",
          a(href := "/bio", cls := "btn btn-lg btn-info fw-bold py-3", "💤 Registrar sueño"),
          if (esDiaDePartido) a(href := "/match-center", cls := "btn btn-lg btn-warning fw-bold py-3", "⚽ Registrar partido") else frag())
      )
    }
    val tabsNav: Modifier = div(cls := "d-flex gap-2 mb-3",
      button(tpe := "button", id := "tabBtnHoy", cls := "btn btn-warning fw-bold flex-fill", onclick := "mostrarTabDashboard('hoy')", "☀️ HOY"),
      button(tpe := "button", id := "tabBtnCompleto", cls := "btn btn-outline-secondary fw-bold flex-fill", onclick := "mostrarTabDashboard('completo')", "📊 COMPLETO"))

    val content = basePage("home",
      faseGuardianModal,
      tabsNav,
      div(id := "tabHoy", hoyTab),
      div(id := "tabCompleto", style := "display:none;",
      div(
        // ── BLOQUE F2: RIESGO DE LESION CRITICO (maxima prioridad, por encima del desgaste) ─
        riesgoCriticoAlert,

        // ── BLOQUE 5.6: DESGASTE SILENCIOSO (prioridad maxima sobre todo lo demas) ─
        desgasteWidget,
        enfermedadWidget,

        // ── BLOQUE C: MODO DIA DE PARTIDO ───────────────────────────────────
        diaPartidoBanner,
        postPartidoWidget,

        // ── BLOQUE 2.7: ALERTAS DE TEMPORADA ────────────────────────────────
        temporadaAlertWidget,

        // ── BLOQUE B2: PENDIENTE DE REGISTRAR ───────────────────────────────
        pendienteWidget,

        // ── BLOQUE 4.2: ULTIMA ACADEMIA ─────────────────────────────────────
        ultimaAcademiaWidget,
        academiaVideoTipWidget,

        // ── BLOQUE 5.4/5.5: FOCO SEMANAL Y PREPARACION ──────────────────────
        microObjetivoWidget,
        preparacionWidget,

        // ── BLOQUE I: SUGERENCIAS DE SKILL DESDE EL FEEDBACK ─────────────────
        sugerenciasSkillWidget,

        // ── BLOQUE E: HITOS RECIENTES ────────────────────────────────────────
        hitosWidget,

        // ── BLOQUE A3: INDICE DE FORMA DIARIO ───────────────────────────────
        formaWidget,
        formaProyectadaWidget,
        vozPorteroWidget,
        arquetipoWidget,
        deudaSuenoWidget,
        riesgoLesionWidget,
        rfmfPendientesWidget,
        DatabaseManager.getPercentilRealHector() match {
          case Some(p) => conConfianza("rfmf_benchmarking", p("pjHector").asInstanceOf[Int])(rffmWidget)
          case None => rffmWidget
        },

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
                    card.clubNombre),
                  div(style := "font-size:9px; opacity:.75; font-weight:bold; letter-spacing:0.5px;",
                    card.categoria)),
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
              // BLOQUE B5: badge discreto de la temporada activa — lleva al historial
              a(href := "/history", style := "text-decoration:none;",
                div(style := "display:inline-block; margin-bottom:8px; font-size:9px; color:#94a3b8; background:#1e293b; border:1px solid #334155; border-radius:6px; padding:2px 8px;",
                  s"📅 $temporadaActivaNombre")
              ),
              // BLOQUE F: fase actual de Guardian — al pulsar abre el detalle
              div(style := "font-size:9px; color:#94a3b8; margin:-4px 0 8px; cursor:pointer;", onclick := "abrirFaseGuardian()",
                s"${faseGuardian("emoji")} Guardian — Fase ${faseGuardian("numero")}: ${faseGuardian("nombre").toString.toLowerCase.capitalize} ⓘ"),
              // KPIs rápidos
              div(cls := "row g-2",
                frag(Seq(
                  (f"$avgLast5%2.1f", "RACHA 5", if(trendDiff>0)"#20c997" else if(trendDiff<0)"#ef4444" else "#94a3b8"),
                  (acwrValStr,        "ACWR",    if(acwrInsuficiente)"#94a3b8" else acwrHex(acwr)),
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
              ),
              if (loessActivo) {
                val tendencia = loess("tendencia").asInstanceOf[String]
                val diferencia = loess("diferencia").asInstanceOf[Double]
                val (flecha, color) = tendencia match {
                  case "POSITIVA" => ("↑", "#20c997")
                  case "NEGATIVA" => ("↓", "#ef4444")
                  case _          => ("→", "#94a3b8")
                }
                div(style := "margin-top:6px; text-align:center;",
                  span(
                    attr("title") := "Tendencia basada en los últimos 10 partidos",
                    style := s"font-size:10px; font-weight:800; color:$color; background:#1e293b; border:1px solid #334155; border-radius:6px; padding:3px 8px;",
                    s"$flecha TENDENCIA $tendencia (${if (diferencia >= 0) "+" else ""}${f"$diferencia%.1f"})"
                  )
                )
              } else span()
            )
          )
        ),

        contextWidget,
        idpAlertWidget,

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

            // B4: Alertas estadisticas personalizadas (umbrales relativos al patron historico de Hector)
            if (alertasPersonales.nonEmpty)
              div(cls := "card bg-white border-0 shadow-sm mb-3",
                style := "border-radius:12px; overflow:hidden;",
                div(style := "background:#0f172a; padding:10px 14px;",
                  span(style := "font-size:12px; font-weight:800; color:#f87171;", "📈 ALERTAS ESTADÍSTICAS PERSONALES")
                ),
                div(style := "padding:8px;",
                  frag(alertasPersonales.map { a =>
                    div(style := "font-size:12px; color:#334155; padding:6px 4px; border-bottom:1px solid #f1f5f9;", a)
                  }: _*)
                )
              )
            else span(),

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
                  ("INTELIGENCIA", smartInsightsText.take(30) + (if (smartInsightsText.length > 30) "..." else ""), "#8b5cf6"),
                  ("ACWR HOY",   if (acwrInsuficiente) "Acumulando" else f"$acwr%.2f", if(acwrInsuficiente)"#94a3b8" else acwrHex(acwr)),
                  ("ACWR GPS",   acwrGpsStr,                                acwrGpsColor)
                ).map { case (lbl, v, color) =>
                  div(cls := "col-6",
                    div(style := s"background:#f8fafc; border-left:3px solid $color; border-radius:6px; padding:8px;",
                      div(style := "font-size:9px; font-weight:700; color:#94a3b8;", lbl),
                      div(style := s"font-size:12px; font-weight:800; color:#1e293b; margin-top:2px;",
                        v)
                    )
                  )
                }: _*)
              ),
              a(href := "/career/acwr-proyeccion", cls := "btn btn-sm btn-outline-warning fw-bold w-100", "📅 Planificar próxima semana")
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

            // ── CONSEJOS IA CONSOLIDADOS ────────────────────────────────
            if (eliteConsejos.nonEmpty)
              div(cls := "card bg-white border-0 shadow-sm mb-3",
                style := "border-radius:12px; overflow:hidden;",
                div(style := "background:linear-gradient(135deg,#1e293b,#0f172a); padding:10px 14px; display:flex; align-items:center; gap:8px;",
                  span(style := "font-size:16px;", "✨"),
                  span(style := "font-size:12px; font-weight:800; color:#a78bfa;", "CONSEJOS IA")
                ),
                div(style := "padding:8px;",
                  frag(eliteConsejos.toList.map { case (icon, fuente, texto) =>
                    div(style := "display:flex; gap:8px; padding:8px 6px; border-bottom:1px solid #f1f5f9;",
                      div(style := "width:3px; background:#a78bfa; border-radius:2px; flex-shrink:0; margin-top:2px;"),
                      div(
                        div(style := "font-size:9px; font-weight:800; color:#a78bfa; margin-bottom:2px;",
                          s"$icon $fuente"),
                        div(style := "font-size:11px; color:#334155; line-height:1.5;", texto)
                      )
                    )
                  }: _*)
                )
              )
            else span(),

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
      )),
      script(raw("""
        function mostrarTabDashboard(t){
          var hoy = t !== 'completo';
          document.getElementById('tabHoy').style.display = hoy ? '' : 'none';
          document.getElementById('tabCompleto').style.display = hoy ? 'none' : '';
          document.getElementById('tabBtnHoy').className = 'btn fw-bold flex-fill ' + (hoy ? 'btn-warning' : 'btn-outline-secondary');
          document.getElementById('tabBtnCompleto').className = 'btn fw-bold flex-fill ' + (hoy ? 'btn-outline-secondary' : 'btn-warning');
          try { localStorage.setItem('guardian_dashboard_tab', hoy ? 'hoy' : 'completo'); } catch(e) {}
          if (!hoy) window.dispatchEvent(new Event('resize'));
        }
        (function(){
          var t = 'hoy';
          try { t = localStorage.getItem('guardian_dashboard_tab') || 'hoy'; } catch(e) {}
          mostrarTabDashboard(t);
        })();
        function registrarAutopercepcionDashboard(v){
          fetch('/match/autopercepcion', { method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body:'valor='+v })
            .then(function(){
              for (var i=1;i<=5;i++){
                var btn = document.getElementById('dashAutop'+i);
                if (!btn) continue;
                btn.classList.remove('btn-warning'); btn.classList.remove('btn-outline-light');
                btn.classList.add(i===v ? 'btn-warning' : 'btn-outline-light');
              }
            });
        }
        function cerrarDiaPartidoBanner(){
          var b = document.getElementById('diaPartidoBanner');
          if (b) b.style.display = 'none';
          try { localStorage.setItem('diaPartidoBannerCerrado', new Date().toISOString().slice(0,10)); } catch(e) {}
        }
        (function(){
          try {
            var cerrado = localStorage.getItem('diaPartidoBannerCerrado');
            var hoy = new Date().toISOString().slice(0,10);
            if (cerrado === hoy) {
              var b = document.getElementById('diaPartidoBanner');
              if (b) b.style.display = 'none';
            }
          } catch(e) {}
        })();
      """))
    )
    renderHtml(content)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 5.4 — RUTA: COMPLETAR MICRO-OBJETIVO DE LA SEMANA
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.postForm("/micro-objetivo/completar")
  def completarMicroObjetivoAction(request: cask.Request, resultado: String = "") = withAuth(request) {
    DatabaseManager.completarMicroObjetivo(resultado)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/"))
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 5.5 — RUTA: GENERAR PREPARACION SEMANAL (solo al pulsar el boton)
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.post("/preparacion-semanal/generar")
  def generarPreparacionSemanalAction(request: cask.Request) = withAuth(request) {
    DatabaseManager.generarPreparacionSemanal()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/"))
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
