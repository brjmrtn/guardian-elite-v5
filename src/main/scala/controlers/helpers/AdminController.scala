import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object AdminController extends cask.Routes {

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C3 — PANEL DE CONTROL DEL PERFIL PUBLICO
  // ─────────────────────────────────────────────────────────────────────────────
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B5 — ESTRUCTURA SEMANAL FIJA DE HECTOR (Elite exclusivamente)
  // ─────────────────────────────────────────────────────────────────────────────
  private val diasSemanaLabel = Map(1 -> "Lunes", 2 -> "Martes", 3 -> "Miércoles", 4 -> "Jueves", 5 -> "Viernes", 6 -> "Sábado", 7 -> "Domingo")
  private def etiquetaTipoSesionUI(tipo: String): String = tipo match {
    case "JUDO"     => "🥋 Judo"
    case "ACADEMIA" => "🥅 Academia"
    case "EQUIPO"   => "⚽ Entreno equipo"
    case "PARTIDO"  => "🏟️ Partido"
    case other      => other
  }
  private def weeklyStructurePanel(): Modifier = {
    val slots = DatabaseManager.getWeeklyStructure()
    div(cls := "card bg-dark border-info shadow mb-4 p-3",
      h5(cls := "text-info", "📅 ESTRUCTURA SEMANAL DE HÉCTOR"),
      p(cls := "small text-muted", "Activa/desactiva sesiones y cambia el día si cambia la rutina (ej: mover el partido de sábado a viernes)."),
      div(slots.map { s =>
        val slotId = s("id").asInstanceOf[Int]
        val dia = s("diaSemana").asInstanceOf[Int]
        val tipo = s("tipoSesion").asInstanceOf[String]
        val activo = s("activo").asInstanceOf[Boolean]
        form(action := "/settings/weekly-structure/save", method := "post", cls := "row g-2 align-items-center mb-2 border-bottom border-secondary pb-2",
          input(tpe := "hidden", name := "id", value := slotId.toString),
          div(cls := "col-4 small fw-bold text-white", etiquetaTipoSesionUI(tipo)),
          div(cls := "col-3",
            select(name := "diaSemana", cls := "form-select form-select-sm bg-dark text-white border-secondary",
              (1 to 7).map(d => option(value := d.toString, if (d == dia) selected := "selected" else frag(), diasSemanaLabel(d)))
            )
          ),
          div(cls := "col-3 form-check",
            input(cls := "form-check-input", tpe := "checkbox", name := "activo", id := s"activoSlot$slotId", if (activo) attr("checked") := "checked" else frag()),
            label(`for` := s"activoSlot$slotId", cls := "form-check-label xx-small text-muted", "Activo")
          ),
          div(cls := "col-2", button(tpe := "submit", cls := "btn btn-sm btn-outline-info fw-bold", "💾"))
        )
      })
    )
  }

  private def perfilPublicoPanel(): Modifier = {
    val cfg = DatabaseManager.getPerfilPublicoConfig()
    val activo = cfg("activo").asInstanceOf[Boolean]
    val visitas = cfg("visitas").asInstanceOf[Int]
    val ultimaVisita: String = cfg("ultimaVisita").asInstanceOf[String]

    def toggle(fieldName: String, checked: Boolean, labelText: String) =
      div(cls := "form-check form-switch mb-2",
        input(cls := "form-check-input", tpe := "checkbox", name := fieldName, id := fieldName,
          if (checked) attr("checked") := "checked" else frag()),
        label(`for` := fieldName, cls := "form-check-label small text-white", labelText)
      )

    div(cls := "card bg-dark text-white border-info shadow p-4 mb-3",
      h4(cls := "text-info mb-3", "🌍 PERFIL PÚBLICO"),
      p(cls := "small text-muted", "Comparte este enlace y la contraseña solo con personas de confianza — ojeadores, entrenadores de academia, representantes. Ellos verán el desarrollo real de Héctor en tiempo real."),
      form(action := "/settings/perfil_publico/save", method := "post",
        div(cls := "form-check form-switch mb-3",
          input(cls := "form-check-input", tpe := "checkbox", name := "activo", id := "ppActivo",
            if (activo) attr("checked") := "checked" else frag()),
          label(`for` := "ppActivo", cls := "form-check-label fw-bold text-warning", "Activar perfil público")
        ),
        div(cls := "mb-3",
          label(cls := "form-label small text-muted fw-bold", "Contraseña de lectura"),
          input(tpe := "text", name := "password", cls := "form-control fw-bold",
            placeholder := "Dejar en blanco para no cambiarla")
        ),
        div(cls := "border-top border-secondary pt-3 mb-3",
          h6(cls := "text-muted small text-uppercase mb-2", "Secciones visibles"),
          toggle("mostrarCarta", cfg("mostrarCarta").asInstanceOf[Boolean], "Carta FUT"),
          toggle("mostrarProgresion", cfg("mostrarProgresion").asInstanceOf[Boolean], "Progresión de rating"),
          toggle("mostrarVideoIa", cfg("mostrarVideoIa").asInstanceOf[Boolean], "Últimos análisis de vídeo IA"),
          toggle("mostrarIdp", cfg("mostrarIdp").asInstanceOf[Boolean], "Plan de Desarrollo Individual"),
          toggle("mostrarInforme", cfg("mostrarInforme").asInstanceOf[Boolean], "Informe de captación"),
          toggle("mostrarCognitivo", cfg("mostrarCognitivo").asInstanceOf[Boolean], "Índice cognitivo"),
          toggle("mostrarMedico", cfg("mostrarMedico").asInstanceOf[Boolean], "Datos médicos (no recomendado)"),
          toggle("mostrarArquetipo", cfg("mostrarArquetipo").asInstanceOf[Boolean], "Arquetipo de portero")
        ),
        div(cls := "d-grid mb-3", button(tpe := "submit", cls := "btn btn-info fw-bold", "Guardar configuración"))
      ),
      div(cls := "row text-center border-top border-secondary pt-3",
        div(cls := "col-6",
          div(cls := "fw-bold text-warning", visitas.toString), div(cls := "xx-small text-muted", "Visitas totales")),
        div(cls := "col-6",
          div(cls := "fw-bold text-white small", if (ultimaVisita.nonEmpty) ultimaVisita.take(16) else "—"),
          div(cls := "xx-small text-muted", "Última visita"))
      ),
      div(cls := "d-grid mt-3",
        button(tpe := "button", id := "btnCopyLink", cls := "btn btn-outline-warning fw-bold",
          onclick := "copyPublicLink()", "📋 Copiar enlace")
      ),
      script(raw("""
        function copyPublicLink() {
          var link = window.location.origin + '/hector';
          navigator.clipboard.writeText(link).then(function() {
            var btn = document.getElementById('btnCopyLink');
            var original = btn.textContent;
            btn.textContent = '✅ Enlace copiado';
            setTimeout(function() { btn.textContent = original; }, 2000);
          }).catch(function() { alert(link); });
        }
      """))
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 2.5 — GESTION DE TEMPORADAS (UI)
  // ─────────────────────────────────────────────────────────────────────────────
  private def temporadasPanel(msg: String): Modifier = {
    val activa = DatabaseManager.getTemporadaActivaInfo()
    val cerradas = DatabaseManager.getTemporadasCerradas()

    val msgBox: Modifier = if (msg.nonEmpty) div(cls := "alert alert-success small p-2 mb-3", msg) else div()

    val panelActiva: Modifier = activa match {
      case Some(temp) =>
        val pj = temp("pj").asInstanceOf[Int]
        val media = temp("media").asInstanceOf[Double]
        val sid = temp("id").asInstanceOf[Int]
        div(cls := "border border-secondary rounded p-3 mb-3",
          div(cls := "d-flex justify-content-between align-items-start",
            div(
              div(cls := "fw-bold text-warning", s"${temp("nombre")} — ${temp("categoria")}"),
              div(cls := "xx-small text-muted", s"Inicio: ${temp("fechaInicio")}"),
              div(cls := "small text-white mt-1", s"$pj partidos jugados · media ${"%.1f".format(media)}")
            ),
            div(cls := "d-flex gap-2",
              if (pj > 0) form(action := "/admin/season-action/close", method := "post",
                button(tpe := "submit", cls := "btn btn-sm btn-outline-danger fw-bold",
                  onclick := "return confirm('¿Cerrar la temporada actual? Esta acción no se puede deshacer.');",
                  "🔒 Cerrar temporada")
              ) else frag(),
              a(href := s"/admin/season-report/$sid", target := "_blank",
                cls := "btn btn-sm btn-outline-info fw-bold", "📄 Informe final")
            )
          )
        )
      case None => div(cls := "text-muted small mb-3", "No hay temporada activa.")
    }

    val puedeCrearNueva = activa.isEmpty || Option(activa.get("fechaFin").asInstanceOf[String]).exists(_.nonEmpty)

    val formNueva: Modifier = if (puedeCrearNueva)
      form(action := "/admin/season-action/new", method := "post", cls := "border border-secondary rounded p-3 mb-3",
        h6(cls := "text-info small text-uppercase", "Nueva temporada"),
        div(cls := "mb-2",
          label(cls := "form-label small text-muted", "Categoría *"),
          input(tpe := "text", name := "categoria", required := true, cls := "form-control form-control-sm fw-bold",
            placeholder := "Ej: Benjamín A")
        ),
        div(cls := "mb-2",
          label(cls := "form-label small text-muted", "Nombre del club (opcional)"),
          input(tpe := "text", name := "nombreClub", cls := "form-control form-control-sm fw-bold")
        ),
        div(cls := "mb-2",
          label(cls := "form-label small text-muted", "Fecha de inicio"),
          input(tpe := "date", name := "fechaInicio", cls := "form-control form-control-sm fw-bold",
            value := java.time.LocalDate.now().toString)
        ),
        div(cls := "form-check mb-2",
          input(cls := "form-check-input", tpe := "checkbox", name := "confirmar", id := "confirmarReset", required := true),
          label(`for` := "confirmarReset", cls := "form-check-label xx-small text-muted",
            "Entiendo que se reiniciará la caché de IA y los micro-objetivos")
        ),
        div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-sm btn-success fw-bold", "Iniciar nueva temporada"))
      )
    else div()

    val historial: Modifier = if (cerradas.isEmpty)
      div(cls := "text-muted xx-small text-center py-2", "Sin temporadas cerradas todavía.")
    else
      table(cls := "table table-dark table-sm small mb-0",
        thead(tr(th("Temporada"), th("Inicio"), th("Fin"), th("PJ"), th("Media"), th("P0"), th(""))),
        tbody(
          frag(cerradas.map { c =>
            tr(
              td(c("nombre").asInstanceOf[String]),
              td(c("fechaInicio").asInstanceOf[String]),
              td(c("fechaFin").asInstanceOf[String]),
              td(c("pj").asInstanceOf[Int].toString),
              td("%.1f".format(c("mediaFinal").asInstanceOf[Double])),
              td(c("porteriasCero").asInstanceOf[Int].toString),
              td(if (c("tieneInforme").asInstanceOf[Boolean])
                a(href := s"/admin/season-report/${c("id")}", target := "_blank", cls := "text-info small", "Ver informe")
                else span(cls := "text-muted xx-small", "—"))
            )
          }: _*)
        )
      )

    div(cls := "card bg-dark border-warning shadow mb-4 p-3",
      h5(cls := "text-warning", "📅 GESTIÓN DE TEMPORADAS"),
      msgBox,
      panelActiva,
      formNueva,
      div(cls := "border-top border-secondary pt-3 mt-2",
        div(cls := "d-flex justify-content-between align-items-center mb-2",
          h6(cls := "text-muted small text-uppercase mb-0", "Historial de temporadas cerradas"),
          a(href := "/career/comparativa", cls := "btn btn-sm btn-outline-info fw-bold", "📊 Comparar temporadas")
        ),
        historial
      )
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO — PANEL DE BACKUPS AUTOMATICOS
  // ─────────────────────────────────────────────────────────────────────────────
  private def backupsPanel(msg: String): Modifier = {
    val backups = DatabaseManager.getBackupsLog()
    val estado: Modifier = backups.headOption match {
      case Some(b) =>
        div(cls := "alert alert-success small p-2 mb-3",
          s"✅ Último backup: ${b("fecha").asInstanceOf[String]} · ${b("tamanoKb").asInstanceOf[Int]}KB")
      case None =>
        div(cls := "alert alert-warning small p-2 mb-3", "⚠️ Sin backups recientes")
    }

    val msgBox: Modifier = if (msg.nonEmpty) div(cls := "alert alert-info small p-2 mb-3", msg) else div()

    val filasBackups: Modifier = if (backups.isEmpty)
      div(cls := "text-muted small text-center py-2", "Todavía no se ha generado ningún backup.")
    else
      frag(backups.map { b =>
        val id = b("id").asInstanceOf[Int]
        div(cls := "d-flex justify-content-between align-items-center border-bottom border-secondary py-2",
          div(
            div(cls := "small text-white fw-bold", b("fecha").asInstanceOf[String]),
            div(cls := "xx-small text-muted", s"${b("tamanoKb").asInstanceOf[Int]}KB · ${b("destinos").asInstanceOf[String]}")
          ),
          a(href := s"/admin/backup/download/$id", cls := "btn btn-sm btn-outline-info fw-bold", "⬇️ Descargar")
        )
      }: _*)

    div(cls := "card bg-dark text-white border-warning shadow p-4 mb-3",
      h4(cls := "text-warning mb-3", "💾 BACKUPS"),
      msgBox,
      estado,
      p(cls := "small text-muted",
        "Guardian genera un backup automático cada domingo a las 3:00 AM. El backup se guarda en la base de datos y se envía a tu email si está configurado. Contiene todos los datos de Héctor desde el inicio del registro."),
      div(cls := "d-flex gap-2 mb-3",
        form(action := "/admin/backup/generate", method := "post", cls := "flex-grow-1",
          button(tpe := "submit", cls := "btn btn-warning fw-bold w-100", "🔄 Generar backup ahora")
        ),
        form(action := "/admin/backup/send-email", method := "post", cls := "flex-grow-1",
          button(tpe := "submit", cls := "btn btn-outline-warning fw-bold w-100", "📧 Enviarme el backup ahora por email")
        )
      ),
      div(cls := "border-top border-secondary pt-3",
        h6(cls := "text-muted small text-uppercase mb-2", "Últimos backups"),
        filasBackups
      )
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE RFFM — PANEL DE BENCHMARKING REAL CONTRA LA CATEGORIA
  // ─────────────────────────────────────────────────────────────────────────────
  private def rffmBenchmarkPanel(msg: String): Modifier = {
    val estado = DatabaseManager.getRffmSyncEstado()
    val competicionId = DatabaseManager.getRffmCompeticionId()
    val temporada = DatabaseManager.getRffmTemporada()
    val enProgreso = estado == "IN_PROGRESS"

    val estadoBox: Modifier =
      if (enProgreso) div(cls := "alert alert-info small p-2 mb-3", "⏳ Sincronizando datos de la RFFM...")
      else if (estado.startsWith("ERROR")) div(cls := "alert alert-warning small p-2 mb-3",
        "⚠️ No se pudo conectar con rffm.es — los percentiles se calculan con los últimos datos disponibles.")
      else div(cls := "alert alert-secondary small p-2 mb-3", estado)

    val msgBox: Modifier = if (msg.nonEmpty) div(cls := "alert alert-info small p-2 mb-3", msg) else div()

    // BLOQUE A7: tipo de liga de la temporada activa (INTERNA/RFMF) y equipo de Hector en la RFMF
    val ligaConfig = DatabaseManager.getLigaRFMFConfig()
    val ligaTipo = ligaConfig("ligaTipo").asInstanceOf[String]
    val nombreEquipo = ligaConfig("nombreEquipo").asInstanceOf[String]
    val grupoIdSeason = ligaConfig("grupoId").asInstanceOf[String]
    val esRfmf = ligaTipo == "RFMF"

    div(cls := "card bg-dark text-white border-info shadow p-4 mb-3",
      h4(cls := "text-info mb-3", "📊 BENCHMARK RFMF"),
      msgBox,
      estadoBox,
      p(cls := "small text-muted",
        "Guardian sincroniza cada lunes a las 6:00 AM los resultados de la categoría Prebenjamín F7 de la RFMF para calcular en qué percentil está Héctor frente a su categoría real."),
      form(action := "/admin/rffm/sync", method := "post", cls := "d-grid mb-3",
        button(tpe := "submit", cls := "btn btn-info fw-bold", if (enProgreso) "⏳ Sincronizando..." else "🔄 Sincronizar ahora")
      ),
      div(cls := "border-top border-secondary pt-3 mb-3",
        h6(cls := "text-muted small text-uppercase mb-2", "Temporada activa"),
        form(action := "/admin/rffm/liga", method := "post", cls := "row g-2 align-items-end",
          div(cls := "col-4",
            label(cls := "xx-small text-muted fw-bold", "Tipo de liga"),
            select(name := "ligaTipo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
              option(value := "INTERNA", if (!esRfmf) selected := "selected" else frag(), "Interna / amistosa"),
              option(value := "RFMF", if (esRfmf) selected := "selected" else frag(), "Liga oficial RFMF")
            )
          ),
          div(cls := "col-4",
            label(cls := "xx-small text-muted fw-bold", "Nombre del equipo (RFMF)"),
            input(tpe := "text", name := "nombreEquipo", cls := "form-control form-control-sm bg-dark text-white border-secondary", value := nombreEquipo, placeholder := "Ej: Rayo Vallecano B")
          ),
          div(cls := "col-4",
            label(cls := "xx-small text-muted fw-bold", "ID de grupo (opcional)"),
            input(tpe := "text", name := "grupoId", cls := "form-control form-control-sm bg-dark text-white border-secondary", value := grupoIdSeason)
          ),
          div(cls := "col-12", button(tpe := "submit", cls := "btn btn-sm btn-outline-info fw-bold w-100 mt-1", "Guardar liga"))
        ),
        div(cls := "xx-small text-muted mt-1",
          "Si es liga oficial RFMF y pones el nombre del equipo, Guardian detectará automáticamente los resultados de Héctor y los propondrá para confirmar en el dashboard.")
      ),
      div(cls := "border-top border-secondary pt-3",
        h6(cls := "text-muted small text-uppercase mb-2", "Configuración de competición"),
        form(action := "/admin/rffm/config", method := "post", cls := "row g-2 align-items-end",
          div(cls := "col-6",
            label(cls := "xx-small text-muted fw-bold", "ID de competición RFMF"),
            input(tpe := "text", name := "competicionId", cls := "form-control form-control-sm bg-dark text-white border-secondary", value := competicionId)
          ),
          div(cls := "col-4",
            label(cls := "xx-small text-muted fw-bold", "Temporada"),
            input(tpe := "text", name := "temporada", cls := "form-control form-control-sm bg-dark text-white border-secondary", value := temporada)
          ),
          div(cls := "col-2", button(tpe := "submit", cls := "btn btn-sm btn-outline-info fw-bold w-100", "Guardar"))
        ),
        div(cls := "xx-small text-muted mt-1", "Cambia el ID si Héctor pasa a otro grupo de la competición en temporadas futuras.")
      )
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C — CALENDARIO DE CARGA COGNITIVA ESCOLAR
  // ─────────────────────────────────────────────────────────────────────────────
  private def calendarioEscolarTipoLabel(tipo: String): String = tipo match {
    case "EXAMENES" => "📝 Exámenes"
    case "TRIMESTRE_FIN" => "📅 Fin de trimestre"
    case "VACACIONES" => "🏖️ Vacaciones"
    case "EVENTO_ESPECIAL" => "🎉 Evento especial"
    case other => other
  }
  private def calendarioEscolarPanel(): Modifier = {
    val periodos = DatabaseManager.getCalendarioEscolar()
    div(cls := "card bg-dark text-white border-secondary shadow p-4 mb-3",
      h4(cls := "mb-3", "📚 CALENDARIO ESCOLAR"),
      p(cls := "small text-muted",
        "Registra los periodos de exámenes o fin de trimestre de Héctor — Guardian los tendrá en cuenta al interpretar bajadas de energía o rendimiento que puedan tener origen escolar, no deportivo."),
      form(action := "/settings/calendario-escolar/save", method := "post", cls := "row g-2 align-items-end mb-3",
        div(cls := "col-6 col-md-3",
          label(cls := "xx-small text-muted fw-bold", "Fecha inicio"),
          input(tpe := "date", name := "fechaInicio", cls := "form-control form-control-sm bg-dark text-white border-secondary", required := true)
        ),
        div(cls := "col-6 col-md-3",
          label(cls := "xx-small text-muted fw-bold", "Fecha fin"),
          input(tpe := "date", name := "fechaFin", cls := "form-control form-control-sm bg-dark text-white border-secondary", required := true)
        ),
        div(cls := "col-6 col-md-3",
          label(cls := "xx-small text-muted fw-bold", "Tipo"),
          select(name := "tipo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
            option(value := "EXAMENES", "📝 Exámenes"),
            option(value := "TRIMESTRE_FIN", "📅 Fin de trimestre"),
            option(value := "VACACIONES", "🏖️ Vacaciones"),
            option(value := "EVENTO_ESPECIAL", "🎉 Evento especial")
          )
        ),
        div(cls := "col-6 col-md-3",
          label(cls := "xx-small text-muted fw-bold", "Descripción (opcional)"),
          input(tpe := "text", name := "descripcion", cls := "form-control form-control-sm bg-dark text-white border-secondary")
        ),
        div(cls := "col-12", button(tpe := "submit", cls := "btn btn-sm btn-outline-info fw-bold w-100 mt-1", "Añadir periodo"))
      ),
      if (periodos.isEmpty) div(cls := "text-muted small text-center py-2", "Sin periodos registrados.")
      else div(periodos.map { p =>
        val id = p("id").asInstanceOf[Int]
        div(cls := "d-flex justify-content-between align-items-center border-bottom border-secondary py-2",
          div(
            div(cls := "small text-white fw-bold", calendarioEscolarTipoLabel(p("tipo").asInstanceOf[String])),
            div(cls := "xx-small text-muted", s"${p("fechaInicio")} → ${p("fechaFin")}" + (if (p("descripcion").asInstanceOf[String].nonEmpty) s" · ${p("descripcion")}" else ""))
          ),
          form(action := s"/settings/calendario-escolar/$id/delete", method := "post",
            button(tpe := "submit", cls := "btn btn-sm btn-outline-danger fw-bold", "✕"))
        )
      })
    )
  }

  @cask.post("/settings/calendario-escolar/save")
  def saveCalendarioEscolar(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val fi = p.getOrElse("fechaInicio", ""); val ff = p.getOrElse("fechaFin", "")
    if (fi.nonEmpty && ff.nonEmpty) DatabaseManager.saveCalendarioEscolar(fi, ff, p.getOrElse("tipo", "EXAMENES"), p.getOrElse("descripcion", ""))
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/settings"))
  }

  @cask.post("/settings/calendario-escolar/:id/delete")
  def deleteCalendarioEscolarAction(request: cask.Request, id: Int) = withAuth(request) {
    DatabaseManager.deleteCalendarioEscolar(id)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/settings"))
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE O — RUTINA PRE-PARTIDO DE HECTOR
  // ─────────────────────────────────────────────────────────────────────────────
  private def rutinaPrepartidoPanel(): Modifier = {
    val rutina = DatabaseManager.getRutinaActiva()
    div(cls := "card bg-dark text-white border-secondary shadow p-4 mb-3",
      h4(cls := "mb-3", "🔄 RUTINA PRE-PARTIDO DE HÉCTOR"),
      p(cls := "small text-muted",
        "Describe la rutina habitual de Héctor antes de un partido — Guardian podrá comparar su rendimiento cuando la sigue y cuando no."),
      rutina.map(r => div(cls := "alert alert-secondary small p-2 mb-3", strong("Rutina actual: "), r)).getOrElse(div()),
      form(action := "/settings/rutina/save", method := "post",
        textarea(name := "descripcion", cls := "form-control form-control-sm bg-dark text-white border-secondary mb-2", rows := "3",
          placeholder := "Ej: Se pone siempre primero el guante derecho, bebe agua, hace tres saltos...",
          if (rutina.nonEmpty) rutina.get else ""),
        button(tpe := "submit", cls := "btn btn-sm btn-outline-info fw-bold w-100", "Guardar rutina")
      )
    )
  }

  @cask.post("/settings/rutina/save")
  def saveRutinaAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val descripcion = p.getOrElse("descripcion", "")
    if (descripcion.trim.nonEmpty) DatabaseManager.saveRutinaDefinicion(descripcion)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/settings"))
  }

  @cask.get("/settings") def settingsPage(backupMsg: String = "", rffmMsg: String = "") = {
    val card = DatabaseManager.getLatestCardData()
    val content = div(cls := "row justify-content-center", div(cls := "col-md-8 col-12", div(cls := "card bg-dark text-white border-secondary shadow p-4 mb-3", h2(cls := "text-warning mb-4", "Configuracion General"),
      form(action := "/settings/save_base64", method := "post",
        div(cls := "mb-4", label(cls := "form-label text-info fw-bold", "Nombre Visual (Carta)"), input(tpe := "text", name := "nombreClub", cls := "form-control fw-bold", value:=card.clubNombre, placeholder := "Ej: Rayo (Corto)")),
        div(cls := "mb-4", label(cls := "form-label text-success fw-bold", "Fecha de Nacimiento"), input(tpe := "date", name := "fechaNac", cls := "form-control fw-bold", value:=card.fechaNacimiento)),
        div(cls := "mb-4 border-top border-secondary pt-3", h5(cls:="text-warning", "Perfil Jugador"),
          div(cls:="row g-3",
            div(cls:="col-6",
              label(cls:="form-label text-info fw-bold small", "Posicion"),
              select(name:="posicion", cls:="form-select fw-bold",
                option(value:="POR", attr("selected"):="selected", "Portero (POR)")
              )
            ),
            div(cls:="col-6",
              label(cls:="form-label text-info fw-bold small", "Pie Dominante"),
              select(name:="pieDominante", cls:="form-select fw-bold",
                option(value:="Derecho", "Derecho"),
                option(value:="Izquierdo", "Izquierdo"),
                option(value:="Ambidiestro", "Ambidiestro")
              )
            )
          )
        ),
        div(cls := "mb-4 border-top border-secondary pt-3", h5(cls:="text-info", "Scouting 2.0 (RFFM)"), div(cls:="mb-3", label(cls:="small text-muted fw-bold", "URL Grupo RFFM"), input(tpe:="text", name:="rffmUrl", cls:="form-control fw-bold", value:=Option(card.rffmUrl).getOrElse(""), placeholder:="https://www.rffm.es/competicion/...")), div(cls:="mb-3", label(cls:="small text-muted fw-bold", "Nombre Oficial (Federacion)"), input(tpe:="text", name:="rffmName", cls:="form-control fw-bold", value:=Option(card.rffmName).getOrElse(""), placeholder:="Ej: RAYO VALLECANO DE MADRID 'B'"))),
        div(cls := "mb-4", label(cls := "form-label text-info fw-bold", "Foto Jugador"), input(tpe := "file", cls := "form-control fw-bold", accept := "image/*", onchange := "convertToBase64(this, 'hidden_foto')"), input(tpe := "hidden", name := "fotoBase64", id := "hidden_foto")),
        div(cls := "mb-4", label(cls := "form-label text-warning fw-bold", "Escudo Club"), input(tpe := "file", cls := "form-control fw-bold", accept := "image/*", onchange := "convertToBase64(this, 'hidden_club')"), input(tpe := "hidden", name := "clubBase64", id := "hidden_club")),
        div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg fw-bold", "Guardar"))),
      script(raw("""function convertToBase64(i,t){if(i.files&&i.files[0]){var r=new FileReader();r.onload=function(e){document.getElementById(t).value=e.target.result;};r.readAsDataURL(i.files[0]);}}"""))), div(cls:="d-flex gap-2 mt-2",
      a(href:="/videoteca", cls:="btn btn-warning fw-bold flex-grow-1", "🎬 VIDEOTECA"),
      a(href:="/admin", cls:="btn btn-outline-danger fw-bold", "⚙️ ADMIN")
    ), weeklyStructurePanel(), perfilPublicoPanel(), rffmBenchmarkPanel(rffmMsg), calendarioEscolarPanel(), rutinaPrepartidoPanel(), backupsPanel(backupMsg)));
    renderHtml(basePage("settings", content))
  }

  private def parseBody(request: cask.Request): Map[String, String] = {
    val body = new String(request.data.readAllBytes(), "UTF-8")
    body.split("&").filter(_.nonEmpty).map { p =>
      val kv = p.split("=", 2)
      java.net.URLDecoder.decode(kv(0), "UTF-8") -> (if (kv.length > 1) java.net.URLDecoder.decode(kv(1), "UTF-8") else "")
    }.toMap
  }

  @cask.post("/settings/weekly-structure/save")
  def saveWeeklyStructure(request: cask.Request) = {
    val p = parseBody(request)
    val id = p.getOrElse("id", "0").toIntOption.getOrElse(0)
    val diaSemana = p.getOrElse("diaSemana", "1").toIntOption.getOrElse(1)
    val activo = p.contains("activo")
    if (id > 0) DatabaseManager.updateWeeklySlot(id, activo, diaSemana)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/settings"))
  }

  @cask.post("/settings/perfil_publico/save")
  def savePerfilPublico(request: cask.Request) = {
    val p = parseBody(request)
    DatabaseManager.updatePerfilPublicoConfig(
      activo = p.contains("activo"),
      password = p.getOrElse("password", ""),
      mostrarCarta = p.contains("mostrarCarta"),
      mostrarProgresion = p.contains("mostrarProgresion"),
      mostrarVideoIa = p.contains("mostrarVideoIa"),
      mostrarIdp = p.contains("mostrarIdp"),
      mostrarInforme = p.contains("mostrarInforme"),
      mostrarCognitivo = p.contains("mostrarCognitivo"),
      mostrarMedico = p.contains("mostrarMedico"),
      mostrarArquetipo = p.contains("mostrarArquetipo")
    )
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/settings"))
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO — ENDPOINTS DE BACKUPS AUTOMATICOS (solo usuario Elite autenticado)
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.post("/admin/backup/generate")
  def generateBackupNow(request: cask.Request) = withAuth(request) {
    val msg = try {
      val sql = DatabaseManager.generarBackupSQL()
      val bytes = sql.getBytes("UTF-8")
      val fecha = java.time.LocalDate.now().toString
      DatabaseManager.guardarBackupEnBD(sql, fecha)

      val emailDest = sys.env.getOrElse("BACKUP_EMAIL", "")
      if (emailDest.nonEmpty) {
        BackupService.enviarPorEmail(emailDest, s"guardian_backup_$fecha.sql", bytes)
      }
      s"✅ Backup generado (${bytes.length / 1024}KB)."
    } catch { case e: Exception => s"⚠️ Error generando el backup: ${e.getMessage.take(150)}" }

    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/settings?backupMsg=${java.net.URLEncoder.encode(msg, "UTF-8")}"
    ))
  }

  @cask.post("/admin/backup/send-email")
  def sendBackupEmailNow(request: cask.Request) = withAuth(request) {
    val emailDest = sys.env.getOrElse("BACKUP_EMAIL", "")
    val smtpUser  = sys.env.getOrElse("SMTP_USER", "")
    val smtpPass  = sys.env.getOrElse("SMTP_PASS", "")

    val msg =
      if (emailDest.isEmpty || smtpUser.isEmpty || smtpPass.isEmpty) {
        "⚠️ Configura BACKUP_EMAIL, SMTP_USER y SMTP_PASS en Render para poder enviar el backup por email."
      } else {
        try {
          val sql = DatabaseManager.generarBackupSQL()
          val bytes = sql.getBytes("UTF-8")
          val fecha = java.time.LocalDate.now().toString
          DatabaseManager.guardarBackupEnBD(sql, fecha)
          BackupService.enviarPorEmail(emailDest, s"guardian_backup_$fecha.sql", bytes)
          s"✅ Backup enviado a $emailDest."
        } catch { case e: Exception => s"⚠️ Error enviando el backup: ${e.getMessage.take(150)}" }
      }

    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/settings?backupMsg=${java.net.URLEncoder.encode(msg, "UTF-8")}"
    ))
  }

  @cask.post("/admin/rffm/sync")
  def syncRffmNow(request: cask.Request) = withAuth(request) {
    DatabaseManager.syncRFFMBenchmarkAsync()
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/settings?rffmMsg=${java.net.URLEncoder.encode("⏳ Sincronización lanzada en segundo plano. Recarga la página en unos minutos.", "UTF-8")}"
    ))
  }

  @cask.post("/admin/rffm/config")
  def saveRffmConfig(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    DatabaseManager.setRffmConfig(p.getOrElse("competicionId", ""), p.getOrElse("temporada", ""))
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/settings?rffmMsg=${java.net.URLEncoder.encode("✅ Configuración RFFM guardada.", "UTF-8")}"
    ))
  }

  @cask.post("/admin/rffm/liga")
  def saveLigaRFMF(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    DatabaseManager.setLigaRFMFConfig(p.getOrElse("ligaTipo", "INTERNA"), p.getOrElse("nombreEquipo", ""), p.getOrElse("grupoId", ""))
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/settings?rffmMsg=${java.net.URLEncoder.encode("✅ Liga de la temporada guardada.", "UTF-8")}"
    ))
  }

  @cask.get("/admin/backup/download/:id")
  def downloadBackup(request: cask.Request, id: Int) = withAuth(request) {
    DatabaseManager.getBackupSqlById(id) match {
      case Some((fecha, sql)) =>
        cask.Response(sql.getBytes("UTF-8"), headers = Seq(
          "Content-Type" -> "application/sql",
          "Content-Disposition" -> s"""attachment; filename="guardian_backup_$fecha.sql""""
        ))
      case None =>
        cask.Response("Backup no encontrado".getBytes("UTF-8"), statusCode = 404, headers = Seq("Content-Type" -> "text/plain; charset=utf-8"))
    }
  }

  @cask.postForm("/settings/save_base64")
  def saveSettingsBase64(fotoBase64: String, clubBase64: String, nombreClub: String,
                         fechaNac: String, rffmUrl: String, rffmName: String,
                         posicion: String = "GK", pieDominante: String = "Derecho") = {
    val fechaFinal = if (fechaNac != null && fechaNac.nonEmpty) fechaNac else "2020-06-19"
    DatabaseManager.updateRFFMSettings(
      if (rffmUrl   != null) rffmUrl   else "",
      if (rffmName  != null) rffmName  else ""
    )
    val res = DatabaseManager.updateSeasonSettings(
      if (fotoBase64  != null) fotoBase64  else "",
      if (clubBase64  != null) clubBase64  else "",
      if (nombreClub  != null) nombreClub  else "",
      fechaFinal
    )
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.title("Exito"), tags2.style(raw(getCss()))),
      body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';",
        h1("OK"),
        h2(res),
        div(style := "margin-top: 20px;",
          a(href := "/", cls := "btn btn-warning fw-bold", "Volver")
        )
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.get("/admin")
  def adminPage(msg: String = "") = {
    val objs = DatabaseManager.getSeasonObjectives()
    val content = basePage("settings",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-danger text-center mb-4", "ADMINISTRACION"),
          if (msg.nonEmpty) div(cls := "alert alert-success small p-2 mb-3", msg) else div(),
          temporadasPanel(""),
          div(cls := "card bg-dark border-warning shadow mb-4 p-3",
            h5(cls := "text-warning", "Base de Datos Leyendas"),
            p(cls := "small text-muted fw-bold", "Si no ves la comparacion en Trayectoria, pulsa aqui."),
            a(href := "/admin/init_legends", cls := "btn btn-outline-warning w-100 fw-bold",
              "Inicializar BBDD Leyendas")
          ),
          div(cls := "card bg-secondary bg-opacity-25 border-secondary mb-4 p-3",
            h5(cls := "text-white", "Copia de Seguridad"),
            p(cls := "small text-muted fw-bold", "Descarga los partidos, o todas las tablas principales en un ZIP."),
            div(cls := "d-grid gap-2",
              a(href := "/admin/download_csv", cls := "btn btn-primary w-100 fw-bold", "⬇️ CSV Partidos"),
              a(href := "/admin/download_full_csv", cls := "btn btn-outline-primary w-100 fw-bold", "⬇️ Exportación completa (ZIP)")
            )
          ),
          div(cls := "card bg-secondary bg-opacity-25 border-secondary mb-4 p-3",
            h5(cls := "text-white", "Informe PDF"),
            p(cls := "small text-muted fw-bold", "Genera un informe limpio para imprimir o guardar como PDF."),
            div(cls := "d-grid gap-2",
              a(href := "/admin/print_report", target := "_blank", cls := "btn btn-info w-100 fw-bold",
                "Generar Informe PDF"),
              a(href := "/admin/captacion", target := "_blank", cls := "btn btn-outline-warning w-100 fw-bold",
                "🎭 Dossier Captacion Anonimo")
            )
          ),
          div(cls := "card bg-dark border-info shadow p-3",
            h5(cls := "text-info", "Gestionar Objetivos"),
            if (objs.isEmpty) div("Sin objetivos.")
            else div(
              (for (o <- objs) yield
                form(action := "/admin/update_obj", method := "post",
                  cls := "row align-items-center mb-2",
                  div(cls := "col-7 small text-white fw-bold", o.descripcion),
                  div(cls := "col-3",
                    input(tpe := "number", name := "meta", value := o.meta.toString,
                      cls := "form-control form-control-sm text-center fw-bold")
                  ),
                  input(tpe := "hidden", name := "id", value := o.id.toString),
                  div(cls := "col-2",
                    button(tpe := "submit", cls := "btn btn-sm btn-outline-success fw-bold", "S")
                  )
                )
                ).toSeq
            )
          ),
          div(cls := "d-grid mt-4",
            a(href := "/admin/importer", cls := "btn btn-warning fw-bold", "IMPORTAR DATOS MASIVOS (CSV)")
          )
        )
      )
    )
    renderHtml(content)
  }
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE H4 — FIX DE ENCODING EN REGISTROS HISTORICOS (mantenimiento, solo auth)
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.get("/admin/fix-encoding")
  def fixEncodingAction(request: cask.Request) = withAuth(request) {
    var count = 0
    val conn = DatabaseManager.getConnection()
    try {
      // matches
      val rs = conn.createStatement().executeQuery(
        "SELECT id, rival, estadio, notas_partido FROM matches WHERE rival LIKE '%Ã%' OR estadio LIKE '%Ã%'")
      val ps = conn.prepareStatement("UPDATE matches SET rival=?, estadio=?, notas_partido=? WHERE id=?")
      while (rs.next()) {
        ps.setString(1, fixEncoding(rs.getString("rival")))
        ps.setString(2, fixEncoding(Option(rs.getString("estadio")).getOrElse("")))
        ps.setString(3, fixEncoding(Option(rs.getString("notas_partido")).getOrElse("")))
        ps.setInt(4, rs.getInt("id"))
        ps.executeUpdate()
        count += 1
      }
      // rivals
      val rs2 = conn.createStatement().executeQuery(
        "SELECT nombre FROM rivals WHERE nombre LIKE '%Ã%'")
      val ps2 = conn.prepareStatement("UPDATE rivals SET nombre=? WHERE nombre=?")
      while (rs2.next()) {
        val nombreOld = rs2.getString("nombre")
        ps2.setString(1, fixEncoding(nombreOld))
        ps2.setString(2, nombreOld)
        ps2.executeUpdate()
        count += 1
      }
    } finally { conn.close() }
    renderHtml(s"<h2>Fix encoding completado — $count registros corregidos</h2>")
  }

  @cask.get("/admin/init_legends")
  def initLegendsAction() = {
    val msg = DatabaseManager.initLegendsTable()
    cask.Response(msg.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/plain"))
  }
  @cask.postForm("/admin/update_obj")
  def updateObj(id: Int, meta: Int) = {
    DatabaseManager.updateObjective(id, meta)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/admin"))
  }
  @cask.get("/admin/download_csv")
  def downloadCsv() = {
    cask.Response(DatabaseManager.getBackupCSV().getBytes("UTF-8"),
      headers = Seq(
        "Content-Type"        -> "text/csv; charset=utf-8",
        "Content-Disposition" -> "attachment; filename=guardian_backup.csv"
      )
    )
  }
  // BLOQUE H: exportacion completa — un CSV por tabla principal, empaquetados en ZIP
  @cask.get("/admin/download_full_csv")
  def downloadFullCsv() = {
    val csvMap = DatabaseManager.getFullExportCSV()
    val baos = new java.io.ByteArrayOutputStream()
    val zos = new java.util.zip.ZipOutputStream(baos)
    csvMap.foreach { case (nombre, contenido) =>
      zos.putNextEntry(new java.util.zip.ZipEntry(nombre))
      zos.write(contenido.getBytes("UTF-8"))
      zos.closeEntry()
    }
    zos.close()
    cask.Response(baos.toByteArray,
      headers = Seq(
        "Content-Type" -> "application/zip",
        "Content-Disposition" -> s"attachment; filename=guardian_hector_${java.time.LocalDate.now()}.zip"
      ))
  }
  @cask.get("/admin/print_report")
  def printReport() = {
    val card    = DatabaseManager.getLatestCardData()
    val matches = DatabaseManager.getMatchesList()
    val evolution = DatabaseManager.getSeasonEvolution()

    val totalPj = matches.size
    val totalGc = matches.map(m => m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0)).sum
    val avgNota = if (matches.nonEmpty) f"${matches.map(_.nota).sum / matches.size}%.1f" else "—"
    val pcs     = matches.count(m => m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).contains(0))
    val totalPar = matches.map(_.paradas).sum

    val aniosJs  = evolution.map(e => s""""${e._1}"""").mkString("[",",","]")
    val mediasJs = evolution.map(e => f"${e._2}%.1f").mkString("[",",","]")
    val gcsJs    = evolution.map(_._4.toString).mkString("[",",","]")

    val matchRows = matches.take(30).map { m =>
      val gcMatch = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0)
      val notaColor = if(m.nota >= 7) "#27ae60" else if(m.nota >= 5) "#e67e22" else "#c0392b"
      s"""<tr>
        <td>${m.fecha}</td>
        <td><b>${m.rival}</b></td>
        <td style="text-align:center;">${m.resultado}</td>
        <td style="text-align:center;">${m.paradas}</td>
        <td style="text-align:center; color:$notaColor; font-weight:bold;">${m.nota}</td>
        <td style="text-align:center;">${if(gcMatch==0)"✓" else ""}</td>
      </tr>"""
    }.mkString("")

    val htmlStr = s"""<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="utf-8"/>
<title>Informe Guardian Elite - ${card.nombre}</title>
<script src="https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"></script>
<style>
  @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@400;700&display=swap');
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: 'Oswald', sans-serif; color: #1a1a1a; background: #fff; padding: 20px; }
  .no-print { text-align:center; margin-bottom:24px; }
  .print-btn { background:#d4af37; color:#000; border:none; padding:12px 32px; font-size:16px; font-weight:700; border-radius:6px; cursor:pointer; letter-spacing:1px; }
  .header { display:flex; justify-content:space-between; align-items:center; border-bottom:3px solid #d4af37; padding-bottom:16px; margin-bottom:24px; }
  .header-title h1 { font-size:28px; color:#1a1a1a; letter-spacing:2px; }
  .header-title p { color:#666; font-size:13px; margin-top:4px; }
  .stats-grid { display:grid; grid-template-columns:repeat(5,1fr); gap:12px; margin-bottom:24px; }
  .stat-card { border:2px solid #e0e0e0; border-radius:8px; text-align:center; padding:12px; }
  .stat-card .value { font-size:28px; font-weight:700; color:#d4af37; }
  .stat-card .label { font-size:11px; color:#888; margin-top:4px; text-transform:uppercase; letter-spacing:0.5px; }
  .attrs-grid { display:grid; grid-template-columns:repeat(6,1fr); gap:8px; margin-bottom:24px; }
  .attr-box { border:1px solid #ddd; border-radius:6px; text-align:center; padding:10px 6px; }
  .attr-box .av { font-size:24px; font-weight:700; }
  .attr-box .al { font-size:10px; color:#888; }
  .charts-row { display:grid; grid-template-columns:1fr 1fr; gap:20px; margin-bottom:24px; }
  .chart-box { border:1px solid #e0e0e0; border-radius:8px; padding:16px; }
  .chart-box h3 { font-size:13px; color:#666; margin-bottom:12px; text-transform:uppercase; letter-spacing:0.5px; }
  table { width:100%; border-collapse:collapse; font-size:12px; }
  thead tr { background:#1a1a1a; color:white; }
  th,td { border:1px solid #e0e0e0; padding:7px 10px; }
  tbody tr:nth-child(even) { background:#f9f9f9; }
  .section-title { font-size:16px; font-weight:700; color:#1a1a1a; border-left:4px solid #d4af37; padding-left:10px; margin-bottom:12px; }
  .footer { margin-top:24px; text-align:center; color:#aaa; font-size:11px; border-top:1px solid #eee; padding-top:12px; }
  @media print {
    .no-print { display:none; }
    body { padding:10px; }
    .charts-row canvas { max-height:200px; }
  }
</style>
</head>
<body>
<div class="no-print">
  <button class="print-btn" onclick="window.print()">IMPRIMIR / GUARDAR PDF</button>
</div>

<div class="header">
  <div class="header-title">
    <h1>GUARDIAN ELITE — INFORME</h1>
    <p>${card.nombre} | ${card.posicion} | Generado: ${java.time.LocalDate.now()}</p>
  </div>
  <div style="text-align:right;">
    <div style="font-size:40px; font-weight:700; color:#d4af37;">${card.media}</div>
    <div style="font-size:12px; color:#666;">MEDIA GLOBAL</div>
  </div>
</div>

<div class="stats-grid">
  <div class="stat-card"><div class="value">$totalPj</div><div class="label">Partidos</div></div>
  <div class="stat-card"><div class="value">$totalPar</div><div class="label">Paradas</div></div>
  <div class="stat-card"><div class="value" style="color:#27ae60;">$pcs</div><div class="label">Port. a 0</div></div>
  <div class="stat-card"><div class="value" style="color:#c0392b;">$totalGc</div><div class="label">Goles enc.</div></div>
  <div class="stat-card"><div class="value">$avgNota</div><div class="label">Nota media</div></div>
</div>

<p class="section-title">ATRIBUTOS</p>
<div class="attrs-grid">
  <div class="attr-box"><div class="av" style="color:#3498db;">${card.div}</div><div class="al">DIV</div></div>
  <div class="attr-box"><div class="av" style="color:#9b59b6;">${card.han}</div><div class="al">HAN</div></div>
  <div class="attr-box"><div class="av" style="color:#e67e22;">${card.kic}</div><div class="al">KIC</div></div>
  <div class="attr-box"><div class="av" style="color:#e74c3c;">${card.ref}</div><div class="al">REF</div></div>
  <div class="attr-box"><div class="av" style="color:#2ecc71;">${card.spd}</div><div class="al">SPD</div></div>
  <div class="attr-box"><div class="av" style="color:#f1c40f;">${card.pos}</div><div class="al">POS</div></div>
</div>

<div class="charts-row">
  <div class="chart-box">
    <h3>Evolucion de nota media</h3>
    <canvas id="chartNota" height="180"></canvas>
  </div>
  <div class="chart-box">
    <h3>Goles encajados por temporada</h3>
    <canvas id="chartGc" height="180"></canvas>
  </div>
</div>

<p class="section-title">HISTORIAL DE PARTIDOS (ULTIMOS 30)</p>
<table>
  <thead><tr><th>Fecha</th><th>Rival</th><th>Res.</th><th>Par.</th><th>Nota</th><th>P0</th></tr></thead>
  <tbody>$matchRows</tbody>
</table>

<div class="footer">
  Guardian Elite v6.0 — Borja Martin R&D Edition — "No buscamos porteros que paren. Buscamos atletas que piensen, lideren y dominen."
</div>

<script>
  const anios = $aniosJs;
  const medias = $mediasJs;
  const gcs = $gcsJs;
  if (anios.length > 0) {
    new Chart(document.getElementById('chartNota'), {
      type: 'line',
      data: { labels: anios, datasets: [{ label: 'Nota', data: medias, borderColor: '#d4af37', backgroundColor: 'rgba(212,175,55,0.15)', borderWidth:2, pointRadius:4, fill:true, tension:0.3 }] },
      options: { responsive:true, plugins:{ legend:{ display:false } }, scales:{ y:{ min:0, max:100 } } }
    });
    new Chart(document.getElementById('chartGc'), {
      type: 'bar',
      data: { labels: anios, datasets: [{ label: 'Goles enc.', data: gcs, backgroundColor: 'rgba(220,53,69,0.7)', borderRadius:3 }] },
      options: { responsive:true, plugins:{ legend:{ display:false } } }
    });
  }
</script>
</body>
</html>"""

    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.get("/admin/importer")
  def importerPage() = {
    val content = basePage("settings",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8",
          h2(cls := "text-info text-center mb-4", "IMPORTADOR DE DATOS"),
          div(cls := "card bg-dark text-white border-success shadow p-4 mb-4",
            h4("🌍 Conexion RFFM"),
            p(cls := "small text-muted fw-bold", "Descarga calendario y rivales directamente de la Federacion."),
            form(action := "/admin/sync_rffm", method := "post",
              button(tpe := "submit", cls := "btn btn-success w-100 fw-bold", "🔄 Sincronizar Calendario")
            )
          ),
          div(cls := "card bg-dark text-white border-primary shadow p-4 mb-4",
            h4("📅 Importar Calendario Manual"),
            p(cls := "small text-muted fw-bold", "Formato: FECHA, RIVAL, TIPO"),
            form(action := "/admin/upload_calendar", method := "post",
              textarea(name := "csvContent", cls := "form-control mb-3 fw-bold", rows := "3"),
              button(tpe := "submit", cls := "btn btn-primary w-100 fw-bold", "Cargar")
            )
          ),
          div(cls := "card bg-dark text-white border-warning shadow p-4 mb-4",
            h4("Importar Historial"),
            form(action := "/admin/upload_matches", method := "post",
              textarea(name := "csvContent", cls := "form-control mb-3 fw-bold", rows := "3"),
              button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", "Procesar")
            )
          ),
          div(cls := "card bg-dark text-white border-info shadow p-4",
            h4("Importar Wellness"),
            form(action := "/admin/upload_wellness", method := "post",
              textarea(name := "csvContent", cls := "form-control mb-3 fw-bold", rows := "3"),
              button(tpe := "submit", cls := "btn btn-info w-100 fw-bold", "Procesar")
            )
          ),
          div(cls := "mt-3 text-center",
            a(href := "/admin", cls := "btn btn-outline-light fw-bold", "Volver")
          )
        )
      )
    )
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/admin/upload_calendar")
  def uploadCalendar(csvContent: String) = {
    val res     = DatabaseManager.importCalendarCSV(fixEncoding(csvContent))
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.style(raw(getCss()))),
      body(style := "background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';",
        h1("CALENDARIO"), h3(res),
        div(style := "margin-top:20px;", a(href := "/admin/importer", cls := "btn btn-primary fw-bold", "Volver"))
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/admin/upload_matches")
  def uploadMatches(csvContent: String) = {
    val res     = DatabaseManager.importMatchesCSV(fixEncoding(csvContent))
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.style(raw(getCss()))),
      body(style := "background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';",
        h1("IMPORTACION"), h3(res),
        div(style := "margin-top:20px;", a(href := "/admin/importer", cls := "btn btn-warning fw-bold", "Volver"))
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/admin/upload_wellness")
  def uploadWellness(csvContent: String) = {
    val res     = DatabaseManager.importWellnessCSV(fixEncoding(csvContent))
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.style(raw(getCss()))),
      body(style := "background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';",
        h1("IMPORTACION"), h3(res),
        div(style := "margin-top:20px;", a(href := "/admin/importer", cls := "btn btn-info fw-bold", "Volver"))
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/admin/sync_rffm")
  def syncRffmAction() = {
    val log     = DatabaseManager.syncRFFMCalendar()
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.style(raw(getCss()))),
      body(style := "background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';",
        h1("SCOUTING 2.0"),
        pre(style := "text-align:left; background:#333; padding:20px; margin:20px;", log),
        div(style := "margin-top:20px;",
          a(href := "/admin/importer", cls := "btn btn-primary fw-bold", "Volver")
        )
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.get("/admin/test-ai")
  def testAI(request: cask.Request) = withAuth(request) {
    val resultado = DatabaseManager.testAIConnection()
    renderHtml(basePage("settings", div(cls:="container mt-5 text-center",
      h2("Diagnostico de IA"),
      div(cls:=s"alert ${if(resultado.contains("OK")) "alert-success" else "alert-danger"}", resultado),
      a(href:="/admin", cls:="btn btn-primary", "Volver")
    )))
  }

  @cask.get("/tactics") def tacticsPage(request: cask.Request) = withAuth(request) {
    val content = basePage("tactics",
      div(cls := "row justify-content-center",
        div(cls := "col-12",
          h2(cls := "text-center text-info mb-3", "PIZARRA TACTICA"),
          div(cls:="d-flex justify-content-center gap-2 mb-3",
            button(cls:="btn btn-outline-light", onclick:="setColor('#ffffff')", "⚪"),
            button(cls:="btn btn-outline-warning", onclick:="setColor('#ffc107')", "🟡"),
            button(cls:="btn btn-outline-danger", onclick:="setColor('#dc3545')", "🔴"),
            button(cls:="btn btn-outline-info", onclick:="setColor('#0dcaf0')", "🔵"),
            button(cls:="btn btn-secondary", onclick:="clearBoard()", "🗑 BORRAR")
          ),
          div(cls:="field-container shadow border border-secondary",
            canvas(id:="tacticsBoard", width:="350", height:="500")
          ),
          div(cls:="text-center text-muted small mt-2", "Dibuja con el dedo para explicar la jugada.")
        )
      ),
      script(raw("""
        const canvas = document.getElementById('tacticsBoard');
        const ctx = canvas.getContext('2d');
        let painting = false;
        let color = '#ffffff';

        function resize() {
          const parent = canvas.parentElement;
          canvas.width = parent.clientWidth;
          canvas.height = parent.clientHeight;
          drawField();
        }
        window.addEventListener('resize', resize);

        function drawField() {
          ctx.fillStyle = '#2e7d32';
          ctx.fillRect(0, 0, canvas.width, canvas.height);
          ctx.fillStyle = 'rgba(255,255,255,0.05)';
          for(let i=0; i<canvas.height; i+=40) ctx.fillRect(0, i, canvas.width, 20);
          ctx.strokeStyle = 'rgba(255,255,255,0.8)';
          ctx.lineWidth = 2;
          ctx.beginPath();
          ctx.rect(10, 10, canvas.width-20, canvas.height-20);
          const midX = canvas.width / 2;
          const topBoxY = 10;
          const botBoxY = canvas.height - 10;
          ctx.rect(midX - 100, topBoxY, 200, 100);
          ctx.rect(midX - 100, botBoxY - 100, 200, 100);
          ctx.rect(midX - 40, topBoxY, 80, 40);
          ctx.rect(midX - 40, botBoxY - 40, 80, 40);
          ctx.moveTo(10, canvas.height/2);
          ctx.lineTo(canvas.width-10, canvas.height/2);
          ctx.moveTo(midX + 40, canvas.height/2);
          ctx.arc(midX, canvas.height/2, 40, 0, Math.PI * 2);
          ctx.stroke();
          ctx.fillStyle = 'rgba(255,255,255,0.8)';
          ctx.beginPath();
          ctx.arc(midX, topBoxY + 80, 3, 0, Math.PI * 2);
          ctx.arc(midX, botBoxY - 80, 3, 0, Math.PI * 2);
          ctx.arc(midX, canvas.height/2, 3, 0, Math.PI * 2);
          ctx.fill();
        }

        function startPosition(e) { painting = true; draw(e); }
        function finishedPosition() { painting = false; ctx.beginPath(); }
        function draw(e) {
          if (!painting) return;
          e.preventDefault();
          const rect = canvas.getBoundingClientRect();
          const clientX = e.touches ? e.touches[0].clientX : e.clientX;
          const clientY = e.touches ? e.touches[0].clientY : e.clientY;
          const x = clientX - rect.left;
          const y = clientY - rect.top;
          ctx.lineWidth = 3;
          ctx.lineCap = 'round';
          ctx.strokeStyle = color;
          ctx.lineTo(x, y);
          ctx.stroke();
          ctx.beginPath();
          ctx.moveTo(x, y);
        }

        canvas.addEventListener('mousedown', startPosition);
        canvas.addEventListener('mouseup', finishedPosition);
        canvas.addEventListener('mousemove', draw);
        canvas.addEventListener('touchstart', startPosition);
        canvas.addEventListener('touchend', finishedPosition);
        canvas.addEventListener('touchmove', draw);

        function setColor(c) { color = c; }
        function clearBoard() { drawField(); }
        setTimeout(resize, 100);
      """))
    )
    renderHtml(content)
  }

  // NOTA: basePage() y getCss() se heredan de SharedLayout (import SharedLayout._).
  // Antes habia una copia local desactualizada aqui (solo 7 botones de nav) que
  // eclipsaba la de SharedLayout y dejaba la barra de navegacion incompleta en
  // /tactics y en varias paginas de /settings.

  // ==========================================
  // PAGINAS FALTANTES (RESTAURADAS)
  // ==========================================

  // --- 1. EL ORACULO (Prediccion de Altura) ---

  // ── INFORME DE CAPTACION ANONIMO ────────────────────────────────────────────
  @cask.get("/admin/captacion")
  def captacionReport(request: cask.Request) = withAuth(request) {
    val card   = DatabaseManager.getLatestCardData()
    val career = DatabaseManager.getCareerSummary()
    val matches = DatabaseManager.getMatchesList().take(20)
    val avgNota = if (matches.nonEmpty) f"${matches.map(_.nota).sum / matches.size}%.1f" else "—"
    val totalPJ = matches.size
    val totalCS = matches.count(m => { val p = m.resultado.split("-"); p.lastOption.flatMap(_.trim.toIntOption).getOrElse(1) == 0 })
    val totalGC = matches.map(m => m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0)).sum
    val csRate  = if (totalPJ > 0) f"${totalCS * 100 / totalPJ}%%" else "—"

    val htmlContent = "<!DOCTYPE html>" + html(
      head(
        meta(charset := "utf-8"),
        tags2.title("Dossier Captacion"),
        tags2.style(raw("""
          @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@400;600;700&family=Roboto:wght@300;400;500&display=swap');
          * { margin:0; padding:0; box-sizing:border-box; }
          body { font-family:'Roboto',sans-serif; background:#fff; color:#222; }
          .cover { background:linear-gradient(135deg,#1a1a2e 0%,#16213e 50%,#0f3460 100%); color:#fff; padding:60px 40px; text-align:center; }
          .cover h1 { font-family:'Oswald',sans-serif; font-size:42px; letter-spacing:4px; color:#d4af37; }
          .cover .subtitle { font-size:14px; color:#aaa; letter-spacing:3px; margin-top:8px; text-transform:uppercase; }
          .cover .anon-badge { display:inline-block; margin-top:16px; background:rgba(212,175,55,0.2); border:1px solid #d4af37; padding:4px 16px; border-radius:20px; font-size:11px; color:#d4af37; letter-spacing:2px; }
          .section { padding:32px 40px; }
          .section h2 { font-family:'Oswald',sans-serif; font-size:18px; letter-spacing:2px; color:#0f3460; border-bottom:2px solid #d4af37; padding-bottom:6px; margin-bottom:20px; text-transform:uppercase; }
          .kpi-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:16px; margin-bottom:24px; }
          .kpi { text-align:center; padding:16px; background:#f8f9fa; border-radius:8px; border-top:3px solid #d4af37; }
          .kpi .val { font-family:'Oswald',sans-serif; font-size:32px; font-weight:700; color:#0f3460; }
          .kpi .lbl { font-size:10px; color:#888; text-transform:uppercase; letter-spacing:1px; margin-top:4px; }
          .attr-grid { display:grid; grid-template-columns:repeat(6,1fr); gap:8px; margin-bottom:24px; }
          .attr { text-align:center; padding:12px 6px; background:#0f3460; border-radius:6px; }
          .attr .val { font-family:'Oswald',sans-serif; font-size:26px; font-weight:700; color:#d4af37; }
          .attr .lbl { font-size:9px; color:#aaa; text-transform:uppercase; letter-spacing:1px; }
          .season-row { display:flex; justify-content:space-between; padding:10px 0; border-bottom:1px solid #eee; font-size:13px; }
          .match-row { display:grid; grid-template-columns:80px 1fr 60px 60px; gap:8px; padding:6px 0; border-bottom:1px solid #eee; font-size:12px; align-items:center; }
          .nota-badge { text-align:center; padding:2px 8px; border-radius:12px; font-weight:700; font-size:11px; }
          .nota-good { background:#d4edda; color:#155724; }
          .nota-med  { background:#fff3cd; color:#856404; }
          .nota-bad  { background:#f8d7da; color:#721c24; }
          .footer { background:#1a1a2e; color:#aaa; text-align:center; padding:20px; font-size:11px; letter-spacing:1px; }
          @media print { body { -webkit-print-color-adjust:exact; } }
        """))
      ),
      body(
        // Portada
        div(cls := "cover",
          div(style := "font-size:64px; margin-bottom:16px;", "🛡️"),
          div(cls := "subtitle", "DOSSIER TECNICO DE CAPTACION"),
          h1("PORTERO — GK"),
          div(cls := "anon-badge", "DATOS PERSONALES ANONIMIZADOS"),
          div(style := "margin-top:24px; color:#888; font-size:12px;",
            s"Generado: ${java.time.LocalDate.now()} | Guardian Elite v6.0")
        ),

        // KPIs globales
        div(cls := "section",
          h2("Estadisticas Globales"),
          div(cls := "kpi-grid",
            Seq(
              ("Partidos jugados", totalPJ.toString),
              ("Media global", avgNota),
              ("Porterias a 0", s"$totalCS ($csRate)"),
              ("Goles encajados", totalGC.toString)
            ).map { case (lbl, v) =>
              div(cls := "kpi",
                div(cls := "val", v),
                div(cls := "lbl", lbl)
              )
            }
          ),

          // Atributos
          h2("Perfil Tecnico"),
          div(cls := "attr-grid",
            Seq(("DIV", card.div), ("HAN", card.han), ("KIC", card.kic),
              ("REF", card.ref), ("SPD", card.spd), ("POS", card.pos)).map { case (lbl, v) =>
              div(cls := "attr",
                div(cls := "val", v.toString),
                div(cls := "lbl", lbl)
              )
            }
          ),

          // Trayectoria por temporadas
          if (career.nonEmpty) div(
            h2("Trayectoria por Temporada"),
            frag(career.toSeq.map { s =>
              div(cls := "season-row",
                span(style := "font-weight:600; color:#0f3460;", s.categoria),
                span(s"${s.partidosJugados} partidos jugados"),
                span(s"${s.golesContra} goles encajados"),
                span(style := "font-weight:700; color:#d4af37;", s"Media: ${s.mediaFinal}")
              )
            }: _*)
          ) else div(),

          // Ultimos 20 partidos (sin nombre del rival)
          h2("Ultimos Partidos (datos tecnicos)"),
          div(
            div(cls := "match-row",
              span(style:="font-weight:700; font-size:10px; color:#888;", "FECHA"),
              span(style:="font-weight:700; font-size:10px; color:#888;", "RESULTADO"),
              span(style:="font-weight:700; font-size:10px; color:#888; text-align:center;", "PARADAS"),
              span(style:="font-weight:700; font-size:10px; color:#888; text-align:center;", "NOTA")
            ),
            matches.map { m =>
              val notaCls = if (m.nota >= 7) "nota-good" else if (m.nota >= 5) "nota-med" else "nota-bad"
              div(cls := "match-row",
                span(style:="color:#888;", m.fecha.take(10)),
                span(style:="font-weight:500;", m.resultado),
                span(style:="text-align:center;", m.paradas.toString),
                span(cls := s"nota-badge $notaCls", m.nota.toString)
              )
            }
          )
        ),

        // Footer
        div(cls := "footer",
          "GUARDIAN ELITE v6.0 — Dossier generado automaticamente | Datos personales anonimizados para proceso de captacion",
          div(style := "margin-top:6px;",
            a(href := "javascript:window.print()",
              style := "color:#d4af37; font-weight:700; font-size:13px; letter-spacing:2px; text-decoration:none; cursor:pointer;",
              "IMPRIMIR / GUARDAR PDF")
          )
        )
      )
    ).render
    renderHtml(htmlContent)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 2.6 — RUTAS DE GESTION DE TEMPORADAS
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.post("/admin/season-action/close")
  def cerrarTemporada(request: cask.Request) = withAuth(request) {
    val msg = DatabaseManager.cerrarTemporadaActual() match {
      case Right(m) => m
      case Left(e) => e
    }
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/admin?msg=${java.net.URLEncoder.encode(msg, "UTF-8")}"
    ))
  }

  @cask.postForm("/admin/season-action/new")
  def nuevaTemporada(request: cask.Request, categoria: String, nombreClub: String = "", fechaInicio: String = "") = withAuth(request) {
    val msg = DatabaseManager.startNewSeason(fixEncoding(categoria), fixEncoding(nombreClub), fechaInicio) match {
      case Right(m) => m
      case Left(e) => e
    }
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/admin?msg=${java.net.URLEncoder.encode(msg, "UTF-8")}"
    ))
  }

  @cask.get("/admin/season-report/:id")
  def verInformeTemporada(request: cask.Request, id: Int) = withAuth(request) {
    DatabaseManager.getInformeFinTemporada(id) match {
      case Some((nombre, informe)) if informe.nonEmpty =>
        val htmlStr = s"""<!DOCTYPE html>
<html lang="es"><head><meta charset="utf-8"/><title>Informe — $nombre</title>
<style>
  @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@400;700&display=swap');
  body { font-family:'Oswald',sans-serif; background:#fff; color:#1a1a1a; padding:40px; max-width:800px; margin:0 auto; }
  h2 { color:#d4af37; border-bottom:3px solid #d4af37; padding-bottom:10px; }
  h3 { color:#0f3460; margin-top:24px; }
  .no-print { text-align:center; margin-bottom:24px; }
  .print-btn { background:#d4af37; color:#000; border:none; padding:12px 32px; font-size:16px; font-weight:700; border-radius:6px; cursor:pointer; }
  @media print { .no-print { display:none; } }
</style></head>
<body>
<div class="no-print"><button class="print-btn" onclick="window.print()">IMPRIMIR / GUARDAR PDF</button></div>
$informe
</body></html>"""
        cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
      case Some((nombre, _)) =>
        val htmlStr = doctype("html")(html(
          head(meta(charset := "utf-8"), tags2.style(raw(getCss()))),
          body(style := "background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';",
            h1("Informe no generado"), h3(s"Temporada: $nombre"),
            form(action := s"/admin/season-report/$id/generate", method := "post", cls := "d-inline-block mt-3",
              button(tpe := "submit", cls := "btn btn-warning fw-bold", "🧠 Generar informe ahora")
            ),
            div(style := "margin-top:20px;", a(href := "/admin", cls := "btn btn-outline-light fw-bold", "Volver"))
          )
        )).render
        cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
      case None =>
        cask.Response("Temporada no encontrada".getBytes("UTF-8"), statusCode = 404, headers = Seq("Content-Type" -> "text/plain; charset=utf-8"))
    }
  }

  @cask.post("/admin/season-report/:id/generate")
  def generarInformeTemporadaAction(request: cask.Request, id: Int) = withAuth(request) {
    DatabaseManager.generarInformeFinTemporada(id)
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/admin?msg=${java.net.URLEncoder.encode("Generando informe en segundo plano, vuelve a consultar en un minuto.", "UTF-8")}"
    ))
  }

  initialize()
}
