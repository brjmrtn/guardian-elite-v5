import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

import java.net.URLEncoder

object MatchController extends cask.Routes {

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A — REGISTRO DE PARTIDO POR NLP (solo Elite; nunca toca tablas am_*)
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.post("/match-center/nlp")
  def matchCenterNlp(request: cask.Request) = withAuth(request) {
    val bodyString = new String(request.data.readAllBytes(), "UTF-8")
    val formData = bodyString.split("&").filter(_.nonEmpty).map { part =>
      val pair = part.split("=", 2)
      val key = java.net.URLDecoder.decode(pair(0), "UTF-8")
      val value = if (pair.length > 1) java.net.URLDecoder.decode(pair(1), "UTF-8") else ""
      key -> value
    }.toMap
    val texto = formData.getOrElse("texto", "")

    if (texto.trim.isEmpty) {
      val json = ujson.Obj("error" -> "Texto vacío")
      cask.Response(json.render().getBytes("UTF-8"), statusCode = 400, headers = Seq("Content-Type" -> "application/json"))
    } else {
      val raw = DatabaseManager.extraerPartidoNLP(texto)
      try {
        val parsed = ujson.read(raw) // valida que sea JSON antes de reenviarlo al frontend
        cask.Response(parsed.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
      } catch {
        case _: Exception =>
          val json = ujson.Obj("error" -> "No se pudo interpretar la respuesta de la IA", "confianza" -> 0.0)
          cask.Response(json.render().getBytes("UTF-8"), statusCode = 502, headers = Seq("Content-Type" -> "application/json"))
      }
    }
  }

  // BLOQUE D: clima automatico (Open-Meteo) — solo lectura, nunca Gemini
  @cask.get("/match-center/clima")
  def matchCenterClima(fecha: String) = {
    val clima = DatabaseManager.getClimaParaFecha(fecha)
    val json = ujson.Obj("clima" -> clima)
    cask.Response(json.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
  }

  // BLOQUE B1: autopercepcion pre-partido registrada desde el banner del dashboard — SQL puro, sin Gemini
  @cask.post("/match/autopercepcion")
  def guardarAutopercepcion(request: cask.Request) = withAuth(request) {
    val bodyString = new String(request.data.readAllBytes(), "UTF-8")
    val formData = bodyString.split("&").filter(_.nonEmpty).map { part =>
      val pair = part.split("=", 2)
      java.net.URLDecoder.decode(pair(0), "UTF-8") -> (if (pair.length > 1) java.net.URLDecoder.decode(pair(1), "UTF-8") else "")
    }.toMap
    val valor = formData.getOrElse("valor", "").toIntOption.getOrElse(0)
    if (valor >= 1 && valor <= 5) DatabaseManager.guardarAutopercepcionTemporal(valor)
    val json = ujson.Obj("ok" -> (valor >= 1 && valor <= 5))
    cask.Response(json.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
  }

  // BLOQUE A6 (RFMF): confirmar/descartar un resultado detectado automaticamente desde rffm.es
  @cask.post("/match/rfmf-pendiente/:matchId/confirmar")
  def confirmarRfmfPendiente(request: cask.Request, matchId: Int) = withAuth(request) {
    DatabaseManager.confirmarPartidoRFMFPendiente(matchId)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/"))
  }

  @cask.post("/match/rfmf-pendiente/:matchId/descartar")
  def descartarRfmfPendiente(request: cask.Request, matchId: Int) = withAuth(request) {
    DatabaseManager.descartarPartidoRFMFPendiente(matchId)
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/"))
  }

  @cask.get("/match-center")
  def matchCenterPage(request: cask.Request, scheduleId: Int = 0) = withAuth(request) {
    val today = java.time.LocalDate.now().toString
    var preRival = ""; var preFecha = today; var isScheduled = false; var preEstadio = ""
    // BLOQUE B2: si ya se registro la autopercepcion desde el banner del dashboard, viene pre-seleccionada
    val autopercepcionPrefill = DatabaseManager.getAutopercepcionTemporalHoy()
    // BLOQUE O: rutina pre-partido definida (si existe, se pregunta en el formulario)
    val rutinaActiva = DatabaseManager.getRutinaActiva()

    // Si venimos de un partido programado, cargamos datos
    if(scheduleId > 0) {
      val matches = DatabaseManager.getUpcomingMatches()
      matches.find(_.id == scheduleId).foreach { m =>
        preRival = m.rival
        preFecha = m.fecha
        preEstadio = m.estadio
        isScheduled = true
      }
    }

    // Celdas de la porteria
    val gridCells = for(r <- Seq("T","M","B"); c <- Seq("L","C","R")) yield {
      val zoneId = r + c
      div(cls:=s"goal-cell zone-$zoneId", onclick:=s"registerAction('$zoneId')", span(cls:="action-marker", ""))
    }

    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-6 col-12",
          div(cls := "card bg-dark text-white border-warning shadow",
            div(cls := "card-header bg-warning text-dark fw-bold text-center", "MATCH TRACKER PRO"),
            div(cls := "card-body p-3",
              // BLOQUE D: formulario independiente del registro minimo — sus campos viven dentro del
              // formulario principal (no se pueden anidar forms) y se asocian con el atributo form=
              form(id := "quickRegisterForm", action := "/match-center/quick-save", method := "post", attr("accept-charset") := "UTF-8"),
              form(action := "/match-center/save", method := "post", attr("accept-charset") := "UTF-8",

                // ── BLOQUE A: REGISTRO RAPIDO POR VOZ O TEXTO (NLP con Gemini) ──
                div(cls:="mb-4 p-3 border border-warning rounded", style:="background:rgba(212,175,55,0.06);",
                  div(cls:="d-flex justify-content-between align-items-center", style:="cursor:pointer;", onclick:="toggleNlpPanel()",
                    label(cls:="text-warning fw-bold small mb-0", style:="cursor:pointer;", "⚡ REGISTRO RÁPIDO POR VOZ O TEXTO"),
                    span(id:="nlpChevron", cls:="text-warning small", "▲")
                  ),
                  div(id:="nlpPanel", style:="display:block;",
                    div(cls:="xx-small text-muted mt-2 mb-2",
                      "Describe el partido libremente y la IA rellenará el formulario. Tú siempre revisas y guardas."),
                    textarea(id:="nlpTexto", cls:="form-control form-control-sm bg-dark text-white border-warning mb-2", rows:="3",
                      placeholder:="Ej: 'Ganamos 2-1 al Rayo B fuera de casa, llovía. Héctor estuvo muy bien, sacó 5 paradas, le puse un 8. Sin comunicar mucho con la defensa.'"),
                    div(cls:="d-flex gap-2",
                      button(tpe:="button", id:="nlpDictarBtn", cls:="btn btn-sm btn-outline-warning fw-bold", onclick:="nlpToggleDictado()", "🎤 Dictar"),
                      button(tpe:="button", id:="nlpExtraerBtn", cls:="btn btn-sm btn-warning fw-bold flex-fill", onclick:="nlpExtraer()", "🧠 Extraer datos")
                    ),
                    div(id:="nlpStatus", cls:="xx-small text-muted mt-2"),
                    div(id:="nlpConfianzaWarning", cls:="xx-small text-danger mt-1", style:="display:none;",
                      "⚠️ Algunos campos pueden no ser correctos — revísalos antes de guardar")
                  )
                ),

                // ── BLOQUE D: REGISTRO MINIMO EN 30 SEGUNDOS (rival, resultado, nota) ──
                div(cls:="mb-4 p-3 border border-info rounded", style:="background:rgba(13,202,240,0.05);",
                  div(cls:="d-flex justify-content-between align-items-center", style:="cursor:pointer;",
                    onclick:="var p=document.getElementById('quickPanel');var c=document.getElementById('quickChevron');var o=p.style.display!=='none';p.style.display=o?'none':'block';c.textContent=o?'▼':'▲';",
                    label(cls:="text-info fw-bold small mb-0", style:="cursor:pointer;", "⚡ REGISTRO MÍNIMO"),
                    span(id:="quickChevron", cls:="text-info small", "▼")
                  ),
                  div(id:="quickPanel", style:="display:none;",
                    div(cls:="xx-small text-muted mt-2 mb-2", "Solo rival, resultado y nota. Podrás completar el resto después con ✏️ Editar."),
                    input(tpe:="text", name:="rival", attr("form"):="quickRegisterForm", required:=true, attr("list"):="rivalesSugeridos", attr("autocomplete"):="off",
                      cls:="form-control form-control-sm bg-dark text-white border-info fw-bold mb-2", placeholder:="Rival"),
                    div(cls:="d-flex gap-2 align-items-center mb-2",
                      input(tpe:="number", name:="goles_favor", attr("form"):="quickRegisterForm", required:=true, attr("min"):="0", attr("max"):="99",
                        cls:="form-control form-control-sm bg-dark text-white border-info text-center fw-bold", placeholder:="GF"),
                      span(cls:="text-white fw-bold", "-"),
                      input(tpe:="number", name:="goles_contra", attr("form"):="quickRegisterForm", required:=true, attr("min"):="0", attr("max"):="99",
                        cls:="form-control form-control-sm bg-dark text-white border-info text-center fw-bold", placeholder:="GC"),
                      input(tpe:="number", name:="nota", attr("form"):="quickRegisterForm", required:=true, attr("min"):="0", attr("max"):="10", attr("step"):="0.5",
                        cls:="form-control form-control-sm bg-dark text-white border-info text-center fw-bold", placeholder:="Nota")
                    ),
                    button(tpe:="submit", attr("form"):="quickRegisterForm", cls:="btn btn-sm btn-info fw-bold w-100", "Guardar registro mínimo")
                  )
                ),

                // 1. DATOS GENERALES
                input(tpe:="hidden", name:="scheduleId", value:=scheduleId.toString),
                div(cls:="mb-3", label(cls:="form-label text-white fw-bold small", "TIPO DE PARTIDO"),
                  if(isScheduled) {
                    div(input(tpe:="hidden", name:="tipo", value:="LIGA"), input(tpe:="text", cls:="form-control bg-dark text-white border-primary fw-bold", value:="🏆 LIGA (OFICIAL RFFM)", readonly:=true))
                  } else {
                    div(cls:="d-flex", select(name:="tipo", cls:="form-select bg-dark text-white fw-bold flex-grow-1", option(value:="AMISTOSO", "🤝 AMISTOSO"), option(value:="TORNEO", "🏅 TORNEO"), option(value:="LIGA", "🏆 LIGA (Manual)")), a(href:="/tournament/new", cls:="btn btn-sm btn-outline-warning ms-2 d-flex align-items-center fw-bold", "+ CREAR TORNEO"))
                  }
                ),
                div(cls := "mb-3",
                  label(cls := "form-label text-warning fw-bold small", "RIVAL"),
                  input(
                    tpe := "text",
                    name := "rival",
                    id := "rivalInput",
                    attr("list") := "rivalesSugeridos",
                    attr("autocomplete") := "off",
                    cls := "form-control form-control-lg fw-bold text-white",
                    value := (if (preRival.nonEmpty) fixEncoding(preRival) else ""),
                    placeholder := "Ej: Rayo Vallecano",
                    required := true,
                    // CAMBIO CLAVE: Si scheduleId es 0, no se renderiza ningun atributo readonly
                    if (scheduleId > 0) readonly := true else ()
                  )
                ),
                // BLOQUE J: autocompletado de rivales, del mas al menos frecuente, con encoding corregido
                tag("datalist")(id := "rivalesSugeridos", frag(DatabaseManager.getRivalesFrecuentes().map(r => option(value := r)): _*)),
                div(cls := "mb-3", label(cls := "form-label text-white fw-bold small", "FECHA"), input(tpe := "date", name := "fecha", id := "fechaInput", cls := "form-control", value := preFecha)),
                div(cls:="mb-3", label(cls:="form-label text-white fw-bold small", "ESTADIO / CAMPO"), input(tpe:="text", name:="estadio", cls:="form-control bg-dark text-white", value:=fixEncoding(preEstadio), placeholder:="Ej: Valdebebas Campo 3")),
                div(cls:="mb-3",
                  label(cls:="form-label text-white fw-bold small", "¿LOCAL O VISITANTE?"),
                  div(cls:="d-flex gap-2",
                    label(cls:="flex-fill text-center border border-secondary rounded p-2 fw-bold small",
                      style:="cursor:pointer;",
                      input(tpe:="radio", name:="esLocal", value:="", cls:="d-none",
                        attr("checked"):="checked"),
                      span(id:="lblNeutro", "— Sin especificar")
                    ),
                    label(cls:="flex-fill text-center border border-success rounded p-2 fw-bold small text-success",
                      style:="cursor:pointer;",
                      input(tpe:="radio", name:="esLocal", value:="true", cls:="d-none"),
                      span("🏠 Local")
                    ),
                    label(cls:="flex-fill text-center border border-info rounded p-2 fw-bold small text-info",
                      style:="cursor:pointer;",
                      input(tpe:="radio", name:="esLocal", value:="false", cls:="d-none"),
                      span("✈️ Visitante")
                    )
                  ),
                  script(raw("""
                    (function() {
                      var radios = document.querySelectorAll('input[name="esLocal"]');
                      radios.forEach(function(r) {
                        r.addEventListener('change', function() {
                          radios.forEach(function(x) {
                            var lbl = x.parentElement;
                            lbl.style.background = '';
                            lbl.style.opacity = '0.6';
                          });
                          var sel = this.parentElement;
                          sel.style.opacity = '1';
                          if (this.value === 'true')  sel.style.background = 'rgba(40,167,69,0.2)';
                          if (this.value === 'false') sel.style.background = 'rgba(13,202,240,0.2)';
                          if (this.value === '')      sel.style.background = 'rgba(255,255,255,0.05)';
                        });
                      });
                    })();
                  """))
                ),

                // 2. MARCADOR Y PARADAS
                div(cls := "row mb-3 bg-secondary bg-opacity-25 p-2 rounded mx-0",
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "GOLES (GC)"), input(tpe := "number", name := "gc", id:="gcInput", cls := "form-control text-center bg-danger text-white border-0 fw-bold fs-4", value := "0", readonly:=true)),
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "PARADAS"), input(tpe := "number", name := "paradas", id:="parInput", cls := "form-control text-center bg-success text-white border-0 fw-bold fs-4", value := "0", readonly:=true)),
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "A FAVOR (GF)"), input(tpe := "number", name := "gf", cls := "form-control text-center", value := "0", attr("inputmode"):="numeric"))
                ),

                // 3. DISTRIBUCION (EDERSON)
                div(cls:="mb-4 p-2 border border-info rounded bg-info bg-opacity-10", label(cls:="form-label text-info small fw-bold w-100 text-center", "DISTRIBUCION"),
                  div(cls:="row mb-2 align-items-center", div(cls:="col-4 text-end small fw-bold", "CORTO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pc', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pc', false)", "X"), input(tpe:="text", id:="display_pc", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true)))),
                  div(cls:="row align-items-center", div(cls:="col-4 text-end small fw-bold", "LARGO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pl', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pl', false)", "X"), input(tpe:="text", id:="display_pl", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true))))
                ),
                input(tpe:="hidden", name:="passData", id:="passData", value:="0,0,0,0"), input(tpe:="hidden", id:="pcTot", value:="0"), input(tpe:="hidden", id:="pcOk", value:="0"), input(tpe:="hidden", id:="plTot", value:="0"), input(tpe:="hidden", id:="plOk", value:="0"),

                // 4. PORTERIA (REJILLA 3x3)
                div(cls:="tactical-section mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  div(cls:="d-flex justify-content-center mb-2", div(cls:="btn-group w-100", role:="group", input(tpe:="radio", cls:="btn-check", name:="mode", id:="modeSave", autocomplete:="off", checked:=true, onclick:="setMode('save')"), label(cls:="btn btn-outline-success fw-bold", attr("for"):="modeSave", "MODO PARADA"), input(tpe:="radio", cls:="btn-check", name:="mode", id:="modeGoal", autocomplete:="off", onclick:="setMode('goal')"), label(cls:="btn btn-outline-danger fw-bold", attr("for"):="modeGoal", "MODO GOL"))),
                  div(cls:="goal-grid-3x3", gridCells),
                  input(tpe:="hidden", name:="zonaGoles", id:="hiddenGoles"), input(tpe:="hidden", name:="zonaParadas", id:="hiddenParadas"),
                  div(cls:="text-center mt-2 small text-muted", "Toca la zona para registrar la accion"),

                  label(cls:="form-label text-white small fw-bold w-100 text-center mt-3 border-top pt-2", "ACCIONES"),
                  div(cls:="row g-2",
                    div(cls:="col-4", div(cls:="d-grid", button(tpe:="button", cls:="btn btn-outline-info btn-sm", onclick:="incCounter('p1v1')", "1vs1"), input(tpe:="text", id:="disp_p1v1", value:="0", cls:="form-control form-control-sm text-center mt-1 bg-dark text-white border-0", readonly:=true))),
                    div(cls:="col-4", div(cls:="d-grid", button(tpe:="button", cls:="btn btn-outline-warning btn-sm", onclick:="incCounter('pAir')", "Aereo"), input(tpe:="text", id:="disp_pAir", value:="0", cls:="form-control form-control-sm text-center mt-1 bg-dark text-white border-0", readonly:=true))),
                    div(cls:="col-4", div(cls:="d-grid", button(tpe:="button", cls:="btn btn-outline-light btn-sm", onclick:="incCounter('pPie')", "Pie"), input(tpe:="text", id:="disp_pPie", value:="0", cls:="form-control form-control-sm text-center mt-1 bg-dark text-white border-0", readonly:=true)))
                  ),
                  input(tpe:="hidden", name:="actionData", id:="actionData", value:="0,0,0"), input(tpe:="hidden", id:="cnt_p1v1", value:="0"), input(tpe:="hidden", id:="cnt_pAir", value:="0"), input(tpe:="hidden", id:="cnt_pPie", value:="0"),

                  // BLOQUE H: desglose de 1v1 por angulo de entrada (opcional)
                  div(cls:="mb-2 mt-2 p-2 border border-info rounded bg-info bg-opacity-10",
                    div(cls:="d-flex justify-content-between align-items-center", style:="cursor:pointer;", onclick:="toggleAngulo1v1()",
                      label(cls:="text-info fw-bold small mb-0", style:="cursor:pointer;", "📐 DESGLOSE POR ÁNGULO (opcional)"),
                      span(id:="angulo1v1Chevron", cls:="text-info small", "▼")
                    ),
                    div(id:="angulo1v1Panel", style:="display:none;",
                      div(cls:="xx-small text-muted mt-2 mb-2", "Para cada 1v1, indica desde qué ángulo llegó el rival y si fue parada o gol."),
                      Seq(("central", "CENTRAL"), ("izq", "DIAGONAL IZQ"), ("der", "DIAGONAL DER")).map { case (key, label_) =>
                        div(cls:="row g-2 align-items-center mb-2",
                          div(cls:="col-4 xx-small text-white fw-bold", label_),
                          div(cls:="col-4 d-flex align-items-center justify-content-center gap-1",
                            button(tpe:="button", cls:="btn btn-outline-success btn-sm px-2", onclick:=s"adjustAngulo1v1('${key}_ok',-1)", "-"),
                            input(tpe:="text", id:=s"angulo_${key}_ok", value:="0", cls:="form-control form-control-sm text-center bg-dark text-success fw-bold border-success", style:="width:45px;", readonly:=true),
                            button(tpe:="button", cls:="btn btn-outline-success btn-sm px-2", onclick:=s"adjustAngulo1v1('${key}_ok',1)", "+")
                          ),
                          div(cls:="col-4 d-flex align-items-center justify-content-center gap-1",
                            button(tpe:="button", cls:="btn btn-outline-danger btn-sm px-2", onclick:=s"adjustAngulo1v1('${key}_gc',-1)", "-"),
                            input(tpe:="text", id:=s"angulo_${key}_gc", value:="0", cls:="form-control form-control-sm text-center bg-dark text-danger fw-bold border-danger", style:="width:45px;", readonly:=true),
                            button(tpe:="button", cls:="btn btn-outline-danger btn-sm px-2", onclick:=s"adjustAngulo1v1('${key}_gc',1)", "+")
                          )
                        )
                      },
                      div(cls:="row xx-small text-muted", div(cls:="col-4"), div(cls:="col-4 text-center", "paradas"), div(cls:="col-4 text-center", "goles")),
                      input(tpe:="hidden", name:="angulo1v1Data", id:="angulo1v1DataInput", value:="")
                    )
                  ),

                  // SCANNING RATE — Escaneos antes de recibir el balon
                  div(cls:="mb-2 mt-3 p-2 border border-info rounded bg-info bg-opacity-10",
                    label(cls:="form-label text-info small fw-bold w-100 text-center mb-2", "👁️ SCANNING RATE — Escaneos de campo"),
                    div(cls:="d-flex align-items-center justify-content-center gap-3",
                      button(tpe:="button", cls:="btn btn-outline-info btn-sm px-3",
                        onclick:="adjustScanning(-1)", "-"),
                      div(cls:="text-center",
                        input(tpe:="number", name:="scanningRate", id:="scanningRate",
                          value:="0", cls:="form-control form-control-sm text-center bg-dark text-info fw-bold border-info",
                          style:="width:70px; font-size:1.3rem;",
                          attr("inputmode"):="numeric", attr("min"):="0"),
                        div(cls:="xx-small text-muted mt-1", "escaneos")
                      ),
                      button(tpe:="button", cls:="btn btn-outline-info btn-sm px-3",
                        onclick:="adjustScanning(1)", "+")
                    ),
                    div(cls:="text-center xx-small text-muted mt-1",
                      "Nº de veces que mira al campo antes de recibir una cesion"
                    ),
                    // BLOQUE G: efectividad del scanning — de esos escaneos, cuantos encontraron compañero libre
                    div(cls:="d-flex align-items-center justify-content-center gap-3 mt-3 pt-2 border-top border-info",
                      button(tpe:="button", cls:="btn btn-outline-info btn-sm px-3", onclick:="adjustScanningEfectivo(-1)", "-"),
                      div(cls:="text-center",
                        input(tpe:="number", name:="scanningEfectivo", id:="scanningEfectivo",
                          value:="0", cls:="form-control form-control-sm text-center bg-dark text-info fw-bold border-info",
                          style:="width:70px; font-size:1.3rem;",
                          attr("inputmode"):="numeric", attr("min"):="0"),
                        div(cls:="xx-small text-muted mt-1", "efectivos")
                      ),
                      button(tpe:="button", cls:="btn btn-outline-info btn-sm px-3", onclick:="adjustScanningEfectivo(1)", "+")
                    ),
                    div(cls:="text-center xx-small text-muted mt-1",
                      "De esos escaneos, ¿cuántas veces encontró un compañero libre?"
                    )
                  ),

                  // BYPASS RATE — Lineas Superadas en salida con pie
                  div(cls:="mb-2 mt-3 p-2 border border-success rounded bg-success bg-opacity-10",
                    label(cls:="form-label text-success small fw-bold w-100 text-center mb-2", "⚡ BYPASS RATE — Lineas superadas"),
                    div(cls:="d-flex align-items-center justify-content-center gap-3",
                      button(tpe:="button", cls:="btn btn-outline-success btn-sm px-3",
                        onclick:="adjustBypass(-1)", "-"),
                      div(cls:="text-center",
                        input(tpe:="number", name:="lineasSuperadas", id:="lineasSuperadas",
                          value:="0", cls:="form-control form-control-sm text-center bg-dark text-success fw-bold border-success",
                          style:="width:70px; font-size:1.3rem;",
                          attr("inputmode"):="numeric", attr("min"):="0"),
                        div(cls:="xx-small text-muted mt-1", "rivales superados")
                      ),
                      button(tpe:="button", cls:="btn btn-outline-success btn-sm px-3",
                        onclick:="adjustBypass(1)", "+")
                    ),
                    div(cls:="text-center xx-small text-muted mt-1",
                      "Nº de rivales que quedan por detras tras un pase en salida"
                    )
                  ),

                  label(cls:="form-label text-white small fw-bold w-100 text-center mt-3", "ZONAS DE ATAQUE (Tiros)"),
                  div(cls:="shot-origin d-flex gap-2 justify-content-center", div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Left')", "Izquierda"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Center')", "Centro"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Right')", "Derecha"), input(tpe:="hidden", name:="zonaTiros", id:="hiddenOrigin"))
                ),

                // 5. NUEVO: MAPA DE CALOR DE CAMPO (AQUI ESTA LA INTEGRACION)
                div(cls:="mb-4 p-2 border border-success rounded bg-success bg-opacity-10",
                  label(cls:="form-label text-success small fw-bold w-100 text-center", "MAPA DE CALOR (INTERVENCIONES)"),
                  div(cls:="position-relative mx-auto shadow", style:="width: 280px; height: 380px; background-color: #2e7d32; border: 2px solid white; border-radius: 4px;",
                    div(style:="position:absolute; top:0; left:50%; transform:translateX(-50%); width:60%; height:15%; border:2px solid rgba(255,255,255,0.6); border-top:none;"),
                    div(style:="position:absolute; bottom:0; left:50%; transform:translateX(-50%); width:60%; height:15%; border:2px solid rgba(255,255,255,0.6); border-bottom:none;"),
                    div(style:="position:absolute; top:50%; width:100%; height:2px; background:rgba(255,255,255,0.4);"),
                    div(style:="position:absolute; top:50%; left:50%; transform:translate(-50%,-50%); width:60px; height:60px; border:2px solid rgba(255,255,255,0.4); border-radius:50%;"),
                    div(id:="fieldMap", style:="width:100%; height:100%; cursor:crosshair; z-index:10;", onclick:="regFieldPos(event)")
                  ),
                  div(cls:="text-center mt-1 small text-muted", "Toca donde intervino (Parada/Corte/Pase)"),
                  input(tpe:="hidden", name:="mapaCampo", id:="hiddenFieldMap", value:="")
                ),

                // 6. ENTORNO Y NOTAS
                div(cls:="mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  label(cls:="form-label text-white small fw-bold w-100 text-center", "ENTORNO"),
                  div(cls:="row mb-2",
                    div(cls:="col-6",
                      div(cls:="d-flex justify-content-between align-items-center",
                        label(cls:="small text-muted fw-bold", "Clima"),
                        span(id:="climaAutoBadge", cls:="badge bg-info text-dark xx-small", style:="display:none;", "🌐 Auto")
                      ),
                      select(name:="clima", id:="climaSelect", cls:="form-select form-select-sm bg-dark text-white fw-bold", onchange:="document.getElementById('climaAutoBadge').style.display='none';", option(value:="Sol", "Sol"), option(value:="Nubes", "Nubes"), option(value:="Lluvia", "Lluvia"), option(value:="Nublado", "Nublado"), option(value:="Frio", "Frio"), option(value:="Calor", "Calor"), option(value:="Viento", "Viento"))),
                    div(cls:="col-6", label(cls:="small text-muted fw-bold", "Temp (C)"), input(tpe:="number", name:="temp", cls:="form-control form-control-sm bg-dark text-white fw-bold", value:="20"))
                  )
                ),
                div(cls:="mb-3 p-2 border border-danger rounded bg-danger bg-opacity-10", label(cls:="form-label text-danger small fw-bold w-100 text-center", "SALA DE VIDEO"), input(tpe:="url", name:="video", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="Link Video (Youtube/Drive)")),
                div(cls:="mb-3", label(cls:="form-label text-white small fw-bold", "ANOTACIONES DEL ENTRENADOR"), textarea(name:="notas", cls:="form-control form-control-sm bg-dark text-white fw-bold", rows:="3", placeholder:="Notas generales: Saques, posicionamiento, lectura del juego, voz de mando...")),
                input(tpe:="hidden", name:="reaccion", value:=""),  // campo legacy mantenido para DB
                div(cls := "mb-3", label(cls := "form-label small fw-bold", "MINUTOS"), input(tpe := "number", name := "minutos", cls := "form-control fw-bold", value := "40", attr("inputmode") := "numeric")),

                // ── BLOQUE 4.1: RUBRICA DE VALORACION (opcional pero recomendado) ─
                div(cls := "mb-3 p-2 border border-warning rounded bg-warning bg-opacity-10",
                  div(cls := "d-flex justify-content-between align-items-center", style := "cursor:pointer;", onclick := "toggleRubrica()",
                    label(cls := "text-warning fw-bold small mb-0", style := "cursor:pointer;", "📋 RÚBRICA DE VALORACIÓN (opcional pero recomendado)"),
                    span(id := "rubricaChevron", cls := "text-warning small", "▼")
                  ),
                  div(id := "rubricaPanel", style := "display:none;",
                    div(cls := "xx-small text-muted mt-2 mb-2", "Puntúa 1-5 cada dimensión. Se sugerirá una nota automática que podrás editar."),
                    Seq(
                      ("rubricaPosicion", "Posición y movimientos", "1=siempre fuera de lugar / 5=anticipa siempre el juego"),
                      ("rubricaDecisiones", "Decisiones bajo presión", "1=duda siempre, sale tarde / 5=decisiones rápidas en 1v1"),
                      ("rubricaPies", "Juego con los pies", "1=evita el balón / 5=distribuye con intención bajo presión"),
                      ("rubricaComunicacion", "Comunicación", "1=no habla / 5=dirige activamente la defensa"),
                      ("rubricaActitud", "Actitud y concentración", "1=se desconecta tras errores / 5=líder todo el partido")
                    ).map { case (fieldName, label_, hint) =>
                      div(cls := "mb-2",
                        label(cls := "xx-small text-white fw-bold d-block", label_),
                        div(cls := "xx-small text-muted mb-1", hint),
                        select(name := fieldName, id := fieldName, cls := "form-select form-select-sm bg-dark text-white border-warning", onchange := "calcNotaSugerida()",
                          option(value := "", "— Sin puntuar —"),
                          option(value := "1", "1"), option(value := "2", "2"), option(value := "3", "3"),
                          option(value := "4", "4"), option(value := "5", "5")
                        )
                      )
                    },
                    div(id := "notaSugeridaBox", cls := "xx-small text-warning fw-bold text-center mt-2", "")
                  )
                ),

                div(cls := "mb-4", label(cls := "form-label text-warning fw-bold small", "NOTA (0-10)"), input(tpe := "number", step := "0.1", name := "nota", id := "notaInput", cls := "form-control form-control-lg text-center fw-bold", placeholder := "Ej: 7.5", required := true, attr("inputmode") := "decimal")),

                // ── REGISTRO DE GOLES ENCAJADOS ─────────────────────────
                div(cls:="mb-4 p-3 border border-danger rounded",
                  style:="background:rgba(220,53,69,0.05);",
                  div(cls:="d-flex justify-content-between align-items-center mb-2",
                    label(cls:="text-danger fw-bold small", "ANALISIS DE GOLES ENCAJADOS"),
                    tag("button")(tpe:="button", cls:="btn btn-outline-danger btn-sm fw-bold",
                      onclick:="addGoalRow()", "Añadir gol")
                  ),
                  div(cls:="xx-small text-muted mb-2",
                    "Registra el contexto de cada gol para analisis avanzado (PSxG, Clutch, Nota ajustada)"),
                  div(id:="goalsContainer"),
                  input(tpe:="hidden", name:="goalsData", id:="goalsDataInput", value:="")
                ),

                // ── FOOTBAR (SENSOR GPS DE RENDIMIENTO) — OPCIONAL ───────
                div(cls:="mb-4 p-3 border border-info rounded", style:="background:rgba(13,202,240,0.05);",
                  div(cls:="d-flex justify-content-between align-items-center", style:="cursor:pointer;", onclick:="toggleFootbar()",
                    label(cls:="text-info fw-bold small mb-0", style:="cursor:pointer;", "🦵 DATOS FOOTBAR (opcional)"),
                    span(id:="footbarChevron", cls:="text-info small", "▼")
                  ),
                  div(id:="footbarPanel", style:="display:none;",
                    div(cls:="xx-small text-muted mt-2 mb-2", "Introduce los datos del sensor Footbar tras el partido. Se guardan solo si rellenas la distancia."),
                    div(cls:="xx-small text-info fw-bold mb-1", "INFORME FISICO"),
                    div(cls:="row g-2 mb-2",
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Distancia (km)"), input(tpe:="number", step:="0.01", min:="0", name:="fbDistancia", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Alta intensidad (m)"), input(tpe:="number", step:="1", min:="0", name:="fbAltaIntensidad", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Sprint max (km/h)"), input(tpe:="number", step:="0.1", min:="0", name:="fbSprintMax", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "% Actividad"), input(tpe:="number", step:="0.1", min:="0", max:="100", name:="fbPctActividad", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Tiempo actividad (min)"), input(tpe:="number", step:="1", min:="0", name:="fbTiempoActividad", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Aceleraciones"), input(tpe:="number", step:="1", min:="0", name:="fbAceleraciones", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Desaceleraciones"), input(tpe:="number", step:="1", min:="0", name:="fbDesaceleraciones", cls:="form-control form-control-sm bg-dark text-white border-info"))
                    ),
                    div(cls:="xx-small text-info fw-bold mb-1 mt-2", "INFORME TECNICO"),
                    div(cls:="row g-2",
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Balones"), input(tpe:="number", step:="1", min:="0", name:="fbBalones", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Pases"), input(tpe:="number", step:="1", min:="0", name:="fbPases", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Tiempo con balon (s)"), input(tpe:="number", step:="1", min:="0", name:="fbTiempoBalon", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Disparos"), input(tpe:="number", step:="1", min:="0", name:="fbDisparos", cls:="form-control form-control-sm bg-dark text-white border-info")),
                      div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Tiro max (km/h)"), input(tpe:="number", step:="0.1", min:="0", name:="fbTiroMax", cls:="form-control form-control-sm bg-dark text-white border-info"))
                    )
                  )
                ),

                // ── BLOQUE C: SET-PIECE CONTROL (opcional) ────────────────
                div(cls:="mb-4 p-3 border border-warning rounded bg-warning bg-opacity-10",
                  label(cls:="form-label text-warning small fw-bold w-100 text-center mb-2", "🏴 BALÓN PARADO (opcional)"),
                  div(cls:="row g-2",
                    div(cls:="col-4 text-center",
                      div(cls:="xx-small text-muted fw-bold mb-1", "Corners dominados"),
                      div(cls:="d-flex align-items-center justify-content-center gap-1",
                        button(tpe:="button", cls:="btn btn-outline-warning btn-sm px-2", onclick:="adjustSetPiece('cornersDominados',-1)", "-"),
                        input(tpe:="number", name:="cornersDominados", id:="cornersDominados", value:="0",
                          cls:="form-control form-control-sm text-center bg-dark text-warning fw-bold border-warning",
                          style:="width:50px;", attr("inputmode"):="numeric", attr("min"):="0"),
                        button(tpe:="button", cls:="btn btn-outline-warning btn-sm px-2", onclick:="adjustSetPiece('cornersDominados',1)", "+")
                      ),
                      div(cls:="xx-small text-muted mt-1", "Salidas aéreas exitosas — llegó con decisión")
                    ),
                    div(cls:="col-4 text-center",
                      div(cls:="xx-small text-muted fw-bold mb-1", "Corners cedidos"),
                      div(cls:="d-flex align-items-center justify-content-center gap-1",
                        button(tpe:="button", cls:="btn btn-outline-warning btn-sm px-2", onclick:="adjustSetPiece('cornersCedidos',-1)", "-"),
                        input(tpe:="number", name:="cornersCedidos", id:="cornersCedidos", value:="0",
                          cls:="form-control form-control-sm text-center bg-dark text-warning fw-bold border-warning",
                          style:="width:50px;", attr("inputmode"):="numeric", attr("min"):="0"),
                        button(tpe:="button", cls:="btn btn-outline-warning btn-sm px-2", onclick:="adjustSetPiece('cornersCedidos',1)", "+")
                      ),
                      div(cls:="xx-small text-muted mt-1", "No salió o llegó tarde")
                    ),
                    div(cls:="col-4 text-center",
                      div(cls:="xx-small text-muted fw-bold mb-1", "Faltas área dominadas"),
                      div(cls:="d-flex align-items-center justify-content-center gap-1",
                        button(tpe:="button", cls:="btn btn-outline-warning btn-sm px-2", onclick:="adjustSetPiece('faltasAreaDominadas',-1)", "-"),
                        input(tpe:="number", name:="faltasAreaDominadas", id:="faltasAreaDominadas", value:="0",
                          cls:="form-control form-control-sm text-center bg-dark text-warning fw-bold border-warning",
                          style:="width:50px;", attr("inputmode"):="numeric", attr("min"):="0"),
                        button(tpe:="button", cls:="btn btn-outline-warning btn-sm px-2", onclick:="adjustSetPiece('faltasAreaDominadas',1)", "+")
                      ),
                      div(cls:="xx-small text-muted mt-1", "Despejó con autoridad")
                    )
                  )
                ),

                // ── COMPORTAMIENTO BAJO PRESION ──────────────────────────
                div(cls := "mb-4 p-2 border border-info rounded bg-info bg-opacity-10",
                  label(cls := "form-label text-info small fw-bold w-100 text-center", "🧠 COMPORTAMIENTO TRAS GOLES ENCAJADOS"),
                  select(name := "comportamientoPresion", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                    option(value := "NA", "— Sin goles encajados / No aplica"),
                    option(value := "RAPIDO", "✅ Se repuso rápido — recuperó concentración en menos de 2 minutos"),
                    option(value := "LIDER", "💪 Lideró al equipo — animó a los compañeros o dirigió la defensa"),
                    option(value := "NEUTRO", "😐 Neutro — ni se afectó ni lideró"),
                    option(value := "AFECTADO", "😟 Se afectó visiblemente — bajó el nivel en los siguientes minutos"),
                    option(value := "INTENSO", "🔥 Reaccionó con más intensidad — el gol le activó")
                  )
                ),

                // ── NUTRICION PRE-PARTIDO ─────────────────────────────────
                div(cls := "mb-4 p-2 border border-success rounded bg-success bg-opacity-10",
                  label(cls := "form-label text-success small fw-bold w-100 text-center", "🍽️ COMIDA LAS 3H ANTES DEL PARTIDO (opcional)"),
                  select(name := "nutricionPrepartido", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                    option(value := "", "— Sin especificar —"),
                    option(value := "completa", "Comida completa (pasta, arroz, proteína)"),
                    option(value := "ligera", "Comida ligera (bocadillo, fruta)"),
                    option(value := "snack", "Solo snack (galletas, barrita)"),
                    option(value := "sin_comer", "Sin comer o muy poco"),
                    option(value := "no_adecuada", "Comida no adecuada (rápida, pesada)")
                  )
                ),

                // ── BLOQUE 4.3: METRICAS DE CANTERA (opcional) ────────────
                div(cls := "mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  label(cls := "form-label text-white small fw-bold w-100 text-center", "📐 MÉTRICAS DE CANTERA (opcional)"),
                  div(cls := "mb-2",
                    label(cls := "xx-small text-muted fw-bold", "Posición en goles encajados"),
                    select(name := "posicionSet", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                      option(value := "", "— Sin especificar —"),
                      option(value := "BIEN_PLANTADO", "Bien plantado"),
                      option(value := "DESPLAZAMIENTO_TARDIO", "Desplazamiento tardío"),
                      option(value := "PASO_NEGATIVO", "Paso negativo"),
                      option(value := "IMPARABLE", "Imparable")
                    )
                  ),
                  div(cls := "mb-2",
                    label(cls := "xx-small text-muted fw-bold", "Altura defensiva del bloque"),
                    select(name := "alturaBloque", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                      option(value := "", "— Sin especificar —"),
                      option(value := "BAJO_PALOS", "Bajo palos"),
                      option(value := "ADELANTADO_LIBERO", "Adelantado / líbero")
                    )
                  ),
                  div(cls := "mb-2",
                    label(cls := "xx-small text-muted fw-bold", "Acciones con pie no dominante"),
                    input(tpe := "number", step := "1", min := "0", name := "pieNoDominanteAcciones", cls := "form-control form-control-sm bg-dark text-white fw-bold", value := "0")
                  ),
                  div(cls := "mb-1",
                    label(cls := "xx-small text-muted fw-bold", "Iniciativa vocal"),
                    select(name := "iniciativaVocal", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                      option(value := "", "— Sin especificar —"),
                      option(value := "SI", "✅ Sí"),
                      option(value := "PARCIAL", "🟡 Parcial"),
                      option(value := "TIMIDO", "🔇 Tímido")
                    )
                  )
                ),

                // ── BLOQUE B2: AUTOPERCEPCION PRE-PARTIDO DE HECTOR ───────
                div(cls := "mb-4 p-2 border border-primary rounded bg-primary bg-opacity-10",
                  label(cls := "form-label text-white small fw-bold w-100 text-center", "🎯 ¿Cómo se encontraba Héctor antes del partido?"),
                  div(cls := "btn-group w-100", attr("role") := "group",
                    Seq((1, "😞 1"), (2, "😕 2"), (3, "😐 3"), (4, "🙂 4"), (5, "😃 5")).map { case (v, txt) =>
                      frag(
                        input(tpe := "radio", cls := "btn-check", name := "autopercepcionPrepartido", id := s"autop$v", value := v.toString,
                          if (autopercepcionPrefill.contains(v)) attr("checked") := "checked" else frag()),
                        label(cls := "btn btn-outline-primary btn-sm", `for` := s"autop$v", txt)
                      )
                    }
                  ),
                  div(cls := "xx-small text-muted mt-1 text-center", "1=Muy mal · 2=Regular · 3=Normal · 4=Bien · 5=Muy bien")
                ),

                // ── BLOQUE C: CONTEXTO AVANZADO (opcional) ─────────────────
                div(cls := "mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  div(cls := "d-flex justify-content-between align-items-center", style := "cursor:pointer;", onclick := "toggleContextoAvanzado()",
                    label(cls := "text-white fw-bold small mb-0", style := "cursor:pointer;", "⚙️ CONTEXTO AVANZADO (opcional)"),
                    span(id := "contextoAvanzadoChevron", cls := "text-white small", "▼")
                  ),
                  div(id := "contextoAvanzadoPanel", style := "display:none;",
                    div(cls := "row g-2 mt-1",
                      div(cls := "col-6",
                        label(cls := "xx-small text-muted fw-bold", "Minutos de calentamiento"),
                        input(tpe := "number", step := "1", min := "0", name := "calentamientoMin", cls := "form-control form-control-sm bg-dark text-white border-secondary")
                      ),
                      div(cls := "col-6",
                        label(cls := "xx-small text-muted fw-bold", "Tipo de calentamiento"),
                        select(name := "calentamientoTipo", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                          option(value := "", "— Sin especificar —"),
                          option(value := "NINGUNO", "Ninguno"),
                          option(value := "CARRERA_SUAVE", "Carrera suave"),
                          option(value := "ESPECIFICO_PORTERO", "Específico de portero"),
                          option(value := "COMPLETO_EQUIPO", "Completo con el equipo")
                        )
                      )
                    ),
                    div(cls := "mt-2",
                      label(cls := "xx-small text-muted fw-bold", "Tipo de césped"),
                      select(name := "superficie", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                        option(value := "", "— Sin especificar —"),
                        option(value := "NATURAL", "Natural"),
                        option(value := "ARTIFICIAL", "Artificial"),
                        option(value := "TIERRA", "Tierra"),
                        option(value := "INTERIOR", "Interior")
                      )
                    ),
                    div(cls := "mt-2",
                      label(cls := "xx-small text-muted fw-bold", "¿Hay algún factor externo relevante hoy? (enfermedad incipiente, mal descanso por viaje, examen, cumpleaños...)"),
                      textarea(name := "factoresExternos", cls := "form-control form-control-sm bg-dark text-white border-secondary", rows := "2")
                    ),
                    div(cls := "mt-2",
                      label(cls := "xx-small text-muted fw-bold", "Velocidad de transición parada → distribución"),
                      div(cls := "xx-small text-muted mb-1", "El tiempo desde que para el balón hasta que lo pone en juego. Los porteros de élite distribuyen en <4s para presionar al rival antes de que se reorganice."),
                      select(name := "velocidadDistribucion", cls := "form-select form-select-sm bg-dark text-white border-secondary",
                        option(value := "", "— Sin especificar —"),
                        option(value := "INMEDIATO", "Inmediato (<3 segundos)"),
                        option(value := "NORMAL", "Normal (3-6 segundos)"),
                        option(value := "LENTO", "Lento (>6 segundos)")
                      )
                    ),
                    div(cls := "mt-2",
                      label(cls := "xx-small text-muted fw-bold", "Economía de movimiento (Dive Economy)"),
                      div(cls := "xx-small text-muted mb-1", "¿Cuántas paradas fueron innecesariamente acrobáticas cuando con mejor posición inicial hubieran sido cómodas? 1=Muchas dives innecesarias · 5=Siempre llegó con comodidad"),
                      input(tpe := "range", cls := "form-range", min := "1", max := "5", name := "economiaMovimiento")
                    ),
                    div(cls := "mt-2",
                      label(cls := "xx-small text-muted fw-bold", "Calidad de decisión (independiente del resultado, %)"),
                      div(cls := "xx-small text-muted mb-1", "¿Qué % de las decisiones tomó correctamente independientemente de si el resultado fue favorable? Un portero puede decidir bien y que el balón entre de rebote, o quedarse estático y que el delantero falle."),
                      input(tpe := "number", step := "1", min := "0", max := "100", name := "calidadDecisionPct", cls := "form-control form-control-sm bg-dark text-white border-secondary")
                    )
                  )
                ),

                // ── BLOQUE S: TOOLKIT DE REGULACIÓN EMOCIONAL (solo si hay goles encajados) ──
                div(id := "regulacionEmocionalPanel", style := "display:none;", cls := "mb-4 p-2 border border-info rounded bg-info bg-opacity-10",
                  label(cls := "form-label text-info small fw-bold w-100 text-center", "🧠 ¿QUÉ HIZO HÉCTOR EN LOS 30 SEGUNDOS DESPUÉS DEL GOL MÁS IMPORTANTE?"),
                  div(cls := "xx-small text-muted text-center mb-2", "Clave para su perfil de regulación emocional."),
                  div(cls := "row g-1",
                    Seq(
                      ("HABLA_SOLO", "🗣️ Habla solo (refuerzo interno)"), ("RESPIRA", "😤 Miró al cielo y respiró"),
                      ("ENFADO", "😠 Golpeó postes / gestos de enfado"), ("NEUTRAL", "😐 Sin reacción visible"),
                      ("REORGANIZA", "🧭 Buscó a los defensas para reorganizarse"), ("DECAIDO", "😔 Bajó la cabeza y tardó en recuperarse")
                    ).map { case (v, txt) =>
                      div(cls := "col-6",
                        input(tpe := "radio", cls := "btn-check", name := "regulacionEmocional", id := s"regem_$v", value := v),
                        label(cls := "btn btn-outline-info btn-sm w-100 mb-1", `for` := s"regem_$v", txt)
                      )
                    }
                  )
                ),

                // ── BLOQUE O: RUTINA PRE-PARTIDO (solo si hay una rutina definida) ──
                if (rutinaActiva.nonEmpty) div(cls := "mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  label(cls := "form-label text-white small fw-bold w-100 text-center", "🔄 ¿Siguió Héctor su rutina pre-partido?"),
                  div(cls := "btn-group w-100", attr("role") := "group",
                    Seq(("SI", "✅ Sí"), ("NO", "❌ No"), ("SIN_RUTINA", "🤷 Sin rutina hoy")).map { case (v, txt) =>
                      frag(
                        input(tpe := "radio", cls := "btn-check", name := "rutinaPrepartido", id := s"rutina_$v", value := v),
                        label(cls := "btn btn-outline-secondary btn-sm", `for` := s"rutina_$v", txt)
                      )
                    }
                  )
                ) else frag(),

                // ── BLOQUE B: AUTOEVAL COMPORTAMIENTO DEL PADRE EN LA BANDA (privado) ──
                div(cls := "mb-4 p-2 border border-secondary rounded bg-secondary bg-opacity-10",
                  label(cls := "form-label text-white small fw-bold w-100 text-center", "👨 ¿CÓMO TE COMPORTASTE EN LA BANDA HOY?"),
                  div(cls := "btn-group w-100 flex-wrap", attr("role") := "group",
                    Seq(
                      (1, "😤 1"), (2, "😕 2"), (3, "😐 3"), (4, "🙂 4"), (5, "🧘 5")
                    ).map { case (v, txt) =>
                      frag(
                        input(tpe := "radio", cls := "btn-check", name := "conductaPadre", id := s"conducta$v", value := v.toString),
                        label(cls := "btn btn-outline-secondary btn-sm", `for` := s"conducta$v", txt)
                      )
                    }
                  ),
                  div(cls := "xx-small text-muted mt-1 text-center",
                    "1=Grité instrucciones o correcciones · 2=Algún gesto negativo bajo presión · 3=Neutral · 4=Solo refuerzo positivo · 5=Observador puro"),
                  div(cls := "xx-small text-muted fst-italic text-center mt-1",
                    "Autoevaluación — solo visible para ti, nunca en el perfil público.")
                ),

                div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg py-3 fw-bold", "GUARDAR PARTIDO"))
              ) // fin form
            ),

            // SCRIPTS
            script(raw("""
              function toggleFootbar(){var p=document.getElementById('footbarPanel');var c=document.getElementById('footbarChevron');var open=p.style.display!=='none';p.style.display=open?'none':'block';c.textContent=open?'▼':'▲';}
              function toggleRubrica(){var p=document.getElementById('rubricaPanel');var c=document.getElementById('rubricaChevron');var open=p.style.display!=='none';p.style.display=open?'none':'block';c.textContent=open?'▼':'▲';}
              function toggleContextoAvanzado(){var p=document.getElementById('contextoAvanzadoPanel');var c=document.getElementById('contextoAvanzadoChevron');var open=p.style.display!=='none';p.style.display=open?'none':'block';c.textContent=open?'▼':'▲';}
              function calcNotaSugerida(){
                var ids=['rubricaPosicion','rubricaDecisiones','rubricaPies','rubricaComunicacion','rubricaActitud'];
                var vals=ids.map(function(id){var v=document.getElementById(id).value;return v?parseInt(v):null;});
                var box=document.getElementById('notaSugeridaBox');
                if(vals.some(function(v){return v===null;})){box.textContent='';return;}
                var nota=(vals[0]*0.25+vals[1]*0.25+vals[2]*0.20+vals[3]*0.15+vals[4]*0.15)*2;
                nota=Math.round(nota*10)/10;
                box.textContent='Nota sugerida por rúbrica: '+nota+' (editable abajo)';
                document.getElementById('notaInput').value=nota;
              }
              var currentMode='save';var goals=[];var saves=[];var origins=[];
              function setMode(mode){currentMode=mode;}
              function registerAction(zone){const cell=document.querySelector('.zone-'+zone);const marker=cell.querySelector('.action-marker');if(currentMode==='save'){saves.push(zone);marker.innerHTML+='<span style="color:#198754; font-weight:bold;">*</span>';document.getElementById('parInput').value=parseInt(document.getElementById('parInput').value||0)+1;document.getElementById('hiddenParadas').value=saves.join(',');}else{goals.push(zone);marker.innerHTML+='<span style="color:#dc3545; font-weight:bold;">*</span>';document.getElementById('gcInput').value=parseInt(document.getElementById('gcInput').value||0)+1;document.getElementById('hiddenGoles').value=goals.join(',');toggleRegulacionEmocional();}}
              function toggleRegulacionEmocional(){var gc=parseInt(document.getElementById('gcInput').value||0);var p=document.getElementById('regulacionEmocionalPanel');if(p) p.style.display = gc>0 ? 'block' : 'none';}
              function incCounter(key){var el=document.getElementById('cnt_'+key); var val=parseInt(el.value||0)+1; el.value=val; document.getElementById('disp_'+key).value=val; updateActionData();}
              function adjustBypass(delta){var el=document.getElementById('lineasSuperadas'); var v=Math.max(0,parseInt(el.value||0)+delta); el.value=v;}
              function adjustSetPiece(fieldId, delta){var el=document.getElementById(fieldId); var v=Math.max(0,parseInt(el.value||0)+delta); el.value=v;}
              function adjustScanning(delta){var el=document.getElementById('scanningRate'); var v=Math.max(0,parseInt(el.value||0)+delta); el.value=v;}
              function adjustScanningEfectivo(delta){var el=document.getElementById('scanningEfectivo'); var v=Math.max(0,parseInt(el.value||0)+delta); el.value=v;}
              function toggleAngulo1v1(){var p=document.getElementById('angulo1v1Panel');var c=document.getElementById('angulo1v1Chevron');var open=p.style.display!=='none';p.style.display=open?'none':'block';c.textContent=open?'▼':'▲';}
              function adjustAngulo1v1(key,delta){
                var el=document.getElementById('angulo_'+key); var v=Math.max(0,parseInt(el.value||0)+delta); el.value=v;
                var keys=['central_ok','central_gc','izq_ok','izq_gc','der_ok','der_gc'];
                var obj={};
                keys.forEach(function(k){ obj[k]=parseInt(document.getElementById('angulo_'+k).value||0); });
                document.getElementById('angulo1v1DataInput').value=JSON.stringify(obj);
              }
              function updateActionData(){var d = [document.getElementById('cnt_p1v1').value, document.getElementById('cnt_pAir').value, document.getElementById('cnt_pPie').value]; document.getElementById('actionData').value = d.join(',');}
              function toggleOrigin(el,origin){el.classList.toggle('active');el.classList.toggle('btn-warning');if(origins.includes(origin)){origins=origins.filter(o=>o!==origin);}else{origins.push(origin);}document.getElementById('hiddenOrigin').value=origins.join(',');}
              function pass(type, success) { var totEl = document.getElementById(type+'Tot'); var okEl = document.getElementById(type+'Ok'); var dispEl = document.getElementById('display_'+type); var t = parseInt(totEl.value)+1; var o = parseInt(okEl.value) + (success ? 1 : 0); totEl.value=t; okEl.value=o; dispEl.value = o + '/' + t; updatePassData(); }
              function updatePassData(){var d = [document.getElementById('pcTot').value, document.getElementById('pcOk').value, document.getElementById('plTot').value, document.getElementById('plOk').value]; document.getElementById('passData').value = d.join(',');}

              // SCRIPT NUEVO MAPA CAMPO
              var fieldPoints = [];
              function regFieldPos(e) {
                var rect = e.target.getBoundingClientRect();
                var x = e.clientX - rect.left;
                var y = e.clientY - rect.top;
                var pctX = Math.round((x / rect.width) * 100);
                var pctY = Math.round((y / rect.height) * 100);
                fieldPoints.push(pctX + ":" + pctY);
                document.getElementById('hiddenFieldMap').value = fieldPoints.join(',');
                var dot = document.createElement('div');
                dot.style.cssText = 'position:absolute; width:10px; height:10px; background:orange; border:1px solid white; border-radius:50%; transform:translate(-50%,-50%); pointer-events:none; left:'+x+'px; top:'+y+'px;';
                e.target.appendChild(dot);
              }

              // ── GOLES ENCAJADOS ─────────────────────────────────────
              var goalCount = 0;
              function addGoalRow() {
                goalCount++;
                var n = goalCount;
                var html = '<div id="goal_'+n+'" class="p-2 mb-2 rounded" style="background:rgba(220,53,69,0.1); border:1px solid rgba(220,53,69,0.3);">' +
                  '<div class="d-flex justify-content-between mb-1"><span class="text-danger fw-bold xx-small">GOL '+n+'</span><button type="button" class="btn btn-link btn-sm text-danger p-0" onclick="removeGoal('+n+')">x</button></div>' +
                  '<div class="row g-1">' +
                  '<div class="col-3"><label class="xx-small text-muted">Minuto</label><input type="number" class="form-control form-control-sm bg-dark text-white border-secondary" id="gMin_'+n+'" value="0" min="0" max="90" onchange="updateGoalsData()"></div>' +
                  '<div class="col-9"><label class="xx-small text-muted">Origen</label><select class="form-select form-select-sm bg-dark text-white border-secondary" id="gOrigen_'+n+'" onchange="updateGoalsData()">' +
                  '<option>Jugada elaborada</option><option>Contragolpe</option><option>Error defensivo</option>' +
                  '<option>Corner</option><option>Falta directa</option><option>Penalti</option><option>Otro</option></select></div>' +
                  '<div class="col-6"><label class="xx-small text-muted">Situacion</label><select class="form-select form-select-sm bg-dark text-white border-secondary" id="gSit_'+n+'" onchange="updateGoalsData()">' +
                  '<option>1 vs 1</option><option>2 vs 1</option><option>Tiro lejano</option>' +
                  '<option>Remate cabeza</option><option>Penalti</option><option>Gol en propia</option><option>Otro</option></select></div>' +
                  '<div class="col-6"><label class="xx-small text-muted">Responsabilidad portero</label><select class="form-select form-select-sm bg-dark text-white border-secondary" id="gResp_'+n+'" onchange="updateGoalsData()">' +
                  '<option>Ninguna</option><option>Media</option><option>Alta</option></select></div>' +
                  '<div class="col-6"><label class="xx-small text-muted">Era parable</label><select class="form-select form-select-sm bg-dark text-white border-secondary" id="gPar_'+n+'" onchange="updateGoalsData()">' +
                  '<option>No</option><option>Dudoso</option><option>Si</option></select></div>' +
                  '<div class="col-6"><label class="xx-small text-muted">Zona</label><select class="form-select form-select-sm bg-dark text-white border-secondary" id="gZona_'+n+'" onchange="updateGoalsData()">' +
                  '<option value="">-</option><option>TL</option><option>TC</option><option>TR</option><option>ML</option><option>MC</option><option>MR</option><option>BL</option><option>BC</option><option>BR</option></select></div>' +
                  '<div class="col-12"><input type="text" class="form-control form-control-sm bg-dark text-white border-secondary" id="gNota_'+n+'" placeholder="Nota breve..." onchange="updateGoalsData()"></div>' +
                  '</div></div>';
                document.getElementById('goalsContainer').insertAdjacentHTML('beforeend', html);
                updateGoalsData();
              }
              function removeGoal(n) {
                var el = document.getElementById('goal_'+n);
                if (el) el.remove();
                updateGoalsData();
              }
              function updateGoalsData() {
                var rows = [];
                for (var i = 1; i <= goalCount; i++) {
                  var el = document.getElementById('goal_'+i);
                  if (!el) continue;
                  var min   = document.getElementById('gMin_'+i).value;
                  var orig  = document.getElementById('gOrigen_'+i).value;
                  var sit   = document.getElementById('gSit_'+i).value;
                  var resp  = document.getElementById('gResp_'+i).value;
                  var par   = document.getElementById('gPar_'+i).value;
                  var zona  = document.getElementById('gZona_'+i).value;
                  var nota2 = document.getElementById('gNota_'+i).value;
                  rows.push([min,orig,sit,resp,par,zona,nota2].join('|'));
                }
                document.getElementById('goalsDataInput').value = rows.join(';');
              }

              // ── BLOQUE A: REGISTRO RAPIDO POR VOZ O TEXTO (NLP) ─────────
              function toggleNlpPanel(){var p=document.getElementById('nlpPanel');var c=document.getElementById('nlpChevron');var open=p.style.display!=='none';p.style.display=open?'none':'block';c.textContent=open?'▼':'▲';}
              var nlpRecognition = null; var nlpDictando = false;
              function nlpToggleDictado(){
                var SR = window.SpeechRecognition || window.webkitSpeechRecognition;
                if (!SR) { alert('Reconocimiento de voz no disponible en este navegador.'); return; }
                var btn = document.getElementById('nlpDictarBtn');
                if (nlpDictando) { if (nlpRecognition) nlpRecognition.stop(); return; }
                nlpRecognition = new SR();
                nlpRecognition.lang = 'es-ES';
                nlpRecognition.continuous = true; nlpRecognition.interimResults = false;
                nlpRecognition.onstart = function(){ nlpDictando = true; btn.textContent = '⏹ Parar'; };
                nlpRecognition.onresult = function(e){
                  var texto = '';
                  for (var i = 0; i < e.results.length; i++) texto += e.results[i][0].transcript + ' ';
                  document.getElementById('nlpTexto').value = texto.trim();
                };
                nlpRecognition.onerror = function(){ nlpDictando = false; btn.textContent = '🎤 Dictar'; };
                nlpRecognition.onend = function(){ nlpDictando = false; btn.textContent = '🎤 Dictar'; };
                nlpRecognition.start();
              }
              function nlpMarkAuto(el){ if(el) el.style.border = '2px solid #d4af37'; }
              function nlpSetVal(name, val){
                if (val === null || val === undefined || val === '') return;
                var el = document.querySelector('[name="'+name+'"]:not([type="radio"])');
                if (el) { el.value = val; nlpMarkAuto(el); }
              }
              function nlpSetRadio(name, val){
                var el = document.querySelector('[name="'+name+'"][value="'+val+'"]');
                if (el) { el.checked = true; el.dispatchEvent(new Event('change')); }
              }
              function nlpExtraer(){
                var texto = document.getElementById('nlpTexto').value.trim();
                if (!texto) { alert('Describe el partido primero.'); return; }
                var status = document.getElementById('nlpStatus');
                status.textContent = '⏳ Extrayendo datos con Gemini...';
                document.getElementById('nlpExtraerBtn').disabled = true;
                var body = 'texto=' + encodeURIComponent(texto);
                fetch('/match-center/nlp', { method: 'POST', headers: {'Content-Type':'application/x-www-form-urlencoded'}, body: body })
                  .then(function(r){ return r.json().then(function(j){ return {ok: r.ok, body: j}; }); })
                  .then(function(res){
                    document.getElementById('nlpExtraerBtn').disabled = false;
                    if (!res.ok) { status.textContent = '⚠️ ' + (res.body.error || 'Error al extraer datos.'); return; }
                    var d = res.body;
                    status.textContent = '✅ Datos extraídos — revisa los campos marcados en amarillo.';
                    nlpSetVal('rival', d.rival);
                    nlpSetVal('gf', d.goles_favor);
                    nlpSetVal('gc', d.goles_contra);
                    nlpSetVal('nota', d.nota);
                    nlpSetVal('paradas', d.paradas);
                    if (d.clima) { nlpSetVal('clima', d.clima); document.getElementById('climaAutoBadge').style.display='none'; }
                    if (d.sede === 'Casa') nlpSetRadio('esLocal', 'true');
                    else if (d.sede === 'Fuera') nlpSetRadio('esLocal', 'false');
                    if (d.tipo_partido) nlpSetVal('tipo', d.tipo_partido);
                    if (d.iniciativa_vocal) nlpSetVal('iniciativaVocal', d.iniciativa_vocal);
                    nlpSetVal('rubricaPosicion', d.rubrica_posicion);
                    nlpSetVal('rubricaDecisiones', d.rubrica_decisiones);
                    nlpSetVal('rubricaPies', d.rubrica_pies);
                    nlpSetVal('rubricaComunicacion', d.rubrica_comunicacion);
                    nlpSetVal('rubricaActitud', d.rubrica_actitud);
                    nlpSetVal('notas', d.notas_partido);
                    var warn = document.getElementById('nlpConfianzaWarning');
                    warn.style.display = (typeof d.confianza === 'number' && d.confianza < 0.6) ? 'block' : 'none';
                  })
                  .catch(function(err){
                    document.getElementById('nlpExtraerBtn').disabled = false;
                    status.textContent = '⚠️ Error de red: ' + err.message;
                  });
              }

              // ── BLOQUE D: CLIMA AUTOMATICO (Open-Meteo) ─────────────────
              function cargarClimaAuto(fecha){
                if (!fecha) return;
                fetch('/match-center/clima?fecha=' + encodeURIComponent(fecha)).then(function(r){ return r.json(); }).then(function(j){
                  if (j.clima) {
                    document.getElementById('climaSelect').value = j.clima;
                    document.getElementById('climaAutoBadge').style.display = 'inline-block';
                  }
                }).catch(function(){});
              }
              var fechaInputEl = document.getElementById('fechaInput');
              if (fechaInputEl) {
                fechaInputEl.addEventListener('change', function(){ cargarClimaAuto(this.value); });
                if (fechaInputEl.value) cargarClimaAuto(fechaInputEl.value);
              }
            """))
          )
        )
      )
    );
    renderHtml(content)
  }

  @cask.post("/match-center/save")
  def saveMatch(request: cask.Request) = withAuth(request) {
    // CORRECCION: Usamos request.data para leer los bytes del cuerpo
    val bodyBytes = request.data.readAllBytes()
    val bodyString = new String(bodyBytes, "UTF-8")

    // Parseamos el string clave=valor&clave2=valor2
    val formData = bodyString.split("&").map { part =>
      val pair = part.split("=", 2)
      val key = java.net.URLDecoder.decode(pair(0), "UTF-8")
      val value = if (pair.length > 1) java.net.URLDecoder.decode(pair(1), "UTF-8") else ""
      key -> value
    }.toMap

    // Funciones auxiliares de extraccion
    def getStr(key: String): String = formData.getOrElse(key, "")
    def getInt(key: String): Int = try { getStr(key).toInt } catch { case _: Exception => 0 }
    def getDouble(key: String): Double = try { getStr(key).toDouble } catch { case _: Exception => 0.0 }

    // Los 23 parametros (Extraidos manualmente del mapa)
    val scheduleId = getInt("scheduleId")
    val rival = getStr("rival")
    val gf = getInt("gf")
    val gc = getInt("gc")
    val minutos = getInt("minutos")
    val nota = getDouble("nota")
    val paradas = getInt("paradas")
    val zonaGoles = getStr("zonaGoles")
    val zonaTiros = getStr("zonaTiros")
    val zonaParadas = getStr("zonaParadas")
    val clima = getStr("clima")
    val estadio = getStr("estadio")
    val temp = getInt("temp")
    val notas = getStr("notas")
    val video = getStr("video")
    val reaccion = getStr("reaccion")
    val fecha = getStr("fecha")
    val mode = getStr("mode")
    val passData = getStr("passData")
    val actionData = getStr("actionData")
    val tipo = getStr("tipo")
    val mapaCampo  = getStr("mapaCampo")
    val goalsData  = getStr("goalsData")
    val lineasSup    = getInt("lineasSuperadas")
    val scanningRate = getInt("scanningRate")
    // BLOQUE C: Set-Piece Control
    val cornersDominados     = getInt("cornersDominados")
    val cornersCedidos       = getInt("cornersCedidos")
    val faltasAreaDominadas  = getInt("faltasAreaDominadas")
    val esLocalStr   = getStr("esLocal")
    val esLocalOpt: Option[Boolean] = esLocalStr match {
      case "true"  => Some(true)
      case "false" => Some(false)
      case _       => None
    }
    val comportamientoPresion = getStr("comportamientoPresion")
    val nutricionPrepartido   = getStr("nutricionPrepartido")

    // Bloque 4.1: Rubrica de valoracion (opcional)
    def getOptInt(key: String): Option[Int] = getStr(key).toIntOption
    val rubricaPosicion     = getOptInt("rubricaPosicion")
    val rubricaDecisiones   = getOptInt("rubricaDecisiones")
    val rubricaPies         = getOptInt("rubricaPies")
    val rubricaComunicacion = getOptInt("rubricaComunicacion")
    val rubricaActitud      = getOptInt("rubricaActitud")

    // Bloque 4.3: Metricas de cantera (opcional)
    val posicionSet             = getStr("posicionSet")
    val alturaBloque            = getStr("alturaBloque")
    val pieNoDominanteAcciones  = getInt("pieNoDominanteAcciones")
    val iniciativaVocal         = getStr("iniciativaVocal")

    // BLOQUE B2: autopercepcion pre-partido (opcional)
    val autopercepcionPrepartido = getOptInt("autopercepcionPrepartido")

    // BLOQUE C: contexto avanzado del partido (opcional)
    val calentamientoMin      = getOptInt("calentamientoMin")
    val calentamientoTipo     = getStr("calentamientoTipo")
    val superficie             = getStr("superficie")
    val factoresExternos       = getStr("factoresExternos")
    val velocidadDistribucion  = getStr("velocidadDistribucion")
    val economiaMovimiento     = getOptInt("economiaMovimiento")
    val calidadDecisionPct     = getOptInt("calidadDecisionPct")

    // BLOQUE B: autoevaluacion privada de la conducta del padre en la banda (opcional)
    val conductaPadre = getOptInt("conductaPadre")
    // BLOQUE G: efectividad del scanning (amplia scanning_rate)
    val scanningEfectivo = getInt("scanningEfectivo")
    // BLOQUE H: desglose de 1v1 por angulo de entrada (JSON, opcional)
    val angulo1v1Data = getStr("angulo1v1Data")
    // BLOQUE O: rutina pre-partido (opcional)
    val rutinaPrepartido = getStr("rutinaPrepartido")
    // BLOQUE S: toolkit de regulacion emocional (opcional, solo si hubo goles encajados)
    val regulacionEmocional = getStr("regulacionEmocional")

    // Footbar (sensor GPS de rendimiento) — opcional
    val fbDistancia        = getDouble("fbDistancia")
    val fbAltaIntensidad   = getDouble("fbAltaIntensidad")
    val fbSprintMax        = getDouble("fbSprintMax")
    val fbPctActividad     = getDouble("fbPctActividad")
    val fbTiempoActividad  = getInt("fbTiempoActividad")
    val fbAceleraciones    = getInt("fbAceleraciones")
    val fbDesaceleraciones = getInt("fbDesaceleraciones")
    val fbBalones          = getInt("fbBalones")
    val fbPases            = getInt("fbPases")
    val fbTiempoBalon      = getInt("fbTiempoBalon")
    val fbDisparos         = getInt("fbDisparos")
    val fbTiroMax          = getDouble("fbTiroMax")

    // --- LOGICA DE PROCESAMIENTO (Base de datos y calculos) ---
    val pArr = passData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (pcTot, pcOk, plTot, plOk) = if(pArr.length >= 4) (pArr(0), pArr(1), pArr(2), pArr(3)) else (0,0,0,0)
    val aArr = actionData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (p1v1, pAir, pPie) = if(aArr.length >= 3) (aArr(0), aArr(1), aArr(2)) else (0,0,0)

    val cleanRival = fixEncoding(rival)
    val cleanNotas = fixEncoding(notas)
    val cleanReaccion = fixEncoding(reaccion)

    val c = DatabaseManager.getLatestCardData()
    // Footbar: la distancia recorrida ya esta en memoria (fbDistancia, parseada del form
    // mas arriba) — se usa directamente, sin esperar a que se guarde en footbar_sessions.
    val n = StatsCalculator.calculateGrowth(c, minutos, gc, nota, paradas, pcTot, pcOk, plTot, plOk, fbDistancia)
    DatabaseManager.updateStats(n)

    // BLOQUE E: logMatch devuelve el id via RETURNING id — sin condicion de carrera con SELECT MAX(id)
    val savedMatchId: Int = if (scheduleId > 0) {
      DatabaseManager.playScheduledMatch(scheduleId, gf, gc, minutos, nota, paradas, cleanNotas, video, cleanReaccion, clima, estadio, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, pcTot, pcOk, plTot, plOk, mapaCampo, fbDistancia, comportamientoPresion, nutricionPrepartido, cornersDominados, cornersCedidos, faltasAreaDominadas)
      scheduleId
    } else {
      DatabaseManager.logMatch(cleanRival, gf, gc, minutos, nota, n.media, paradas, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, clima, estadio, temp, cleanNotas, video, cleanReaccion, fecha, tipo, pcTot, pcOk, plTot, plOk, mapaCampo, lineasSup, scanningRate, esLocalOpt, comportamientoPresion, nutricionPrepartido, cornersDominados, cornersCedidos, faltasAreaDominadas)
    }

    // Guardar contexto de goles encajados
    if (goalsData.nonEmpty) {
      DatabaseManager.deleteMatchGoals(savedMatchId)  // limpiar si es re-save
      var minutosGoles = List[Int]()
      goalsData.split(";").foreach { row =>
        val parts = row.split("\\|", -1)
        if (parts.length >= 6) {
          val minuto   = try parts(0).toInt catch { case _: Exception => 0 }
          val origen   = if (parts.length > 1) parts(1) else ""
          val situacion= if (parts.length > 2) parts(2) else ""
          val resp     = if (parts.length > 3) parts(3) else "Media"
          val parable  = if (parts.length > 4) parts(4) else "Dudoso"
          val zona     = if (parts.length > 5) parts(5) else ""
          val notaG    = if (parts.length > 6) parts(6) else ""
          DatabaseManager.saveMatchGoal(savedMatchId, minuto, origen, situacion, resp, parable, zona, notaG)
          minutosGoles = minutosGoles :+ minuto
        }
      }
      // BLOQUE Q: rendimiento por fase del partido — el cuarto se deriva del minuto ya registrado por gol
      DatabaseManager.saveMinutoGoles(savedMatchId, minutosGoles)
    }

    // Guardar datos Footbar (solo si se ha introducido distancia)
    if (fbDistancia > 0) {
      DatabaseManager.saveFootbar(
        savedMatchId, fbDistancia, fbAltaIntensidad, fbSprintMax, fbPctActividad,
        fbTiempoActividad, fbAceleraciones, fbDesaceleraciones,
        fbBalones, fbPases, fbTiempoBalon, fbDisparos, fbTiroMax
      )
    }

    // Bloque 4.1/4.3: Rubrica y metricas de cantera + Bloque 4.4: guia de conversacion en background
    // BLOQUE B2/C: autopercepcion pre-partido y contexto avanzado (opcionales)
    DatabaseManager.updateMatchExtras(savedMatchId, rubricaPosicion, rubricaDecisiones, rubricaPies,
      rubricaComunicacion, rubricaActitud, posicionSet, alturaBloque, pieNoDominanteAcciones, iniciativaVocal,
      autopercepcionPrepartido, calentamientoMin, calentamientoTipo, superficie,
      factoresExternos, velocidadDistribucion, economiaMovimiento, calidadDecisionPct, conductaPadre, scanningEfectivo, angulo1v1Data, rutinaPrepartido, regulacionEmocional)
    DatabaseManager.generarGuiaConversacion(savedMatchId)

    // BLOQUE H: calcula el CPI del partido en background, sin bloquear la respuesta
    DatabaseManager.actualizarCPI(savedMatchId)

    // BLOQUE E: detecta hitos de carrera tras guardar el partido — en background, nunca bloquea la respuesta
    new Thread(new Runnable {
      def run(): Unit = DatabaseManager.detectarHitos()
    }).start()

    // BLOQUE G3: notificacion Telegram al guardar el partido — en background, nunca bloquea la respuesta
    new Thread(new Runnable {
      def run(): Unit = TelegramService.enviar(s"✅ Partido vs $cleanRival registrado — $gf-$gc · Nota: $nota")
    }).start()

    // Respuesta visual renderizada como Array[Byte] para cumplir con withAuth
    val d = n.media - c.media
    val msg = if(d > 0) s"SUBIDA DE NIVEL! +$d" else "Experiencia acumulada..."

    val mediaAntes = c.media.toInt
    val mediaDespues = n.media
    cask.Response(
      "".getBytes("UTF-8"),
      statusCode = 302,
      headers = Seq("Location" -> s"/match-center/saved?antes=$mediaAntes&despues=$mediaDespues&msg=${java.net.URLEncoder.encode(msg, "UTF-8")}")
    )
  }
  // BLOQUE D: registro minimo — status PLAYED, source 'quick', resto con los DEFAULT de la tabla
  @cask.post("/match-center/quick-save")
  def quickSaveMatch(request: cask.Request) = withAuth(request) {
    val p = parseFormBody(request)
    val rival = fixEncoding(p.getOrElse("rival", "")).trim
    val gf = p.getOrElse("goles_favor", "").toIntOption
    val gc = p.getOrElse("goles_contra", "").toIntOption
    val nota = p.getOrElse("nota", "").replace(",", ".").toDoubleOption.filter(n => n >= 0 && n <= 10)
    val msg = (gf, gc, nota) match {
      case (Some(f), Some(c), Some(n)) if rival.nonEmpty && f >= 0 && c >= 0 =>
        DatabaseManager.quickSaveMatch(rival, f, c, n)
        new Thread(() => DatabaseManager.detectarHitos()).start()
        "⚡ Partido guardado en modo rápido — pulsa ✏️ Editar para completar los datos."
      case _ => "⚠️ Registro mínimo no guardado: faltan el rival, el resultado o una nota válida (0-10)."
    }
    cask.Response(Array.emptyByteArray, 302, headers = Seq(
      "Location" -> s"/history?msg=${java.net.URLEncoder.encode(msg, "UTF-8")}"))
  }

  @cask.get("/match-center/saved")
  def matchSavedPage(request: cask.Request, antes: Int = 0, despues: Int = 0, msg: String = "") = withAuth(request) {
    val diff = despues - antes
    val (arrowColor, arrowSymbol) = if (diff > 0) ("success", "+") else if (diff == 0) ("warning", "=") else ("danger", "")
    renderHtml(doctype("html")(
      html(
        head(
          meta(charset := "utf-8"),
          tags2.title("Guardado"),
          tags2.style(raw(getCss())),
          meta(attr("http-equiv") := "refresh", content := "4; url=/")
        ),
        body(style := "background:#1a1a1a; color:white; text-align:center; padding-top:60px; font-family:'Oswald';",
          div(style := "font-size:52px; margin-bottom:10px;", "✅"),
          h2(style := "color:#d4af37; letter-spacing:2px;", "ANALISIS GUARDADO"),
          div(style := "margin:30px auto; width:320px; background:#242424; padding:24px; border-radius:12px; border:1px solid #444;",
            div(style := "font-size:13px; color:#aaa; margin-bottom:8px; text-transform:uppercase; letter-spacing:1px;", "Media Global"),
            div(style := "font-size:52px; font-weight:700;",
              span(style := "color:#888;", s"$antes"),
              span(style := "color:#555; margin:0 10px;", "→"),
              span(style := s"color:${if(diff>0)"#28a745"else if(diff==0)"#ffc107"else"#dc3545"};", s"$despues")
            ),
            if (diff != 0)
              p(style := s"color:${if(diff>0)"#28a745"else"#dc3545"}; font-size:20px; font-weight:700; margin-top:8px;",
                s"$arrowSymbol$diff puntos")
            else p(),
            p(style := "color:#ffc107; font-size:14px; margin-top:10px;", java.net.URLDecoder.decode(msg, "UTF-8"))
          ),
          p(style := "color:#555; font-size:13px; margin-top:20px;", "Redirigiendo al inicio en 4 segundos..."),
          a(href := "/", style := "color:white; text-decoration:none; border:1px solid #555; padding:10px 24px; border-radius:6px; font-size:14px; letter-spacing:1px;", "INICIO")
        )
      )
    ).render)
  }

  @cask.get("/tournament/new")
  def newTournamentPage() = {
    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8",
          div(cls := "card bg-dark text-white border-warning shadow",
            div(cls := "card-header bg-warning text-dark fw-bold text-center", "🏆 NUEVO TORNEO"),
            div(cls := "card-body",
              form(action := "/tournament/create", method := "post",
                div(cls := "mb-3",
                  label(cls := "form-label fw-bold", "Nombre del Torneo"),
                  input(tpe := "text", name := "nombre", cls := "form-control fw-bold",
                    placeholder := "Ej: Mundialito Algarve", required := true)
                ),
                div(cls := "mb-3",
                  label(cls := "form-label fw-bold", "Cuadro / Estructura"),
                  div(cls := "alert alert-secondary small p-2 fw-bold",
                    "Formato por linea: FASE | RIVAL | FECHA (AAAA-MM-DD)"),
                  textarea(name := "estructura", cls := "form-control bg-secondary text-white fw-bold",
                    rows := "6",
                    placeholder := "Fase Grupos | Betis | 2026-04-12\nFase Grupos | Benfica | 2026-04-12\nCuartos | ? | 2026-04-13\nFinal | ? | 2026-04-14")
                ),
                div(cls := "d-grid",
                  button(tpe := "submit", cls := "btn btn-warning fw-bold", "GENERAR CUADRO")
                )
              )
            )
          ),
          div(cls := "text-center mt-3",
            a(href := "/match-center", cls := "text-muted", "Cancelar")
          )
        )
      )
    )
    cask.Response(content.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/tournament/create")
  def createTournamentAction(nombre: String, estructura: String) = {
    val res = DatabaseManager.createTournament(nombre, estructura)
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.style(raw(getCss()))),
      body(style := "background:#1a1a1a;color:white;text-align:center;padding-top:50px;font-family:'Oswald';",
        h1("TORNEO CREADO"),
        h3(res),
        div(style := "margin-top:20px;",
          a(href := "/match-center", cls := "btn btn-warning fw-bold", "Ir a Jugar")
        )
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }

  @cask.get("/match/delete/:id")
  def deleteMatchAction(id: Int) = {
    DatabaseManager.deleteMatch(id)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history"))
  }
  // v7.4 — edit mejorado
  @cask.get("/match/edit/:matchId")
  def editMatchPage(request: cask.Request, matchId: Int) = withAuth(request) {
    val m = DatabaseManager.getMatchById(matchId)
    if (m.isEmpty) {
      cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history"))
    } else {
      val matchData = m.get
      val (gf, gc)  = if (matchData.resultado.contains("-"))
        (matchData.resultado.split("-")(0), matchData.resultado.split("-")(1))
      else ("0", "0")
      val tags = DatabaseManager.getVideoTags(matchId)

      // --- BLOQUE A3: Analisis de video con IA (solo lectura de BD al cargar) ---
      val videoStatus = DatabaseManager.getVideoAnalysisStatus(matchId)
      val videoDoneOpt: Option[(String, String)] = videoStatus.get("status") match {
        case Some("done") => Some((videoStatus("analisis").asInstanceOf[String], videoStatus("fecha").asInstanceOf[String]))
        case _ => None
      }
      val videoResultBlock: Modifier = videoDoneOpt match {
        case Some((analisis, fecha)) =>
          val secciones = DatabaseManager.parseVideoAnalysisSections(analisis)
          val txtFuertes: String = secciones.getOrElse("PUNTOS FUERTES", "")
          val txtMejorar: String = secciones.getOrElse("PUNTOS A MEJORAR", "")
          val txtEjercicio: String = secciones.getOrElse("EJERCICIO RECOMENDADO", "")
          val txtNota: String = secciones.getOrElse("NOTA TÉCNICA GLOBAL", "")
          val txtAcciones: String = secciones.getOrElse("ACCIONES DETECTADAS", "")
          div(id := "videoResultBlock",
            div(cls := "xx-small text-muted mb-2", s"Analizado el ${fecha.take(16)}"),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(32,201,151,0.12); border-left:3px solid #20c997;",
              strong(cls := "text-success d-block mb-1", "✅ PUNTOS FUERTES"),
              div(cls := "small", style := "white-space:pre-wrap;", txtFuertes)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(220,53,69,0.12); border-left:3px solid #dc3545;",
              strong(cls := "text-danger d-block mb-1", "⚠️ PUNTOS A MEJORAR"),
              div(cls := "small", style := "white-space:pre-wrap;", txtMejorar)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(13,110,253,0.12); border-left:3px solid #0d6efd;",
              strong(cls := "text-info d-block mb-1", "🏋️ EJERCICIO RECOMENDADO"),
              div(cls := "small", style := "white-space:pre-wrap;", txtEjercicio)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(255,193,7,0.15); border-left:3px solid #ffc107;",
              strong(cls := "text-warning d-block mb-1", "⭐ NOTA TÉCNICA GLOBAL"),
              div(cls := "small", style := "white-space:pre-wrap;", txtNota)),
            div(cls := "xx-small text-muted", "Acciones detectadas: ", txtAcciones),
            button(tpe := "button", cls := "btn btn-sm btn-outline-secondary mt-2", onclick := "toggleVideoReanalyze()", "🔄 Re-analizar")
          )
        case None => div(id := "videoResultBlock")
      }
      val videoUploadForm = div(id := "videoUploadForm", style := (if (videoDoneOpt.isDefined) "display:none;" else "display:block;"),
        p(cls := "xx-small text-muted", "Sube el vídeo del partido (o solo el fragmento donde aparece Héctor, recomendado) y Gemini analizará su actuación como portero."),
        div(cls := "mb-2",
          label(cls := "xx-small text-muted fw-bold d-block", "📹 Vídeo completo o ✂️ fragmento de Héctor"),
          input(tpe := "file", id := "videoFileInput", accept := "video/mp4,video/webm,video/quicktime",
            cls := "form-control form-control-sm bg-dark text-white")
        ),
        div(id := "videoUploadProgress", style := "display:none;",
          div(cls := "progress mb-2", style := "height:8px;",
            div(cls := "progress-bar progress-bar-striped progress-bar-animated bg-info", style := "width:100%")),
          div(cls := "xx-small text-info", "⏳ Subiendo y analizando con Gemini... 30-60 segundos")
        ),
        div(id := "videoUploadError", cls := "xx-small text-danger mt-1")
      )

      // --- Widget de tags de video ---
      val tagList = if (matchData.video.nonEmpty) {
        val tItems = for (t <- tags) yield {
          val link = if (matchData.video.contains("?"))
            s"${matchData.video}&t=${t.minuto * 60 + t.segundo}"
          else
            s"${matchData.video}?t=${t.minuto * 60 + t.segundo}"
          a(href := link, target := "_blank",
            cls := "list-group-item list-group-item-action bg-dark text-white border-secondary d-flex justify-content-between align-items-center",
            div(
              span(cls := "badge bg-danger me-2", s"${t.minuto}:${t.segundo}"),
              span(t.tipo)
            ),
            a(href := s"/video/delete_tag/${t.id}/$matchId",
              cls := "text-danger fw-bold text-decoration-none", "X")
          )
        }
        div(
          form(action := "/video/add_tag", method := "post", cls := "row g-2 mb-3",
            input(tpe := "hidden", name := "matchId", value := matchId.toString),
            div(cls := "col-3",
              input(tpe := "number", name := "min", cls := "form-control form-control-sm",
                placeholder := "Min", required := true)
            ),
            div(cls := "col-3",
              input(tpe := "number", name := "sec", cls := "form-control form-control-sm",
                placeholder := "Sec", required := true)
            ),
            div(cls := "col-6",
              div(cls := "input-group input-group-sm",
                select(name := "tipo", cls := "form-select",
                  option("PARADA"), option("ERROR"), option("GOL"), option("PASE")
                ),
                button(tpe := "submit", cls := "btn btn-warning", "+")
              )
            )
          ),
          div(cls := "list-group", tItems)
        )
      } else {
        div(cls := "alert alert-secondary small", "Anade URL de video para usar tags.")
      }

      val content = basePage("history",
        div(cls := "row justify-content-center",
          div(cls := "col-md-8 col-lg-7 col-12",

            div(cls := "d-flex justify-content-between align-items-center mb-3",
              h4(cls := "text-white fw-black mb-0", "✏️ Editar Partido"),
              a(href := "/history", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Historial")
            ),

            div(cls := "card bg-dark border-secondary shadow mb-3",
              div(cls := "card-header border-secondary fw-bold text-white small text-uppercase",
                "Datos del partido"),
              div(cls := "card-body p-3",
                form(action := "/match/update", method := "post",
                  attr("accept-charset") := "UTF-8",
                  input(tpe := "hidden", name := "id", value := matchId.toString),

                  // Rival + fecha
                  div(cls := "row g-2 mb-3",
                    div(cls := "col-8",
                      label(cls := "form-label small text-muted fw-bold", "RIVAL"),
                      input(tpe := "text", name := "rival", value := matchData.rival,
                        cls := "form-control bg-dark text-white border-secondary")),
                    div(cls := "col-4",
                      label(cls := "form-label small text-muted fw-bold", "FECHA"),
                      input(tpe := "date", name := "fecha", value := matchData.fecha,
                        cls := "form-control bg-dark text-white border-secondary"))
                  ),

                  // Resultado + nota
                  div(cls := "row g-2 mb-3",
                    div(cls := "col-3",
                      label(cls := "form-label small text-muted fw-bold", "GF"),
                      input(tpe := "number", name := "gf", value := gf,
                        cls := "form-control bg-dark text-white border-secondary", attr("min") := "0")),
                    div(cls := "col-3",
                      label(cls := "form-label small text-muted fw-bold", "GC"),
                      input(tpe := "number", name := "gc", value := gc,
                        cls := "form-control bg-dark text-white border-secondary", attr("min") := "0")),
                    div(cls := "col-3",
                      label(cls := "form-label small text-muted fw-bold", "NOTA"),
                      input(tpe := "number", step := "0.1", name := "nota",
                        value := matchData.nota.toString, attr("min") := "0", attr("max") := "10",
                        cls := "form-control bg-dark text-white border-secondary")),
                    div(cls := "col-3",
                      label(cls := "form-label small text-muted fw-bold", "MIN"),
                      input(tpe := "number", name := "minutos", value := matchData.minutos.toString,
                        cls := "form-control bg-dark text-white border-secondary", attr("min") := "0"))
                  ),

                  // Tipo + clima
                  div(cls := "row g-2 mb-3",
                    div(cls := "col-6",
                      label(cls := "form-label small text-muted fw-bold", "TIPO"),
                      select(name := "tipo", cls := "form-select bg-dark text-white border-secondary",
                        frag(Seq("LIGA","TORNEO","CUP","AMISTOSO").map { t =>
                          if (matchData.tipo == t) option(value := t, attr("selected") := "selected", t)
                          else option(value := t, t)
                        })
                      )
                    ),
                    div(cls := "col-6",
                      label(cls := "form-label small text-muted fw-bold", "CLIMA"),
                      select(name := "clima", cls := "form-select bg-dark text-white border-secondary",
                        frag(Seq("Sol","Nublado","Lluvia","Frío","Calor","Viento").map { c =>
                          if (matchData.clima == c) option(value := c, attr("selected") := "selected", c)
                          else option(value := c, c)
                        })
                      )
                    )
                  ),

                  // Estadio + local/visitante
                  div(cls := "row g-2 mb-3",
                    div(cls := "col-8",
                      label(cls := "form-label small text-muted fw-bold", "ESTADIO"),
                      input(tpe := "text", name := "estadio", value := matchData.estadio,
                        cls := "form-control bg-dark text-white border-secondary")),
                    div(cls := "col-4",
                      label(cls := "form-label small text-muted fw-bold", "LOCAL/VISIT."),
                      select(name := "esLocal", cls := "form-select bg-dark text-white border-secondary",
                        option(value := "", "—"),
                        option(value := "true",  "🏠 Local"),
                        option(value := "false", "✈️ Visitante")
                      )
                    )
                  ),

                  // Notas + reacción
                  div(cls := "mb-3",
                    label(cls := "form-label small text-muted fw-bold", "NOTAS DEL PARTIDO"),
                    textarea(name := "notas", cls := "form-control bg-dark text-white border-secondary",
                      rows := "3", matchData.notas)),
                  div(cls := "mb-3",
                    label(cls := "form-label small text-muted fw-bold", "REACCIÓN / GOLES ENCAJADOS"),
                    textarea(name := "reaccion", cls := "form-control bg-dark text-white border-secondary",
                      rows := "3", matchData.reaccion)),

                  // BLOQUE D: rubrica editable — completa los partidos guardados con el registro minimo
                  {
                    val rub = DatabaseManager.getRubricaMatch(matchId).getOrElse(Map.empty[String, Int])
                    div(cls := "mb-3",
                      label(cls := "form-label small text-muted fw-bold", "RÚBRICA (1-5)"),
                      div(cls := "row g-1",
                        frag(Seq(("rubricaPosicion", "posicion", "Posición"), ("rubricaDecisiones", "decisiones", "Decisiones"),
                            ("rubricaPies", "pies", "Pies"), ("rubricaComunicacion", "comunicacion", "Comunic."),
                            ("rubricaActitud", "actitud", "Actitud")).map { case (campo, clave, etiqueta) =>
                          div(cls := "col",
                            div(cls := "xx-small text-muted text-center", etiqueta),
                            select(name := campo, cls := "form-select form-select-sm bg-dark text-white border-secondary px-1",
                              option(value := "", "—"),
                              frag((1 to 5).map { v =>
                                if (rub.get(clave).contains(v)) option(value := v.toString, attr("selected") := "selected", v.toString)
                                else option(value := v.toString, v.toString)
                              })
                            )
                          )
                        })
                      )
                    )
                  },

                  // Video
                  div(cls := "mb-4",
                    label(cls := "form-label small text-muted fw-bold", "VIDEO URL (YouTube)"),
                    input(tpe := "text", name := "video", value := matchData.video,
                      cls := "form-control bg-dark text-white border-secondary",
                      placeholder := "https://youtube.com/watch?v=...")),

                  div(cls := "d-flex gap-2",
                    button(tpe := "submit", cls := "btn btn-success fw-bold flex-fill", "💾 Guardar cambios"),
                    a(href := "/history", cls := "btn btn-outline-secondary fw-bold", "Cancelar")
                  )
                )
              )
            ),

              // --- Footer: Diario de voz ---
              div(cls := "card-footer bg-secondary bg-opacity-10 border-top border-secondary mt-3",
                h6(cls := "text-info small fw-bold mb-2", "🎙 DIARIO DE VOZ (POST-PARTIDO)"),
                div(cls := "mb-2 small text-muted", "Graba a Hector contando como se sintio o sube un audio."),
                div(cls := "d-flex gap-2 mb-3",
                  button(id := "btnRecord", cls := "btn btn-sm btn-outline-danger",
                    onclick := "toggleRecording()", "⏺ Grabar"),
                  button(id := "btnStop", cls := "btn btn-sm btn-danger",
                    style := "display:none;", onclick := "stopRecording()", "⏹ Parar"),
                  input(tpe := "file", id := "fileUpload", accept := "audio/*",
                    cls := "form-control form-control-sm bg-dark text-white",
                    onchange := "handleFileUpload(this)")
                ),
                audio(id := "audioPreview", attr("controls") := "true",
                  style := "width: 100%; display:none;", cls := "mb-2"),
                form(action := "/match/analyze_audio", method := "post", id := "audioForm",
                  input(tpe := "hidden", name := "matchId",    value := matchId.toString),
                  input(tpe := "hidden", name := "audioData",  id := "hiddenAudioData"),
                  button(tpe := "button", id := "btnAnalyze", cls := "btn btn-info w-100",
                    onclick := "submitAudio()", attr("disabled") := "disabled", "🧠 Analizar Emociones con IA")
                ),
                if (matchData.analisisVoz.nonEmpty)
                  div(cls := "mt-3 p-2 border border-info rounded bg-dark text-light small",
                    style := "white-space: pre-wrap;",
                    b(cls := "text-info", "Psicologo IA: "), br,
                    fixEncoding(matchData.analisisVoz))
                else div()
              ),

              // --- Footer: Tags de video ---
              div(cls := "card-footer bg-secondary bg-opacity-25",
                h6(cls := "text-white small fw-bold", "CORTES DE VIDEO (TAGS)"),
                tagList
              ),

              // --- Footer: Analisis de video con IA (BLOQUE A3) ---
              div(cls := "card-footer bg-secondary bg-opacity-10 border-top border-secondary",
                h6(cls := "text-white small fw-bold mb-2", "🎬 ANÁLISIS DE VÍDEO CON IA"),
                videoResultBlock,
                videoUploadForm
              ),

              // --- Footer: Bloque 4.1 — Radar de rubrica de valoracion (solo si esta completa) ---
              DatabaseManager.getRubricaMatch(matchId) match {
                case Some(r) =>
                  div(cls := "card-footer bg-secondary bg-opacity-10 border-top border-secondary",
                    h6(cls := "text-white small fw-bold mb-2", "📋 RÚBRICA DE VALORACIÓN"),
                    div(style := "height:220px;", canvas(id := "rubricaRadarChart")),
                    script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
                    script(raw(s"""
                      new Chart(document.getElementById('rubricaRadarChart'), {
                        type: 'radar',
                        data: {
                          labels: ['Posición', 'Decisiones', 'Pies', 'Comunicación', 'Actitud'],
                          datasets: [{
                            label: 'Rúbrica',
                            data: [${r("posicion")}, ${r("decisiones")}, ${r("pies")}, ${r("comunicacion")}, ${r("actitud")}],
                            borderColor: '#ffc107', backgroundColor: 'rgba(255,193,7,0.2)', pointBackgroundColor: '#ffc107'
                          }]
                        },
                        options: { responsive: true, maintainAspectRatio: false,
                          plugins: { legend: { display: false } },
                          scales: { r: { min: 0, max: 5, ticks: { color: '#aaa', backdropColor: 'transparent', stepSize: 1 }, grid: { color: 'rgba(255,255,255,0.1)' }, pointLabels: { color: '#ccc', font: { size: 10 } } } }
                        }
                      });
                    """))
                  )
                case None => div()
              },

              // --- Footer: Bloque 4.4 — Guia de conversacion post-partido (solo lectura de BD) ---
              DatabaseManager.getGuiaConversacion(matchId) match {
                case Some(guia) =>
                  val partes = guia.split("/").map(_.trim)
                  def parte(prefijo: String): String = partes.find(_.startsWith(prefijo)).map(_.drop(prefijo.length).trim).getOrElse("")
                  div(cls := "card-footer bg-secondary bg-opacity-10 border-top border-secondary",
                    h6(cls := "text-white small fw-bold mb-2", "💬 GUÍA DE CONVERSACIÓN POST-PARTIDO"),
                    div(cls := "p-2 mb-2 rounded", style := "background:rgba(32,201,151,0.12); border-left:3px solid #20c997;",
                      strong(cls := "text-success d-block mb-1", "✅ QUÉ RESALTAR"),
                      div(cls := "small", parte("QUE_RESALTAR:"))),
                    div(cls := "p-2 mb-2 rounded", style := "background:rgba(255,193,7,0.15); border-left:3px solid #ffc107;",
                      strong(cls := "text-warning d-block mb-1", "🤐 QUÉ CALLAR"),
                      div(cls := "small", parte("QUE_CALLAR:"))),
                    div(cls := "p-2 rounded", style := "background:rgba(13,110,253,0.12); border-left:3px solid #0d6efd;",
                      strong(cls := "text-info d-block mb-1", "💛 ACCIÓN POSITIVA"),
                      div(cls := "small", parte("ACCION_POSITIVA:")))
                  )
                case None => div()
              }
            ),

            // Script grabacion de audio
            script(raw(""" let mediaRecorder; let audioChunks = []; async function toggleRecording() { try { const stream = await navigator.mediaDevices.getUserMedia({ audio: true }); mediaRecorder = new MediaRecorder(stream); mediaRecorder.start(); document.getElementById('btnRecord').style.display='none'; document.getElementById('btnStop').style.display='inline-block'; document.getElementById('btnAnalyze').disabled = true; mediaRecorder.ondataavailable = event => { audioChunks.push(event.data); }; mediaRecorder.onstop = () => { const audioBlob = new Blob(audioChunks, { type: 'audio/webm' }); const audioUrl = URL.createObjectURL(audioBlob); const audioEl = document.getElementById('audioPreview'); audioEl.src = audioUrl; audioEl.style.display = 'block'; const reader = new FileReader(); reader.readAsDataURL(audioBlob); reader.onloadend = () => { document.getElementById('hiddenAudioData').value = reader.result; document.getElementById('btnAnalyze').disabled = false; document.getElementById('btnAnalyze').innerHTML = "🧠 Analizar Grabacion"; }; audioChunks = []; }; } catch(err) { alert('Error microfono: ' + err); } } function stopRecording() { mediaRecorder.stop(); document.getElementById('btnRecord').style.display='inline-block'; document.getElementById('btnStop').style.display='none'; } function handleFileUpload(input) { if (input.files && input.files[0]) { const reader = new FileReader(); reader.onload = function (e) { document.getElementById('hiddenAudioData').value = e.target.result; document.getElementById('audioPreview').src = e.target.result; document.getElementById('audioPreview').style.display = 'block'; document.getElementById('btnAnalyze').disabled = false; document.getElementById('btnAnalyze').innerHTML = "🧠 Analizar Archivo"; }; reader.readAsDataURL(input.files[0]); } } function submitAudio() { document.getElementById('btnAnalyze').innerHTML = "⏳ Procesando... (puede tardar 10s)"; document.getElementById('btnAnalyze').disabled = true; document.getElementById('audioForm').submit(); } """)),

            // Script analisis de video IA (BLOQUE A3)
            script(raw(s"""
              var VIDEO_MATCH_ID = $matchId;
              var VIDEO_SECTIONS = ['ACCIONES DETECTADAS', 'PUNTOS FUERTES', 'PUNTOS A MEJORAR', 'EJERCICIO RECOMENDADO', 'NOTA TÉCNICA GLOBAL'];
              function toggleVideoReanalyze() {
                document.getElementById('videoUploadForm').style.display = 'block';
              }
              function escVideoTxt(s) {
                var d = document.createElement('div'); d.innerText = s || ''; return d.innerHTML;
              }
              function parseVideoSections(texto) {
                var upper = texto.toUpperCase();
                var result = {};
                for (var i = 0; i < VIDEO_SECTIONS.length; i++) {
                  var sec = VIDEO_SECTIONS[i];
                  var startIdx = upper.indexOf(sec);
                  if (startIdx < 0) { result[sec] = ''; continue; }
                  var contentStart = startIdx + sec.length;
                  var nextIdx = texto.length;
                  for (var j = i + 1; j < VIDEO_SECTIONS.length; j++) {
                    var idx2 = upper.indexOf(VIDEO_SECTIONS[j], contentStart);
                    if (idx2 >= 0) { nextIdx = idx2; break; }
                  }
                  var content = texto.substring(contentStart, nextIdx).trim();
                  if (content.indexOf(':') === 0) content = content.substring(1).trim();
                  result[sec] = content;
                }
                return result;
              }
              function renderVideoResult(analisis, fecha) {
                var s = parseVideoSections(analisis);
                var html = '';
                html += '<div class="xx-small text-muted mb-2">Analizado el ' + escVideoTxt(fecha.substring(0,16)) + '</div>';
                html += '<div class="p-2 mb-2 rounded" style="background:rgba(32,201,151,0.12); border-left:3px solid #20c997;"><strong class="text-success d-block mb-1">✅ PUNTOS FUERTES</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxt(s['PUNTOS FUERTES']) + '</div></div>';
                html += '<div class="p-2 mb-2 rounded" style="background:rgba(220,53,69,0.12); border-left:3px solid #dc3545;"><strong class="text-danger d-block mb-1">⚠️ PUNTOS A MEJORAR</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxt(s['PUNTOS A MEJORAR']) + '</div></div>';
                html += '<div class="p-2 mb-2 rounded" style="background:rgba(13,110,253,0.12); border-left:3px solid #0d6efd;"><strong class="text-info d-block mb-1">🏋️ EJERCICIO RECOMENDADO</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxt(s['EJERCICIO RECOMENDADO']) + '</div></div>';
                html += '<div class="p-2 mb-2 rounded" style="background:rgba(255,193,7,0.15); border-left:3px solid #ffc107;"><strong class="text-warning d-block mb-1">⭐ NOTA TÉCNICA GLOBAL</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxt(s['NOTA TÉCNICA GLOBAL']) + '</div></div>';
                html += '<div class="xx-small text-muted">Acciones detectadas: ' + escVideoTxt(s['ACCIONES DETECTADAS']) + '</div>';
                html += '<button type="button" class="btn btn-sm btn-outline-secondary mt-2" onclick="toggleVideoReanalyze()">🔄 Re-analizar</button>';
                document.getElementById('videoResultBlock').innerHTML = html;
                document.getElementById('videoUploadForm').style.display = 'none';
                document.getElementById('videoUploadProgress').style.display = 'none';
              }
              function pollVideoStatus() {
                var iv = setInterval(function() {
                  fetch('/video/analyze-status/' + VIDEO_MATCH_ID).then(function(r) { return r.json(); }).then(function(j) {
                    if (j.status === 'done') {
                      clearInterval(iv);
                      renderVideoResult(j.analisis, j.fecha);
                    }
                  }).catch(function() {});
                }, 5000);
              }
              var videoFileInputEl = document.getElementById('videoFileInput');
              if (videoFileInputEl) {
                videoFileInputEl.addEventListener('change', function(e) {
                  if (!e.target.files || !e.target.files[0]) return;
                  var fd = new FormData();
                  fd.append('video', e.target.files[0]);
                  fd.append('esFragmento', 'true');
                  document.getElementById('videoUploadProgress').style.display = 'block';
                  document.getElementById('videoUploadError').textContent = '';
                  fetch('/video/analyze-real/' + VIDEO_MATCH_ID, { method: 'POST', body: fd })
                    .then(function(r) { return r.json().then(function(j) { return { ok: r.status === 202, body: j }; }); })
                    .then(function(res) {
                      if (res.ok) {
                        pollVideoStatus();
                      } else {
                        document.getElementById('videoUploadProgress').style.display = 'none';
                        document.getElementById('videoUploadError').textContent = res.body.error || 'Error al subir el vídeo.';
                      }
                    })
                    .catch(function(err) {
                      document.getElementById('videoUploadProgress').style.display = 'none';
                      document.getElementById('videoUploadError').textContent = 'Error de red: ' + err.message;
                    });
                });
              }
            """))
          )
        )
      renderHtml(content)
    }
  }
  @cask.postForm("/match/analyze_audio")
  def analyzeAudioAction(matchId: Int, audioData: String) = {
    // La logica de IA y la actualizacion de la DB ahora ocurren dentro de analyzeAudioLog
    // audioData ya viene como Base64 desde el script del navegador
    DatabaseManager.analyzeAudioLog(matchId, audioData)

    cask.Response(
      "".getBytes("UTF-8"),
      statusCode = 302,
      headers = Seq("Location" -> s"/match/edit/$matchId")
    )
  }
  // ── MODULO 8: AUDIO-DIARIO (PARTIDO / ACADEMIA) — pagina dedicada ────────
  @cask.get("/audio-diary/:tipo/:itemId")
  def audioDiaryPage(request: cask.Request, tipo: String, itemId: Int, processing: String = "") = withAuth(request) {
    val (headerLabel, existingAnalysis, backHref) = tipo match {
      case "academia" =>
        val t = DatabaseManager.getTrainingById(itemId)
        (t.map(tt => s"Academia de porteros · ${tt.fecha.take(10)}").getOrElse("Sesión no encontrada"),
         t.map(_.analisisVozAcademia).getOrElse(""), "/bio")
      case _ =>
        val m = DatabaseManager.getMatchById(itemId)
        (m.map(mm => s"vs ${fixEncoding(mm.rival)} · ${mm.fecha.take(10)} · resultado ${mm.resultado}").getOrElse("Partido no encontrado"),
         m.map(_.analisisVoz).getOrElse(""), "/history")
    }
    val isProcessing = processing == "1" && existingAnalysis.isEmpty

    // ── BLOQUE D: ANALISIS DE VIDEO CON IA EN ENTRENAMIENTOS (solo tipo=academia) ──
    val videoTrainingSection: Modifier = if (tipo != "academia") frag() else {
      val videoStatus = DatabaseManager.getVideoAnalysisStatusTraining(itemId)
      val videoDoneOpt: Option[(String, String)] = videoStatus.get("status") match {
        case Some("done") => Some((videoStatus("analisis").asInstanceOf[String], videoStatus("fecha").asInstanceOf[String]))
        case _ => None
      }
      val videoResultBlock: Modifier = videoDoneOpt match {
        case Some((analisis, fecha)) =>
          val secciones = DatabaseManager.parseVideoAnalysisSectionsTraining(analisis)
          val txtEjercicio   = secciones.getOrElse("EJERCICIO DETECTADO", "")
          val txtProgresion  = secciones.getOrElse("PROGRESIÓN", "")
          val txtError       = secciones.getOrElse("ERROR RECURRENTE", "")
          val txtFuerte      = secciones.getOrElse("PUNTO FUERTE", "")
          val txtRecomend    = secciones.getOrElse("RECOMENDACIÓN ACADEMIA", "")
          div(id := "videoResultBlockTraining",
            div(cls := "xx-small text-muted mb-2", s"Analizado el ${fecha.take(16)}"),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(13,110,253,0.12); border-left:3px solid #0d6efd;",
              strong(cls := "text-info d-block mb-1", "🎯 EJERCICIO DETECTADO"),
              div(cls := "small", style := "white-space:pre-wrap;", txtEjercicio)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(32,201,151,0.12); border-left:3px solid #20c997;",
              strong(cls := "text-success d-block mb-1", "📈 PROGRESIÓN"),
              div(cls := "small", style := "white-space:pre-wrap;", txtProgresion)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(220,53,69,0.12); border-left:3px solid #dc3545;",
              strong(cls := "text-danger d-block mb-1", "⚠️ ERROR RECURRENTE"),
              div(cls := "small", style := "white-space:pre-wrap;", txtError)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(255,193,7,0.15); border-left:3px solid #ffc107;",
              strong(cls := "text-warning d-block mb-1", "💪 PUNTO FUERTE"),
              div(cls := "small", style := "white-space:pre-wrap;", txtFuerte)),
            div(cls := "p-2 mb-2 rounded", style := "background:rgba(139,92,246,0.15); border-left:3px solid #8b5cf6;",
              strong(cls := "d-block mb-1", style := "color:#8b5cf6;", "🎓 RECOMENDACIÓN ACADEMIA"),
              div(cls := "small", style := "white-space:pre-wrap;", txtRecomend)),
            button(tpe := "button", cls := "btn btn-sm btn-outline-secondary mt-2", onclick := "toggleVideoReanalyzeTraining()", "🔄 Re-analizar")
          )
        case None => div(id := "videoResultBlockTraining")
      }
      val videoUploadForm = div(id := "videoUploadFormTraining", style := (if (videoDoneOpt.isDefined) "display:none;" else "display:block;"),
        p(cls := "xx-small text-muted", "Sube el vídeo del entreno (completo o solo el fragmento) y Gemini analizará la progresión técnica de Héctor."),
        div(cls := "mb-2",
          label(cls := "xx-small text-muted fw-bold d-block", "📹 Vídeo completo o ✂️ fragmento"),
          input(tpe := "file", id := "videoFileInputTraining", accept := "video/mp4,video/webm,video/quicktime",
            cls := "form-control form-control-sm bg-dark text-white")
        ),
        div(id := "videoUploadProgressTraining", style := "display:none;",
          div(cls := "progress mb-2", style := "height:8px;",
            div(cls := "progress-bar progress-bar-striped progress-bar-animated bg-info", style := "width:100%")),
          div(cls := "xx-small text-info", "⏳ Subiendo y analizando con Gemini... 30-60 segundos")
        ),
        div(id := "videoUploadErrorTraining", cls := "xx-small text-danger mt-1")
      )

      div(
        div(cls := "card bg-dark border-info shadow mb-3",
          div(cls := "card-header text-info fw-bold small", "🎬 ANÁLISIS DE VÍDEO CON IA"),
          div(cls := "card-body p-3", videoResultBlock, videoUploadForm)
        ),
        script(raw(s"""
          var VIDEO_TRAINING_ID = $itemId;
          var VIDEO_SECTIONS_TRAINING = ['EJERCICIO DETECTADO', 'PROGRESIÓN', 'ERROR RECURRENTE', 'PUNTO FUERTE', 'RECOMENDACIÓN ACADEMIA'];
          function toggleVideoReanalyzeTraining() {
            document.getElementById('videoUploadFormTraining').style.display = 'block';
          }
          function escVideoTxtTraining(s) {
            var d = document.createElement('div'); d.innerText = s || ''; return d.innerHTML;
          }
          function parseVideoSectionsTraining(texto) {
            var upper = texto.toUpperCase();
            var result = {};
            for (var i = 0; i < VIDEO_SECTIONS_TRAINING.length; i++) {
              var sec = VIDEO_SECTIONS_TRAINING[i];
              var startIdx = upper.indexOf(sec);
              if (startIdx < 0) { result[sec] = ''; continue; }
              var contentStart = startIdx + sec.length;
              var nextIdx = texto.length;
              for (var j = i + 1; j < VIDEO_SECTIONS_TRAINING.length; j++) {
                var idx2 = upper.indexOf(VIDEO_SECTIONS_TRAINING[j], contentStart);
                if (idx2 >= 0) { nextIdx = idx2; break; }
              }
              var content = texto.substring(contentStart, nextIdx).trim();
              if (content.indexOf(':') === 0) content = content.substring(1).trim();
              result[sec] = content;
            }
            return result;
          }
          function renderVideoResultTraining(analisis, fecha) {
            var s = parseVideoSectionsTraining(analisis);
            var html = '';
            html += '<div class="xx-small text-muted mb-2">Analizado el ' + escVideoTxtTraining(fecha.substring(0,16)) + '</div>';
            html += '<div class="p-2 mb-2 rounded" style="background:rgba(13,110,253,0.12); border-left:3px solid #0d6efd;"><strong class="text-info d-block mb-1">🎯 EJERCICIO DETECTADO</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxtTraining(s['EJERCICIO DETECTADO']) + '</div></div>';
            html += '<div class="p-2 mb-2 rounded" style="background:rgba(32,201,151,0.12); border-left:3px solid #20c997;"><strong class="text-success d-block mb-1">📈 PROGRESIÓN</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxtTraining(s['PROGRESIÓN']) + '</div></div>';
            html += '<div class="p-2 mb-2 rounded" style="background:rgba(220,53,69,0.12); border-left:3px solid #dc3545;"><strong class="text-danger d-block mb-1">⚠️ ERROR RECURRENTE</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxtTraining(s['ERROR RECURRENTE']) + '</div></div>';
            html += '<div class="p-2 mb-2 rounded" style="background:rgba(255,193,7,0.15); border-left:3px solid #ffc107;"><strong class="text-warning d-block mb-1">💪 PUNTO FUERTE</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxtTraining(s['PUNTO FUERTE']) + '</div></div>';
            html += '<div class="p-2 mb-2 rounded" style="background:rgba(139,92,246,0.15); border-left:3px solid #8b5cf6;"><strong class="d-block mb-1" style="color:#8b5cf6;">🎓 RECOMENDACIÓN ACADEMIA</strong><div class="small" style="white-space:pre-wrap;">' + escVideoTxtTraining(s['RECOMENDACIÓN ACADEMIA']) + '</div></div>';
            html += '<button type="button" class="btn btn-sm btn-outline-secondary mt-2" onclick="toggleVideoReanalyzeTraining()">🔄 Re-analizar</button>';
            document.getElementById('videoResultBlockTraining').innerHTML = html;
            document.getElementById('videoUploadFormTraining').style.display = 'none';
            document.getElementById('videoUploadProgressTraining').style.display = 'none';
          }
          function pollVideoStatusTraining() {
            var iv = setInterval(function() {
              fetch('/video/training-status/' + VIDEO_TRAINING_ID).then(function(r) { return r.json(); }).then(function(j) {
                if (j.status === 'done') {
                  clearInterval(iv);
                  renderVideoResultTraining(j.analisis, j.fecha);
                }
              }).catch(function() {});
            }, 5000);
          }
          var videoFileInputTrainingEl = document.getElementById('videoFileInputTraining');
          if (videoFileInputTrainingEl) {
            videoFileInputTrainingEl.addEventListener('change', function(e) {
              if (!e.target.files || !e.target.files[0]) return;
              var fd = new FormData();
              fd.append('video', e.target.files[0]);
              document.getElementById('videoUploadProgressTraining').style.display = 'block';
              document.getElementById('videoUploadErrorTraining').textContent = '';
              fetch('/video/analyze-training/' + VIDEO_TRAINING_ID, { method: 'POST', body: fd })
                .then(function(r) { return r.json().then(function(j) { return { ok: r.status === 202, body: j }; }); })
                .then(function(res) {
                  if (res.ok) {
                    pollVideoStatusTraining();
                  } else {
                    document.getElementById('videoUploadProgressTraining').style.display = 'none';
                    document.getElementById('videoUploadErrorTraining').textContent = res.body.error || 'Error al subir el vídeo.';
                  }
                })
                .catch(function(err) {
                  document.getElementById('videoUploadProgressTraining').style.display = 'none';
                  document.getElementById('videoUploadErrorTraining').textContent = 'Error de red: ' + err.message;
                });
            });
          }
        """))
      )
    }

    val content = basePage("history",
      div(cls := "row justify-content-center",
        div(cls := "col-md-7 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h4(cls := "text-white fw-black mb-0", "🎙️ Audio-Diario"),
            a(href := backHref, cls := "btn btn-outline-secondary btn-sm fw-bold", "← Volver")
          ),
          div(cls := "card bg-dark border-info shadow mb-3",
            div(cls := "card-header text-info fw-bold small", headerLabel)
          ),

          if (isProcessing)
            div(cls := "alert alert-info text-center",
              "⏳ Analizando con Gemini... esta página se actualizará sola en unos segundos.",
              script(raw("setTimeout(function(){ window.location.reload(); }, 4000);"))
            )
          else if (existingAnalysis.nonEmpty)
            div(cls := "card bg-dark border-success shadow mb-3",
              div(cls := "card-header text-success fw-bold small", "🧠 Análisis guardado"),
              div(cls := "card-body text-light small", style := "white-space:pre-wrap;", fixEncoding(existingAnalysis))
            )
          else span(),

          div(cls := "card bg-dark border-secondary shadow mb-3",
            div(cls := "card-header text-white fw-bold small",
              if (existingAnalysis.nonEmpty) "🔁 Regrabar o subir un nuevo audio" else "Grabar o subir audio"),
            div(cls := "card-body p-3",
              div(cls := "d-flex gap-2 mb-2 align-items-center",
                button(id := "btnRecord", tpe := "button", cls := "btn btn-sm btn-outline-danger", onclick := "adToggleRecording()", "⏺ Grabar"),
                button(id := "btnStop", tpe := "button", cls := "btn btn-sm btn-danger", style := "display:none;", onclick := "adStopRecording()", "⏹ Parar"),
                span(id := "adTimer", cls := "text-muted small")
              ),
              div(cls := "mb-2",
                label(cls := "xx-small text-muted fw-bold", "O sube un archivo (MP3/M4A/WebM/OGG/MP4, máx. 10MB)"),
                input(tpe := "file", id := "adFileUpload", accept := "audio/*",
                  cls := "form-control form-control-sm bg-dark text-white", onchange := "adHandleFile(this)")
              ),
              audio(id := "adPreview", attr("controls") := "true", style := "width:100%; display:none; margin:8px 0;"),
              div(id := "adStatus", cls := "xx-small text-muted mb-2"),
              form(action := "/audio-diary/analyze", method := "post", id := "adForm",
                input(tpe := "hidden", name := "tipo", value := tipo),
                input(tpe := "hidden", name := "id", value := itemId.toString),
                input(tpe := "hidden", name := "audioData", id := "adHiddenData"),
                button(tpe := "button", id := "btnAnalyze", cls := "btn btn-info w-100 fw-bold",
                  attr("disabled") := "disabled", onclick := "adSubmit()", "🧠 Analizar con Gemini")
              )
            )
          ),

          videoTrainingSection
        )
      ),
      script(raw("""
        let adMediaRecorder; let adChunks = []; let adTimerInterval; let adSeconds = 0;
        const AD_MAX_BYTES = 10 * 1024 * 1024;
        async function adToggleRecording() {
          try {
            const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
            adMediaRecorder = new MediaRecorder(stream);
            adChunks = []; adSeconds = 0;
            document.getElementById('adTimer').textContent = '0:00';
            adMediaRecorder.start();
            document.getElementById('btnRecord').style.display = 'none';
            document.getElementById('btnStop').style.display = 'inline-block';
            document.getElementById('btnAnalyze').disabled = true;
            adTimerInterval = setInterval(function() {
              adSeconds++;
              var m = Math.floor(adSeconds / 60), s = adSeconds % 60;
              document.getElementById('adTimer').textContent = m + ':' + String(s).padStart(2,'0');
            }, 1000);
            adMediaRecorder.ondataavailable = function(e) { adChunks.push(e.data); };
            adMediaRecorder.onstop = function() {
              clearInterval(adTimerInterval);
              var blob = new Blob(adChunks, { type: 'audio/webm' });
              var url = URL.createObjectURL(blob);
              var el = document.getElementById('adPreview');
              el.src = url; el.style.display = 'block';
              var reader = new FileReader();
              reader.onloadend = function() {
                document.getElementById('adHiddenData').value = reader.result;
                document.getElementById('btnAnalyze').disabled = false;
                document.getElementById('adStatus').textContent = '✅ Grabación lista para analizar';
              };
              reader.readAsDataURL(blob);
            };
          } catch (err) { alert('Error de micrófono: ' + err); }
        }
        function adStopRecording() {
          adMediaRecorder.stop();
          document.getElementById('btnRecord').style.display = 'inline-block';
          document.getElementById('btnStop').style.display = 'none';
        }
        function adHandleFile(input) {
          if (input.files && input.files[0]) {
            var file = input.files[0];
            if (file.size > AD_MAX_BYTES) { alert('El archivo supera los 10MB'); input.value=''; return; }
            var reader = new FileReader();
            reader.onload = function(e) {
              document.getElementById('adHiddenData').value = e.target.result;
              document.getElementById('adPreview').src = e.target.result;
              document.getElementById('adPreview').style.display = 'block';
              document.getElementById('btnAnalyze').disabled = false;
              document.getElementById('adStatus').textContent = '✅ Archivo listo para analizar';
            };
            reader.readAsDataURL(file);
          }
        }
        function adSubmit() {
          document.getElementById('btnAnalyze').textContent = '⏳ Enviando...';
          document.getElementById('btnAnalyze').disabled = true;
          document.getElementById('adForm').submit();
        }
      """))
    )
    renderHtml(content)
  }

  @cask.postForm("/audio-diary/analyze")
  def audioDiaryAnalyze(tipo: String, id: Int, audioData: String) = {
    // Fire-and-forget en background thread: la nota queda sin referencias tras terminar
    // (nunca se escribe en disco) y no bloquea la navegacion del padre.
    val dataSnapshot = audioData
    new Thread(new Runnable {
      def run(): Unit = {
        try {
          if (tipo == "academia") DatabaseManager.analyzeAudioDiaryAcademia(id, dataSnapshot)
          else DatabaseManager.analyzeAudioDiaryMatch(id, dataSnapshot)
        } catch { case _: Exception => () }
      }
    }).start()

    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> s"/audio-diary/$tipo/$id?processing=1"))
  }

  @cask.postForm("/video/add_tag")
  def addVideoTag(matchId: Int, min: Int, sec: Int, tipo: String) = {
    DatabaseManager.addVideoTag(matchId, min, sec, tipo, "")
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> s"/match/edit/$matchId"))
  }
  @cask.get("/video/delete_tag/:id/:matchId")
  def deleteVideoTag(id: Int, matchId: Int) = {
    DatabaseManager.deleteVideoTag(id)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> s"/match/edit/$matchId"))
  }
  @cask.postForm("/match/update")
  def updateMatchAction(request: cask.Request, id: Int, rival: String, gf: Int, gc: Int,
                        nota: Double, minutos: String = "60", tipo: String = "LIGA",
                        clima: String = "Sol", estadio: String = "",
                        esLocal: String = "", notas: String = "",
                        video: String = "", reaccion: String = "", fecha: String,
                        rubricaPosicion: String = "", rubricaDecisiones: String = "", rubricaPies: String = "",
                        rubricaComunicacion: String = "", rubricaActitud: String = "") = withAuth(request) {
    val min = try minutos.toInt catch { case _: Exception => 60 }
    // BLOQUE D: la rubrica solo se guarda si vienen las 5 dimensiones con valor 1-5
    val rubrica = Seq(rubricaPosicion, rubricaDecisiones, rubricaPies, rubricaComunicacion, rubricaActitud)
      .flatMap(_.toIntOption.filter(v => v >= 1 && v <= 5))
    if (rubrica.size == 5) DatabaseManager.updateRubricaMatch(id, rubrica(0), rubrica(1), rubrica(2), rubrica(3), rubrica(4))
    DatabaseManager.updateMatch(id, fixEncoding(rival), gf, gc, min, nota,
      clima, fixEncoding(estadio), 20, fixEncoding(notas), video, fixEncoding(reaccion), fecha)
    // Update es_local and tipo separately if columns exist
    DatabaseManager.updateMatchExtra(id, tipo, esLocal)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history"))
  }

  @cask.get("/videoteca")
  def videotecaPage(request: cask.Request, tipo: String = "") = withAuth(request) {
    val clips = DatabaseManager.getVideotecaClips(tipo)
    val tipos = List("", "PARADA", "GOL", "PASE", "ERROR")

    // Extraer YouTube video ID de la URL
    def extractYoutubeId(url: String): String = {
      if (url.contains("youtu.be/")) url.split("youtu.be/").last.split("[?&]").head
      else if (url.contains("v=")) url.split("v=").last.split("&").head
      else ""
    }

    val ytClips = clips.filter(c => extractYoutubeId(c.videoUrl).nonEmpty)
    val clipsJson = ytClips.map { c =>
      val vid = extractYoutubeId(c.videoUrl)
      val secs = c.minuto * 60 + c.segundo
      s"""{"videoId":"$vid","start":$secs,"rival":"${c.rival}","fecha":"${c.fecha}","tipo":"${c.tipo}","matchId":${c.matchId}}"""
    }.mkString("[", ",", "]")

    val tipoLabel = if (tipo.isEmpty) "TODOS" else tipo

    val filterTabs = div(cls := "d-flex gap-2 mb-4 flex-wrap",
      tipos.map { t =>
        val label = if (t.isEmpty) "TODOS" else t
        val active = if (t == tipo) "btn-warning" else "btn-outline-secondary"
        a(href := s"/videoteca${if (t.nonEmpty) s"?tipo=$t" else ""}",
          cls := s"btn btn-sm fw-bold $active", label)
      }
    )

    val playlistItems = if (ytClips.isEmpty) {
      div(cls := "alert alert-secondary text-center fw-bold",
        "Sin clips con video de YouTube disponibles. Anade URL de YouTube y tags en los partidos.")
    } else {
      div(cls := "playlist-list", id := "playlist",
        ytClips.zipWithIndex.map { case (c, i) =>
          val icon = c.tipo match {
            case "PARADA" => "🧤"; case "GOL" => "🥅"; case "PASE" => "⚽"; case _ => "📍"
          }
          val activeCls = if (i == 0) " active" else ""
          div(cls := s"playlist-item d-flex align-items-center p-2 mb-1 rounded$activeCls",
            id := s"clip-$i",
            attr("onclick") := s"loadClip($i)",
            div(cls := "me-3 fs-4", icon),
            div(cls := "flex-grow-1",
              div(cls := "fw-bold text-white small", s"${c.rival} — ${c.fecha}"),
              div(cls := "text-muted small", s"${c.minuto}:${"%02d".format(c.segundo)} • ${c.tipo}")
            ),
            span(cls := "badge bg-secondary ms-2", s"${i + 1}")
          )
        }
      )
    }

    val playerSection = if (ytClips.nonEmpty) {
      div(cls := "video-player-section",
        // Player
        div(cls := "ratio ratio-16x9 mb-3 rounded overflow-hidden",
          id := "player-container",
          div(id := "yt-player")
        ),
        // Info del clip actual
        div(cls := "d-flex align-items-center justify-content-between mb-3",
          div(id := "clip-info",
            div(cls := "fw-bold text-warning", id := "clip-title", ytClips.head.rival + " — " + ytClips.head.fecha),
            div(cls := "small text-muted", id := "clip-sub",
              s"${ytClips.head.minuto}:${"%02d".format(ytClips.head.segundo)} • ${ytClips.head.tipo}")
          ),
          div(cls := "badge bg-dark border border-warning text-warning px-3 py-2",
            span(id := "clip-counter", s"1 / ${ytClips.size}")
          )
        ),
        // Controles
        div(cls := "d-flex gap-2 mb-3",
          button(id := "btn-prev", cls := "btn btn-outline-secondary fw-bold", onclick := "prevClip()", "◀ Ant"),
          button(id := "btn-motivame", cls := "btn btn-warning fw-bold flex-grow-1", onclick := "toggleMotivame()",
            "🔥 MOTIVAME"),
          button(id := "btn-next", cls := "btn btn-outline-secondary fw-bold", onclick := "nextClip()", "Sig ▶")
        ),
        // Auto-advance toggle
        div(cls := "d-flex align-items-center gap-2 small text-muted",
          input(tpe := "range", id := "clip-duration", cls := "form-range", attr("min") := "10", attr("max") := "60", attr("value") := "25", style := "width: 120px;"),
          span("Duracion clip: "),
          span(id := "dur-label", "25s"),
          span(cls := "ms-3", "🔁"),
          div(cls := "form-check form-switch mb-0 ms-1",
            input(cls := "form-check-input", tpe := "checkbox", id := "loop-toggle"),
            label(cls := "form-check-label text-muted", attr("for") := "loop-toggle", "Bucle")
          )
        )
      )
    } else div()

    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-10 col-12",
          h2(cls := "text-center text-warning mb-1", "🎬 VIDEOTECA"),
          p(cls := "text-center text-muted small mb-4", s"${clips.size} clips totales • Filtro: $tipoLabel"),
          filterTabs,
          if (ytClips.isEmpty) playlistItems
          else div(cls := "row g-3",
            div(cls := "col-md-7", playerSection),
            div(cls := "col-md-5",
              div(cls := "card bg-dark border-secondary shadow h-100",
                div(cls := "card-header text-warning fw-bold small", s"📋 PLAYLIST — ${ytClips.size} clips"),
                div(cls := "card-body p-2 overflow-auto", style := "max-height: 420px;",
                  playlistItems
                )
              )
            )
          ),
          if (clips.size != ytClips.size) {
            val sinYt = clips.size - ytClips.size
            div(cls := "alert alert-secondary small mt-3 text-muted",
              s"ℹ️ $sinYt clip(s) con URL no-YouTube no se muestran en el player (Drive, etc.)")
          } else div(),
          script(raw(s"""
            const clips = $clipsJson;
            let current = 0;
            let player;
            let motivameTimer = null;
            let isMotivame = false;
            let clipDuration = 25;

            // Duracion slider
            document.getElementById('clip-duration').addEventListener('input', function() {
              clipDuration = parseInt(this.value);
              document.getElementById('dur-label').textContent = clipDuration + 's';
            });

            // Cargar YouTube API
            var tag = document.createElement('script');
            tag.src = "https://www.youtube.com/iframe_api";
            document.head.appendChild(tag);

            function onYouTubeIframeAPIReady() {
              if (clips.length === 0) return;
              player = new YT.Player('yt-player', {
                height: '100%', width: '100%',
                videoId: clips[0].videoId,
                playerVars: { start: clips[0].start, autoplay: 0, rel: 0, modestbranding: 1 },
                events: { onStateChange: onPlayerStateChange }
              });
            }

            function onPlayerStateChange(event) {
              // Si termina el video y esta en modo motivame, siguiente
              if (event.data === YT.PlayerState.ENDED && isMotivame) {
                nextClip();
              }
            }

            function loadClip(idx) {
              if (!player || clips.length === 0) return;
              current = idx;
              const c = clips[idx];
              player.loadVideoById({ videoId: c.videoId, startSeconds: c.start });
              // Actualizar info
              document.getElementById('clip-title').textContent = c.rival + ' — ' + c.fecha;
              document.getElementById('clip-sub').textContent = c.start + 's • ' + c.tipo;
              document.getElementById('clip-counter').textContent = (idx+1) + ' / ' + clips.length;
              // Resaltar playlist
              document.querySelectorAll('.playlist-item').forEach((el, i) => {
                el.classList.toggle('active', i === idx);
              });
              // Scroll en playlist
              const el = document.getElementById('clip-' + idx);
              if (el) el.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
              // Si modo motivame, programar siguiente clip tras clipDuration segundos
              if (isMotivame) {
                clearTimeout(motivameTimer);
                motivameTimer = setTimeout(() => {
                  const doLoop = document.getElementById('loop-toggle').checked;
                  const next = (current + 1) % clips.length;
                  if (!doLoop && next === 0) { stopMotivame(); return; }
                  nextClip();
                }, clipDuration * 1000);
              }
            }

            function nextClip() {
              const doLoop = document.getElementById('loop-toggle').checked;
              const next = (current + 1) % clips.length;
              if (!doLoop && next === 0 && current === clips.length - 1) { stopMotivame(); return; }
              loadClip(next);
            }

            function prevClip() {
              loadClip((current - 1 + clips.length) % clips.length);
            }

            function toggleMotivame() {
              if (isMotivame) { stopMotivame(); } else { startMotivame(); }
            }

            function startMotivame() {
              isMotivame = true;
              document.getElementById('btn-motivame').className = 'btn btn-danger fw-bold flex-grow-1';
              document.getElementById('btn-motivame').textContent = '⏹ DETENER';
              loadClip(0);
            }

            function stopMotivame() {
              isMotivame = false;
              clearTimeout(motivameTimer);
              document.getElementById('btn-motivame').className = 'btn btn-warning fw-bold flex-grow-1';
              document.getElementById('btn-motivame').textContent = '🔥 MOTIVAME';
            }
          """))
        )
      )
    )
    renderHtml(content)
  }

  @cask.get("/tournament/bracket")
  def tournamentBracket(request: cask.Request, nombre: String = "") = withAuth(request) {
    val torneos = DatabaseManager.getTournamentNames()
    val matches = if (nombre.nonEmpty) DatabaseManager.getTournamentMatches(nombre) else List.empty

    // Agrupar por fase
    val byFase = matches.groupBy(_.fase)
    val fasesOrden = Seq("Grupos","Cuartos","Semifinal","Final","Octavos","Ronda 1","Ronda 2","Ronda 3")
    val fases = (fasesOrden.filter(byFase.contains) ++ byFase.keys.filterNot(fasesOrden.contains)).distinct

    def matchCard(m: MatchLog) = {
      val (gf, gc) = m.resultado.split("-") match {
        case Array(a,b) => (a.trim, b.trim); case _ => ("-","-")
      }
      val statusCls = if (m.status == "PLAYED") {
        if (gf.toIntOption.getOrElse(0) > gc.toIntOption.getOrElse(0)) "border-success"
        else if (gf == gc) "border-warning"
        else "border-danger"
      } else "border-secondary"
      val resultBadge = if (m.status == "PLAYED")
        span(cls := s"badge ${if(gf.toIntOption.getOrElse(0) > gc.toIntOption.getOrElse(0)) "bg-success" else "bg-danger"} fw-bold ms-2",
          s"$gf - $gc")
      else span(cls := "badge bg-secondary ms-2", "Pendiente")

      div(cls := s"card bg-dark $statusCls shadow bracket-match mb-2",
        div(cls := "card-body p-2",
          div(cls := "d-flex justify-content-between align-items-center",
            div(
              div(cls := "fw-bold text-white small", fixEncoding(m.rival)),
              div(cls := "xx-small text-muted", m.fecha)
            ),
            resultBadge
          ),
          if (m.status == "PLAYED" && m.nota > 0)
            div(cls := "xx-small text-muted mt-1", s"Nota: ${m.nota} | Paradas: ${m.paradas}")
          else span()
        )
      )
    }

    val bracketContent = if (nombre.isEmpty || matches.isEmpty) {
      div(cls := "alert alert-secondary text-center py-4",
        div(style:="font-size:36px; opacity:0.3;","🏆"),
        div(cls:="fw-bold mt-2", if(nombre.isEmpty)"Selecciona un torneo" else s"Sin partidos en '$nombre'")
      )
    } else {
      div(cls := "bracket-container",
        div(cls := "d-flex gap-3 overflow-auto pb-3",
          fases.map { fase =>
            val ms = byFase.getOrElse(fase, List.empty)
            div(style := "min-width: 220px;",
              div(cls := "text-center mb-2",
                span(cls := "badge bg-warning text-dark fw-bold px-3 py-2", style:="font-size:13px;", fase.toUpperCase)
              ),
              div(ms.map(matchCard))
            )
          }
        ),
        // Stats del torneo
        if (matches.exists(_.status == "PLAYED")) {
          val played = matches.filter(_.status == "PLAYED")
          val wins = played.count(m => { val p = m.resultado.split("-"); p(0).trim.toIntOption.getOrElse(0) > p(1).trim.toIntOption.getOrElse(0) })
          val gc = played.flatMap(m => m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption)).sum
          val avgNota = if (played.nonEmpty) played.map(_.nota).sum / played.size else 0.0
          div(cls := "row g-2 mt-3",
            Seq(
              ("Partidos", played.size.toString, "secondary"),
              ("Victorias", wins.toString, "success"),
              ("Goles Enc.", gc.toString, "danger"),
              ("Nota Media", f"$avgNota%.1f", "warning")
            ).map { case (lbl, v, c) =>
              div(cls := "col-3",
                div(cls := s"card bg-dark border-$c text-center py-2",
                  div(cls := s"text-$c fw-bold fs-5", v),
                  div(cls := "xx-small text-muted", lbl)
                )
              )
            }
          )
        } else div()
      )
    }

    val content = basePage("match-center",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          h2(cls := "text-center text-warning mb-1", "BRACKET TORNEO"),
          p(cls := "text-center text-muted small mb-4", "Visualizacion de fases y resultados"),

          // Selector de torneo
          div(cls := "card bg-dark border-secondary shadow mb-4",
            div(cls := "card-body d-flex gap-2 flex-wrap align-items-center",
              span(cls := "text-muted small fw-bold me-2", "Torneo:"),
              if (torneos.isEmpty)
                span(cls := "text-muted small", "Sin torneos registrados. Crea uno desde Match Center.")
              else
                div(cls := "d-flex gap-2 flex-wrap",
                  a(href := "/tournament/bracket",
                    cls := s"btn btn-sm fw-bold ${if(nombre.isEmpty)"btn-warning"else"btn-outline-secondary"}",
                    "Seleccionar"),
                  torneos.map { t =>
                    a(href := s"/tournament/bracket?nombre=${java.net.URLEncoder.encode(t, "UTF-8")}",
                      cls := s"btn btn-sm fw-bold ${if(nombre == t)"btn-warning"else"btn-outline-secondary"}",
                      fixEncoding(t))
                  }
                ),
              div(cls := "ms-auto",
                a(href := "/tournament/new", cls := "btn btn-sm btn-outline-warning fw-bold", "+ Nuevo Torneo")
              )
            )
          ),

          bracketContent
        )
      )
    )
    renderHtml(content)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE H — SHAREABLE MATCH CARD (540px, pensada para captura de pantalla)
  // Solo datos publicos: nada de rubrica, analisis IA ni datos medicos.
  // ─────────────────────────────────────────────────────────────────────────────
  @cask.get("/match-center/:matchId/card")
  def matchShareCard(request: cask.Request, matchId: Int) = withAuth(request) {
    DatabaseManager.getMatchCardData(matchId) match {
      case None => renderRedirect("/history")
      case Some(m) =>
        val card = DatabaseManager.getLatestCardData()
        val gf = m("golesFavor").asInstanceOf[Option[Int]]
        val gc = m("golesContra").asInstanceOf[Option[Int]]
        val nota = m("nota").asInstanceOf[Double]
        val resultado = (gf, gc) match { case (Some(f), Some(c)) => s"$f - $c"; case _ => "—" }
        val (resColor, resLabel) = (gf, gc) match {
          case (Some(f), Some(c)) if f > c => ("#20c997", "VICTORIA")
          case (Some(f), Some(c)) if f == c => ("#facc15", "EMPATE")
          case (Some(_), Some(_)) => ("#ef4444", "DERROTA")
          case _ => ("#94a3b8", "")
        }
        val porteriaCero = gc.contains(0)
        val fecha = scala.util.Try(java.time.LocalDate.parse(m("fecha").asInstanceOf[String]))
          .map(_.format(java.time.format.DateTimeFormatter.ofPattern("d MMM yyyy", new java.util.Locale("es", "ES")))).getOrElse("")
        val competicion = if (m("torneoNombre").asInstanceOf[String].nonEmpty) m("torneoNombre").asInstanceOf[String] else m("tipoPartido").asInstanceOf[String]
        val notaColor = if (nota >= 7) "#20c997" else if (nota >= 5) "#facc15" else "#ef4444"

        val miniCarta = div(style := "width:120px; height:170px; background:linear-gradient(160deg,#f5d77a,#d4af37 55%,#a8841f); border-radius:14px; color:#2f2f2f; position:relative; box-shadow:0 8px 20px rgba(0,0,0,.5); overflow:hidden; flex-shrink:0;",
          div(style := "position:absolute; top:8px; left:10px; line-height:1; text-align:center;",
            div(style := "font-size:30px; font-weight:700;", card.media.toString),
            div(style := "font-size:12px; font-weight:700;", card.posicion)),
          if (card.fotoUrl.nonEmpty) img(src := card.fotoUrl, style := "position:absolute; right:4px; top:10px; width:80px; height:95px; object-fit:cover; border-radius:8px;") else frag(),
          div(style := "position:absolute; bottom:28px; width:100%; text-align:center; font-size:15px; font-weight:700; letter-spacing:1px;", card.nombre),
          div(style := "position:absolute; bottom:8px; width:100%; display:flex; justify-content:space-around; font-size:9px; font-weight:700;",
            span(s"DIV ${card.div}"), span(s"REF ${card.ref}"), span(s"POS ${card.pos}")))

        val pagina = doctype("html")(html(
          head(
            meta(charset := "utf-8"),
            meta(name := "viewport", content := "width=device-width, initial-scale=1"),
            link(rel := "stylesheet", href := "https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;700&display=swap"),
            tags2.title(s"Héctor vs ${m("rival")}")
          ),
          body(style := "margin:0; background:#0b0f19; font-family:'Oswald',sans-serif; display:flex; justify-content:center; padding:20px 0;",
            div(style := "width:540px; max-width:100vw; box-sizing:border-box; background:radial-gradient(circle at 20% 0%, #1e293b 0%, #0f172a 55%, #020617 100%); border:1px solid #d4af37; border-radius:22px; padding:26px; color:#fff; text-transform:uppercase;",
              div(style := "display:flex; justify-content:space-between; align-items:center; font-size:12px; letter-spacing:2px; color:#d4af37;",
                span("🛡️ GUARDIAN ELITE"), span(style := "color:#94a3b8;", m("temporada").asInstanceOf[String])),
              div(style := "display:flex; gap:22px; align-items:center; margin-top:22px;",
                miniCarta,
                div(style := "flex:1; min-width:0;",
                  div(style := "font-size:12px; color:#94a3b8; letter-spacing:2px;", "VS"),
                  div(style := "font-size:28px; font-weight:700; line-height:1.1; word-wrap:break-word;", m("rival").asInstanceOf[String]),
                  div(style := s"font-size:46px; font-weight:700; color:$resColor; line-height:1.1; margin-top:6px;", resultado),
                  if (resLabel.nonEmpty) div(style := s"font-size:12px; letter-spacing:3px; color:$resColor;", resLabel) else frag())),
              div(style := "display:flex; gap:12px; margin-top:22px;",
                div(style := "flex:1; background:rgba(255,255,255,.05); border-radius:12px; padding:12px; text-align:center;",
                  div(style := "font-size:11px; color:#94a3b8; letter-spacing:2px;", "NOTA"),
                  div(style := s"font-size:34px; font-weight:700; color:$notaColor;", f"$nota%.1f")),
                if (porteriaCero) div(style := "flex:1.4; background:rgba(32,201,151,.12); border:1px solid #20c997; border-radius:12px; padding:12px; display:flex; align-items:center; justify-content:center; font-size:18px; font-weight:700; color:#20c997; letter-spacing:1px;",
                  "🧤 PORTERÍA A CERO") else frag()),
              div(style := "display:flex; justify-content:space-between; margin-top:20px; font-size:12px; color:#94a3b8; letter-spacing:1px;",
                span(fecha), span(competicion))
            )
          )
        ))
        cask.Response(pagina.render.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
    }
  }

  initialize()
}
