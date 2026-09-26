import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object BioController extends cask.Routes {

  def medicalSection(reports: List[MedicalReport]) = {
    div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "🏥 MEDICAL VAULT & PASAPORTE BIOLOGICO"),
      div(cls := "card-body p-3",
        // Formulario de Subida
        form(action := "/bio/medical/upload", method := "post", enctype := "multipart/form-data",
          div(cls:="row g-2 mb-3",
            div(cls:="col-7",
              label(cls:="xx-small text-muted text-uppercase", "Tipo de Informe"),
              select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
                option(value:="Pediatria", "Pediatria (Crecimiento)"),
                option(value:="Analitica", "Analitica de Sangre"),
                option(value:="Traumatologia", "Traumatologia / Fisio"),
                option(value:="Otros", "Otros")
              )
            ),
            div(cls:="col-5",
              label(cls:="xx-small text-muted text-uppercase", "Fecha"),
              input(tpe:="date", name:="fecha", cls:="form-control form-control-sm bg-dark text-white border-secondary", required:=true)
            )
          ),
          div(cls:="mb-3",
            label(cls:="xx-small text-muted text-uppercase", "Archivo (PDF/Imagen)"),
            input(tpe:="file", name:="archivo", cls:="form-control form-control-sm bg-dark text-white", required:=true)
          ),
          div(cls:="form-check form-switch mb-3",
            input(cls:="form-check-input", tpe:="checkbox", name:="esPrevio", id:="checkPrevio"),
            label(cls:="form-check-label small text-muted", `for`:="checkPrevio", "Informe previo al inicio en futbol")
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-danger fw-bold", "Subir y Analizar con IA"))
        ),
        hr(cls:="border-secondary"),
        // Lista de Informes Procesados
        div(cls:="medical-history",
          if(reports.isEmpty) p(cls:="text-center text-muted small", "No hay registros medicos aun.")
          else for(r <- reports) yield div(cls:="border-start border-danger border-3 ps-2 mb-3",
            div(cls:="d-flex justify-content-between",
              span(cls:="fw-bold small text-danger", r.tipo),
              span(cls:="xx-small text-muted", r.fecha)
            ),
            div(cls:="xx-small text-light fst-italic", strong("Diagnostico: "), r.diagnostico),
            div(cls:="xx-small text-info", strong("Rec. IA: "), r.recomendaciones)
          )
        )
      )
    )
  }

  // --- 5. BIO & EVALUACION (CORREGIDO MODO OSCURO) ---

  @cask.get("/bio")
  def bioPage(request: cask.Request) = withAuth(request) {
    val activeDrills = DatabaseManager.getActiveDrills()
    val growthData = DatabaseManager.getGrowthHistory()
    val techChart = DatabaseManager.getTechEvolutionChart()
    val rpg = DatabaseManager.getRPGStatus()
    val cognitiveInsight = DatabaseManager.getCognitiveInsight()
    val medicalReports = DatabaseManager.getMedicalReports()
    val documentVaultDocs = DatabaseManager.getDocumentVaultList()
    val academiaSessions = DatabaseManager.getAcademiaSessions().take(5)
    val tipoSesionHoy = DatabaseManager.getTipoSesionHoy() // BLOQUE B3: pre-relleno segun estructura semanal

    // --- BLOQUE J: RATIO DE ENTRENAMIENTO ESPECIFICO (solo lectura/calculo, sin Gemini) ---
    val ratioEntreno = DatabaseManager.getRatioEntrenamientoEspecifico()
    val ratioEntrenoWidget: Modifier = {
      val total = ratioEntreno("total").asInstanceOf[Int]
      if (total == 0) div()
      else {
        val ratioEspecifico = ratioEntreno("ratioEspecifico").asInstanceOf[Double]
        val edad = ratioEntreno("edad").asInstanceOf[Int]
        val recMin = ratioEntreno("recMin").asInstanceOf[Double]
        val recMax = ratioEntreno("recMax").asInstanceOf[Double]
        val porDebajo = ratioEntreno("porDebajo").asInstanceOf[Boolean]
        div(cls := "card bg-dark text-white border-secondary shadow mb-3",
          div(cls := "card-header text-secondary fw-bold text-center small", "⚖️ RATIO DE ENTRENAMIENTO ESPECÍFICO"),
          div(cls := "card-body p-2",
            canvas(id := "ratioEntrenoChart", style := "max-height:180px;"),
            div(cls := "small text-white text-center mt-2", f"Esta temporada: $ratioEspecifico%.0f%% específico de portero · Recomendado para $edad años: ${recMin.toInt}-${recMax.toInt}%%"),
            if (porDebajo) div(cls := "xx-small text-warning text-center mt-1", "⚠️ El ratio de entrenamiento específico está por debajo del recomendado para su edad. Considera hablar con el entrenador de academia sobre añadir sesiones extra.")
            else div(cls := "xx-small text-success text-center mt-1", "✅ Buen ratio de entrenamiento específico para su edad.")
          )
        )
      }
    }

    // --- BLOQUE P: TRANSFERENCIA DE ENTRENAMIENTO (solo lectura/calculo, sin Gemini) ---
    val transferencia = DatabaseManager.getTransferenciaEntrenamiento()
    val transferenciaTotal = transferencia.map(_("n").asInstanceOf[Int]).sum
    val transferenciaWidget: Modifier =
      if (transferenciaTotal < 5) div()
      else {
        val mejorasTotal = transferencia.map(_("mejoras").asInstanceOf[Int]).sum
        val pctGlobal = if (transferenciaTotal > 0) mejorasTotal * 100.0 / transferenciaTotal else 0.0
        div(cls := "card bg-dark text-white border-info shadow mb-3",
          div(cls := "card-header text-info fw-bold text-center small", "🔁 TRANSFERENCIA DE ENTRENAMIENTO"),
          div(cls := "card-body p-3",
            div(cls := "small text-white text-center mb-2", f"El $pctGlobal%.0f%% de lo que trabaja en academia se ve mejorado en el siguiente partido"),
            transferencia.filter(_("n").asInstanceOf[Int] > 0).map { d =>
              val dim = d("dimension").asInstanceOf[String]
              val pct = d("pctTransferencia").asInstanceOf[Double]
              val n = d("n").asInstanceOf[Int]
              div(cls := "mb-2",
                div(cls := "d-flex justify-content-between xx-small", span(s"$dim ($n)"), span(f"$pct%.0f%%")),
                div(cls := "progress", style := "height:8px;", div(cls := s"progress-bar ${if (pct < 30) "bg-danger" else "bg-info"}", style := f"width:$pct%.0f%%;")),
                if (pct < 30) div(cls := "xx-small text-danger mt-1", s"⚠️ Los trabajos de $dim en academia no se están transfiriendo al partido — puede necesitar más repeticiones o un enfoque diferente")
                else div()
              )
            }
          )
        )
      }

    // --- BLOQUE D: DETECTOR DE JETLAG SOCIAL (solo lectura/calculo, sin Gemini) ---
    val jetlagWidget: Modifier = DatabaseManager.detectarJetlagSocial() match {
      case Some(msg) => div(cls := "card bg-dark border-secondary shadow-sm mb-3 p-2",
        div(cls := "xx-small text-white", msg))
      case None => div()
    }

    // --- BLOQUE A4: VALIDACION DEL INDICE DE FORMA (solo lectura/calculo, sin Gemini) ---
    val formaCorrelacion = DatabaseManager.getFormaCorrelacion()
    val formaSuficiente = formaCorrelacion("suficiente").asInstanceOf[Boolean]
    val formaValidacionWidget: Modifier =
      if (!formaSuficiente)
        div(cls := "card bg-dark border-secondary shadow mt-3",
          div(cls := "card-header text-white fw-bold text-center small", "📊 VALIDACIÓN DEL ÍNDICE DE FORMA"),
          div(cls := "card-body text-center text-muted small p-3", "Necesitas al menos 10 partidos con datos de forma para ver la validación.")
        )
      else {
        val rVal: Double = formaCorrelacion("r").asInstanceOf[Double]
        val notaAlta: Double = formaCorrelacion("notaMediaAlta").asInstanceOf[Double]
        val notaBaja: Double = formaCorrelacion("notaMediaBaja").asInstanceOf[Double]
        val nVal: Int = formaCorrelacion("n").asInstanceOf[Int]
        val corrTxt: String = f"El modelo tiene una correlación de r=$rVal%.2f con el rendimiento real de Héctor."
        val n15Modifier: Modifier =
          if (nVal >= 15) div(cls := "xx-small text-info mt-2", "Con más de 15 partidos, el índice ya predice con precisión estadística el rendimiento de Héctor.")
          else div()
        div(cls := "card bg-dark border-secondary shadow mt-3",
          div(cls := "card-header text-white fw-bold text-center small", "📊 VALIDACIÓN DEL ÍNDICE DE FORMA"),
          div(cls := "card-body p-3",
            canvas(id := "formaScatterChart", style := "max-height:220px;"),
            div(cls := "small text-white mt-3", corrTxt),
            div(cls := "row text-center mt-2",
              div(cls := "col-6",
                div(cls := "xx-small text-muted", "Nota media con FORMA ≥8"),
                div(cls := "fw-bold text-success", f"$notaAlta%.1f")
              ),
              div(cls := "col-6",
                div(cls := "xx-small text-muted", "Nota media con FORMA <6"),
                div(cls := "fw-bold text-danger", f"$notaBaja%.1f")
              )
            ),
            n15Modifier
          )
        )
      }

    // --- BLOQUE B3: AUTOPERCEPCION VS INDICE DE FORMA (solo lectura/calculo, sin Gemini) ---
    val autopercepcionVsForma = DatabaseManager.getAutopercepcionVsForma()
    val autopercepcionWidget: Modifier =
      if (autopercepcionVsForma.size < 5) div()
      else {
        val mayoresDivergencias = autopercepcionVsForma.sortBy(m => -math.abs(m("divergencia").asInstanceOf[Double])).take(3)
        val mediaDivergencia = autopercepcionVsForma.map(_("divergencia").asInstanceOf[Double]).sum / autopercepcionVsForma.size
        val patron =
          if (mediaDivergencia > 0.5) "Héctor tiende a SOBRESTIMAR cómo se va a encontrar — suele decir que está mejor de lo que luego indican sus datos de sueño, energía y carga."
          else if (mediaDivergencia < -0.5) "Héctor tiende a SUBESTIMAR cómo se va a encontrar — suele decir que está peor de lo que luego indican sus datos de sueño, energía y carga."
          else "No hay un patrón claro de sobre o subestimación — su autopercepción suele coincidir con el Índice de Forma."
        div(cls := "card bg-dark border-primary shadow mt-3",
          div(cls := "card-header text-white fw-bold text-center small", "🎯 AUTOPERCEPCIÓN VS ÍNDICE DE FORMA"),
          div(cls := "card-body p-3",
            div(cls := "small text-white mb-2", patron),
            div(cls := "xx-small text-muted fw-bold mb-1", "Mayores divergencias:"),
            mayoresDivergencias.map { m =>
              div(cls := "d-flex justify-content-between xx-small border-bottom border-secondary py-1",
                span(cls := "text-muted", s"${m("fecha")} vs ${m("rival")}"),
                span(cls := "text-light", f"Dijo ${m("autopercepcion").asInstanceOf[Int]}/5 · Forma ${m("formaEn5").asInstanceOf[Double]}%.1f/5 · Nota ${m("nota").asInstanceOf[Double]}%.1f")
              )
            }
          )
        )
      }

    val formaChartJs: String =
      if (!formaSuficiente) ""
      else {
        val puntos = formaCorrelacion("puntos").asInstanceOf[List[Map[String, Double]]]
        val pendiente = formaCorrelacion("pendiente").asInstanceOf[Double]
        val intercepto = formaCorrelacion("intercepto").asInstanceOf[Double]
        val puntosJs = puntos.map(p => s"""{x:${p("x")},y:${p("y")}}""").mkString("[", ",", "]")
        val xMin = puntos.map(_("x")).min
        val xMax = puntos.map(_("x")).max
        val lineaJs = s"""[{x:$xMin, y:${pendiente * xMin + intercepto}},{x:$xMax, y:${pendiente * xMax + intercepto}}]"""
        s"""
        var ctxForma = document.getElementById('formaScatterChart');
        if (ctxForma) {
          new Chart(ctxForma, {
            type: 'scatter',
            data: { datasets: [
              { label: 'Partidos', data: $puntosJs, backgroundColor: '#d4af37' },
              { label: 'Tendencia', data: $lineaJs, type: 'line', borderColor: '#0dcaf0', borderWidth:2, pointRadius:0, fill:false }
            ]},
            options: { responsive:true, plugins:{ legend:{ labels:{ color:'#fff' } } },
              scales: { x: { title:{display:true,text:'Índice de forma',color:'#aaa'}, ticks:{color:'#aaa'}, grid:{color:'#333'} },
                        y: { title:{display:true,text:'Nota partido',color:'#aaa'}, min:0, max:10, ticks:{color:'#aaa'}, grid:{color:'#333'} } }
            }
          });
        }
        """
      }
    // --- WIDGET 1: ANALISIS COGNITIVO ---
    val cognitiveWidget = div(cls:="card bg-dark border-info shadow mb-3",
      div(cls:="card-header border-info text-info fw-bold py-1 text-center small", "🧠 ANALISTA COGNITIVO"),
      div(cls:="card-body p-2",
        p(cls:="text-light small mb-0 text-center fw-bold", raw(cognitiveInsight))
      )
    )

    // --- FASE 2: PANEL ACCESOS RAPIDOS ---
    val fase2Panel = div(cls := "card bg-dark border-warning shadow mb-3",
      div(cls := "card-header text-warning fw-bold small text-center", "ANALITICA AVANZADA"),
      div(cls := "card-body p-2",
        div(cls := "row g-2",
          div(cls := "col-4",
            a(href := "/bio/carga", cls := "btn btn-outline-warning w-100 fw-bold d-flex flex-column align-items-center py-2",
              div(style := "font-size:22px;", "📉"),
              div(cls := "xx-small mt-1", "CARGA RPE")
            )
          ),
          div(cls := "col-4",
            a(href := "/bio/sueno", cls := "btn btn-outline-info w-100 fw-bold d-flex flex-column align-items-center py-2",
              div(style := "font-size:22px;", "🌙"),
              div(cls := "xx-small mt-1", "SUENO")
            )
          ),
          div(cls := "col-4",
            a(href := "/bio/fatiga", cls := "btn btn-outline-danger w-100 fw-bold d-flex flex-column align-items-center py-2",
              div(style := "font-size:22px;", "🧠"),
              div(cls := "xx-small mt-1", "FATIGA")
            )
          )
        )
      )
    )

    // --- FASE 4: MEDICAL VAULT SECTION ---
    val medicalVault = div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "🏥 MEDICAL VAULT & PASAPORTE BIOLOGICO"),
      div(cls := "card-body p-3",
        form(action := "/bio/medical/upload", method := "post", enctype := "multipart/form-data",
          div(cls:="row g-2 mb-3",
            div(cls:="col-7",
              label(cls:="xx-small text-muted text-uppercase", "Tipo de Informe"),
              select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
                option(value:="Pediatria", "Pediatria"), option(value:="Analitica", "Analitica"), option(value:="Traumatologia", "Traumatologia"), option(value:="Otros", "Otros")
              )
            ),
            div(cls:="col-5",
              label(cls:="xx-small text-muted text-uppercase", "Fecha"),
              input(tpe:="date", name:="fecha", cls:="form-control form-control-sm bg-dark text-white border-secondary", required:=true)
            )
          ),
          div(cls:="mb-3",
            label(cls:="xx-small text-muted text-uppercase", "Archivo (PDF/Imagen)"),
            input(tpe:="file", name:="archivo", cls:="form-control form-control-sm bg-dark text-white", required:=true)
          ),
          div(cls:="form-check form-switch mb-3",
            input(cls:="form-check-input", tpe:="checkbox", name:="esPrevio", id:="checkPrevio"),
            label(cls:="form-check-label small text-muted", `for`:="checkPrevio", "Informe previo al futbol")
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-danger fw-bold", "SUBIR Y ANALIZAR"))
        ),
        hr(cls:="border-secondary"),
        div(cls:="medical-history", style:="max-height: 150px; overflow-y: auto;",
          if(medicalReports.isEmpty) p(cls:="text-center text-muted small", "Sin registros medicos.")
          else for(r <- medicalReports) yield div(cls:="border-start border-danger border-2 ps-2 mb-2",
            div(cls:="d-flex justify-content-between xx-small", span(cls:="fw-bold text-danger", r.tipo), span(cls:="text-muted", r.fecha)),
            div(cls:="xx-small text-light", r.diagnostico)
          )
        )
      )
    )

    // ─────────────────────────────────────────────────────────────────────────
    // BLOQUE B — CONTRACT & LICENSE VAULT
    // ─────────────────────────────────────────────────────────────────────────
    def docTipoLabel(t: String): String = t match {
      case "LICENCIA_FEDERATIVA" => "Licencia federativa"
      case "SEGURO_DEPORTIVO"    => "Seguro deportivo"
      case "OFERTA_PRUEBA"       => "Oferta / Prueba"
      case "CONTRATO_ACADEMIA"   => "Contrato academia"
      case "DERECHO_IMAGEN"      => "Derecho de imagen"
      case _                     => "Otro"
    }
    val documentVault = div(cls := "card bg-dark text-white border-info shadow mb-3",
      div(cls := "card-header bg-info text-dark fw-bold text-center small", "📋 VAULT DE DOCUMENTOS"),
      div(cls := "card-body p-3",
        form(action := "/bio/document-vault/upload", method := "post", enctype := "multipart/form-data",
          div(cls:="row g-2 mb-2",
            div(cls:="col-6",
              label(cls:="xx-small text-muted text-uppercase", "Tipo"),
              select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
                option(value:="LICENCIA_FEDERATIVA", "Licencia federativa"),
                option(value:="SEGURO_DEPORTIVO", "Seguro deportivo"),
                option(value:="OFERTA_PRUEBA", "Oferta / Prueba"),
                option(value:="CONTRATO_ACADEMIA", "Contrato academia"),
                option(value:="DERECHO_IMAGEN", "Derecho de imagen"),
                option(value:="OTRO", "Otro")
              )
            ),
            div(cls:="col-6",
              label(cls:="xx-small text-muted text-uppercase", "Fecha"),
              input(tpe:="date", name:="fecha", cls:="form-control form-control-sm bg-dark text-white border-secondary", required:=true)
            )
          ),
          div(cls:="mb-2",
            label(cls:="xx-small text-muted text-uppercase", "Nombre del documento"),
            input(tpe:="text", name:="nombre", cls:="form-control form-control-sm bg-dark text-white border-secondary", placeholder:="Ej: Licencia RFFM 2025-26", required:=true)
          ),
          div(cls:="mb-3",
            label(cls:="xx-small text-muted text-uppercase", "Archivo (PDF/Imagen)"),
            input(tpe:="file", name:="archivo", cls:="form-control form-control-sm bg-dark text-white", required:=true)
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-info fw-bold", "SUBIR Y ANALIZAR"))
        ),
        hr(cls:="border-secondary"),
        div(cls:="document-vault-list", style:="max-height: 200px; overflow-y: auto;",
          if (documentVaultDocs.isEmpty) p(cls:="text-center text-muted small", "Sin documentos aún.")
          else documentVaultDocs.map { doc =>
            val id = doc("id").asInstanceOf[Int]
            val tipo = doc("tipo").asInstanceOf[String]
            val nombre = doc("nombre").asInstanceOf[String]
            val fecha = doc("fecha").asInstanceOf[String]
            val notas = doc("notas").asInstanceOf[String]
            div(cls:="border-start border-info border-2 ps-2 mb-2",
              div(cls:="d-flex justify-content-between align-items-center xx-small",
                span(cls:="badge bg-info text-dark", docTipoLabel(tipo)),
                span(cls:="text-muted", fecha)
              ),
              div(cls:="d-flex justify-content-between align-items-center",
                span(cls:="small fw-bold text-white", nombre),
                a(href:=s"/bio/document-vault/download/$id", cls:="xx-small text-info", "⬇ Descargar")
              ),
              if (notas.nonEmpty) div(cls:="xx-small text-light fst-italic mt-1", notas) else div()
            )
          }
        )
      )
    )

    // --- WIDGET 2: FORMULARIO ACADEMICO ---
    val academicForm = div(cls := "card bg-dark text-white border-warning shadow mb-3",
      div(cls := "card-header bg-warning text-dark fw-bold text-center small", "📚 REGISTRO ACADEMICO"),
      div(cls := "card-body p-3",
        form(action := "/bio/save_academic", method := "post",
          div(cls:="mb-2",
            input(tpe:="text", name:="asignatura", cls:="form-control form-control-sm bg-dark text-white border-secondary", placeholder:="Asignatura", required:=true)
          ),
          div(cls:="row g-2 mb-2",
            div(cls:="col-6", input(tpe:="number", step:="0.1", name:="nota", cls:="form-control form-control-sm text-center bg-dark text-white border-warning fw-bold", placeholder:="Nota 0-10", required:=true)),
            div(cls:="col-6", select(name:="tipo", cls:="form-select form-select-sm bg-dark text-white border-secondary",
              option(value:="Examen", "Examen"), option(value:="Trabajo", "Trabajo"), option(value:="Trimestral", "Trimestral")
            ))
          ),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-sm btn-outline-warning fw-bold", "Guardar Nota"))
        )
      )
    )

    val drillList = if (activeDrills.nonEmpty) { val dItems = for(d <- activeDrills) yield div(cls:="mb-2", div(cls:="d-flex justify-content-between small", span(fixEncoding(d.nombre)), span(s"${d.actual}/${d.objetivo}")), div(cls:="progress", style:="height: 6px;", div(cls:="progress-bar bg-warning", style:=s"width:${(d.actual.toDouble/d.objetivo.toDouble*100).toInt}%"))); div(id:="drillsContainer", style:="display:none;", cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", h6(cls:="text-warning small fw-bold mb-2", "🎯 MISIONES ACTIVAS"), dItems) } else div(id:="drillsContainer", style:="display:none;", cls:="alert alert-dark p-2 small text-center", "Sin misiones activas.")

    val content = basePage("bio", div(cls := "row justify-content-center",
      div(cls := "col-md-6 mb-4",
        // LABORATORIO
        div(cls:="card bg-secondary bg-opacity-10 border-info shadow mb-4", div(cls:="card-header bg-dark text-info fw-bold text-center", "🔬 LABORATORIO DE DATOS"), div(cls:="card-body p-2 d-flex justify-content-around", a(href:="/gear", cls:="btn btn-outline-light flex-fill me-1", div(style:="font-size:20px", "⚽"), span(cls:="small", "Material")), a(href:="/digital-twin", cls:="btn btn-outline-info flex-fill me-1", div(style:="font-size:20px", "🔮"), span(cls:="small", "Twin 2035")), a(href:="/moneyball", cls:="btn btn-outline-warning flex-fill", div(style:="font-size:20px", "📊"), span(cls:="small", "Moneyball")))),
        // --- NUEVO: MODULO JUDO (Insertar aqui) ---
        div(cls:="card bg-dark border-warning shadow mb-4",
          div(cls:="card-header bg-warning text-dark fw-bold text-center", "🥋 ESTADO DOJO (JUDO)"),
          div(cls:="card-body p-3",
            div(cls:="d-flex align-items-center justify-content-between mb-3",
              div(
                div(cls:="small text-muted fw-bold", "Cinturon Actual"),
                h4(cls:="mb-0 text-white", rpg.cinturonJudo) // Asegurate de que 'rpg' este cargado arriba
              ),
              div(cls:="px-3 py-2 rounded border border-light",
                style:=s"background-color: ${rpg.cinturonJudo.toLowerCase match {
                  case "blanco" => "#fff"
                  case "amarillo" => "#ff0"
                  case "naranja" => "#f80"
                  case "verde" => "#080"
                  case _ => "#333"
                }}; color: black; font-weight: bold;", "GRADO")
            ),
            form(action:="/bio/update_belt", method:="post", cls:="d-flex gap-2",
              select(name:="belt", cls:="form-select form-select-sm bg-dark text-white border-warning fw-bold",
                option(value:="Blanco", "Blanco"),
                option(value:="Blanco-Amarillo", "Blanco-Amarillo"),
                option(value:="Amarillo", "Amarillo"),
                option(value:="Naranja", "Naranja")
              ),
              button(tpe:="submit", cls:="btn btn-sm btn-warning fw-bold", "ACTUALIZAR")
            )
          )
        ),
        fase2Panel,
        medicalVault,
        documentVault,
        academicForm, // Entrada de datos escolares


        // BLOQUE E3: FC de esta manana vs RPE registrado ayer
        frag(DatabaseManager.avisosRPEconFCHoy().map(a => div(cls := "alert alert-warning small p-2 mb-2", a)): _*),
        // WELLNESS
        div(cls := "card bg-dark text-white border-info shadow mb-3", div(cls := "card-header bg-info text-dark fw-bold text-center", "DIARIO DE CARGA Y SUENO"), div(cls := "card-body p-3", form(action := "/bio/save_wellness", method := "post", div(cls:="mb-3", label(cls:="small text-danger fw-bold", "Estado Fisico"), select(name:="estadoFisico", cls:="form-select bg-dark text-white border-secondary fw-bold", option(value:="DISPONIBLE", "✅ Disponible"), option(value:="MOLESTIAS", "⚠ Molestias"), option(value:="LESION", "X Lesionado"), option(value:="ENFERMO", "🤒 Enfermo"))), div(cls:="row mb-3 align-items-end", div(cls:="col-6 text-center", label(cls:="small fw-bold", "Calidad Sueno (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="sueno")), div(cls:="col-6", label(cls:="small text-warning fw-bold", "Horas Dormidas"), input(tpe:="number", step:="0.5", name:="horas", cls:="form-control text-center bg-dark text-white border-warning fw-bold", value:="9.0"))), div(cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", label(cls:="small text-muted fw-bold d-block mb-2", "📱 Datos del smartwatch (opcional)"), div(cls:="row g-2", div(cls:="col-4", label(cls:="xx-small text-muted", "Sueño profundo (min)"), input(tpe:="number", step:="1", min:="0", name:="suenoProfundoMin", cls:="form-control form-control-sm bg-dark text-white border-secondary")), div(cls:="col-4", label(cls:="xx-small text-muted", "Sueño ligero (min)"), input(tpe:="number", step:="1", min:="0", name:="suenoLigeroMin", cls:="form-control form-control-sm bg-dark text-white border-secondary")), div(cls:="col-4", label(cls:="xx-small text-muted", "Despierto (min)"), input(tpe:="number", step:="1", min:="0", name:="suenoDespiertoMin", cls:="form-control form-control-sm bg-dark text-white border-secondary")))), div(cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", label(cls:="small text-danger fw-bold d-block mb-1", "❤️ Frecuencia cardíaca en reposo (BPM, opcional)"), input(tpe:="number", step:="1", min:="0", name:="fcReposo", cls:="form-control form-control-sm bg-dark text-white border-danger"), div(cls:="xx-small text-muted mt-1", "Mídela por la mañana antes de que Héctor se levante, 30 segundos con el reloj puesto.")), div(cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", div(cls:="d-flex justify-content-between align-items-center", style:="cursor:pointer;", onclick:="toggleFcImportPanel()", label(cls:="text-info fw-bold small mb-0", style:="cursor:pointer;", "📱 IMPORTAR FC DESDE CAPTURA DE PANTALLA"), span(id:="fcImportChevron", cls:="text-info small", "▼")), div(id:="fcImportPanel", style:="display:none;", div(cls:="xx-small text-muted mt-2 mb-2", "Sube una captura de la app del reloj con el historial de mediciones. Guardian extraerá las fechas y los valores de BPM e importará solo los días que no estén ya registrados."), input(tpe:="file", id:="fcImportFile", accept:="image/png,image/jpeg,image/webp", cls:="form-control form-control-sm bg-dark text-white border-secondary mb-2"), div(cls:="d-grid", button(tpe:="button", cls:="btn btn-sm btn-info fw-bold", onclick:="importarFcReposo()", "🧠 Importar mediciones")), div(id:="fcImportSpinner", style:="display:none;", cls:="text-center text-info small mt-2", "Analizando imagen con IA..."), div(id:="fcImportResultado", cls:="mt-2"))), div(cls:="mb-3 border-top pt-2", label(cls:="small fw-bold", "Energia (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="energia")), div(cls:="mb-3", label(cls:="small text-info fw-bold", "Estado Animico (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="animo"), div(cls:="d-flex justify-content-between xx-small text-muted fw-bold", span("Crisis"), span("Top"))), escala0a3("somnolencia", "☀️ Somnolencia diurna", Seq("No", "Algo", "Bastante", "Mucho")), escala0a3("dolorMuscular", "💪 Dolor muscular / agujetas", Seq("Ninguno", "Leve", "Moderado", "Fuerte")), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Notas conducta"), input(tpe:="text", name:="notas_conducta", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="... ")), div(cls:="mb-3 row", div(cls:="col-6", select(name:="dolor", cls:="form-select fw-bold", option(value:="1","Nada"), option(value:="2","Molestia"), option(value:="3","Dolor"), option(value:="5","Lesion"))), div(cls:="col-6", input(tpe:="text", name:="zona", cls:="form-control fw-bold", placeholder:="Zona?"))), div(cls:="row mb-3 border-top pt-3", div(cls:="col-6", label(cls:="small text-info fw-bold", "Altura (cm)"), input(tpe:="number", name:="altura", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar")), div(cls:="col-6", label(cls:="small text-info fw-bold", "Peso (kg)"), input(tpe:="number", step:="0.1", name:="peso", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar"))), div(cls:="row mb-3", div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Talla sentado (cm)"), input(tpe:="number", step:="0.1", name:="tallaSentado", cls:="form-control form-control-sm bg-dark text-white border-secondary", placeholder:="Opcional (PHV)")), div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Long. pierna (cm)"), input(tpe:="number", step:="0.1", name:="longitudPierna", cls:="form-control form-control-sm bg-dark text-white border-secondary", placeholder:="Opcional (PHV)"))), div(cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", label(cls:="small text-muted fw-bold d-block mb-2", "📊 De la báscula inteligente y medidas (opcional)"), div(cls:="row g-2", div(cls:="col-6", label(cls:="xx-small text-muted", "Músculo (kg)"), input(tpe:="number", step:="0.1", min:="0", name:="kgMusculo", cls:="form-control form-control-sm bg-dark text-white border-secondary")), div(cls:="col-6", label(cls:="xx-small text-muted", "Masa ósea (kg)"), input(tpe:="number", step:="0.1", min:="0", name:="kgMasaOsea", cls:="form-control form-control-sm bg-dark text-white border-secondary")))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-info fw-bold", "Guardar Bio"))))),

        // NUEVO: EVALUACION TECNICA (LABELS BLANCOS FORZADOS)
        div(cls:="card bg-secondary bg-opacity-25 border-warning shadow", div(cls:="card-header bg-warning text-dark fw-bold text-center", "EVALUACION TECNICA (MENSUAL)"), div(cls:="card-body p-3",
          form(action:="/bio/save_eval", method:="post",
            div(cls:="row mb-2", div(cls:="col-6", label(cls:="small fw-bold text-white", "Blocaje Manos"), input(tpe:="number", name:="blocaje", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5")), div(cls:="col-6", label(cls:="small fw-bold text-white", "Juego Pies"), input(tpe:="number", name:="pies", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5"))),
            div(cls:="row mb-2", div(cls:="col-6", label(cls:="small fw-bold text-white", "Juego Aereo"), input(tpe:="number", name:="aereo", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5")), div(cls:="col-6", label(cls:="small fw-bold text-danger", "Valentia"), input(tpe:="number", name:="valentia", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5"))),
            div(cls:="row mb-3", div(cls:="col-6", label(cls:="small fw-bold text-white", "Concentracion"), input(tpe:="number", name:="concentracion", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5")), div(cls:="col-6", label(cls:="small fw-bold text-white", "Coordinacion"), input(tpe:="number", name:="coordinacion", cls:="form-control text-center fw-bold", min:="1", max:="10", value:="5"))),
            div(cls:="mb-3", textarea(name:="notas", cls:="form-control fw-bold", rows:="2", placeholder:="Observaciones del mes...")),
            button(tpe:="submit", cls:="btn btn-warning w-100 fw-bold", "Registrar Evolucion")
          )
        ))
      ),
      div(cls := "col-md-6",
        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header text-secondary fw-bold text-center small", "PROGRESO TECNICO"), div(cls := "card-body p-2", canvas(id:="techChart", style:="max-height:200px;"))),
        div(cls := "card bg-dark text-white border-secondary shadow mb-3", div(cls := "card-header text-secondary fw-bold text-center small", "CURVA DE CRECIMIENTO"), div(cls := "card-body p-2", canvas(id:="growthChart", style:="max-height:150px;"))),
        ratioEntrenoWidget,
        div(cls := "card bg-dark text-white border-success shadow mb-3", div(cls := "card-header bg-success text-dark fw-bold text-center", "REGISTRO ENTRENO"), div(cls := "card-body p-3", form(action := "/bio/save_training", method := "post", div(cls:="mb-3", label(cls:="small fw-bold", "Fecha de la sesión"), input(tpe:="date", name:="fecha", id:="trainingFecha", cls:="form-control bg-dark text-white border-secondary fw-bold", value:=java.time.LocalDate.now().toString, required:=true)), div(cls:="mb-3", label(cls:="small fw-bold", "Tipo"), select(name:="tipo", id:="trainingType", onchange:="toggleDrills()", cls:="form-select bg-dark text-white fw-bold",
              option(value:="Club", if(tipoSesionHoy.contains("Club")) selected:="selected" else frag(), "Club"),
              option(value:="Academia", if(tipoSesionHoy.contains("Academia")) selected:="selected" else frag(), "Academia"),
              option(value:="Judo", if(tipoSesionHoy.contains("Judo")) selected:="selected" else frag(), "🥋 Judo"))), drillList, div(cls:="mb-3", label(cls:="small fw-bold", "Foco / Actividad"), div(cls:="d-flex gap-2", input(tpe:="text", name:="foco", id:="drillFocus", cls:="form-control fw-bold", placeholder:="Ej: Tiros, Resistencia...", required:=true), button(tpe:="button", id:="aiBtn", cls:="btn btn-warning fw-bold", onclick:="generateAI()", style:="display:none;", "🤖 IA"))), div(id:="manualDesign", style:="display:none;", textarea(name:="rutina", id:="rutinaText", cls:="form-control mb-3 fw-bold", rows:="4", placeholder:="Detalle de la sesion...")), div(id:="feedbackEntrenadorBox", style:="display:none;", cls:="mb-3", label(cls:="small text-info fw-bold", "🎓 FEEDBACK DEL ENTRENADOR (opcional)"), textarea(name:="feedbackEntrenador", cls:="form-control form-control-sm bg-dark text-white border-info", rows:="3", placeholder:="Qué dijo el entrenador de academia sobre la sesión...")),
              div(id:="judoInfoBox", style:="display:none;", cls:="alert alert-warning small p-2 mb-3", "El judo se registra con carga estimada fija (RPE 5) para el cálculo del ACWR."),
              div(id:="rpeCalidadAtencionRow", cls:="row mb-3", div(cls:="col-4 text-center", label(cls:="small fw-bold", "RPE"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="rpe", value:="7", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Calidad"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="calidad", value:="8", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Atencion"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="atencion", value:="8", min:="1", max:="10"))),
          div(cls:="mb-3 p-3 border border-info rounded", style:="background:rgba(13,202,240,0.05);",
            div(cls:="d-flex justify-content-between align-items-center", style:="cursor:pointer;", onclick:="toggleFootbarTraining()",
              label(cls:="text-info fw-bold small mb-0", style:="cursor:pointer;", "🦵 DATOS FOOTBAR (opcional)"),
              span(id:="footbarTrainingChevron", cls:="text-info small", "▼")
            ),
            div(id:="footbarTrainingPanel", style:="display:none;",
              div(cls:="xx-small text-muted mt-2 mb-2", "Introduce los datos del sensor Footbar del entreno. Se guardan solo si rellenas la distancia."),
              div(cls:="row g-2",
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Distancia (km)"), input(tpe:="number", step:="0.01", min:="0", name:="fbDistancia", cls:="form-control form-control-sm bg-dark text-white border-info")),
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Alta intensidad (m)"), input(tpe:="number", step:="1", min:="0", name:="fbAltaIntensidad", cls:="form-control form-control-sm bg-dark text-white border-info")),
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Sprint max (km/h)"), input(tpe:="number", step:="0.1", min:="0", name:="fbSprintMax", cls:="form-control form-control-sm bg-dark text-white border-info")),
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "% Actividad"), input(tpe:="number", step:="0.1", min:="0", max:="100", name:="fbPctActividad", cls:="form-control form-control-sm bg-dark text-white border-info")),
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Tiempo activo (min)"), input(tpe:="number", step:="1", min:="0", name:="fbTiempoActivo", cls:="form-control form-control-sm bg-dark text-white border-info")),
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Aceleraciones"), input(tpe:="number", step:="1", min:="0", name:="fbAceleraciones", cls:="form-control form-control-sm bg-dark text-white border-info")),
                div(cls:="col-6", label(cls:="xx-small text-muted fw-bold", "Desaceleraciones"), input(tpe:="number", step:="1", min:="0", name:="fbDesaceleraciones", cls:="form-control form-control-sm bg-dark text-white border-info"))
              )
            )
          ),
          input(tpe:="hidden", name:="tipoAusencia", id:="hiddenTipoAusencia", value:=""),
          // BLOQUE E1: RPE percibido por Hector (opcional)
          div(cls := "mb-3",
            label(cls := "small fw-bold d-block mb-1", "😴 ¿Cómo llegó Héctor a casa? (pregúntale)"),
            div(cls := "btn-group w-100 flex-wrap", role := "group",
              frag(DatabaseManager.etiquetasRpeHector.zipWithIndex.map { case (et, i) =>
                frag(
                  input(tpe := "radio", cls := "btn-check", name := "rpeHector", id := s"rpeHector-${i + 1}", value := (i + 1).toString, autocomplete := "off"),
                  label(cls := "btn btn-sm btn-outline-success", `for` := s"rpeHector-${i + 1}", et))
              }: _*))),
          div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-success fw-bold", "Guardar Sesion")),
          div(cls:="text-center mt-2",
            button(tpe:="button", id:="btnNoAsistio", cls:="btn btn-sm btn-outline-danger fw-bold", onclick:="toggleAusenciaPanel()", "❌ No asistió a esta sesión")
          ),
          div(id:="ausenciaPanel", style:="display:none;", cls:="mt-2 p-2 border border-danger rounded bg-danger bg-opacity-10",
            label(cls:="xx-small text-muted fw-bold d-block mb-1", "Motivo de la ausencia"),
            select(id:="motivoAusencia", cls:="form-select form-select-sm bg-dark text-white border-danger mb-2",
              option(value:="ENFERMEDAD", "Enfermedad"),
              option(value:="FAMILIAR", "Plan familiar / vacaciones"),
              option(value:="DESCANSO", "Descanso planificado"),
              option(value:="OTRO", "Otro")
            ),
            div(cls:="d-grid", button(tpe:="button", cls:="btn btn-sm btn-danger fw-bold", onclick:="confirmarAusencia()", "Confirmar ausencia"))
          )
          )),
          div(cls:="card bg-secondary bg-opacity-10 border-secondary", div(cls:="card-body p-2", h6(cls:="text-muted small mb-2", "+ Anadir Mision Tecnica (10 Sesiones)"), form(action:="/bio/add_drill", method:="post", cls:="d-flex gap-2", input(tpe:="text", name:="nombre", cls:="form-control form-control-sm fw-bold", placeholder:="Ej: Control Orientado", required:=true), button(tpe:="submit", cls:="btn btn-sm btn-secondary fw-bold", "Crear")))),

          // --- MODULO 8: SESIONES DE ACADEMIA (con audio-diario) ---
          div(cls:="card bg-dark border-info shadow mt-3",
            div(cls:="card-header text-info fw-bold text-center small", "🥅 SESIONES DE ACADEMIA"),
            div(cls:="card-body p-2",
              if (academiaSessions.isEmpty)
                div(cls:="text-muted small text-center py-2", "Sin sesiones de academia registradas")
              else
                frag(academiaSessions.map { t =>
                  val audioIcon = if (t.analisisVozAcademia.nonEmpty) span(style:="color:#8b5cf6;", "🎙️") else span("🎤")
                  div(cls:="d-flex justify-content-between align-items-center py-1 border-bottom border-secondary",
                    div(cls:="small text-white", t.fecha.take(10), span(cls:="text-muted xx-small ms-2", s"RPE ${t.rpe}")),
                    a(href:=s"/audio-diary/academia/${t.id}", cls:="text-decoration-none", audioIcon)
                  )
                }: _*)
            )
          ),

          formaValidacionWidget,
          autopercepcionWidget,
          jetlagWidget,
          conConfianza("transferencia", transferenciaTotal, transferenciaTotal >= 5)(transferenciaWidget)
        )
      ), script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""
      const gCtx=document.getElementById('growthChart');const gData=$growthData;if(gCtx){new Chart(gCtx,{type:'line',data:{labels:gData.labels,datasets:[{label:'Altura (cm)',data:gData.data,borderColor:'#0dcaf0',borderWidth:3,tension:0.3,pointBackgroundColor:'#fff',pointRadius:4}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{y:{ticks:{color:'#eee',font:{weight:'bold'}},grid:{color:'#444'},pointLabels:{color:'#fff'}},x:{display:false}}}});}
      const tCtx=document.getElementById('techChart');const tData=$techChart;if(tCtx){new Chart(tCtx,{type:'line',data:tData,options:{responsive:true,maintainAspectRatio:false,scales:{y:{min:0,max:10,ticks:{color:'#eee'},grid:{color:'#444'}},x:{ticks:{color:'#eee'}}}}});}
      const ratioCtx=document.getElementById('ratioEntrenoChart');
      if(ratioCtx){
        new Chart(ratioCtx,{type:'doughnut',
          data:{labels:['Específico (Academia)','Colectivo (Club)','Complementario (Judo)'],
            datasets:[{data:[${ratioEntreno("especifico").asInstanceOf[Int]},${ratioEntreno("colectivo").asInstanceOf[Int]},${ratioEntreno("complementario").asInstanceOf[Int]}],
              backgroundColor:['#0dcaf0','#ffc107','#8b5cf6']}]},
          options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{position:'bottom',labels:{color:'#eee',font:{size:10}}}}}
        });
      }
      $formaChartJs
      function toggleDrills(){ var type=document.getElementById('trainingType').value; var container=document.getElementById('drillsContainer'); var manual=document.getElementById('manualDesign'); var aiBtn = document.getElementById('aiBtn'); var fbBox = document.getElementById('feedbackEntrenadorBox'); if(container){if(type.includes('Papa') && !type.includes('Jugador')) container.style.display='block'; else container.style.display='none';} if(manual){if(type.includes('Papa')) manual.style.display='block'; else manual.style.display='none';} if(aiBtn){if(type.includes('Papa')) aiBtn.style.display='block'; else aiBtn.style.display='none';} if(fbBox){fbBox.style.display = (type === 'Academia') ? 'block' : 'none';}
      var judoBox=document.getElementById('judoInfoBox'); var rpeRow=document.getElementById('rpeCalidadAtencionRow'); var esJudo=(type==='Judo'); if(judoBox) judoBox.style.display = esJudo ? 'block' : 'none'; if(rpeRow) rpeRow.style.display = esJudo ? 'none' : 'flex'; }
      function toggleFootbarTraining(){ var panel=document.getElementById('footbarTrainingPanel'); var chevron=document.getElementById('footbarTrainingChevron'); if(panel.style.display==='none'){panel.style.display='block'; chevron.textContent='▲';} else {panel.style.display='none'; chevron.textContent='▼';} }
      function toggleFcImportPanel(){ var p=document.getElementById('fcImportPanel'); var c=document.getElementById('fcImportChevron'); if(p.style.display==='none'){p.style.display='block'; c.textContent='▲';} else {p.style.display='none'; c.textContent='▼';} }
      function importarFcReposo(){
        var fileInput = document.getElementById('fcImportFile');
        var resultado = document.getElementById('fcImportResultado');
        var spinner = document.getElementById('fcImportSpinner');
        if(!fileInput.files || !fileInput.files[0]) { alert('Selecciona una imagen primero'); return; }
        var fd = new FormData();
        fd.append('imagen', fileInput.files[0]);
        resultado.innerHTML = '';
        spinner.style.display = 'block';
        fetch('/bio/fc-import', { method:'POST', body: fd })
          .then(function(r){ return r.json(); })
          .then(function(data){
            spinner.style.display = 'none';
            if (data.importados === 0 && data.yaExistian === 0) {
              resultado.innerHTML = '<div class="text-warning small">Gemini no pudo leer mediciones en la imagen. Prueba con una captura más nítida.</div>';
              return;
            }
            if (data.importados === 0 && data.yaExistian > 0) {
              resultado.innerHTML = '<div class="text-muted small">No se encontraron mediciones nuevas — todos los días ya estaban registrados.</div>';
              return;
            }
            var rows = (data.detalle || []).map(function(d){
              var icon = d.estado === 'importado' ? '✅' : '⏭️';
              var partes = d.fecha.split('-');
              var fechaFmt = partes.length === 3 ? (partes[2] + '/' + partes[1] + '/' + partes[0]) : d.fecha;
              var estadoTxt = d.estado === 'importado' ? 'importado' : 'ya existía';
              return '<div class="xx-small text-light py-1 border-bottom border-secondary">' + icon + ' ' + fechaFmt + ' — ' + d.bpm + ' BPM — ' + estadoTxt + '</div>';
            }).join('');
            resultado.innerHTML = '<div class="small text-success fw-bold mb-1">' + data.importados + ' importadas, ' + data.yaExistian + ' ya existían</div>' + rows;
          })
          .catch(function(){ spinner.style.display = 'none'; resultado.innerHTML = '<div class="text-danger small">Error al analizar la imagen.</div>'; });
      }
      // PROBLEMA 3: marcar sesion como "no asistio"
      function toggleAusenciaPanel(){ var p=document.getElementById('ausenciaPanel'); p.style.display = (p.style.display==='none') ? 'block' : 'none'; }
      function confirmarAusencia(){
        var motivo = document.getElementById('motivoAusencia').value;
        document.getElementById('hiddenTipoAusencia').value = motivo;
        document.getElementById('trainingType').closest('form').submit();
      }
      function generateAI(){ var focus = document.getElementById('drillFocus').value; var type = document.getElementById('trainingType').value; if(!focus) { alert('Pon un objetivo primero (ej: Velocidad)'); return; } document.getElementById('rutinaText').value = "Generando..."; fetch('/bio/ai_gen?focus='+encodeURIComponent(focus)+'&mode='+encodeURIComponent(type)).then(r=>r.text()).then(t => document.getElementById('rutinaText').value = t); }
      window.addEventListener('DOMContentLoaded', toggleDrills);
    """))))
    renderHtml(content)
  }

  @cask.postForm("/bio/save_academic")
  def saveAcademic(request: cask.Request, asignatura: String, nota: Double, tipo: String, comentarios: String = "") = withAuth(request) {
    DatabaseManager.saveAcademicNote(asignatura, nota, tipo, comentarios)
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  private def parseBody(request: cask.Request): Map[String, String] = {
    val body = new String(request.data.readAllBytes(), "UTF-8")
    body.split("&").filter(_.nonEmpty).map { p =>
      val kv = p.split("=", 2)
      java.net.URLDecoder.decode(kv(0), "UTF-8") -> (if (kv.length > 1) java.net.URLDecoder.decode(kv(1), "UTF-8") else "")
    }.toMap
  }

  // BLOQUE F: 4 botones (0-3) para campos opcionales del diario de sueno — sin seleccion se guarda NULL
  private def escala0a3(campo: String, titulo: String, etiquetas: Seq[String]): Modifier =
    div(cls := "mb-3",
      label(cls := "small fw-bold d-block mb-1", titulo),
      div(cls := "btn-group w-100", role := "group",
        frag(etiquetas.zipWithIndex.map { case (et, i) =>
          frag(
            input(tpe := "radio", cls := "btn-check", name := campo, id := s"$campo-$i", value := i.toString, autocomplete := "off"),
            label(cls := "btn btn-sm btn-outline-info", `for` := s"$campo-$i", s"$i · $et"))
        }: _*)))

  @cask.post("/bio/save_wellness")
  def saveWellness(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    val sueno    = p.getOrElse("sueno", "3").toIntOption.getOrElse(3)
    val horas    = p.getOrElse("horas", "")
    val energia  = p.getOrElse("energia", "3").toIntOption.getOrElse(3)
    val dolor    = p.getOrElse("dolor", "1").toIntOption.getOrElse(1)
    val zona     = p.getOrElse("zona", "")
    val altura   = p.getOrElse("altura", "")
    val peso     = p.getOrElse("peso", "")
    val animo    = p.getOrElse("animo", "3").toIntOption.getOrElse(3)
    val notas_conducta = p.getOrElse("notas_conducta", "")
    val estadoFisico   = p.getOrElse("estadoFisico", "DISPONIBLE")
    val h   = horas.toDoubleOption.getOrElse(0.0)
    val alt = altura.toIntOption.getOrElse(0)
    val pes = peso.toDoubleOption.getOrElse(0.0)
    val suenoProfundoMin   = p.getOrElse("suenoProfundoMin", "").toIntOption
    val suenoLigeroMin     = p.getOrElse("suenoLigeroMin", "").toIntOption
    val suenoDespiertoMin  = p.getOrElse("suenoDespiertoMin", "").toIntOption
    val tallaSentado       = p.getOrElse("tallaSentado", "").toDoubleOption
    val longitudPierna     = p.getOrElse("longitudPierna", "").toDoubleOption
    val kgMusculo          = p.getOrElse("kgMusculo", "").toDoubleOption
    val kgMasaOsea         = p.getOrElse("kgMasaOsea", "").toDoubleOption
    val fcReposo           = p.getOrElse("fcReposo", "").toIntOption
    val somnolencia        = p.getOrElse("somnolencia", "").toIntOption.filter(v => v >= 0 && v <= 3)
    val dolorMuscular      = p.getOrElse("dolorMuscular", "").toIntOption.filter(v => v >= 0 && v <= 3)
    DatabaseManager.logWellness(sueno, h, energia, dolor, zona, alt, pes, animo, notas_conducta, estadoFisico,
      suenoProfundoMin, suenoLigeroMin, suenoDespiertoMin, tallaSentado, longitudPierna, kgMusculo, kgMasaOsea, fcReposo,
      somnolencia, dolorMuscular)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }
  @cask.post("/bio/save_training")
  def saveTraining(request: cask.Request) = withAuth(request) {
    val params = parseBody(request)
    val tipo   = params.getOrElse("tipo", "")
    val foco   = params.getOrElse("foco", "")
    // PROBLEMA 2: fecha editable — por defecto hoy, pero el padre puede registrar dias anteriores
    val fecha  = params.getOrElse("fecha", "").toString match {
      case f if f.nonEmpty => try { java.time.LocalDate.parse(f).toString } catch { case _: Exception => java.time.LocalDate.now().toString }
      case _ => java.time.LocalDate.now().toString
    }
    // PROBLEMA 3: si es un registro de ausencia, fuerza rpe/calidad/atencion a 0 sin importar el formulario
    val tipoAusencia = params.getOrElse("tipoAusencia", "").trim
    val esAusencia = tipoAusencia.nonEmpty
    // BLOQUE C2: el judo se registra con carga estimada fija — el padre no puede conocer RPE/calidad/atencion reales
    val esJudo = tipo.equalsIgnoreCase("Judo")
    val rpe    = if (esAusencia) 0 else if (esJudo) 5 else params.getOrElse("rpe", "7").toIntOption.getOrElse(7)
    val calidad = if (esAusencia) 0 else if (esJudo) 3 else params.getOrElse("calidad", "8").toIntOption.getOrElse(8)
    val att    = if (esAusencia) 0 else if (esJudo) 3 else params.getOrElse("atencion", "8").toIntOption.getOrElse(8)
    val rutina = params.getOrElse("rutina", "")
    val feedbackEntrenador = params.getOrElse("feedbackEntrenador", "")
    val fbDistancia        = if (esAusencia) None else params.getOrElse("fbDistancia", "").toDoubleOption
    val fbAltaIntensidad   = params.getOrElse("fbAltaIntensidad", "").toDoubleOption.map(_.toInt)
    val fbSprintMax        = params.getOrElse("fbSprintMax", "").toDoubleOption
    val fbPctActividad     = params.getOrElse("fbPctActividad", "").toDoubleOption.map(_.toInt)
    val fbTiempoActivo     = params.getOrElse("fbTiempoActivo", "").toIntOption
    val fbAceleraciones    = params.getOrElse("fbAceleraciones", "").toIntOption
    val fbDesaceleraciones = params.getOrElse("fbDesaceleraciones", "").toIntOption
    val rpeHector = if (esAusencia) None else params.getOrElse("rpeHector", "").toIntOption
    DatabaseManager.logTraining(tipo, foco, rpe, calidad, att, rutina, feedbackEntrenador,
      fbDistancia, fbAltaIntensidad, fbSprintMax, fbPctActividad, fbTiempoActivo, fbAceleraciones, fbDesaceleraciones,
      fecha, if (esAusencia) Some(tipoAusencia) else None, rpeHector = rpeHector)

    // BLOQUE I: detecta skills trabajadas en el feedback del entrenador de academia — sin Gemini
    if (tipo == "Academia" && feedbackEntrenador.nonEmpty) DatabaseManager.guardarSugerenciasSkillDesdeFeedback(feedbackEntrenador)

    // BLOQUE E: detecta hitos de carrera tras guardar el entreno — en background, nunca bloquea la respuesta
    new Thread(new Runnable {
      def run(): Unit = DatabaseManager.detectarHitos()
    }).start()
    val htmlStr = doctype("html")(html(
      head(meta(charset := "utf-8"), tags2.title("Entreno Guardado"), tags2.style(raw(getCss()))),
      body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';",
        h1(style := "color: #28a745; font-size: 60px; margin-bottom: 0;", "✅"),
        h2(style := "color: #d4af37; letter-spacing: 2px;", "SESION COMPLETADA"),
        div(style := "margin: 30px auto; width: 300px; background: #333; padding: 20px; border-radius: 10px; border: 1px solid #444;",
          h4(style := "color: #0dcaf0; margin-bottom: 5px;", tipo.toUpperCase),
          div(style := "font-style: italic; color: #ccc; margin-bottom: 15px;",
            if (foco.nonEmpty) foco else "Entrenamiento General"),
          div(style := "display: flex; justify-content: space-around; margin-top: 15px; border-top: 1px solid #555; padding-top: 10px;",
            div(div(style := "font-size:12px; color:#aaa;", "RPE"),     div(style := "font-weight:bold; font-size:20px;", rpe)),
            div(div(style := "font-size:12px; color:#aaa;", "CALIDAD"), div(style := "font-weight:bold; font-size:20px; color:#ffc107;", calidad)),
            div(div(style := "font-size:12px; color:#aaa;", "ATENCION"),div(style := "font-weight:bold; font-size:20px;", att))
          )
        ),
        p(style := "color: #999; font-size: 14px;", "Datos registrados en el historial."),
        div(style := "margin-top: 40px;",
          a(href := "/bio", cls := "btn btn-outline-light btn-lg", "Continuar")
        )
      )
    )).render
    cask.Response(htmlStr.getBytes("UTF-8"), headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
  }
  @cask.postForm("/bio/update_belt")
  def updateBelt(request: cask.Request, belt: String) = withAuth(request) {
    DatabaseManager.updateJudoBelt(belt)
    cask.Response(Array.emptyByteArray, statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  @cask.postForm("/bio/save_eval")
  def saveEval(request: cask.Request, blocaje: Int, pies: Int, aereo: Int, valentia: Int,
               concentracion: Int, coordinacion: Int, notas: String) = withAuth(request) {
    DatabaseManager.saveTechnicalReview(blocaje, pies, aereo, valentia, concentracion, coordinacion, notas)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }
  @cask.get("/bio/ai_gen")
  def aiGenDrill(request: cask.Request, focus: String, mode: String) = withAuth(request) {
    cask.Response(DatabaseManager.generateTrainingSession(mode, focus).getBytes("UTF-8"))
  }
  @cask.postForm("/bio/add_drill")
  def addDrill(request: cask.Request, nombre: String) = withAuth(request) {
    DatabaseManager.addNewDrill(fixEncoding(nombre), "")
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }


  @cask.postForm("/bio/medical/upload")
  def uploadMedical(request: cask.Request, fecha: String,
                    tipo: String,
                    esPrevio: String = "false",
                    archivo: cask.FormFile) = withAuth(request) {
    val isPrevio = esPrevio == "on"
    // cask 0.9.2: FormFile guarda el archivo en disco. toString = FormFile(name, /tmp/path, headers)
    // Los campos son: fileName (String) y path (java.nio.file.Path o String)
    val fileName: String = try {
      archivo.getClass.getDeclaredFields
        .find(f => f.getName == "fileName" || f.getName == "name")
        .map { f => f.setAccessible(true); f.get(archivo).asInstanceOf[String] }
        .getOrElse("documento.pdf")
    } catch { case _: Exception => "documento.pdf" }

    val fileBytes: Array[Byte] = try {
      // Buscar el campo path (puede ser Path o String)
      val pathField = archivo.getClass.getDeclaredFields
        .find(f => f.getName == "path" || f.getName == "filePath" || f.getName == "tmpFile")
      pathField match {
        case Some(f) =>
          f.setAccessible(true)
          val v = f.get(archivo)
          v match {
            case p: java.nio.file.Path => java.nio.file.Files.readAllBytes(p)
            case s: String             => java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(s))
            case _                     => Array.empty[Byte]
          }
        case None =>
          // Fallback: parsear el toString "FormFile(name, /tmp/path, ...)"
          val str = archivo.toString
          val parts = str.stripPrefix("FormFile(").split(",")
          if (parts.length >= 2) {
            val p = java.nio.file.Paths.get(parts(1).trim)
            if (java.nio.file.Files.exists(p)) java.nio.file.Files.readAllBytes(p)
            else Array.empty[Byte]
          } else Array.empty[Byte]
      }
    } catch { case _: Exception => Array.empty[Byte] }

    if (fileBytes.nonEmpty) {
      // 2. Proceso para Gemini
      val base64Content = java.util.Base64.getEncoder.encodeToString(fileBytes)
      val mimeType = if (fileName.toLowerCase.endsWith(".pdf")) "application/pdf" else "image/jpeg"

      val medicalPrompt = s"Analiza este informe ($tipo) de Hector. Extrae DIAGNOSTICO y RECOMENDACION DEPORTIVA. Formato: DIAGNOSTICO: 📝 | RECOMENDACION: 📝"

      val analisisIA = DatabaseManager.AIProvider.ask(medicalPrompt, Some((mimeType, base64Content)))
      val partes = analisisIA.split("\\|")
      val diag = partes.headOption.getOrElse("No detectado").replace("DIAGNOSTICO:", "").trim
      val rec = partes.lastOption.getOrElse("No detectado").replace("RECOMENDACION:", "").trim

      DatabaseManager.saveMedicalRecordFull(fecha, tipo, diag, rec, isPrevio)
    }

    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio"))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // BLOQUE B — CONTRACT & LICENSE VAULT: parseo manual de multipart/form-data
  // (mismo patron que /video/analyze-real en HistoryController.scala, pero via @cask.post)
  // ─────────────────────────────────────────────────────────────────────────
  private val B_CR: Byte = '\r'.toByte
  private val B_LF: Byte = '\n'.toByte
  private val B_DASH: Byte = '-'.toByte

  private def indexOfBytes(hay: Array[Byte], needle: Array[Byte], from: Int): Int = {
    val n = needle.length
    if (n == 0 || from < 0) return -1
    var i = from
    val limit = hay.length - n
    while (i <= limit) {
      var j = 0
      while (j < n && hay(i + j) == needle(j)) j += 1
      if (j == n) return i
      i += 1
    }
    -1
  }

  private case class MultipartField(filename: Option[String], contentType: Option[String], data: Array[Byte])

  private def parseMultipart(bodyBytes: Array[Byte], contentTypeHeader: String): Map[String, MultipartField] = {
    val marker = "boundary="
    val bIdx = if (contentTypeHeader == null) -1 else contentTypeHeader.indexOf(marker)
    if (bIdx < 0) return Map.empty
    var boundary = contentTypeHeader.substring(bIdx + marker.length).split(";").head.trim
    if (boundary.startsWith("\"") && boundary.endsWith("\"")) boundary = boundary.substring(1, boundary.length - 1)
    val delim = ("--" + boundary).getBytes("ISO-8859-1")
    val headerEnd = Array(B_CR, B_LF, B_CR, B_LF)

    var fields = Map[String, MultipartField]()
    var searchFrom = 0
    var continue = true
    while (continue) {
      val delimPos = indexOfBytes(bodyBytes, delim, searchFrom)
      if (delimPos < 0) { continue = false } else {
        var partStart = delimPos + delim.length
        val isFinal = partStart + 1 < bodyBytes.length && bodyBytes(partStart) == B_DASH && bodyBytes(partStart + 1) == B_DASH
        if (isFinal) { continue = false } else {
          if (partStart + 1 < bodyBytes.length && bodyBytes(partStart) == B_CR && bodyBytes(partStart + 1) == B_LF) partStart += 2
          val hEnd = indexOfBytes(bodyBytes, headerEnd, partStart)
          if (hEnd < 0) { continue = false } else {
            val headersStr = new String(bodyBytes, partStart, hEnd - partStart, "UTF-8")
            val dataStart = hEnd + headerEnd.length
            val nextDelimPos = indexOfBytes(bodyBytes, delim, dataStart)
            if (nextDelimPos < 0) { continue = false } else {
              var dataEnd = nextDelimPos
              if (dataEnd >= dataStart + 2 && bodyBytes(dataEnd - 2) == B_CR && bodyBytes(dataEnd - 1) == B_LF) dataEnd -= 2
              val nameOpt = """name="([^"]*)"""".r.findFirstMatchIn(headersStr).map(_.group(1))
              val fileOpt = """filename="([^"]*)"""".r.findFirstMatchIn(headersStr).map(_.group(1))
              val ctOpt = """(?i)Content-Type:\s*([^\r\n]+)""".r.findFirstMatchIn(headersStr).map(_.group(1).trim)
              nameOpt.foreach { name =>
                val data = java.util.Arrays.copyOfRange(bodyBytes, dataStart, math.max(dataStart, dataEnd))
                fields = fields + (name -> MultipartField(fileOpt, ctOpt, data))
              }
              searchFrom = nextDelimPos
            }
          }
        }
      }
    }
    fields
  }

  @cask.post("/bio/document-vault/upload")
  def uploadDocumentVault(request: cask.Request) = withAuth(request) {
    val contentType = request.exchange.getRequestHeaders.getFirst("Content-Type")
    val bodyBytes = request.data.readAllBytes()
    val fields = parseMultipart(bodyBytes, contentType)

    def fieldStr(name: String): String =
      fields.get(name).map(f => new String(f.data, "UTF-8")).getOrElse("")

    val tipo   = fieldStr("tipo")
    val nombre = fieldStr("nombre")
    val fecha  = fieldStr("fecha")
    val archivoField = fields.get("archivo").filter(_.data.nonEmpty)

    archivoField match {
      case Some(archivo) =>
        val base64Content = java.util.Base64.getEncoder.encodeToString(archivo.data)
        val fileName = archivo.filename.getOrElse("documento.pdf")
        val mimeType = if (fileName.toLowerCase.endsWith(".pdf")) "application/pdf" else "image/jpeg"

        // BLOQUE B: analisis con Gemini SOLO al pulsar "Subir" — nunca en render de pagina
        val docPrompt = s"Analiza este documento ($tipo) de Hector, un portero de futbol base. Extrae: tipo de contrato o documento, vigencia (fechas si las hay), entidad firmante, y cualquier clausula relevante para su carrera deportiva. Responde en texto plano, conciso, maximo 4 lineas."
        val analisisIA = try DatabaseManager.AIProvider.ask(docPrompt, Some((mimeType, base64Content))) catch { case _: Exception => "" }

        DatabaseManager.saveDocumentVault(tipo, nombre, fecha, base64Content, analisisIA)
      case None => ()
    }
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Importacion de FC en reposo por captura de pantalla del smartwatch
  // ─────────────────────────────────────────────────────────────────────────
  @cask.post("/bio/fc-import")
  def importFcReposo(request: cask.Request) = withAuth(request) {
    val contentType = request.exchange.getRequestHeaders.getFirst("Content-Type")
    val bodyBytes = request.data.readAllBytes()
    val fields = parseMultipart(bodyBytes, contentType)

    fields.get("imagen").filter(_.data.nonEmpty) match {
      case Some(imagen) =>
        val base64Content = java.util.Base64.getEncoder.encodeToString(imagen.data)
        val fileName = imagen.filename.getOrElse("captura.jpg").toLowerCase
        val mimeType =
          if (fileName.endsWith(".png")) "image/png"
          else if (fileName.endsWith(".webp")) "image/webp"
          else "image/jpeg"

        val resultado = DatabaseManager.importFcReposoFromImage(base64Content, mimeType)
        val importados = resultado("importados").asInstanceOf[Int]
        val yaExistian  = resultado("yaExistian").asInstanceOf[Int]
        val detalle     = resultado("detalle").asInstanceOf[List[Map[String, Any]]]

        val json = ujson.Obj(
          "importados" -> importados,
          "yaExistian" -> yaExistian,
          "detalle" -> ujson.Arr(detalle.map(d => ujson.Obj(
            "fecha" -> d("fecha").asInstanceOf[String],
            "bpm" -> d("bpm").asInstanceOf[Int],
            "estado" -> d("estado").asInstanceOf[String]
          ): ujson.Value): _*)
        )
        cask.Response(json.render().getBytes("UTF-8"), headers = Seq("Content-Type" -> "application/json"))
      case None =>
        val json = ujson.Obj("error" -> "No se recibio ninguna imagen")
        cask.Response(json.render().getBytes("UTF-8"), statusCode = 400, headers = Seq("Content-Type" -> "application/json"))
    }
  }

  @cask.get("/bio/document-vault/download/:id")
  def downloadDocumentVault(request: cask.Request, id: Int) = withAuth(request) {
    DatabaseManager.getDocumentVaultFile(id) match {
      case Some((nombre, archivoB64)) =>
        val bytes = java.util.Base64.getDecoder.decode(archivoB64)
        val safeNombre = nombre.replaceAll("[^a-zA-Z0-9._-]", "_")
        cask.Response(bytes, headers = Seq(
          "Content-Type" -> "application/octet-stream",
          "Content-Disposition" -> s"attachment; filename=$safeNombre"
        ))
      case None => cask.Response("Documento no encontrado".getBytes("UTF-8"), statusCode = 404)
    }
  }

  // --- 3. MODO LEGADO (RPG) ---

  // ── FASE 2: PAGINA GRAFICO DE CARGA ───────────────────────────────────────

  @cask.get("/bio/carga")
  def cargaPage(request: cask.Request) = withAuth(request) {
    val weekly   = DatabaseManager.getWeeklyLoad(12)
    val rpeHist  = DatabaseManager.getRPEHistory(60)
    // FIX 2: evita mostrar un ratio disparado (ej. 4.00) cuando el historico es insuficiente
    val acwrEstado = DatabaseManager.calcularACWRConEstado()
    val acwrInsuficiente = acwrEstado("status").asInstanceOf[String] == "INSUFICIENTE"
    val acwr     = acwrEstado("acwr").asInstanceOf[Double]
    // BLOQUE D: umbrales adaptados a la edad de Hector
    val (acwrColor, acwrLabel) =
      if (acwrInsuficiente) ("secondary", "ACUMULANDO DATOS")
      else { val (_, color, etiqueta) = DatabaseManager.nivelACWR(acwr); (color, etiqueta) }

    val semanasJs  = weekly.map(s => s""""${s._1}"""").mkString("[",",","]")
    val cargasJs   = weekly.map(_._2.toString).mkString("[",",","]")
    val sesionesJs = weekly.map(_._3.toString).mkString("[",",","]")

    val fechasRPEJs  = rpeHist.map(r => s""""${r._1}"""").mkString("[",",","]")
    val rpeJs        = rpeHist.map(_._2.toString).mkString("[",",","]")
    val calidadJs    = rpeHist.map(_._3.toString).mkString("[",",","]")
    val atencionJs   = rpeHist.map(_._4.toString).mkString("[",",","]")

    val avgRpe  = if (rpeHist.nonEmpty) f"${rpeHist.map(_._2).sum.toDouble / rpeHist.size}%.1f" else "—"
    val avgCal  = if (rpeHist.nonEmpty) f"${rpeHist.map(_._3).sum.toDouble / rpeHist.size}%.1f" else "—"
    val avgAtt  = if (rpeHist.nonEmpty) f"${rpeHist.map(_._4).sum.toDouble / rpeHist.size}%.1f" else "—"

    val content = basePage("bio",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h2(cls := "text-warning mb-0", "GRAFICO DE CARGA"),
            a(href := "/bio", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Bio")
          ),
          // ── PRINCIPAL: ACWR con semaforo, carga aguda vs cronica y proyeccion del sabado ──
          div(cls := s"card bg-dark border-$acwrColor shadow mb-2 p-3 text-center",
            div(cls := "xx-small text-muted fw-bold", "ACWR — RELACIÓN CARGA AGUDA / CRÓNICA"),
            div(cls := s"text-$acwrColor fw-bold", style := "font-size:52px; line-height:1.1;", if (acwrInsuficiente) "📊" else f"$acwr%.2f"),
            div(cls := s"badge bg-$acwrColor fs-6", acwrLabel),
            if (acwrInsuficiente) div(cls := "xx-small text-muted mt-1", "Acumulando datos (mín. 3 semanas)") else frag()),
          div(cls := "xx-small mb-3", style := "color:#64748b;", DatabaseManager.disclaimerACWR),
          {
            val aguda = acwrEstado("aguda").asInstanceOf[Double]; val cronica = acwrEstado("cronica").asInstanceOf[Double]
            val max = math.max(1.0, math.max(aguda, cronica))
            div(cls := "card bg-dark border-secondary p-3 mb-3",
              frag(Seq(("⚡ Carga aguda (7 días)", aguda, "#f59e0b"), ("🧱 Carga crónica (28 días)", cronica, "#0dcaf0")).map { case (et, v, c) =>
                div(cls := "mb-2",
                  div(cls := "d-flex justify-content-between xx-small", span(cls := "text-muted fw-bold", et), span(cls := "fw-bold", style := s"color:$c;", f"$v%.0f /día")),
                  div(cls := "progress", style := "height:10px; background:#334155;", div(cls := "progress-bar", style := s"width:${v / max * 100}%; background:$c;")))
              }: _*))
          },
          DatabaseManager.diasHastaPartidoSabado() match {
            case Some(dias) =>
              val pred = DatabaseManager.predecirFormaPartido(dias)
              if (!pred("disponible").asInstanceOf[Boolean]) frag()
              else div(cls := "card bg-dark border-secondary p-2 mb-3 small",
                f"📊 Forma proyectada para el sábado: ${pred("semaforo")} ${pred("indice").asInstanceOf[Double]}%.1f",
                pred("acwrProyectado").asInstanceOf[Option[Double]].map(a => span(cls := "text-muted", f" · ACWR previsto $a%.2f")).getOrElse(frag()))
            case None => frag()
          },

          // ── DETALLE DE CARGA (colapsable) ──
          seccion("📊 Detalle de carga")(
          div(cls := "row g-2 mb-4",
            div(cls := "col-3",
              div(cls := s"card bg-dark border-$acwrColor text-center py-3",
                div(cls := s"text-$acwrColor fw-bold", style := "font-size:28px;", if (acwrInsuficiente) "📊" else f"$acwr%.2f"),
                div(cls := "xx-small text-muted mt-1", if (acwrInsuficiente) "ACWR: Acumulando datos (mín. 3 semanas)" else "ACWR"),
                div(cls := s"badge bg-$acwrColor mt-1", acwrLabel)
              )
            ),
            Seq(("RPE Medio", avgRpe, "warning"), ("Calidad Media", avgCal, "success"), ("Atencion Media", avgAtt, "info")).map {
              case (lbl, v, c) =>
                div(cls := "col-3",
                  div(cls := s"card bg-dark border-$c text-center py-3",
                    div(cls := s"text-$c fw-bold fs-4", v),
                    div(cls := "xx-small text-muted mt-1", lbl)
                  )
                )
            }
          ),

          {
            val sesiones = DatabaseManager.getSesionesCarga(28)
            if (sesiones.isEmpty) sinDatos("Sesiones de los últimos 28 días")
            else div(cls := "card bg-dark border-secondary mb-3",
              div(cls := "card-header text-white fw-bold small", "CARGA POR SESIÓN — últimos 28 días"),
              div(cls := "table-responsive", style := "max-height:320px; overflow-y:auto;",
                table(cls := "table table-dark table-sm mb-0 xx-small",
                  thead(tr(th("Fecha"), th("Sesión"), th("Detalle"), th(cls := "text-end", "Carga"))),
                  tbody(frag(sesiones.map { case (f, t, d, c) => tr(td(f.drop(5)), td(t), td(d), td(cls := "text-end fw-bold", f"$c%.0f")) }: _*)))))
          },
          {
            val serie = DatabaseManager.getSerieACWR(60)
            if (serie.size < 2) sinDatos("Evolución del ACWR")
            else {
              val u = DatabaseManager.umbralesACWR()
              div(cls := "card bg-dark border-secondary mb-3",
                div(cls := "card-header text-white fw-bold small", "EVOLUCIÓN DEL ACWR — últimos 60 días"),
                div(cls := "card-body", div(style := "position:relative; height:200px;", tag("canvas")(id := "chartAcwrEvol"))),
                script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
                script(raw(s"""
                  (function(){
                    var f = ${serie.map(x => "\"" + x._1.drop(5) + "\"").mkString("[", ",", "]")};
                    var v = ${serie.map(x => f"${x._2}%.3f".replace(",", ".")).mkString("[", ",", "]")};
                    new Chart(document.getElementById('chartAcwrEvol'), { type: 'line',
                      data: { labels: f, datasets: [
                        { label: 'ACWR', data: v, borderColor: '#d4af37', borderWidth: 2, pointRadius: 0, tension: 0.2 },
                        { label: 'Riesgo (${u.riesgo})', data: f.map(function(){ return ${u.riesgo}; }), borderColor: '#ef4444', borderDash: [4,4], borderWidth: 1, pointRadius: 0 },
                        { label: 'Óptimo mín. (${u.optimoMin})', data: f.map(function(){ return ${u.optimoMin}; }), borderColor: '#20c997', borderDash: [4,4], borderWidth: 1, pointRadius: 0 } ] },
                      options: { responsive: true, maintainAspectRatio: false,
                        plugins: { legend: { labels: { color: '#ccc', font: { size: 10 } } } },
                        scales: { x: { ticks: { color: '#888', maxTicksLimit: 8 } }, y: { ticks: { color: '#aaa' } } } } });
                  })();
                """)))
            }
          },
          div(cls := "d-grid mb-3",
            a(href := "/career/acwr-proyeccion", cls := "btn btn-outline-warning btn-sm fw-bold", "📅 Proyección de carga de la próxima semana")),

          if (weekly.isEmpty && rpeHist.isEmpty) {
            div(cls := "alert alert-secondary text-center py-5",
              div(style := "font-size:40px; opacity:0.3;", "📉"),
              div(cls := "fw-bold mt-2", "Sin sesiones de entrenamiento registradas"),
              div(cls := "small text-muted mt-1", "Registra sesiones en Bio para ver tu carga")
            )
          } else div(
            // Carga semanal
            div(cls := "card bg-dark border-warning shadow mb-4",
              div(cls := "card-header text-warning fw-bold small d-flex justify-content-between",
                span("CARGA SEMANAL (RPE x Minutos)"),
                span(cls := "text-muted xx-small fw-bold", "Ultimas 12 semanas")
              ),
              div(cls := "card-body",
                div(style := "position:relative; height:220px;",
                  tag("canvas")(id := "chartCargaSemanal")
                )
              )
            ),
            // RPE + atencion por sesion
            div(cls := "card bg-dark border-secondary shadow mb-4",
              div(cls := "card-header text-white fw-bold small", "RPE / CALIDAD / ATENCION POR SESION"),
              div(cls := "card-body",
                div(style := "position:relative; height:220px;",
                  tag("canvas")(id := "chartRPE")
                )
              )
            ),
            script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
            script(raw(s"""
              const semanas  = $semanasJs;
              const cargas   = $cargasJs;
              const sesiones = $sesionesJs;
              const fechasRPE = $fechasRPEJs;
              const rpes     = $rpeJs;
              const calidad  = $calidadJs;
              const atencion = $atencionJs;

              new Chart(document.getElementById('chartCargaSemanal'), {
                type: 'bar',
                data: {
                  labels: semanas,
                  datasets: [
                    { label: 'Carga (RPE x min)', data: cargas, backgroundColor: 'rgba(212,175,55,0.7)', borderRadius: 4, yAxisID: 'y' },
                    { label: 'Sesiones', data: sesiones, type: 'line', borderColor: '#17a2b8', backgroundColor: 'rgba(23,162,184,0.15)', borderWidth: 2, pointRadius: 4, yAxisID: 'y1' }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                  scales: {
                    x: { ticks: { color: '#888', font: { size: 10 } }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { ticks: { color: '#d4af37' }, grid: { color: 'rgba(255,255,255,0.05)' }, title: { display: true, text: 'Carga', color: '#888' } },
                    y1: { position: 'right', ticks: { color: '#17a2b8' }, grid: { display: false }, title: { display: true, text: 'Sesiones', color: '#888' } }
                  }
                }
              });

              new Chart(document.getElementById('chartRPE'), {
                type: 'line',
                data: {
                  labels: fechasRPE,
                  datasets: [
                    { label: 'RPE', data: rpes, borderColor: '#dc3545', backgroundColor: 'rgba(220,53,69,0.1)', borderWidth: 2, pointRadius: 3, tension: 0.3 },
                    { label: 'Calidad', data: calidad, borderColor: '#28a745', backgroundColor: 'rgba(40,167,69,0.1)', borderWidth: 2, pointRadius: 3, tension: 0.3 },
                    { label: 'Atencion', data: atencion, borderColor: '#17a2b8', backgroundColor: 'rgba(23,162,184,0.1)', borderWidth: 2, pointRadius: 3, tension: 0.3 }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                  scales: {
                    x: { ticks: { color: '#888', maxTicksLimit: 10, font: { size: 10 } }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { min: 0, max: 10, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                  }
                }
              });
            """))
          )
          ),

          // ── ANALISIS AVANZADO (colapsable) ──
          seccion("🔬 Análisis avanzado")(
          // BLOQUE E4: percepcion del esfuerzo padre vs Hector (>=5 entrenos con ambos datos)
          {
            val div4 = DatabaseManager.getDivergenciaRPE(DatabaseManager.getTemporadaActivaId())
            if (!div4("suficiente").asInstanceOf[Boolean]) frag()
            else div(cls := s"card bg-dark shadow mb-3 p-3 border-${if (div4("divergente").asInstanceOf[Boolean]) "warning" else "secondary"}",
              div(cls := "fw-bold small text-white mb-1", "🔄 PERCEPCIÓN DEL ESFUERZO — Padre vs Héctor"),
              div(cls := "small", div4("mensaje").toString),
              div(cls := "xx-small text-muted mt-1", s"${div4("n")} entrenos con ambos datos. El RPE de Héctor (1-5) se compara ×2 en la escala 1-10."))
          },

          {
            val avisos = DatabaseManager.avisosRPEconFCRecientes(14)
            if (avisos.isEmpty) div(cls := "guardian-sin-datos", "❤️ Validación con FC: ninguna sesión de las últimas 2 semanas parece más intensa de lo registrado.")
            else div(cls := "card bg-dark border-warning p-2 mb-2",
              div(cls := "xx-small fw-bold text-warning mb-1", "❤️ VALIDACIÓN CRUZADA CON LA FC DE LA MAÑANA SIGUIENTE"),
              frag(avisos.map { case (sesion, msg) => div(cls := "xx-small mb-1", strong(sesion), " — ", msg) }: _*))
          },
          DatabaseManager.getDivergenciaRPE(DatabaseManager.getTemporadaActivaId())("suficiente") match {
            case true => frag()
            case _ => sinDatos("Percepción del esfuerzo padre vs Héctor", "Se necesitan 5 entrenos con RPE de Héctor")
          }
          )
        )
      )
    )
    renderHtml(content)
  }

  // ── FASE 2: CORRELACION SUENO-RENDIMIENTO ─────────────────────────────────
  // ── Correlaciones sueno-rendimiento (smartwatch) ─────────────────────────
  private def sleepFactorCard(titulo: String, niveles: List[Map[String, Any]]) = {
    if (niveles.isEmpty) {
      div(cls := "card bg-dark border-secondary p-2 h-100",
        div(cls := "xx-small fw-bold text-white mb-1 text-center", titulo),
        div(cls := "xx-small text-muted text-center py-3", "Sin datos suficientes")
      )
    } else {
      div(cls := "card bg-dark border-secondary p-2 h-100",
        div(cls := "xx-small fw-bold text-white mb-2 text-center", titulo),
        frag(niveles.map { n =>
          val nivel      = n("nivel").asInstanceOf[String]
          val notaMedia  = n.getOrElse("notaMedia", 0.0).asInstanceOf[Double]
          val partidos   = n("partidos").asInstanceOf[Int]
          val color      = if (partidos == 0) "#495057" else if (notaMedia > 7) "#20c997" else if (notaMedia >= 5) "#ffc107" else "#dc3545"
          val pct        = if (partidos == 0) 0 else math.min(100, (notaMedia / 10.0 * 100).toInt)
          val valorTxt: String = if (partidos > 0) f"$notaMedia%.1f ($partidos)" else "—"
          div(cls := "mb-2",
            div(cls := "d-flex justify-content-between xx-small",
              span(cls := "text-muted fw-bold", nivel),
              span(cls := "fw-bold", style := s"color:$color;", valorTxt)
            ),
            div(cls := "progress", style := "height:6px;",
              div(cls := "progress-bar", style := s"width:$pct%; background:$color;")
            )
          )
        }: _*)
      )
    }
  }

  @cask.get("/bio/sueno")
  def suenoPage(request: cask.Request) = withAuth(request) {
    val correlation = DatabaseManager.getSleepMatchCorrelation()
    val sleepHist   = DatabaseManager.getSleepHistory(60)

    val correlaciones            = DatabaseManager.getSleepCorrelations()
    val totalParesSmartwatch     = correlaciones("totalPares").asInstanceOf[Int]
    val suenoProfundoNiveles     = correlaciones("suenoProfundo").asInstanceOf[List[Map[String, Any]]]
    val horasNiveles             = correlaciones("horasTotales").asInstanceOf[List[Map[String, Any]]]
    val calidadNiveles           = correlaciones("calidad").asInstanceOf[List[Map[String, Any]]]
    val energiaNiveles           = correlaciones("energia").asInstanceOf[List[Map[String, Any]]]
    val animoNiveles             = correlaciones("animo").asInstanceOf[List[Map[String, Any]]]
    val combinacionOptima        = correlaciones("combinacionOptima").asInstanceOf[Option[Map[String, Any]]]
    val nutricionNiveles         = correlaciones("nutricion").asInstanceOf[List[Map[String, Any]]]
    val analisisIACacheado       = DatabaseManager.getSleepAnalysisCached()

    val nutricionLabels = Map(
      "completa" -> "Comida completa", "ligera" -> "Comida ligera", "snack" -> "Solo snack",
      "sin_comer" -> "Sin comer", "no_adecuada" -> "No adecuada"
    )
    val nutricionCard = div(cls := "card bg-dark border-secondary p-2 h-100",
      div(cls := "xx-small fw-bold text-white mb-2 text-center", "🍽️ Nutrición prepartido"),
      if (nutricionNiveles.isEmpty)
        div(cls := "xx-small text-muted text-center py-3", "Registra la comida prepartido en al menos 5 partidos para ver esta correlación")
      else frag(nutricionNiveles.map { n =>
        val tipo = n("tipo").asInstanceOf[String]
        val notaMedia = n("notaMedia").asInstanceOf[Double]
        val partidos = n("partidos").asInstanceOf[Int]
        val color = if (notaMedia > 7) "#20c997" else if (notaMedia >= 5) "#ffc107" else "#dc3545"
        val pct = math.min(100, (notaMedia / 10.0 * 100).toInt)
        val tipoLabel: String = nutricionLabels.getOrElse(tipo, tipo)
        val valorTxt: String = f"$notaMedia%.1f ($partidos)"
        div(cls := "mb-2",
          div(cls := "d-flex justify-content-between xx-small",
            span(cls := "text-muted fw-bold", tipoLabel),
            span(cls := "fw-bold", style := s"color:$color;", valorTxt)
          ),
          div(cls := "progress", style := "height:6px;",
            div(cls := "progress-bar", style := s"width:$pct%; background:$color;")
          )
        )
      }: _*)
    )

    val correlacionesSection = div(cls := "card bg-dark border-warning shadow mb-4",
      div(cls := "card-header text-warning fw-bold small", "🧠 CORRELACIONES SUEÑO-RENDIMIENTO"),
      div(cls := "card-body p-3",
        if (totalParesSmartwatch == 0)
          div(cls := "text-muted small text-center py-3",
            "Registra el sueño en el Diario de Carga (con datos de smartwatch si es posible) para ver correlaciones con el rendimiento en partido")
        else div(
          div(cls := "row g-2 mb-3",
            div(cls := "col-6 col-md-4", sleepFactorCard("😴 Sueño profundo", suenoProfundoNiveles)),
            div(cls := "col-6 col-md-4", sleepFactorCard("🕐 Horas totales", horasNiveles)),
            div(cls := "col-6 col-md-4", sleepFactorCard("⭐ Calidad subjetiva", calidadNiveles)),
            div(cls := "col-6 col-md-4", sleepFactorCard("⚡ Energía", energiaNiveles)),
            div(cls := "col-6 col-md-4", sleepFactorCard("🙂 Ánimo", animoNiveles))
          ),
          combinacionOptima match {
            case Some(c) =>
              val texto = c("texto").asInstanceOf[String]
              val partidosC = c("partidos").asInstanceOf[Int]
              div(cls := "alert alert-success small mb-3",
                div(cls := "fw-bold mb-1", "✨ COMBINACIÓN ÓPTIMA"),
                div(s"$texto ($partidosC partidos)")
              )
            case None =>
              div(cls := "alert alert-secondary small mb-3", "✨ Sin datos suficientes todavía para detectar la combinación óptima")
          },
          analisisIACacheado match {
            case Some(texto) =>
              div(cls := "alert alert-dark border-warning small mb-3", style := "white-space:pre-wrap;", texto)
            case None =>
              div(cls := "text-muted small mb-3 fst-italic", "Sin análisis IA generado todavía — pulsa el botón")
          },
          form(action := "/bio/sueno/analizar", method := "post",
            button(tpe := "submit", cls := "btn btn-warning w-100 fw-bold", "🧠 Análisis IA completo")
          )
        ),
        div(cls := "row g-2 mt-1",
          div(cls := "col-6 col-md-4", nutricionCard)
        )
      )
    )

    val avgHoras = if (sleepHist.nonEmpty) f"${sleepHist.map(_._2).sum / sleepHist.size}%.1f" else "—"
    val avgCalidad = if (sleepHist.nonEmpty) f"${sleepHist.map(_._3).sum.toDouble / sleepHist.size}%.1f" else "—"

    // Calcular correlacion: partidos con buen sueno (>=8h) vs mal sueno (<7h)
    val buenSueno = correlation.filter(_._2 >= 8.0)
    val malSueno  = correlation.filter(c => c._2 > 0 && c._2 < 7.0)
    val avgNotaBueno = if (buenSueno.nonEmpty) f"${buenSueno.map(_._3).sum / buenSueno.size}%.1f" else "—"
    val avgNotaMalo  = if (malSueno.nonEmpty)  f"${malSueno.map(_._3).sum / malSueno.size}%.1f" else "—"

    val fechasJs  = sleepHist.map(s => s""""${s._1}"""").mkString("[",",","]")
    val horasJs   = sleepHist.map(_._2.toString).mkString("[",",","]")
    val calidadJs = sleepHist.map(_._3.toString).mkString("[",",","]")

    val corrFechasJs = correlation.map(c => s""""${c._1}"""").mkString("[",",","]")
    val corrHorasJs  = correlation.map(_._2.toString).mkString("[",",","]")
    val corrNotasJs  = correlation.map(_._3.toString).mkString("[",",","]")

    val corrRows = correlation.map { case (fecha, horas, nota, rival) =>
      val horasCls = if (horas >= 8) "text-success" else if (horas >= 7) "text-warning" else if (horas > 0) "text-danger" else "text-muted"
      val notaCls  = if (nota >= 7) "text-success" else if (nota >= 5) "text-warning" else "text-danger"
      tr(
        td(fecha),
        td(cls := "fw-bold text-white", fixEncoding(rival)),
        td(cls := s"text-center fw-bold $horasCls", if (horas > 0) f"${horas}%.1fh" else "—"),
        td(cls := s"text-center fw-bold $notaCls", nota.toString)
      )
    }

    // ── Bloque principal: el sueno de hoy (resumen o formulario rapido) ──
    val suenoHoyWidget: Modifier = DatabaseManager.getSuenoHoy() match {
      case Some(s) =>
        val horas = s("horas").asInstanceOf[Double]
        div(cls := "card bg-dark border-info shadow mb-3 p-3",
          div(cls := "xx-small text-info fw-bold", "💤 SUEÑO DE ESTA NOCHE — REGISTRADO"),
          div(cls := "d-flex align-items-baseline gap-3 mt-1",
            span(style := "font-size:36px; font-weight:900; color:#fff;", f"$horas%.1fh"),
            s("profundo").asInstanceOf[Option[Int]].map(p => span(cls := "small text-info", s"$p min profundo")).getOrElse(frag())),
          div(cls := "xx-small text-muted",
            Seq(s("energia").asInstanceOf[Option[Int]].map(v => s"Energía $v/5"), s("animo").asInstanceOf[Option[Int]].map(v => s"Ánimo $v/5"),
              s("somnolencia").asInstanceOf[Option[Int]].map(v => s"Somnolencia $v/3")).flatten.mkString(" · ")),
          a(href := "/bio", cls := "xx-small", "Completar o corregir en Bio →"))
      case None =>
        div(cls := "card bg-dark border-info shadow mb-3 p-3",
          div(cls := "xx-small text-info fw-bold mb-2", "💤 ¿CÓMO DURMIÓ HÉCTOR ESTA NOCHE?"),
          form(action := "/bio/sueno/registrar", method := "post",
            div(cls := "row g-2",
              div(cls := "col-6", label(cls := "xx-small text-muted", "Horas"),
                input(tpe := "number", name := "horas", step := "0.5", min := "0", max := "16", required := true, value := "9",
                  cls := "form-control form-control-lg text-center fw-bold bg-dark text-white border-info", attr("inputmode") := "decimal")),
              div(cls := "col-6", label(cls := "xx-small text-muted", "Sueño profundo (min, opcional)"),
                input(tpe := "number", name := "profundo", min := "0", cls := "form-control form-control-lg text-center bg-dark text-white border-secondary", attr("inputmode") := "numeric")),
              frag(Seq(("energia", "⚡ Energía", 1 to 5), ("animo", "🙂 Ánimo", 1 to 5), ("somnolencia", "☀️ Somnolencia", 0 to 3)).map { case (campo, et, rango) =>
                div(cls := "col-12",
                  div(cls := "xx-small text-muted", et),
                  div(cls := "btn-group w-100", role := "group",
                    frag(rango.map { v => frag(
                      input(tpe := "radio", cls := "btn-check", name := campo, id := s"sq_${campo}_$v", value := v.toString, autocomplete := "off"),
                      label(cls := "btn btn-sm btn-outline-info", `for` := s"sq_${campo}_$v", v.toString)) }: _*)))
              }: _*)),
            div(cls := "d-grid mt-3", button(tpe := "submit", cls := "btn btn-info fw-bold", "💤 Guardar sueño")),
            div(cls := "xx-small text-muted mt-1", "No borra nada de lo ya registrado hoy en Bio (FC, dolor, notas...).")))
    }
    // Indice de forma de hoy con sus componentes
    val formaHoy = DatabaseManager.calcularFormaHoy()
    val formaHoyWidget: Modifier = {
      val indice = formaHoy("indiceForma").asInstanceOf[Double]
      val color = if (indice >= 7.5) "#20c997" else if (indice >= 5.0) "#facc15" else "#ef4444"
      div(cls := "card bg-dark border-secondary shadow mb-3 p-3",
        div(cls := "d-flex justify-content-between align-items-center",
          span(cls := "xx-small text-muted fw-bold", "ÍNDICE DE FORMA HOY"),
          span(style := s"font-size:26px; font-weight:900; color:$color;", f"${DatabaseManager.formaSemaforo(indice)} $indice%.1f")),
        div(cls := "row row-cols-3 row-cols-md-6 g-1 mt-1",
          frag(Seq("Sueño" -> "suenoScore", "Energía" -> "energiaScore", "Ánimo" -> "animoScore", "Carga" -> "acwrScore",
            "Descanso" -> "descansoScore", "PHV" -> "phvScore").map { case (et, k) =>
            val v = formaHoy(k).asInstanceOf[Double]
            div(cls := "col", div(cls := "xx-small text-center text-muted", et),
              div(cls := "progress", style := "height:5px;", div(cls := "progress-bar bg-info", style := s"width:${v * 10}%;")),
              div(cls := "xx-small text-center fw-bold", style := "color:#e2e8f0;", f"$v%.1f"))
          }: _*)))
    }
    // Deuda de sueno semanal: solo si es MODERADA o superior
    val deuda = formaHoy("deudaSueno").asInstanceOf[Map[String, Any]]
    val deudaWidget: Modifier = deuda("nivel").asInstanceOf[String] match {
      case "MINIMA" => frag()
      case nivel => div(cls := s"alert ${if (nivel == "CRITICA") "alert-danger" else "alert-warning"} small p-2 mb-3",
        f"💤 Deuda de sueño ${nivel.toLowerCase}: ${deuda("deudaHoras").asInstanceOf[Double]}%.1fh esta semana (${deuda("mediaDiaria").asInstanceOf[Double]}%.1fh/noche de media)")
    }
    // Wellness: FC en reposo, somnolencia, dolor muscular y RPE de Hector
    val wellnessWidget: Modifier = {
      val w = DatabaseManager.getWellnessReciente(14)
      val rpe = DatabaseManager.getRpeHectorReciente(8)
      def op(o: Option[Int]): String = o.map(_.toString).getOrElse("-")
      div(
        if (w.isEmpty) sinDatos("FC, somnolencia y dolor muscular", "Se registran en Bio o por Telegram")
        else div(cls := "card bg-dark border-secondary p-2 mb-2",
          div(cls := "xx-small fw-bold text-white mb-1", "❤️ FC en reposo · ☀️ somnolencia (0-3) · 💪 dolor muscular (0-3) — últimos 14 días"),
          table(cls := "table table-dark table-sm mb-0 xx-small text-center",
            thead(tr(th("Fecha"), th("FC"), th("Somnol."), th("Dolor"))),
            tbody(frag(w.map { case (f, fc, so, dm) =>
              tr(td(f.drop(5)), td(op(fc)), td(op(so)), td(op(dm)))
            }: _*)))),
        if (rpe.isEmpty) sinDatos("RPE de Héctor", "Se pregunta al registrar un entreno (en Bio o por Telegram)")
        else div(cls := "card bg-dark border-secondary p-2 mb-2",
          div(cls := "xx-small fw-bold text-white mb-1", "😴 Cómo llegó Héctor a casa (1-5) vs RPE registrado (1-10)"),
          frag(rpe.map { case (f, tipo, rp, rh) =>
            div(cls := "xx-small d-flex justify-content-between", span(s"${f.drop(5)} · $tipo"),
              span(s"Héctor ${DatabaseManager.etiquetasRpeHector.lift(rh - 1).getOrElse(rh.toString).toLowerCase} ($rh) · RPE $rp"))
          }: _*)),
        div(cls := "xx-small", a(href := "/bio", "Importar FC por captura de pantalla en Bio →")))
    }

    val content = basePage("bio",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h2(cls := "text-info mb-0", "SUENO & RENDIMIENTO"),
            a(href := "/bio", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Bio")
          ),

          // ── BLOQUE PRINCIPAL: sueno de hoy, indice de forma y deuda de sueno ──
          suenoHoyWidget,
          formaHoyWidget,
          deudaWidget,

          // ── HISTORICO Y ANALISIS (colapsable) ──
          seccion("📊 Histórico y análisis")(
          DatabaseManager.detectarJetlagSocial() match {
            case Some(msg) => div(cls := "alert alert-warning small p-2 mb-3", msg)
            case None => frag()
          },
          div(cls := "row g-2 mb-4",
            Seq(
              ("Media Horas", avgHoras + "h", "info"),
              ("Calidad Media", avgCalidad + "/5", "warning"),
              ("Nota c/buen sueno", avgNotaBueno, "success"),
              ("Nota c/mal sueno", avgNotaMalo, "danger")
            ).map { case (lbl, v, c) =>
              div(cls := "col-3",
                div(cls := s"card bg-dark border-$c text-center py-3",
                  div(cls := s"text-$c fw-bold fs-4", v),
                  div(cls := "xx-small text-muted mt-1", lbl)
                )
              )
            }
          ),

          correlacionesSection,

          if (sleepHist.isEmpty) {
            div(cls := "alert alert-secondary text-center py-5",
              div(style := "font-size:40px; opacity:0.3;", "🌙"),
              div(cls := "fw-bold mt-2", "Sin datos de sueno registrados"),
              div(cls := "small text-muted mt-1", "Registra el sueno diario en Bio")
            )
          } else div(
            // Grafico sueno historico
            div(cls := "card bg-dark border-info shadow mb-4",
              div(cls := "card-header text-info fw-bold small", "HORAS DE SUENO — Ultimos 60 dias"),
              div(cls := "card-body",
                div(style := "position:relative; height:200px;",
                  tag("canvas")(id := "chartSueno")
                )
              )
            ),
            div(cls := "row g-3",
              // Scatter correlacion
              div(cls := "col-md-6",
                div(cls := "card bg-dark border-success shadow",
                  div(cls := "card-header text-success fw-bold small", "CORRELACION: SUENO → NOTA PARTIDO"),
                  div(cls := "card-body",
                    div(style := "position:relative; height:220px;",
                      tag("canvas")(id := "chartCorr")
                    )
                  )
                )
              ),
              // Tabla
              div(cls := "col-md-6",
                div(cls := "card bg-dark border-secondary shadow",
                  div(cls := "card-header text-white fw-bold small", "HISTORIAL SUENO - PARTIDO"),
                  div(cls := "card-body p-0",
                    div(cls := "table-responsive", style := "max-height:240px; overflow-y:auto;",
                      table(cls := "table table-dark table-sm mb-0 small",
                        thead(tr(th("Fecha"), th("Rival"), th(cls:="text-center","Sueno"), th(cls:="text-center","Nota"))),
                        tbody(corrRows)
                      )
                    )
                  )
                )
              )
            ),
            script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
            script(raw(s"""
              const fechas  = $fechasJs;
              const horas   = $horasJs;
              const calidad = $calidadJs;
              const corrFechas = $corrFechasJs;
              const corrHoras  = $corrHorasJs;
              const corrNotas  = $corrNotasJs;

              new Chart(document.getElementById('chartSueno'), {
                type: 'bar',
                data: {
                  labels: fechas,
                  datasets: [
                    { label: 'Horas dormidas', data: horas, backgroundColor: 'rgba(23,162,184,0.6)', borderRadius: 3, yAxisID: 'y' },
                    { label: 'Calidad (1-5)', data: calidad, type: 'line', borderColor: '#ffc107', backgroundColor: 'transparent', borderWidth: 2, pointRadius: 3, yAxisID: 'y1' }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc', font: { size: 11 } } } },
                  scales: {
                    x: { ticks: { color: '#888', maxTicksLimit: 12, font: { size: 9 } }, grid: { color: 'rgba(255,255,255,0.04)' } },
                    y: { min: 0, max: 12, ticks: { color: '#17a2b8' }, title: { display: true, text: 'Horas', color: '#888' } },
                    y1: { position: 'right', min: 0, max: 5, ticks: { color: '#ffc107' }, grid: { display: false } }
                  }
                }
              });

              // Scatter: horas sueno vs nota partido
              const scatterData = corrHoras.map((h, i) => ({x: parseFloat(h), y: parseFloat(corrNotas[i])})).filter(p => p.x > 0);
              new Chart(document.getElementById('chartCorr'), {
                type: 'scatter',
                data: {
                  datasets: [{
                    label: 'Partido (horas → nota)',
                    data: scatterData,
                    backgroundColor: 'rgba(40,167,69,0.7)',
                    pointRadius: 7,
                    pointHoverRadius: 9
                  }]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: { legend: { labels: { color: '#ccc' } }, tooltip: {
                    callbacks: { label: ctx => corrFechas[ctx.dataIndex] + ' — ' + ctx.parsed.x + 'h → nota ' + ctx.parsed.y }
                  }},
                  scales: {
                    x: { min: 4, max: 12, title: { display: true, text: 'Horas sueno', color: '#888' }, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } },
                    y: { min: 0, max: 10, title: { display: true, text: 'Nota partido', color: '#888' }, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.05)' } }
                  }
                }
              });
            """))
          ),
          div(cls := "small mb-3", "📱 ¿Tienes el sueño en el reloj? ", a(href := "/bio", "Importa la FC por captura o completa el registro con los datos del smartwatch en Bio →"))
          ),

          // ── WELLNESS (colapsable) ──
          seccion("📈 Wellness")(wellnessWidget)
        )
      )
    )
    renderHtml(content)
  }

  // Registro rapido del sueno desde /bio/sueno (no pisa el resto de campos del dia)
  @cask.post("/bio/sueno/registrar")
  def registrarSuenoAction(request: cask.Request) = withAuth(request) {
    val p = parseBody(request)
    def int(k: String, min: Int, max: Int) = p.get(k).flatMap(_.toIntOption).filter(v => v >= min && v <= max)
    p.get("horas").flatMap(_.replace(",", ".").toDoubleOption).filter(h => h > 0 && h <= 16).foreach { horas =>
      DatabaseManager.registrarSuenoHoy(horas, int("profundo", 0, 600), None, None, int("energia", 1, 5), int("animo", 1, 5), int("somnolencia", 0, 3))
    }
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/bio/sueno"))
  }

  // Llamada explicita a Gemini disparada por boton POST — nunca en el render de pagina
  @cask.post("/bio/sueno/analizar")
  def analizarSueno(request: cask.Request) = withAuth(request) {
    DatabaseManager.generateSleepAnalysisIA()
    cask.Response(Array.emptyByteArray, 302, headers = Seq("Location" -> "/bio/sueno"))
  }

  // ── FASE 2: DETECTOR DE FATIGA MENTAL ─────────────────────────────────────
  @cask.get("/bio/fatiga")
  def fatigaPage(request: cask.Request) = withAuth(request) {
    val datos    = DatabaseManager.getFatigaDetector(45)
    val cogInsight = DatabaseManager.getCognitiveInsight()

    val fechasJs   = datos.map(d => s""""${d._1}"""").mkString("[",",","]")
    val atencionJs = datos.map(_._2.toString).mkString("[",",","]")
    val acadJs     = datos.map(_._3.toString).mkString("[",",","]")
    val animoJs    = datos.map(_._4.toString).mkString("[",",","]")

    // Alertas de fatiga
    val sesionesLowAtt = datos.filter(_._2 < 6).size
    val avgAtt7 = { val last7 = datos.takeRight(7); if (last7.nonEmpty) last7.map(_._2).sum.toDouble / last7.size else 0.0 }
    val avgAttAll = if (datos.nonEmpty) datos.map(_._2).sum.toDouble / datos.size else 0.0
    val tendencia = avgAtt7 - avgAttAll
    val (tendLabel, tendColor) = if (tendencia < -1.5) ("BAJANDO", "danger")
    else if (tendencia > 1.0) ("SUBIENDO", "success")
    else ("ESTABLE", "secondary")

    val alertaWidget = if (sesionesLowAtt >= 5 || avgAtt7 < 6.0) {
      div(cls := "alert alert-danger border-danger d-flex align-items-start gap-3 mb-4",
        div(style := "font-size:28px;", "⚠️"),
        div(
          div(cls := "fw-bold", "ALERTA FATIGA MENTAL DETECTADA"),
          div(cls := "small mt-1", s"$sesionesLowAtt sesiones con atencion < 6 en los ultimos 45 dias."),
          div(cls := "small", s"Media de atencion ultimos 7 dias: ${f"$avgAtt7%.1f"}/10"),
          div(cls := "small fst-italic mt-1 text-warning", "Recomendacion: Reducir intensidad cognitiva y aumentar recuperacion activa.")
        )
      )
    } else if (tendencia < -0.8) {
      div(cls := "alert alert-warning border-warning d-flex align-items-start gap-3 mb-4",
        div(style := "font-size:28px;", "📉"),
        div(
          div(cls := "fw-bold", "TENDENCIA A LA BAJA"),
          div(cls := "small mt-1", "La atencion en los entrenamientos recientes esta por debajo de tu media."),
          div(cls := "small fst-italic text-warning", "Considera revisar carga academica o descanso.")
        )
      )
    } else {
      div(cls := "alert alert-success border-success d-flex align-items-start gap-3 mb-4",
        div(style := "font-size:24px;", "✅"),
        div(
          div(cls := "fw-bold", "ESTADO COGNITIVO OPTIMO"),
          div(cls := "small mt-1", "Atencion en niveles normales. Sin senales de fatiga mental.")
        )
      )
    }

    val content = basePage("bio",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h2(cls := "text-warning mb-0", "DETECTOR FATIGA MENTAL"),
            a(href := "/bio", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Bio")
          ),

          // KPIs
          div(cls := "row g-2 mb-3",
            Seq(
              ("Media Atencion", f"$avgAttAll%.1f/10", "info"),
              ("Ultimos 7 dias", f"$avgAtt7%.1f/10", if(avgAtt7 >= 7)"success" else "danger"),
              ("Sesiones bajo 6", sesionesLowAtt.toString, if(sesionesLowAtt >= 5)"danger" else "secondary"),
              ("Tendencia", tendLabel, tendColor)
            ).map { case (lbl, v, c) =>
              div(cls := "col-3",
                div(cls := s"card bg-dark border-$c text-center py-2",
                  div(cls := s"text-$c fw-bold fs-5", v),
                  div(cls := "xx-small text-muted", lbl)
                )
              )
            }
          ),

          alertaWidget,

          // Insight IA
          div(cls := "card bg-dark border-info shadow mb-4",
            div(cls := "card-header text-info fw-bold small", "ANALISIS IA — SINCRONIZACION COGNITIVA"),
            div(cls := "card-body small text-light", raw(cogInsight))
          ),

          if (datos.isEmpty) {
            div(cls := "alert alert-secondary text-center py-4",
              div(cls := "fw-bold", "Sin datos de entrenamiento suficientes"),
              div(cls := "small text-muted mt-1", "Registra sesiones con campo de atencion en Bio")
            )
          } else {
            div(cls := "card bg-dark border-secondary shadow",
              div(cls := "card-header text-white fw-bold small", "ATENCION vs RENDIMIENTO ACADEMICO — Ultimos 45 dias"),
              div(cls := "card-body",
                div(style := "position:relative; height:280px;",
                  tag("canvas")(id := "chartFatiga")
                )
              )
            )
          },

          script(src := "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js"),
          script(raw(s"""
            const fechas   = $fechasJs;
            const atencion = $atencionJs;
            const acad     = $acadJs;
            const animo    = $animoJs;

            if (fechas.length > 0) {
              new Chart(document.getElementById('chartFatiga'), {
                type: 'line',
                data: {
                  labels: fechas,
                  datasets: [
                    { label: 'Atencion entreno', data: atencion, borderColor: '#ffc107', backgroundColor: 'rgba(255,193,7,0.1)', borderWidth: 2, pointRadius: 4, tension: 0.3, yAxisID: 'y' },
                    { label: 'Nota academica', data: acad.map(v => v > 0 ? v : null), borderColor: '#dc3545', backgroundColor: 'rgba(220,53,69,0.1)', borderWidth: 2, pointRadius: 4, tension: 0.3, yAxisID: 'y', spanGaps: true },
                    { label: 'Animo (x2)', data: animo.map(v => v * 2), borderColor: '#17a2b8', backgroundColor: 'transparent', borderWidth: 1, borderDash: [4,4], pointRadius: 2, tension: 0.3, yAxisID: 'y' }
                  ]
                },
                options: {
                  responsive: true, maintainAspectRatio: false,
                  plugins: {
                    legend: { labels: { color: '#ccc', font: { size: 11 } } },
                    annotation: {}
                  },
                  scales: {
                    x: { ticks: { color: '#888', maxTicksLimit: 12, font: { size: 9 } }, grid: { color: 'rgba(255,255,255,0.04)' } },
                    y: { min: 0, max: 10, ticks: { color: '#aaa' }, grid: { color: 'rgba(255,255,255,0.06)' } }
                  }
                }
              });
            }
          """))
        )
      )
    )
    renderHtml(content)
  }

  // ── TERMOMETRO DE GUANTES ───────────────────────────────────────────────────
  @cask.get("/bio/guantes")
  def guantesPage(request: cask.Request) = withAuth(request) {
    val (latex, motivo, guanteActual) = DatabaseManager.getGloveThermostat()
    val gear = DatabaseManager.getActiveGear().filter(_.tipo == "Guantes")
    val recentMatches = DatabaseManager.getMatchesList().take(10)
    val avgTemp = if (recentMatches.nonEmpty) {
      val temps = recentMatches.map(m => {
        // Extraer temperatura de notas o usar 15 por defecto
        15  // placeholder — la temp se guarda en tabla matches
      })
      temps.sum / temps.size
    } else 15

    // Datos climaticos de partidos recientes
    val climaStats = recentMatches.groupBy(_.clima).map { case (c, ms) => c -> ms.size }
    val latexColor = latex match {
      case s if s.contains("Frio")   => "info"
      case s if s.contains("Aqua")   => "primary"
      case s if s.contains("Soft")   => "warning"
      case s if s.contains("Duo")    => "danger"
      case _                         => "success"
    }

    val content = basePage("bio",
      div(cls := "row justify-content-center",
        div(cls := "col-md-10 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h2(cls := "text-warning mb-0", "TERMOMETRO DE GUANTES"),
            a(href := "/bio", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Bio")
          ),

          // Recomendacion principal
          div(cls := s"card bg-dark border-$latexColor shadow mb-4",
            div(cls := s"card-header bg-$latexColor bg-opacity-10 border-$latexColor d-flex justify-content-between align-items-center",
              span(cls := s"text-$latexColor fw-bold", "RECOMENDACION PARA EL PROXIMO PARTIDO"),
              span(cls := s"badge bg-$latexColor fw-bold", "IA")
            ),
            div(cls := "card-body",
              div(cls := "d-flex align-items-center gap-4",
                div(style := "font-size:52px;", "🧤"),
                div(
                  div(cls := s"text-$latexColor fw-bold fs-5 mb-1", latex),
                  div(cls := "text-light small", motivo),
                  if (guanteActual.nonEmpty && guanteActual != "Sin guantes registrados")
                    div(cls := "mt-2",
                      span(cls := "xx-small text-muted fw-bold", "GUANTE EN USO: "),
                      span(cls := "text-warning fw-bold small", guanteActual)
                    )
                  else div()
                )
              )
            )
          ),

          div(cls := "row g-3",
            // Guia de latex
            div(cls := "col-md-6",
              div(cls := "card bg-dark border-secondary shadow",
                div(cls := "card-header text-white fw-bold small", "GUIA DE LATEX POR CONDICION"),
                div(cls := "card-body p-2",
                  Seq(
                    ("❄️ Frio (< 5C)", "Latex Hibrido Frio", "Mantiene agarre y flexibilidad en bajas temperaturas", "info"),
                    ("🌡️ Fresco (5-12C)", "Latex Soft Grip", "Optimo agarre en frio moderado", "primary"),
                    ("🌧️ Lluvia / Humedad", "Latex Aqua", "Tratado para condiciones mojadas, agarre superior", "primary"),
                    ("☀️ Calor (> 25C)", "Latex Duo Soft", "Transpirable, evita sudor excesivo en el guante", "danger"),
                    ("🌤️ Condiciones ideales", "Latex Contact", "El latex estandar de referencia para dia normal", "success")
                  ).map { case (cond, rec, desc, c) =>
                    div(cls := s"d-flex gap-2 p-2 mb-1 rounded border-start border-$c border-2",
                      style := "background:rgba(255,255,255,0.03);",
                      div(
                        div(cls := "xx-small text-muted fw-bold", cond),
                        div(cls := s"text-$c fw-bold small", rec),
                        div(cls := "xx-small text-muted", desc)
                      )
                    )
                  }
                )
              )
            ),

            // Historial clima partidos + inventario
            div(cls := "col-md-6",
              div(cls := "card bg-dark border-secondary shadow mb-3",
                div(cls := "card-header text-white fw-bold small", "CONDICIONES ULTIMOS PARTIDOS"),
                div(cls := "card-body p-2",
                  if (climaStats.isEmpty)
                    div(cls := "text-muted text-center small py-2", "Sin partidos registrados")
                  else div(
                    climaStats.toList.sortBy(-_._2).map { case (clima, count) =>
                      val icon = clima match {
                        case s if s.contains("Lluvia") => "🌧️"
                        case s if s.contains("Nublado") => "☁️"
                        case s if s.contains("Viento") => "💨"
                        case _ => "☀️"
                      }
                      div(cls := "d-flex justify-content-between align-items-center p-1 mb-1",
                        span(cls := "small", s"$icon $clima"),
                        span(cls := "badge bg-secondary fw-bold", s"$count partidos")
                      )
                    }
                  )
                )
              ),
              if (gear.nonEmpty) div(cls := "card bg-dark border-warning shadow",
                div(cls := "card-header text-warning fw-bold small", "INVENTARIO DE GUANTES"),
                div(cls := "card-body p-2",
                  gear.map { g =>
                    val pct = if (g.maxUsos > 0) (g.usos * 100 / g.maxUsos).min(100) else 0
                    val barColor = if (pct >= 90) "danger" else if (pct >= 70) "warning" else "success"
                    div(cls := "mb-2",
                      div(cls := "d-flex justify-content-between xx-small mb-1",
                        span(cls := "fw-bold text-white", g.nombre),
                        span(cls := s"text-$barColor fw-bold", s"${g.usos}/${g.maxUsos} usos")
                      ),
                      div(cls := "progress", style := "height:6px;",
                        div(cls := s"progress-bar bg-$barColor", style := s"width:$pct%;")
                      )
                    )
                  }
                )
              ) else div()
            )
          )
        )
      )
    )
    renderHtml(content)
  }

  initialize()
}