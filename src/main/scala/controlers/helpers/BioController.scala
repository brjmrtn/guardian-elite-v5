import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

import cask.model.FormValue

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
        div(cls:="card bg-secondary bg-opacity-10 border-info shadow mb-4", div(cls:="card-header bg-dark text-info fw-bold text-center", "🔬 LABORATORIO DE DATOS"), div(cls:="card-body p-2 d-flex justify-content-around", a(href:="/gear", cls:="btn btn-outline-light flex-fill me-1", div(style:="font-size:20px", "⚽"), span(cls:="small", "Material")), a(href:="/oracle", cls:="btn btn-outline-info flex-fill me-1", div(style:="font-size:20px", "🔮"), span(cls:="small", "Oraculo")), a(href:="/distribution", cls:="btn btn-outline-warning flex-fill", div(style:="font-size:20px", "📊"), span(cls:="small", "Moneyball")))),
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
        academicForm, // Entrada de datos escolares


        // WELLNESS
        div(cls := "card bg-dark text-white border-info shadow mb-3", div(cls := "card-header bg-info text-dark fw-bold text-center", "DIARIO DE CARGA Y SUENO"), div(cls := "card-body p-3", form(action := "/bio/save_wellness", method := "post", div(cls:="mb-3", label(cls:="small text-danger fw-bold", "Estado Fisico"), select(name:="estadoFisico", cls:="form-select bg-dark text-white border-secondary fw-bold", option(value:="DISPONIBLE", "✅ Disponible"), option(value:="MOLESTIAS", "⚠ Molestias"), option(value:="LESION", "X Lesionado"), option(value:="ENFERMO", "🤒 Enfermo"))), div(cls:="row mb-3 align-items-end", div(cls:="col-6 text-center", label(cls:="small fw-bold", "Calidad Sueno (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="sueno")), div(cls:="col-6", label(cls:="small text-warning fw-bold", "Horas Dormidas"), input(tpe:="number", step:="0.5", name:="horas", cls:="form-control text-center bg-dark text-white border-warning fw-bold", value:="9.0"))), div(cls:="mb-3 border-top pt-2", label(cls:="small fw-bold", "Energia (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="energia")), div(cls:="mb-3", label(cls:="small text-info fw-bold", "Estado Animico (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="animo"), div(cls:="d-flex justify-content-between xx-small text-muted fw-bold", span("Crisis"), span("Top"))), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Notas conducta"), input(tpe:="text", name:="notas_conducta", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="... ")), div(cls:="mb-3 row", div(cls:="col-6", select(name:="dolor", cls:="form-select fw-bold", option(value:="1","Nada"), option(value:="2","Molestia"), option(value:="3","Dolor"), option(value:="5","Lesion"))), div(cls:="col-6", input(tpe:="text", name:="zona", cls:="form-control fw-bold", placeholder:="Zona?"))), div(cls:="row mb-3 border-top pt-3", div(cls:="col-6", label(cls:="small text-info fw-bold", "Altura (cm)"), input(tpe:="number", name:="altura", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar")), div(cls:="col-6", label(cls:="small text-info fw-bold", "Peso (kg)"), input(tpe:="number", step:="0.1", name:="peso", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar"))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-info fw-bold", "Guardar Bio"))))),

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
        div(cls := "card bg-dark text-white border-success shadow mb-3", div(cls := "card-header bg-success text-dark fw-bold text-center", "REGISTRO ENTRENO"), div(cls := "card-body p-3", form(action := "/bio/save_training", method := "post", div(cls:="mb-3", label(cls:="small fw-bold", "Tipo"), select(name:="tipo", id:="trainingType", onchange:="toggleDrills()", cls:="form-select bg-dark text-white fw-bold", option(value:="Club", "Club"), option(value:="Academia", "Academia"), option(value:="Judo", "🥋 Judo"), option(value:="Papa", "Papa (Portero)"), option(value:="Papa (Jugador)", "Papa (Jugador)"))), drillList, div(cls:="mb-3", label(cls:="small fw-bold", "Foco / Actividad"), div(cls:="d-flex gap-2", input(tpe:="text", name:="foco", id:="drillFocus", cls:="form-control fw-bold", placeholder:="Ej: Tiros, Resistencia...", required:=true), button(tpe:="button", id:="aiBtn", cls:="btn btn-warning fw-bold", onclick:="generateAI()", style:="display:none;", "🤖 IA"))), div(id:="manualDesign", style:="display:none;", textarea(name:="rutina", id:="rutinaText", cls:="form-control mb-3 fw-bold", rows:="4", placeholder:="Detalle de la sesion...")), div(cls:="row mb-3", div(cls:="col-4 text-center", label(cls:="small fw-bold", "RPE"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="rpe", value:="7", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Calidad"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="calidad", value:="8", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Atencion"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="atencion", value:="8", min:="1", max:="10"))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-success fw-bold", "Guardar Sesion")))),
          div(cls:="card bg-secondary bg-opacity-10 border-secondary", div(cls:="card-body p-2", h6(cls:="text-muted small mb-2", "+ Anadir Mision Tecnica (10 Sesiones)"), form(action:="/bio/add_drill", method:="post", cls:="d-flex gap-2", input(tpe:="text", name:="nombre", cls:="form-control form-control-sm fw-bold", placeholder:="Ej: Control Orientado", required:=true), button(tpe:="submit", cls:="btn btn-sm btn-secondary fw-bold", "Crear"))))
        )
      ), script(src := "https://cdn.jsdelivr.net/npm/chart.js"), script(raw(s"""
      const gCtx=document.getElementById('growthChart');const gData=$growthData;if(gCtx){new Chart(gCtx,{type:'line',data:{labels:gData.labels,datasets:[{label:'Altura (cm)',data:gData.data,borderColor:'#0dcaf0',borderWidth:3,tension:0.3,pointBackgroundColor:'#fff',pointRadius:4}]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{display:false}},scales:{y:{ticks:{color:'#eee',font:{weight:'bold'}},grid:{color:'#444'},pointLabels:{color:'#fff'}},x:{display:false}}}});}
      const tCtx=document.getElementById('techChart');const tData=$techChart;if(tCtx){new Chart(tCtx,{type:'line',data:tData,options:{responsive:true,maintainAspectRatio:false,scales:{y:{min:0,max:10,ticks:{color:'#eee'},grid:{color:'#444'}},x:{ticks:{color:'#eee'}}}}});}
      function toggleDrills(){ var type=document.getElementById('trainingType').value; var container=document.getElementById('drillsContainer'); var manual=document.getElementById('manualDesign'); var aiBtn = document.getElementById('aiBtn'); if(container){if(type.includes('Papa') && !type.includes('Jugador')) container.style.display='block'; else container.style.display='none';} if(manual){if(type.includes('Papa')) manual.style.display='block'; else manual.style.display='none';} if(aiBtn){if(type.includes('Papa')) aiBtn.style.display='block'; else aiBtn.style.display='none';} }
      function generateAI(){ var focus = document.getElementById('drillFocus').value; var type = document.getElementById('trainingType').value; if(!focus) { alert('Pon un objetivo primero (ej: Velocidad)'); return; } document.getElementById('rutinaText').value = "Generando..."; fetch('/bio/ai_gen?focus='+encodeURIComponent(focus)+'&mode='+encodeURIComponent(type)).then(r=>r.text()).then(t => document.getElementById('rutinaText').value = t); }
      window.addEventListener('DOMContentLoaded', toggleDrills);
    """))))
    renderHtml(content)
  }

  @cask.postForm("/bio/save_academic")
  def saveAcademic(asignatura: String, nota: Double, tipo: String, comentarios: String = "") = {
    DatabaseManager.saveAcademicNote(asignatura, nota, tipo, comentarios)
    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  @cask.postForm("/bio/save_wellness")
  def saveWellness(sueno: Int, horas: String, energia: Int, dolor: Int, zona: String,
                   altura: String, peso: String, animo: Int, notas_conducta: String, estadoFisico: String) = {
    val h   = if (horas.nonEmpty)   horas.toDouble  else 0.0
    val alt = if (altura.nonEmpty)  altura.toInt    else 0
    val pes = if (peso.nonEmpty)    peso.toDouble   else 0.0
    DatabaseManager.logWellness(sueno, h, energia, dolor, zona, alt, pes, animo, notas_conducta, estadoFisico)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }
  @cask.postForm("/bio/save_training")
  def saveTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: String, rutina: String) = {
    val att = if (atencion != null && atencion.nonEmpty) atencion.toInt else 3
    DatabaseManager.logTraining(tipo, foco, rpe, calidad, att, rutina)
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
  def updateBelt(belt: String) = {
    DatabaseManager.updateJudoBelt(belt)
    cask.Response("", statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  @cask.postForm("/bio/save_eval")
  def saveEval(blocaje: Int, pies: Int, aereo: Int, valentia: Int,
               concentracion: Int, coordinacion: Int, notas: String) = {
    DatabaseManager.saveTechnicalReview(blocaje, pies, aereo, valentia, concentracion, coordinacion, notas)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }
  @cask.get("/bio/ai_gen")
  def aiGenDrill(focus: String, mode: String) = {
    cask.Response(DatabaseManager.generateTrainingSession(mode, focus))
  }
  @cask.postForm("/bio/add_drill")
  def addDrill(nombre: String) = {
    DatabaseManager.addNewDrill(fixEncoding(nombre), "")
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/bio"))
  }

  @cask.postForm("/bio/medical/upload")
  def uploadMedical(fecha: String,
                    tipo: String,
                    esPrevio: String = "false",
                    archivo: cask.FormFile) = {
    val isPrevio = esPrevio == "on"
    val fileBytes = archivo.bytes
    val fileName  = if (archivo.fileName.nonEmpty) archivo.fileName else "documento.pdf"

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
  // --- 3. MODO LEGADO (RPG) ---

  // ── FASE 2: PAGINA GRAFICO DE CARGA ───────────────────────────────────────
  @cask.get("/bio/carga")
  def cargaPage(request: cask.Request) = withAuth(request) {
    val weekly   = DatabaseManager.getWeeklyLoad(12)
    val rpeHist  = DatabaseManager.getRPEHistory(60)
    val acute    = DatabaseManager.getWorkloads(7)
    val chronic  = DatabaseManager.getWorkloads(28)
    val acwr     = StatsCalculator.calculateACWR(acute, chronic)
    val (acwrColor, acwrLabel) = if (acwr > 2.0) ("danger","RIESGO ALTO")
    else if (acwr > 1.5) ("warning","SOBRECARGA")
    else if (acwr < 0.8) ("info","BAJA CARGA")
    else ("success","OPTIMO")

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

          // ACWR + KPIs
          div(cls := "row g-2 mb-4",
            div(cls := "col-3",
              div(cls := s"card bg-dark border-$acwrColor text-center py-3",
                div(cls := s"text-$acwrColor fw-bold", style := "font-size:28px;", f"$acwr%.2f"),
                div(cls := "xx-small text-muted mt-1", "ACWR"),
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
        )
      )
    )
    renderHtml(content)
  }

  // ── FASE 2: CORRELACION SUENO-RENDIMIENTO ─────────────────────────────────
  @cask.get("/bio/sueno")
  def suenoPage(request: cask.Request) = withAuth(request) {
    val correlation = DatabaseManager.getSleepMatchCorrelation()
    val sleepHist   = DatabaseManager.getSleepHistory(60)

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

    val content = basePage("bio",
      div(cls := "row justify-content-center",
        div(cls := "col-md-11 col-12",
          div(cls := "d-flex justify-content-between align-items-center mb-3",
            h2(cls := "text-info mb-0", "SUENO & RENDIMIENTO"),
            a(href := "/bio", cls := "btn btn-outline-secondary btn-sm fw-bold", "← Bio")
          ),

          // KPIs
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
          )
        )
      )
    )
    renderHtml(content)
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