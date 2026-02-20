import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

import cask.model.FormValue

object BioController extends cask.Routes {

  def medicalSection(reports: List[MedicalReport]) = {
    div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "[hospital] MEDICAL VAULT & PASAPORTE BIOLOGICO"),
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
      div(cls:="card-header border-info text-info fw-bold py-1 text-center small", "[IA] ANALISTA COGNITIVO"),
      div(cls:="card-body p-2",
        p(cls:="text-light small mb-0 text-center fw-bold", cognitiveInsight)
      )
    )

    // --- FASE 4: MEDICAL VAULT SECTION ---
    val medicalVault = div(cls := "card bg-dark text-white border-danger shadow mb-3",
      div(cls := "card-header bg-danger text-white fw-bold text-center small", "[hospital] MEDICAL VAULT & PASAPORTE BIOLOGICO"),
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
      div(cls := "card-header bg-warning text-dark fw-bold text-center small", "[libros] REGISTRO ACADEMICO"),
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

    val drillList = if (activeDrills.nonEmpty) { val dItems = for(d <- activeDrills) yield div(cls:="mb-2", div(cls:="d-flex justify-content-between small", span(fixEncoding(d.nombre)), span(s"${d.actual}/${d.objetivo}")), div(cls:="progress", style:="height: 6px;", div(cls:="progress-bar bg-warning", style:=s"width:${(d.actual.toDouble/d.objetivo.toDouble*100).toInt}%"))); div(id:="drillsContainer", style:="display:none;", cls:="mb-3 p-2 border border-secondary rounded bg-secondary bg-opacity-10", h6(cls:="text-warning small fw-bold mb-2", "[objetivo] MISIONES ACTIVAS"), dItems) } else div(id:="drillsContainer", style:="display:none;", cls:="alert alert-dark p-2 small text-center", "Sin misiones activas.")

    val content = basePage("bio", div(cls := "row justify-content-center",
      div(cls := "col-md-6 mb-4",
        // LABORATORIO
        div(cls:="card bg-secondary bg-opacity-10 border-info shadow mb-4", div(cls:="card-header bg-dark text-info fw-bold text-center", "[lab] LABORATORIO DE DATOS"), div(cls:="card-body p-2 d-flex justify-content-around", a(href:="/gear", cls:="btn btn-outline-light flex-fill me-1", div(style:="font-size:20px", "[botas]"), span(cls:="small", "Material")), a(href:="/oracle", cls:="btn btn-outline-info flex-fill me-1", div(style:="font-size:20px", "[oracle]"), span(cls:="small", "Oraculo")), a(href:="/distribution", cls:="btn btn-outline-warning flex-fill", div(style:="font-size:20px", "[stats]"), span(cls:="small", "Moneyball")))),
        // --- NUEVO: MODULO JUDO (Insertar aqui) ---
        div(cls:="card bg-dark border-warning shadow mb-4",
          div(cls:="card-header bg-warning text-dark fw-bold text-center", "[judo] ESTADO DOJO (JUDO)"),
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
        medicalVault,
        academicForm, // Entrada de datos escolares


        // WELLNESS
        div(cls := "card bg-dark text-white border-info shadow mb-3", div(cls := "card-header bg-info text-dark fw-bold text-center", "DIARIO DE CARGA Y SUENO"), div(cls := "card-body p-3", form(action := "/bio/save_wellness", method := "post", div(cls:="mb-3", label(cls:="small text-danger fw-bold", "Estado Fisico"), select(name:="estadoFisico", cls:="form-select bg-dark text-white border-secondary fw-bold", option(value:="DISPONIBLE", "[OK] Disponible"), option(value:="MOLESTIAS", "[!] Molestias"), option(value:="LESION", "X Lesionado"), option(value:="ENFERMO", "[enfermo] Enfermo"))), div(cls:="row mb-3 align-items-end", div(cls:="col-6 text-center", label(cls:="small fw-bold", "Calidad Sueno (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="sueno")), div(cls:="col-6", label(cls:="small text-warning fw-bold", "Horas Dormidas"), input(tpe:="number", step:="0.5", name:="horas", cls:="form-control text-center bg-dark text-white border-warning fw-bold", value:="9.0"))), div(cls:="mb-3 border-top pt-2", label(cls:="small fw-bold", "Energia (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="energia")), div(cls:="mb-3", label(cls:="small text-info fw-bold", "Estado Animico (1-5)"), input(tpe:="range", cls:="form-range", min:="1", max:="5", name:="animo"), div(cls:="d-flex justify-content-between xx-small text-muted fw-bold", span("Crisis"), span("Top"))), div(cls:="mb-2", label(cls:="small text-muted fw-bold", "Notas conducta"), input(tpe:="text", name:="notas_conducta", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="... ")), div(cls:="mb-3 row", div(cls:="col-6", select(name:="dolor", cls:="form-select fw-bold", option(value:="1","Nada"), option(value:="2","Molestia"), option(value:="3","Dolor"), option(value:="5","Lesion"))), div(cls:="col-6", input(tpe:="text", name:="zona", cls:="form-control fw-bold", placeholder:="Zona?"))), div(cls:="row mb-3 border-top pt-3", div(cls:="col-6", label(cls:="small text-info fw-bold", "Altura (cm)"), input(tpe:="number", name:="altura", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar")), div(cls:="col-6", label(cls:="small text-info fw-bold", "Peso (kg)"), input(tpe:="number", step:="0.1", name:="peso", cls:="form-control bg-dark text-white fw-bold", placeholder:="Actualizar"))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-info fw-bold", "Guardar Bio"))))),

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
        div(cls := "card bg-dark text-white border-success shadow mb-3", div(cls := "card-header bg-success text-dark fw-bold text-center", "REGISTRO ENTRENO"), div(cls := "card-body p-3", form(action := "/bio/save_training", method := "post", div(cls:="mb-3", label(cls:="small fw-bold", "Tipo"), select(name:="tipo", id:="trainingType", onchange:="toggleDrills()", cls:="form-select bg-dark text-white fw-bold", option(value:="Club", "Club"), option(value:="Academia", "Academia"), option(value:="Judo", "[judo] Judo"), option(value:="Papa", "Papa (Portero)"), option(value:="Papa (Jugador)", "Papa (Jugador)"))), drillList, div(cls:="mb-3", label(cls:="small fw-bold", "Foco / Actividad"), div(cls:="d-flex gap-2", input(tpe:="text", name:="foco", id:="drillFocus", cls:="form-control fw-bold", placeholder:="Ej: Tiros, Resistencia...", required:=true), button(tpe:="button", id:="aiBtn", cls:="btn btn-warning fw-bold", onclick:="generateAI()", style:="display:none;", "[robot] IA"))), div(id:="manualDesign", style:="display:none;", textarea(name:="rutina", id:="rutinaText", cls:="form-control mb-3 fw-bold", rows:="4", placeholder:="Detalle de la sesion...")), div(cls:="row mb-3", div(cls:="col-4 text-center", label(cls:="small fw-bold", "RPE"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="rpe", value:="7", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Calidad"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="calidad", value:="8", min:="1", max:="10")), div(cls:="col-4 text-center", label(cls:="small fw-bold", "Atencion"), input(tpe:="number", cls:="form-control text-center p-1 fw-bold", name:="atencion", value:="8", min:="1", max:="10"))), div(cls:="d-grid", button(tpe:="submit", cls:="btn btn-outline-success fw-bold", "Guardar Sesion")))),
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
        h1(style := "color: #28a745; font-size: 60px; margin-bottom: 0;", "[OK]"),
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
                    esPrevio: String = "false", // Cambiado a String
                    archivo: cask.FormValue) = {
    val isPrevio = esPrevio == "on"

    // 1. Extraemos los bytes directamente usando la interfaz de datos de Cask
    // Intentamos obtener los bytes y el nombre sin llamar a la clase interna .File
    val fileBytes = try {
      archivo.getClass.getMethod("data").invoke(archivo).asInstanceOf[Array[Byte]]
    } catch {
      case _: Exception => Array.empty[Byte]
    }

    val fileName = try {
      archivo.getClass.getMethod("name").invoke(archivo).asInstanceOf[String]
    } catch {
      case _: Exception => "documento.pdf"
    }

    if (fileBytes.nonEmpty) {
      // 2. Proceso para Gemini
      val base64Content = java.util.Base64.getEncoder.encodeToString(fileBytes)
      val mimeType = if (fileName.toLowerCase.endsWith(".pdf")) "application/pdf" else "image/jpeg"

      val medicalPrompt = s"Analiza este informe ($tipo) de Hector. Extrae DIAGNOSTICO y RECOMENDACION DEPORTIVA. Formato: DIAGNOSTICO: [texto] | RECOMENDACION: [texto]"

      val analisisIA = DatabaseManager.AIProvider.ask(medicalPrompt, Some((mimeType, base64Content)))
      val partes = analisisIA.split("\\|")
      val diag = partes.headOption.getOrElse("No detectado").replace("DIAGNOSTICO:", "").trim
      val rec = partes.lastOption.getOrElse("No detectado").replace("RECOMENDACION:", "").trim

      DatabaseManager.saveMedicalRecordFull(fecha, tipo, diag, rec, isPrevio)
    }

    cask.Response("".getBytes("UTF-8"), statusCode=302, headers=Seq("Location" -> "/bio"))
  }
  // --- 3. MODO LEGADO (RPG) ---

  initialize()
}
