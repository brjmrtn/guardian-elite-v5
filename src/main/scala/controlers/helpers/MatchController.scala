import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

import java.net.URLEncoder

object MatchController extends cask.Routes {

  @cask.get("/match-center")
  def matchCenterPage(request: cask.Request, scheduleId: Int = 0) = withAuth(request) {
    val today = java.time.LocalDate.now().toString
    var preRival = ""; var preFecha = today; var isScheduled = false; var preEstadio = ""

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

    // Celdas de la portería
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
              form(action := "/match-center/save", method := "post", attr("accept-charset") := "UTF-8",

                // 1. DATOS GENERALES
                input(tpe:="hidden", name:="scheduleId", value:=scheduleId.toString),
                div(cls:="mb-3", label(cls:="form-label text-white fw-bold small", "TIPO DE PARTIDO"),
                  if(isScheduled) {
                    div(input(tpe:="hidden", name:="tipo", value:="LIGA"), input(tpe:="text", cls:="form-control bg-dark text-white border-primary fw-bold", value:="🏆 LIGA (OFICIAL RFFM)", readonly:=true))
                  } else {
                    div(cls:="d-flex", select(name:="tipo", cls:="form-select bg-dark text-white fw-bold flex-grow-1", option(value:="AMISTOSO", "🤝 AMISTOSO"), option(value:="TORNEO", "🏅 TORNEO"), option(value:="LIGA", "🏆 LIGA (Manual)")), a(href:="/tournament/new", cls:="btn btn-sm btn-outline-warning ms-2 d-flex align-items-center fw-bold", "➕ CREAR TORNEO"))
                  }
                ),
                div(cls := "mb-3",
                  label(cls := "form-label text-warning fw-bold small", "RIVAL"),
                  input(
                    tpe := "text",
                    name := "rival",
                    id := "rivalInput",
                    cls := "form-control form-control-lg fw-bold text-white",
                    value := (if (preRival.nonEmpty) fixEncoding(preRival) else ""),
                    placeholder := "Ej: Rayo Vallecano",
                    required := true,
                    // CAMBIO CLAVE: Si scheduleId es 0, no se renderiza ningún atributo readonly
                    if (scheduleId > 0) readonly := true else ()
                  )
                ),
                div(cls := "mb-3", label(cls := "form-label text-white fw-bold small", "FECHA"), input(tpe := "date", name := "fecha", cls := "form-control", value := preFecha)),
                div(cls:="mb-3", label(cls:="form-label text-white fw-bold small", "ESTADIO / CAMPO"), input(tpe:="text", name:="estadio", cls:="form-control bg-dark text-white", value:=fixEncoding(preEstadio), placeholder:="Ej: Valdebebas Campo 3")),

                // 2. MARCADOR Y PARADAS
                div(cls := "row mb-3 bg-secondary bg-opacity-25 p-2 rounded mx-0",
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "GOLES (GC)"), input(tpe := "number", name := "gc", id:="gcInput", cls := "form-control text-center bg-danger text-white border-0 fw-bold fs-4", value := "0", readonly:=true)),
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "PARADAS"), input(tpe := "number", name := "paradas", id:="parInput", cls := "form-control text-center bg-success text-white border-0 fw-bold fs-4", value := "0", readonly:=true)),
                  div(cls := "col-4 text-center", label(cls := "small fw-bold", "A FAVOR (GF)"), input(tpe := "number", name := "gf", cls := "form-control text-center", value := "0", attr("inputmode"):="numeric"))
                ),

                // 3. DISTRIBUCIÓN (EDERSON)
                div(cls:="mb-4 p-2 border border-info rounded bg-info bg-opacity-10", label(cls:="form-label text-info small fw-bold w-100 text-center", "DISTRIBUCIÓN"),
                  div(cls:="row mb-2 align-items-center", div(cls:="col-4 text-end small fw-bold", "CORTO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pc', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pc', false)", "❌"), input(tpe:="text", id:="display_pc", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true)))),
                  div(cls:="row align-items-center", div(cls:="col-4 text-end small fw-bold", "LARGO"), div(cls:="col-8", div(cls:="btn-group w-100", button(tpe:="button", cls:="btn btn-outline-success btn-sm", onclick:="pass('pl', true)", "✅"), button(tpe:="button", cls:="btn btn-outline-danger btn-sm", onclick:="pass('pl', false)", "❌"), input(tpe:="text", id:="display_pl", cls:="btn btn-dark btn-sm", style:="width:50px;", value:="0/0", readonly:=true))))
                ),
                input(tpe:="hidden", name:="passData", id:="passData", value:="0,0,0,0"), input(tpe:="hidden", id:="pcTot", value:="0"), input(tpe:="hidden", id:="pcOk", value:="0"), input(tpe:="hidden", id:="plTot", value:="0"), input(tpe:="hidden", id:="plOk", value:="0"),

                // 4. PORTERÍA (REJILLA 3x3)
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

                  label(cls:="form-label text-white small fw-bold w-100 text-center mt-3", "ZONAS DE ATAQUE (Tiros)"),
                  div(cls:="shot-origin d-flex gap-2 justify-content-center", div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Left')", "Izquierda"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Center')", "Centro"), div(cls:="btn btn-outline-secondary btn-sm shot-btn", onclick:="toggleOrigin(this, 'Right')", "Derecha"), input(tpe:="hidden", name:="zonaTiros", id:="hiddenOrigin"))
                ),

                // 5. NUEVO: MAPA DE CALOR DE CAMPO (AQUÍ ESTÁ LA INTEGRACIÓN)
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
                    div(cls:="col-6", label(cls:="small text-muted fw-bold", "Clima"), select(name:="clima", cls:="form-select form-select-sm bg-dark text-white fw-bold", option(value:="Sol", "Sol"), option(value:="Nubes", "Nubes"), option(value:="Lluvia", "Lluvia"), option(value:="Nublado", "Nublado"), option(value:="Frio", "Frio"), option(value:="Viento", "Viento"))),
                    div(cls:="col-6", label(cls:="small text-muted fw-bold", "Temp (C)"), input(tpe:="number", name:="temp", cls:="form-control form-control-sm bg-dark text-white fw-bold", value:="20"))
                  )
                ),
                div(cls:="mb-3 p-2 border border-danger rounded bg-danger bg-opacity-10", label(cls:="form-label text-danger small fw-bold w-100 text-center", "SALA DE VIDEO"), input(tpe:="url", name:="video", cls:="form-control form-control-sm bg-dark text-white fw-bold", placeholder:="Link Video (Youtube/Drive)")),
                div(cls:="mb-3", label(cls:="form-label text-white small fw-bold", "ANOTACIONES DEL ENTRENADOR"), textarea(name:="notas", cls:="form-control form-control-sm bg-dark text-white fw-bold", rows:="3", placeholder:="Notas generales: Saques, posicionamiento, lectura del juego, voz de mando...")),
                div(cls:="mb-4", label(cls:="form-label text-danger small fw-bold", "ANALISIS GOLES / REACCION"), textarea(name:="reaccion", cls:="form-control form-control-sm bg-dark text-white border-danger fw-bold", rows:="3", placeholder:="Descripcion goles encajados y reaccion mental posterior.")),
                div(cls := "mb-3", label(cls := "form-label small fw-bold", "MINUTOS"), input(tpe := "number", name := "minutos", cls := "form-control fw-bold", value := "40", attr("inputmode") := "numeric")),
                div(cls := "mb-4", label(cls := "form-label text-warning fw-bold small", "NOTA (0-10)"), input(tpe := "number", step := "0.1", name := "nota", cls := "form-control form-control-lg text-center fw-bold", placeholder := "Ej: 7.5", required := true, attr("inputmode") := "decimal")),

                div(cls := "d-grid", button(tpe := "submit", cls := "btn btn-success btn-lg py-3 fw-bold", "GUARDAR PARTIDO"))
              ) // fin form
            ),

            // SCRIPTS
            script(raw("""
              var currentMode='save';var goals=[];var saves=[];var origins=[];
              function setMode(mode){currentMode=mode;}
              function registerAction(zone){const cell=document.querySelector('.zone-'+zone);const marker=cell.querySelector('.action-marker');if(currentMode==='save'){saves.push(zone);marker.innerHTML+='<span style="color:#198754; font-weight:bold;">●</span>';document.getElementById('parInput').value=parseInt(document.getElementById('parInput').value||0)+1;document.getElementById('hiddenParadas').value=saves.join(',');}else{goals.push(zone);marker.innerHTML+='<span style="color:#dc3545; font-weight:bold;">●</span>';document.getElementById('gcInput').value=parseInt(document.getElementById('gcInput').value||0)+1;document.getElementById('hiddenGoles').value=goals.join(',');}}
              function incCounter(key){var el=document.getElementById('cnt_'+key); var val=parseInt(el.value||0)+1; el.value=val; document.getElementById('disp_'+key).value=val; updateActionData();}
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
            """))
          )
        )
      )
    );
    renderHtml(content)
  }

  @cask.post("/match-center/save")
  def saveMatch(request: cask.Request) = withAuth(request) {
    // CORRECCIÓN: Usamos request.data para leer los bytes del cuerpo
    val bodyBytes = request.data.readAllBytes()
    val bodyString = new String(bodyBytes, "UTF-8")

    // Parseamos el string clave=valor&clave2=valor2
    val formData = bodyString.split("&").map { part =>
      val pair = part.split("=", 2)
      val key = java.net.URLDecoder.decode(pair(0), "UTF-8")
      val value = if (pair.length > 1) java.net.URLDecoder.decode(pair(1), "UTF-8") else ""
      key -> value
    }.toMap

    // Funciones auxiliares de extracción
    def getStr(key: String): String = formData.getOrElse(key, "")
    def getInt(key: String): Int = try { getStr(key).toInt } catch { case _: Exception => 0 }
    def getDouble(key: String): Double = try { getStr(key).toDouble } catch { case _: Exception => 0.0 }

    // Los 23 parámetros (Extraídos manualmente del mapa)
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
    val mapaCampo = getStr("mapaCampo")

    // --- LÓGICA DE PROCESAMIENTO (Base de datos y cálculos) ---
    val pArr = passData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (pcTot, pcOk, plTot, plOk) = if(pArr.length >= 4) (pArr(0), pArr(1), pArr(2), pArr(3)) else (0,0,0,0)
    val aArr = actionData.split(",").map(s => try s.toInt catch { case _:Exception => 0 })
    val (p1v1, pAir, pPie) = if(aArr.length >= 3) (aArr(0), aArr(1), aArr(2)) else (0,0,0)

    val cleanRival = fixEncoding(rival)
    val cleanNotas = fixEncoding(notas)
    val cleanReaccion = fixEncoding(reaccion)

    val c = DatabaseManager.getLatestCardData()
    val n = StatsCalculator.calculateGrowth(c, minutos, gc, nota, paradas, pcTot, pcOk, plTot, plOk)
    DatabaseManager.updateStats(n)

    if (scheduleId > 0) {
      DatabaseManager.playScheduledMatch(scheduleId, gf, gc, minutos, nota, paradas, cleanNotas, video, cleanReaccion, clima, estadio, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, pcTot, pcOk, plTot, plOk, mapaCampo)
    } else {
      DatabaseManager.logMatch(cleanRival, gf, gc, minutos, nota, n.media, paradas, zonaGoles, zonaTiros, zonaParadas, p1v1, pAir, pPie, clima, estadio, temp, cleanNotas, video, cleanReaccion, fecha, tipo, pcTot, pcOk, plTot, plOk, mapaCampo)
    }

    // Respuesta visual renderizada como Array[Byte] para cumplir con withAuth
    val d = n.media - c.media
    val msg = if(d > 0) s"SUBIDA DE NIVEL! +$d" else "Experiencia acumulada..."

    renderHtml(doctype("html")(
      html(
        head(tags2.style(raw(getCss()))),
        body(style := "background: #1a1a1a; color: white; text-align: center; padding-top: 50px; font-family: 'Oswald';",
          h1("OK"),
          h2(style := "color: #d4af37;", "ANÁLISIS GUARDADO"),
          div(style := "margin: 30px auto; width: 300px; background: #333; padding: 20px; border-radius: 10px;",
            h3("Media Global"),
            div(style := "font-size: 50px; font-weight: bold;", s"${c.media.toInt} -> ${n.media}"),
            p(style := "color: #ffc107;", msg)
          ),
          a(href := "/", style := "color: white; text-decoration: none; border: 1px solid white; padding: 10px 20px;", "Volver a Inicio")
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
  @cask.get("/match/edit/:matchId")
  def editMatchPage(matchId: Int) = {
    val m = DatabaseManager.getMatchById(matchId)
    if (m.isEmpty) {
      cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history"))
    } else {
      val matchData = m.get
      val (gf, gc)  = if (matchData.resultado.contains("-"))
        (matchData.resultado.split("-")(0), matchData.resultado.split("-")(1))
      else ("0", "0")
      val tags = DatabaseManager.getVideoTags(matchId)

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
        div(cls := "alert alert-secondary small", "Añade URL de video para usar tags.")
      }

      val content = basePage("history",
        div(cls := "row justify-content-center",
          div(cls := "col-md-6 col-12",
            div(cls := "card bg-dark text-white border-primary shadow",
              div(cls := "card-header bg-primary text-white fw-bold text-center",
                "EDITAR PARTIDO & VIDEO"),
              div(cls := "card-body p-3",
                form(action := "/match/update", method := "post",
                  attr("accept-charset") := "UTF-8",
                  input(tpe := "hidden", name := "id", value := matchId.toString),
                  div(cls := "mb-3", label("Rival"),
                    input(tpe := "text", name := "rival", value := matchData.rival, cls := "form-control")),
                  div(cls := "mb-3", label("Fecha"),
                    input(tpe := "date", name := "fecha", value := matchData.fecha, cls := "form-control")),
                  div(cls := "row mb-3",
                    div(cls := "col-6", label("GF"),
                      input(tpe := "number", name := "gf", value := gf, cls := "form-control")),
                    div(cls := "col-6", label("GC"),
                      input(tpe := "number", name := "gc", value := gc, cls := "form-control"))
                  ),
                  div(cls := "mb-3", label("Estadio"),
                    input(tpe := "text", name := "estadio", value := matchData.estadio, cls := "form-control")),
                  div(cls := "mb-3", label("Nota"),
                    input(tpe := "number", step := "0.1", name := "nota",
                      value := matchData.nota.toString, cls := "form-control")),
                  div(cls := "mb-3", label("Notas Texto"),
                    textarea(name := "notas", cls := "form-control", rows := "3", matchData.notas)),
                  div(cls := "mb-3", label("Reaccion/Goles"),
                    textarea(name := "reaccion", cls := "form-control", rows := "3", matchData.reaccion)),
                  div(cls := "mb-3", label("Video URL (Youtube)"),
                    input(tpe := "text", name := "video", value := matchData.video, cls := "form-control")),
                  div(cls := "d-grid gap-2 mb-4",
                    button(tpe := "submit", cls := "btn btn-success", "Guardar Cambios"),
                    a(href := "/history", cls := "btn btn-outline-secondary", "Cancelar")
                  )
                )
              ),

              // --- Footer: Diario de voz ---
              div(cls := "card-footer bg-secondary bg-opacity-10 border-top border-secondary mt-3",
                h6(cls := "text-info small fw-bold mb-2", "🎙️ DIARIO DE VOZ (POST-PARTIDO)"),
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
                    onclick := "submitAudio()", disabled := true, "🧠 Analizar Emociones con IA")
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
              )
            ),

            // Script grabación de audio
            script(raw(""" let mediaRecorder; let audioChunks = []; async function toggleRecording() { try { const stream = await navigator.mediaDevices.getUserMedia({ audio: true }); mediaRecorder = new MediaRecorder(stream); mediaRecorder.start(); document.getElementById('btnRecord').style.display='none'; document.getElementById('btnStop').style.display='inline-block'; document.getElementById('btnAnalyze').disabled = true; mediaRecorder.ondataavailable = event => { audioChunks.push(event.data); }; mediaRecorder.onstop = () => { const audioBlob = new Blob(audioChunks, { type: 'audio/webm' }); const audioUrl = URL.createObjectURL(audioBlob); const audioEl = document.getElementById('audioPreview'); audioEl.src = audioUrl; audioEl.style.display = 'block'; const reader = new FileReader(); reader.readAsDataURL(audioBlob); reader.onloadend = () => { document.getElementById('hiddenAudioData').value = reader.result; document.getElementById('btnAnalyze').disabled = false; document.getElementById('btnAnalyze').innerHTML = "🧠 Analizar Grabacion"; }; audioChunks = []; }; } catch(err) { alert('Error microfono: ' + err); } } function stopRecording() { mediaRecorder.stop(); document.getElementById('btnRecord').style.display='inline-block'; document.getElementById('btnStop').style.display='none'; } function handleFileUpload(input) { if (input.files && input.files[0]) { const reader = new FileReader(); reader.onload = function (e) { document.getElementById('hiddenAudioData').value = e.target.result; document.getElementById('audioPreview').src = e.target.result; document.getElementById('audioPreview').style.display = 'block'; document.getElementById('btnAnalyze').disabled = false; document.getElementById('btnAnalyze').innerHTML = "🧠 Analizar Archivo"; }; reader.readAsDataURL(input.files[0]); } } function submitAudio() { document.getElementById('btnAnalyze').innerHTML = "⏳ Procesando... (puede tardar 10s)"; document.getElementById('btnAnalyze').disabled = true; document.getElementById('audioForm').submit(); } """))
          )
        )
      )
      renderHtml(content.render)
    }
  }
  @cask.postForm("/match/analyze_audio")
  def analyzeAudioAction(matchId: Int, audioData: String) = {
    // La lógica de IA y la actualización de la DB ahora ocurren dentro de analyzeAudioLog
    // audioData ya viene como Base64 desde el script del navegador
    DatabaseManager.analyzeAudioLog(matchId, audioData)

    cask.Response(
      "".getBytes("UTF-8"),
      statusCode = 302,
      headers = Seq("Location" -> s"/match/edit/$matchId")
    )
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
  def updateMatchAction(id: Int, rival: String, gf: Int, gc: Int, nota: Double,
                        notas: String, video: String, reaccion: String, fecha: String, estadio: String) = {
    val cleanRival    = fixEncoding(rival)
    val cleanNotas    = fixEncoding(notas)
    val cleanReaccion = fixEncoding(reaccion)
    DatabaseManager.updateMatch(id, cleanRival, gf, gc, 60, nota, "Sol", estadio, 20, cleanNotas, video, cleanReaccion, fecha)
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> "/history"))
  }

  initialize()
}