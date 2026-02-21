import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object AdminController extends cask.Routes {

  @cask.get("/settings") def settingsPage() = {
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
                option(value:="POR", if(card.posicion=="POR") attr("selected"):="true" else attr(""), "Portero (POR)"),
                option(value:="GK",  if(card.posicion=="GK")  attr("selected"):="true" else attr(""), "Goalkeeper (GK)")
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
    )));
    renderHtml(basePage("settings", content))
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
  def adminPage() = {
    val objs = DatabaseManager.getSeasonObjectives()
    val content = basePage("settings",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-danger text-center mb-4", "ADMINISTRACION"),
          div(cls := "card bg-dark border-warning shadow mb-4 p-3",
            h5(cls := "text-warning", "Base de Datos Leyendas"),
            p(cls := "small text-muted fw-bold", "Si no ves la comparacion en Trayectoria, pulsa aqui."),
            a(href := "/admin/init_legends", cls := "btn btn-outline-warning w-100 fw-bold",
              "Inicializar BBDD Leyendas")
          ),
          div(cls := "card bg-secondary bg-opacity-25 border-secondary mb-4 p-3",
            h5(cls := "text-white", "Copia de Seguridad"),
            p(cls := "small text-muted fw-bold", "Descarga todos los partidos en formato Excel/CSV."),
            a(href := "/admin/download_csv", cls := "btn btn-primary w-100 fw-bold", "Descargar CSV")
          ),
          div(cls := "card bg-secondary bg-opacity-25 border-secondary mb-4 p-3",
            h5(cls := "text-white", "Informe PDF"),
            p(cls := "small text-muted fw-bold", "Genera un informe limpio para imprimir o guardar como PDF."),
            a(href := "/admin/print_report", target := "_blank", cls := "btn btn-info w-100 fw-bold",
              "Generar Informe")
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
            div(span(cls := "text-warning", "G"), " GUARDIAN ELITE"),
            div(cls:="d-flex align-items-center gap-3",
              a(href:="/logout", style:="text-decoration:none; color:#ff4d4d; font-size:11px; font-weight:bold; border: 1px solid #ff4d4d; padding: 2px 8px; border-radius: 4px;", "SALIR"),
              a(href:="/settings", style:="text-decoration:none; color:white; font-size:24px;", "⚙️")
            )
          ),
          div(cls := "container main-content", pageContents), tags2.nav(cls := "bottom-nav", a(href:="/", cls:=s"nav-item ${if(activeLink=="home") "active" else ""}", div(cls:="nav-icon", "H"), span(cls:="nav-label", "Inicio")), a(href:="/match-center", cls:=s"nav-item ${if(activeLink=="match-center") "active" else ""}", div(cls:="nav-icon", "P"), span(cls:="nav-label", "Jugar")), a(href:="/bio", cls:=s"nav-item ${if(activeLink=="bio") "active" else ""}", div(cls:="nav-icon", "B"), span(cls:="nav-label", "Bio")), a(href:="/career/legacy", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon text-warning", "⭐"), span(cls:="nav-label text-warning", "Legado")), a(href:="/tactics", cls:=s"nav-item ${if(activeLink=="tactics") "active" else ""}", div(cls:="nav-icon", "ℹ️"), span(cls:="nav-label", "Pizarra")), a(href:="/career", cls:=s"nav-item ${if(activeLink=="career") "active" else ""}", div(cls:="nav-icon", "T"), span(cls:="nav-label", "Trayect.")), a(href:="/history", cls:=s"nav-item ${if(activeLink=="history") "active" else ""}", div(cls:="nav-icon", "L"), span(cls:="nav-label", "Historial"))))
      ).render
  }

  def getCss() = """
    body { background-color: #121212; color: #f0f0f0; font-family: 'Oswald', sans-serif; padding-bottom: 80px; margin: 0; font-weight: 500; }

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

  initialize()
}