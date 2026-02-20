import cask._
import scalatags.Text.all._
import scalatags.Text.tags2
import SharedLayout._

object HistoryController extends cask.Routes {

  @cask.get("/scouting")
  def scoutingPage(request: cask.Request, query: String = "") = withAuth(request) {

    val (matches, stats) = if (query.nonEmpty) DatabaseManager.getRivalScouting(query)
                           else (List[MatchLog](), Map[String, Int]())
    val rivalInfo = if (query.nonEmpty) DatabaseManager.getRivalInfo(query) else None

    // --- Widget ficha del rival (editable) ---
    val rivalCardWidget = if (query.nonEmpty) {
      val estiloVal = rivalInfo.map(_.estilo).getOrElse("Desconocido")
      val clavesVal = rivalInfo.map(_.claves).getOrElse("")
      val notasVal  = rivalInfo.map(_.notas).getOrElse("")
      div(cls := "card bg-dark border-secondary shadow mb-4",
        div(cls := "card-header bg-secondary text-white fw-bold", "FICHA RIVAL"),
        div(cls := "card-body",
          form(action := "/scouting/save_rival", method := "post",
            input(tpe := "hidden", name := "nombre", value := query),
            div(cls := "mb-2",
              label(cls := "small text-muted fw-bold", "Estilo"),
              select(name := "estilo", cls := "form-select form-select-sm bg-dark text-white fw-bold",
                option(value := "Desconocido", "?"),
                option(value := "Directo",     "Balon Largo"),
                option(value := "Combinativo", "Toque"),
                option(value := "Contra",      "Contraataque"),
                attr("value") := estiloVal
              )
            ),
            div(cls := "mb-2",
              label(cls := "small text-muted fw-bold", "Claves"),
              textarea(name := "claves", cls := "form-control form-control-sm bg-dark text-white fw-bold",
                rows := "2", fixEncoding(clavesVal))
            ),
            div(cls := "mb-2",
              label(cls := "small text-muted fw-bold", "Notas"),
              textarea(name := "notas", cls := "form-control form-control-sm bg-dark text-white fw-bold",
                rows := "2", fixEncoding(notasVal))
            ),
            button(tpe := "submit", cls := "btn btn-sm btn-outline-warning w-100 fw-bold", "Guardar Ficha")
          )
        )
      )
    } else div()

    // --- Widget resultados vs el rival ---
    val resultsWidget = if (query.nonEmpty && matches.isEmpty) {
      div(cls := "alert alert-warning text-center", s"Sin datos vs '$query'")
    } else if (matches.nonEmpty) {
      val rows = for (m <- matches) yield {
        val extra = if (m.video.nonEmpty)
          a(href := m.video, target := "_blank", cls := "btn btn-sm btn-outline-danger w-100", "Video")
        else span("")
        div(cls := "card bg-dark border-secondary shadow mb-3",
          div(cls := "card-body",
            div(cls := "d-flex justify-content-between align-items-center mb-2",
              div(strong(cls := "text-warning", m.fecha), span(cls := "ms-2 badge bg-secondary", m.clima)),
              div(cls := "fs-5 fw-bold text-white", m.resultado)
            ),
            if (m.estadio.nonEmpty) div(cls := "small text-muted mb-2 fw-bold", s"📍 ${m.estadio}"),
            if (m.notas.nonEmpty)   div(cls := "alert alert-dark border-secondary p-2 small text-light fst-italic mb-2 fw-bold",
              s"Nota: ${fixEncoding(m.notas)}"),
            extra
          )
        )
      }
      div(
        div(cls := "card bg-secondary bg-opacity-25 border-info mb-4 p-3",
          h5(cls := "text-center text-white mb-3", s"Vs ${matches.head.rival}"),
          div(cls := "d-flex justify-content-around text-center text-white",
            div(h3(stats("pj")),              span(cls := "small text-muted", "PJ")),
            div(h3(cls := "text-success", stats("ganados")), span(cls := "small text-muted", "G")),
            div(h3(cls := "text-danger",  stats("gc")),      span(cls := "small text-muted", "GC"))
          )
        ),
        h6(cls := "text-white border-bottom border-secondary pb-2 mb-3", "Partidos"),
        div(rows)
      )
    } else div(cls := "text-center text-muted mt-5", "Busca un rival...")

    val content = basePage("scouting",
      div(cls := "row justify-content-center",
        div(cls := "col-md-8 col-12",
          h2(cls := "text-info mb-4 text-center", "SCOUTING"),
          form(action := "/scouting", method := "get", cls := "mb-4",
            div(cls := "input-group",
              input(tpe := "text", name := "query",
                cls := "form-control form-control-lg bg-dark text-white border-secondary fw-bold",
                placeholder := "Nombre equipo", value := query),
              button(tpe := "submit", cls := "btn btn-info fw-bold", "Buscar")
            )
          ),
          rivalCardWidget,
          resultsWidget
        )
      )
    )
    renderHtml(content)
  }
  @cask.postForm("/scouting/save_rival")
  def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String) = {
    DatabaseManager.saveRivalInfo(fixEncoding(nombre), estilo, fixEncoding(claves), fixEncoding(notas))
    cask.Response("".getBytes("UTF-8"), statusCode = 302, headers = Seq("Location" -> s"/scouting?query=$nombre"))
  }

  @cask.get("/history")
  def historyPage(request: cask.Request) = withAuth(request) {
    val matches = DatabaseManager.getMatchesList()

    // 1. Generamos las filas de la tabla
    val tableRows = if (matches.isEmpty) {
      Seq(tr(td(colspan := 4, cls := "text-center p-4", "Sin partidos")))
    } else {
      matches.map(m => renderMatchRow(m))
    }

    // 2. Definimos el contenido central (SIN llamar a basePage aquí)
    val mainContent = div(cls := "row justify-content-center",
      div(cls := "col-md-10 col-12",
        h2(cls := "text-warning mb-3 text-center", "HISTORIAL"),
        div(cls := "card shadow-sm border-0",
          div(cls := "card-body p-0",
            table(cls := "table table-hover tm-table mb-0",
              thead(tr(
                th("Rival"),
                th(cls:="text-center", "Res"),
                th(cls:="text-center", "Nota"),
                th(cls:="text-end", "Acción")
              )),
              tbody(tableRows)
            )
          )
        )
      )
    )

    // 3. Renderizamos llamando a basePage UNA SOLA VEZ
    renderHtml(basePage("history", mainContent))
  }

  initialize()
}
