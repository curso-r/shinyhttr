#' Add a progress bar compatible with shinyWidgets::progressBar.
#'
#' Add a progress bar to request just like the vanilla httr::progress but with capability to talk to shinyWidgets::progressBar by giving the session and id arguments to it.
#'
#' @param session (from shinyWidgets::updateProgressBar doc) The 'session' object passed to function given to shinyServer.
#' @param id (from shinyWidgets::updateProgressBar doc) An id used to update the progress bar.
#' @param type 	(from httr::progress doc) Type of progress to display: either number of bytes uploaded or downloaded.
#' @param con (from httr::progress doc) Connection to send output too. Usually \code{stdout()} or \code{stderr}.
#' @param title (from shinyWidgets::updateProgressBar doc) character, optional title.
#' @param status (from shinyWidgets::updateProgressBar doc) Color, must be a valid Bootstrap status : primary, info, success, warning, danger.
#' @param range_value (from shinyWidgets::updateProgressBar doc) Default is to display percentage ([0, 100]), but you can specify a custom range, e.g. -50, 50.
#' @param unit_mark (from shinyWidgets::updateProgressBar doc) Unit for value displayed on the progress bar, default to "\%".
#'
#' @export
#'
#' @seealso \code{\link[httr]{progress}}, \code{\link[shinyWidgets]{progressBar}}, \code{\link[shinyWidgets]{updateProgressBar}}
#'
#' @examples
#'
#'## Not run:
#'if (interactive()) {
#' library(shiny)
#' library(shinyWidgets)
#' library(httr)
#'
#' ui <- fluidPage(
#'
#'   sidebarLayout(
#'
#'     NULL,
#'
#'     mainPanel(
#'       actionButton('download', 'Download 100MB file...'),
#'       tags$p("see R console to compare both progress bars."),
#'       progressBar(
#'         id = "pb",
#'         value = 0,
#'         title = "",
#'         display_pct = TRUE
#'       )
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$download, {
#'     GET(
#'       url = "https://speed.hetzner.de/100MB.bin",
#'       progress(session, id = "pb")
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#'}

progress <- function (
  session,
  id,

  type = c("down", "up"),
  con = stdout(),

  title = NULL,
  status = NULL,
  range_value =  NULL,
  unit_mark = "%"
) {
  type <- match.arg(type)
  httr:::request(options = list(
    noprogress = FALSE,
    progressfunction = progress_bar(
      type,
      con,

      session = session,
      id = id,
      title = title,
      status = status,
      range_value = range_value,
      unit_mark = unit_mark
    )
  )
  )
}


#' progress_bar
#'
#' same as httr:::progress_bar but with capability to talk to shinyWidgets::progressBar.
#'
#' @param type 	(from httr::progress doc) Type of progress to display: either number of bytes uploaded or downloaded.
#' @param con (from httr::progress doc) Connection to send output too. Usually \code{stdout()} or \code{stderr}.
#' @param session (from shinyWidgets::updateProgressBar doc) The 'session' object passed to function given to shinyServer.
#' @param id (from shinyWidgets::updateProgressBar doc) An id used to update the progress bar.
#' @param title (from shinyWidgets::updateProgressBar doc) character, optional title.
#' @param status (from shinyWidgets::updateProgressBar doc) Color, must be a valid Bootstrap status : primary, info, success, warning, danger.
#' @param range_value (from shinyWidgets::updateProgressBar doc) Default is to display percentage ([0, 100]), but you can specify a custom range, e.g. -50, 50.
#' @param unit_mark (from shinyWidgets::updateProgressBar doc) Unit for value displayed on the progress bar, default to "\%".
#'
#'
#' @return a function with rules to print out the progress.
#'
#' @seealso \code{\link{progress}}
#'
progress_bar <- function (
  type,
  con,

  session,
  id,
  title = NULL,
  status = NULL,
  range_value =  NULL,
  unit_mark = "%"
) {
  bar <- NULL
  show_progress <- function(down, up) {
    if (type == "down") {
      total <- down[[1]]
      now <- down[[2]]
    }
    else {
      total <- up[[1]]
      now <- up[[2]]
    }

    if (total == 0 && now == 0) {
      bar <<- NULL
    }
    else if (total == 0) {
      cat("\rDownloading: ", bytes(now, digits = 2), "     ",
          sep = "", file = con)
      utils::flush.console()
    }
    else {
      if (is.null(bar)) {
        bar <<- utils::txtProgressBar(max = total, style = 3,
                                      file = con)
      }
      utils::setTxtProgressBar(bar, now)
      if (now == total)
        close(bar)
    }

    shinyWidgets::updateProgressBar(
      session = session,
      id = id,
      value = now,
      total = total,
      title = title,
      status = status,
      range_value = range_value,
      unit_mark = unit_mark
    )

    TRUE
  }
  show_progress
}

