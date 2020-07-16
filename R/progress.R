#' Add a progress bar compatible with `shinyWidgets::updateProgressBar()`.
#'
#' Add a progress bar to request just like the vanilla `httr::progress()` but with capability to talk to `shinyWidgets::updateProgressBar()` by giving the session and id arguments to it.
#'
#' @param session (from `shinyWidgets::updateProgressBar()`` doc) The 'session' object passed to function given to shinyServer.
#' @param id (from `shinyWidgets::updateProgressBar()` doc) An id used to update the progress bar.
#' @param type 	(from `httr::progress()`` doc) Type of progress to display: either number of bytes uploaded or downloaded.
#' @param con (from `httr::progress()` doc) Connection to send output too. Usually \code{stdout()} or \code{stderr}.
#' @param title (from `shinyWidgets::updateProgressBar()` doc) character, optional title.
#' @param status (from `shinyWidgets::updateProgressBar()` doc) Color, must be a valid Bootstrap status : primary, info, success, warning, danger.
#' @param range_value (from `shinyWidgets::updateProgressBar()` doc) Default is to display percentage ([0, 100]), but you can specify a custom range, e.g. -50, 50.
#' @param unit_mark (from `shinyWidgets::updateProgressBar()` doc) Unit for value displayed on the progress bar, default to "\%".
#'
#' @export
#' 
#'
#' @seealso \code{\link[httr:progress]{progress}}, \code{\link[shinyWidgets:progressBar]{progressBar}}, \code{\link[shinyWidgets:updateProgressBar]{updateProgressBar}}
#'
#' @examples
#'
#' if (interactive()) {
#'   
#'   library(shiny)
#'   library(shinyWidgets)
#'   library(shinyhttr)
#'   
#'   ui <- fluidPage(
#'     
#'     sidebarLayout(
#'       
#'       NULL,
#'       
#'       mainPanel(
#'         actionButton('download', 'Download 100MB file...'),
#'         tags$p("see R console to compare both progress bars."),
#'         progressBar(
#'           id = "pb",
#'           value = 0,
#'           title = "",
#'           display_pct = TRUE
#'         )
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     observeEvent(input$download, {
#'       httr::GET(
#'         url = "https://speed.hetzner.de/100MB.bin",
#'         progress(session, id = "pb")
#'       )
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#' }
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
  request <- utils::getFromNamespace("request", "httr")
  type <- match.arg(type)
  request(options = list(
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
#' Same as `httr:::progress_bar()` but with capability to talk to `shinyWidgets::progressBar()`.
#'
#' @param type 	(from `httr::progress()` doc) Type of progress to display: either number of bytes uploaded or downloaded.
#' @param con (from `httr::progress()` doc) Connection to send output too. Usually \code{stdout()} or \code{stderr}.
#' @param session (from `shinyWidgets::updateProgressBar()` doc) The 'session' object passed to function given to shinyServer.
#' @param id (from `shinyWidgets::updateProgressBar()` doc) An id used to update the progress bar.
#' @param title (from `shinyWidgets::updateProgressBar()` doc) character, optional title.
#' @param status (from `shinyWidgets::updateProgressBar()` doc) Color, must be a valid Bootstrap status : primary, info, success, warning, danger.
#' @param range_value (from `shinyWidgets::updateProgressBar()` doc) Default is to display percentage ([0, 100]), but you can specify a custom range, e.g. -50, 50.
#' @param unit_mark (from `shinyWidgets::updateProgressBar()` doc) Unit for value displayed on the progress bar, default to "\%".
#'
#'
#' @return a function with rules to print out the progress.
#'
#' @seealso \code{\link[shinyhttr]{progress}}, \code{\link[shinyWidgets:progressBar]{progressBar}}
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
  bytes <- utils::getFromNamespace("bytes", "httr")
  bar <- NULL
  show_progress <- function(down, up) {
    if (type == "down") {
      total <- down[[1]]
      now <- down[[2]]
    } else {
      total <- up[[1]]
      now <- up[[2]]
    }
    if (total == 0 && now == 0) {
      bar <<- NULL
    } else if (total == 0) {
      cat("\rDownloading: ", bytes(now, digits = 2), "     ",
          sep = "", file = con)
      utils::flush.console()
    } else {
      if (is.null(bar)) {
        bar <<- utils::txtProgressBar(max = total, style = 3, file = con)
      }
      utils::setTxtProgressBar(bar, now)
      if (now == total)
        close(bar)
    }
    shinyWidgets::updateProgressBar(
      session = session,
      id = session$ns(id),
      value = round(now/total * 100, 0),
      title = title,
      status = status,
      range_value = range_value,
      unit_mark = unit_mark
    )
    
    TRUE
  }
  show_progress
}


#' runExample
#'
#' Launch shiny example application using shinyhttr::progress_bar. This app also uses module to show that it works with it too.
#'
#' @param display.mode The mode in which to display the example. Defaults to showcase, but may be set to normal to see the example without code or commentary.
#' 
#' @export
runExample <- function(display.mode = "showcase") {
  appDir <- system.file("shiny-apps", "shinyhttrExample", package = "shinyhttr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyhttr`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = display.mode)
}
