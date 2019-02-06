# UI ----------------------------------#

pbUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    actionButton(ns('download'), 'Download 100MB file...'),
    tags$p('see R console to compare both progress bars.'),
    progressBar(
      id = ns('pb'),
      value = 0,
      title = '',
      display_pct = TRUE
    )
  )

}

# Server ----------------------------------#

pb <- function(input, output, session) {
  observeEvent(input$download, {
    GET(
      url = 'https://speed.hetzner.de/100MB.bin',
      shinyhttr::progress(session, id = 'pb') 
    )
  })
}