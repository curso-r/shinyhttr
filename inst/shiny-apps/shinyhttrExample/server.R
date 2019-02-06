library(shiny)
library(shinyWidgets)
library(httr)
library(shinyhttr)

function(input, output, session) {
  shiny::callModule(
    module = pb,
    id = "pb_id"
  )
}
