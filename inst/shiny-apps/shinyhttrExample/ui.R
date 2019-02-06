library(shiny)
library(shinyWidgets)
library(httr)
library(shinyhttr)

source("modules/pb.R")

fluidPage(
  sidebarLayout(
    NULL,
    mainPanel(
      h3("shinyhttr example working with shiny modules."),
      pbUI(id = "pb_id")
    )
  )
)
