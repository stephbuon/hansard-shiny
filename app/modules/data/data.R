data_ui <- function(id) {
  tagList(
    
fluidPage(
  fluidRow(
    column(10,
           offset = 1,
           h3("Data"),
           br(),
           p(),
           "This page offers links to download the data used by this app.",
           br(),
           p(),
           strong("Hansard:"),
           "SMU version of the Hansard data with improved speaker names.",
           br(),
           p(),
           strong("Nations:"),
           "placeholder description",
           br(),
           p(),
           strong("Stop Words:"),
           "")
  ) ) ) }