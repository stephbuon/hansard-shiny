code_ui <- function(id) {
  tagList(

fluidPage(
  fluidRow(
    column(10,
           offset = 1,
           h3("Code"),
           br(), 
           p(),
           "Our project values transparency, precision, and innovation. 
                                 
                                 
                                            All of our code is open source and can be found on our",
           HTML(" <a href='https://github.com/stephbuon/hansard-shiny'>hansard-shiny</a> GitHub repository."),
           #HTML("<ul><li>Transparency</li><li>...more text...</li></ul>"),
           "Transparency.",
           br(),
           p(),
           "For the source code, enter"),
  ) ) )}