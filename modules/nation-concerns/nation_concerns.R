nation_concerns_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2), 
      
      mainPanel(plotlyOutput(NS(id, "nation_concerns"))))) }


nation_concerns_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    c <- fread("~/projects/hansard-shiny/app-data/nations/treemap_1800.csv")
    output$nation_concerns <- renderPlotly({
      
      a <- c$a
      b <- c$b
      values <- c$d
      
      plot_ly(
        type="treemap",
        labels = a,
        parents = b,
        values = values) %>%
        layout(height = 600, width = 1000, treemapcolorway=c("3CBB75FF","2D708EFF", "FDE725FF", "481567FF", "1F968BFF")) %>%
        config(displayModeBar = F) })
    
    }) }
