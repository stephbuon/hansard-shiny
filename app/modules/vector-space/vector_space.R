vector_space_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        helpText("Say something meaningful"),
        width = 2),
      mainPanel(plotlyOutput(NS(id, "vector_space"))))
    
  )}

vector_space_server <- function(id) {
  moduleServer(id, function(input, output, session) { 
    
    forplot11 <- fread(paste0(data_dir, "word_embeddings/for_plot.csv"))
    
    output$vector_space <- renderPlotly({
      
      plot_ly(data = forplot11, 
              x = ~V1, 
              y = ~V2, 
              type = 'scatter', 
              mode = 'text',
              text = ~word) %>% 
        layout(#autosize = F,
          xaxis = list(title = "Placeholder"),
          yaxis = list(title = "Placeholder")) %>%
        config(displayModeBar = F) })
    
  } ) }