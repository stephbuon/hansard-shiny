library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)

hansard <- read_csv("~/projects/hansard-shiny/speech_lengths.csv") %>%
  rename(speech_length = n)

# 1 - 1000, 1000 - 10000, 10000- end 

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("about_speech_lengths", 
                   "About This Page",
                   style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
      p(),
      selectInput("drop_down_hist", 
                   "Speeches:",
                   c("Overview" = "overview",
                     "Short Speeches (1-49 words)" = "short",
                     "Mid-Range Speeches (50-999 words) " = "mid",
                     "Long Speeches (1000+ words)" = "long")),
      

      conditionalPanel(
        condition = "input.drop_down_hist != 'overview'",
        sliderTextInput(
          inputId = "decade_hist", 
          label = "Decade: ", 
          grid = TRUE, 
          force_edges = TRUE,
          choices = c("1800",
                      "1810", 
                      "1820", 
                      "1830", 
                      "1840",
                      "1850", 
                      "1860",
                      "1870",
                      "1880",
                      "1890")),
        
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 5,
                    max = 25,
                    value = 15, 
                    step = 10) ),
      
      width = 2),
    
    mainPanel(
      plotlyOutput("value_plot"),
      DTOutput('speech_lengths_table')))
)

server <- function(input, output, session) {
  
  
  output$value_plot <- renderPlotly({
    
    if (input$drop_down_hist == "overview") {
      
      viz <- read_csv("~/projects/hansard-shiny/speech_lengths_overview.csv")
      
      labs <- c("Short", "Mid-Range", "Long")
      
      plot <- ggplot(viz, 
                     aes(x = speech_length_type,
                         y = n,
                         fill = `Speech Length`,
                         show.legend = FALSE)) + 
        geom_bar(stat="identity") + 
        scale_x_discrete(labels = labs) +
        scale_y_continuous(labels = comma) +
        facet_wrap(~decade) +
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        theme_bw() 
      
      ggplotly(plot) %>%
        layout(height = 750) %>%
        config(displayModeBar = F)
      
      
      
      
    } else {
      
    
    hansard <- hansard %>%
      filter(decade == input$decade_hist)
    
    
    if (input$drop_down_hist == "short") {
      hansard <- hansard %>%
        filter(speech_length > 0,
               speech_length < 50) } 
    else if (input$drop_down_hist == "mid") {
      hansard <- hansard %>%
        filter(speech_length > 49,
               speech_length < 1000) }
    else if (input$drop_down_hist == "long") {
      hansard <- hansard %>%
        filter(speech_length > 999) }

    d <- hansard %>%
      group_by(speech_length) %>%
      add_count(speech_length)

    render_value_range_hist(d) 
    
  plot_ly(x = d$speech_length, 
                 nbinsx = input$bins,
                 type = "histogram",
                 source="YYY",
                 marker = list(color = 'rgb(158,202,225)')) %>%
      layout(
        bargap=0.1) %>%
      config(displayModeBar = F) }
    
  })
  
  

  
  render_value_range_hist <- function(d){
    
    output$speech_lengths_table <- renderDT({
      
      if (input$drop_down_hist == "overview") {
      } else {
      
      s <- event_data("plotly_click", source = "YYY")
      
      if (input$drop_down_hist == "mid") {
        
        if (input$bins == 15) {
          top <- s$x + 50.5
          bottom <- s$x - 49.5 }
        else if (input$bins == 5) {
          top <- s$x + 99.5
          bottom <- s$x - 99.5 } 
        else if (input$bins == 25) {
          top <- s$x + 24.5
          bottom <- s$x - 24.5 } } 
      
      else if (input$drop_down_hist == "short") {
        if (input$bins == 15) {
          top <- s$x + 3
          bottom <- s$x - 3 } 
        else if (input$bins == 25) { 
          top <- s$x + 1.5
          bottom <- s$x - 1.5 }
        else if (input$bins == 5) {
          top <- s$x + 5.5
          bottom <- s$x - 5.5 } }
      
      else if (input$drop_down_hist == "long") {
        if (input$bins == 15) {
        print(s) }
        
      }
     
      d <- d %>%
        filter(speech_length < top,
               speech_length > bottom)
      }
      
    })  
  }           
}

shinyApp(ui, server)

# there are enter debates that have enter words 

