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
      actionButton("about_nation_pairs", 
                   "About This Page",
                   style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
      p(),
      selectInput("radio_buttons_hist", 
                   "Speeches:",
                   c("Overview" = "overview",
                     "Short Speeches" = "short",
                     "Mid-Range Speeches" = "mid",
                     "Long Speeches" = "long")),
      
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
                  step = 10),
      
      width = 2),
    mainPanel(
      plotlyOutput("mid_range"),
      DTOutput('tbl44')))
)

server <- function(input, output, session) {
  
  output$low_range <- renderPlotly({
    
    hansard <- hansard %>%
      filter(decade == input$decade_hist)
    
    hansard <- hansard %>%
      filter(speech_length > 0,
             speech_length < 50)
    
    d <- hansard %>%
      group_by(speech_length) %>%
      add_count(speech_length)
    #summarize(count = sum(n)) #n()) # You can programatically add count for each row
    
  render_value_low_range_hist(d)
    
    
    plot_ly(x = d$speech_length, 
                 nbinsx = input$bins,
                 type = "histogram",
                 source="XXX",
                 marker = list(color = 'rgb(158,202,225)')) %>%
      layout(
        bargap=0.1) %>%
      config(displayModeBar = F)
    
  })
  
  
  render_value_low_range_hist <- function(d) {
    
    output$tbl444 <- renderDT({
      s <- event_data("plotly_click", source = "XXX")
      
      print(s)
      
      s <- s %>%
        mutate(f = floor(x / 100))
      
      
      s$f <- as.character(s$f)
      s$f <- str_replace(s$f, "$", "000")
      
      s$f <- as.numeric(s$f)
      s$zz <- s$f + 99
      
      bottom <- s$f
      top <- s$zz
      
      print(top)
      print(bottom)
      
      print(d)
      
      d <- d %>%
        filter(speech_length <= top,
               speech_length >= bottom)
      
      print(d)
      
    })  
  }
  
  
  
  
  
  
  
  
  output$mid_range <- renderPlotly({
    
    hansard <- hansard %>%
      filter(decade == input$decade_hist)
    
    hansard <- hansard %>%
      filter(speech_length > 49,
             speech_length < 1000)

    d <- hansard %>%
      group_by(speech_length) %>%
      add_count(speech_length)
      #summarize(count = sum(n)) #n()) # You can programatically add count for each row
    
    render_value_mid_range_hist(d)
    
    
  plot_ly(x = d$speech_length, 
                 nbinsx = input$bins,
                 type = "histogram",
                 source="YYY",
                 marker = list(color = 'rgb(158,202,225)')) %>%
      layout(
        bargap=0.1) %>%
      config(displayModeBar = F)
    
  })
  
  

  
  render_value_mid_range_hist <- function(d){
    
    output$tbl44 <- renderDT({
      s <- event_data("plotly_click", source = "YYY")
      
      s <- s %>%
        mutate(f = floor(x / 1000))
      
      
        s$f <- as.character(s$f)
        s$f <- str_replace(s$f, "$", "000")
        
        s$f <- as.numeric(s$f)
        s$zz <- s$f + 999
        
        bottom <- s$f
        top <- s$zz
        
        print(top)
        print(bottom)
        
        print(d)
        
        d <- d %>%
          filter(speech_length <= top,
                 speech_length >= bottom)
        
        print(d)
      
    })  
  }           
}

shinyApp(ui, server)

# there are enter debates that have enter words 
