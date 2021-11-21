source("/home/stephbuon/projects/hansard-shiny/modules/speech-lengths/speech_length_annotations.txt")

speech_lengths_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_speech_lengths"), 
                     "About This Page",
                     style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
        p(),
        selectInput(NS(id, "drop_down_hist"), 
                    "Speeches:",
                    c("Overview" = "overview",
                      "Short Speeches (1-49 words)" = "Short (1-49 Word)",
                      "Mid-Length Speeches (50-999 words) " = "Mid-Length (50-999 Word)",
                      "Long Speeches (1000+ words)" = "long")),
        
        
        conditionalPanel(
          condition = "input.drop_down_hist != 'overview'", ns = ns,
          sliderTextInput(
            inputId = NS(id, "decade_hist"), 
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
          
          sliderInput(inputId = NS(id, "bins"),
                      label = "Number of bins:",
                      min = 5,
                      max = 25,
                      value = 15, 
                      step = 10) ),
        
        width = 2),
      
      mainPanel(
        plotlyOutput(NS(id, "speech_lengths")),
        DTOutput(NS(id, 'speech_lengths_table'))))
    
    
  ) }


speech_lengths_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")
    hansard_speech_lengths <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths.csv") #%>%
    #rename(speech_length = n)
    
    output$speech_lengths <- renderPlotly({
      
      if (input$drop_down_hist == "overview") {
        viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")

        splitted_list <- split(viz, viz$decade)
        plot_list <- lapply(splitted_list, 
                            plot_ly, 
                            x = ~speech_length_type, 
                            y = ~n, 
                            type = 'bar', 
                            marker = list(color = c('rgb(135, 206, 235)', 'rgb(70, 130, 180)', 'rgb(15, 82, 186)'))) 
        fig <- subplot(plot_list, nrows = 3, margin = c(.02, .02, .03, .03)) %>%
          layout(title = "Speech Lengths from 1800 to 1910",
                 plot_bgcolor='#e5ecf6')
        
        fig %>%
          layout(annotations = annotations,
                 height = 650,
                 margin = list(t = 50),
                 showlegend = F,
                 yaxis = list(range = c(0, 100000)),
                 yaxis2 = list(range = c(0, 100000)),
                 yaxis3 = list(range = c(0, 100000)),
                 yaxis4 = list(range = c(0, 100000)),
                 yaxis5 = list(range = c(0, 100000)),
                 yaxis6 = list(range = c(0, 100000)),    
                 yaxis7 = list(range = c(0, 100000)),    
                 yaxis8 = list(range = c(0, 100000)),    
                 yaxis9 = list(range = c(0, 100000)),
                 yaxis10 = list(range = c(0, 100000)),    
                 yaxis11 = list(range = c(0, 100000)),    
                 yaxis12 = list(range = c(0, 100000))) %>%
          config(displayModeBar = F)
        
        
         
        # fig_1910 <- plot_ly(data_1910, 
        #                     x = ~speech_length_type,
        #                     y = ~n, 
        #                     type = 'bar',
        #                     marker = list(color = c('rgb(135, 206, 235)', 'rgb(70, 130, 180)', 'rgb(15, 82, 186)'))) %>%
        # layout(xaxis = list(zerolinecolor = '#ffff', 
        #                     zerolinewidth = 2, 
        #                     gridcolor = 'ffff'),
        #        yaxis = list(range = c(0, 130000)))
        
        
        
        
      } else {
        
        
        hansard_speech_lengths <- hansard_speech_lengths %>%
          filter(decade == input$decade_hist) %>%
          select(-decade)
        
        hansard_speech_lengths <- hansard_speech_lengths %>%
          filter(case_when(input$drop_down_hist == "Short (1-49 Word)" ~ speech_length > 0 & speech_length < 50, 
                           input$drop_down_hist == "Mid-Length (50-999 Word)" ~ speech_length > 49 & speech_length < 1000,
                           input$drop_down_hist == "long" ~ speech_length > 999))
        
        
        d <- hansard_speech_lengths %>%
          add_count(speech_length) 
        
        render_value_range_hist(d) 
        
        plot_ly(x = d$speech_length, 
                nbinsx = input$bins,
                type = "histogram",
                source="YYY",
                marker = list(color = 'rgb(158,202,225)')) %>%
          layout(title = paste0(input$drop_down_hist, " Speeches In ", input$decade_hist),
                 bargap = 0.1) %>%
          config(displayModeBar = F) }
      
    })
    
    
    
    
    render_value_range_hist <- function(d){
      
      output$speech_lengths_table <- renderDT({
        
        if (input$drop_down_hist == "overview") {
          
        } else {
          
          s <- event_data("plotly_click", source = "YYY")
          
          validate(need(!is.null(s), "Click on a bar to view binned speeches"))
          
          
          if (input$drop_down_hist == "Mid-Length (50-999 Word)") {
            
            if (input$bins == 15) {
              top <- s$x + 50.5
              bottom <- s$x - 49.5 }
            else if (input$bins == 5) {
              top <- s$x + 99.5
              bottom <- s$x - 99.5 } 
            else if (input$bins == 25) {
              top <- s$x + 24.5
              bottom <- s$x - 24.5 } } 
          
          else if (input$drop_down_hist == "Short (1-49 Word)") {
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
          
          datatable(d,
                    options = list(dom = 'ip'),
                    filter = list(position = "top"))
        }
        
      })  
    }
    
    
  } ) }