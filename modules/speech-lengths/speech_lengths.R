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
          condition = "input.drop_down_hist == 'overview'", ns = ns,
          
          
          
          sidebarPanel(
            HTML("
<div id='speech_lengths-test' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
     <label class='control-label' for='speech_lengths-test'>Select Keyword:</label>
<div class='shiny-options-group'>
     <hr class ='radio'>
     
     Property
     <hr class ='radio'>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='Tenant' checked='checked'>
               <span>Tenant</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='Property'>
               <span>Property</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='Landlord'>
               <span>Landlord</span>
          </label>
     </div>
     
     
     
     Concerns
     <hr class ='radio'>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='ireland'>
               <span>Ireland</span>
          </label>
     </div>
     
     Nations
     <hr class ='radio'>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='ireland'>
               <span>Ireland</span>
          </label>
     </div>
     
     
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='Scotland'>
               <span>Scotland</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='India'>
               <span>India</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='France'>
               <span>France</span>
          </label>
     </div>
     
     Cities
     <hr class = 'radio'>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='four'>
               <span>four</span>
          </label>
     </div>
     
</div>
</div>")),
          
          
          textInput(NS(id, "keyword_search"), 
                    "Custom Search:", 
                    value = "")
          
          ),
        
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
        DTOutput(NS(id, 'speech_lengths_table')),
        textOutput(NS(id, "selection"))))
    
    
  ) }


speech_lengths_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$selection <- renderText({
      input$test})

    viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")
    hansard_speech_lengths <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths.csv") #%>%
    #rename(speech_length = n)
    
    output$speech_lengths <- renderPlotly({
      
      
      if (input$drop_down_hist == "overview") {
        viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")
        
        print(input$test)
        

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
                 xaxis = list(showticklabels = FALSE),
                 xaxis2 = list(showticklabels = FALSE),
                 xaxis3 = list(showticklabels = FALSE),
                 xaxis4 = list(showticklabels = FALSE),
                 xaxis5 = list(showticklabels = FALSE),
                 xaxis6 = list(showticklabels = FALSE),
                 xaxis7 = list(showticklabels = FALSE),
                 xaxis8 = list(showticklabels = FALSE),
                 xaxis9 = list(title = "Short     Mid     Long",
                               showticklabels = FALSE),
                 xaxis10 = list(title = "Short     Mid     Long",
                                showticklabels = FALSE),
                 xaxis11 = list(title = "Short     Mid     Long",
                                showticklabels = FALSE),
                 xaxis12 = list(title = "Short     Mid     Long",
                                showticklabels = FALSE),
                 
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

        
      d %>%
        plot_ly(x = ~speech_length, 
                nbinsx = input$bins,
                type = "histogram",
                source="YYY",
                marker = list(color = 'rgb(158,202,225)')) %>%
          layout(title = paste0(input$drop_down_hist, " Speeches In ", input$decade_hist),
                 bargap = 0.1) %>%
          config(displayModeBar = F) 
        

        
        }
      
    })
    
    
    
    
    
    # 
    # 
    # number_of_debates_from_1803_1910 %>%
    #   highlight_key(~decade) %>%
    #   plot_ly(
    #     x = ~decade, 
    #     y = ~no_of_debates, 
    #     type = 'bar', 
    #     text = ~paste0("Decade: ", "<b>", decade, "</b>", "\n",
    #                    "Number of Debates: ", "<b>", no_of_debates, "</b>", "\n"),
    #     hoverinfo = "text",
    #     marker = list(color = 'rgb(158,202,225)',
    #                   line = list(color = 'rgb(8,48,107)',
    #                               width = 1.5))) %>% 
    #   highlight(on = "plotly_click", off = "plotly_doubleclick") %>%
    #   layout(barmode = "overlay",
    #          title = paste0("The Hansard Parliamentary Debates", "\n", "Debate Count by Decade: 1803—1910"),
    #          xaxis = list(title = ""),
    #          yaxis = list(title = "")) %>%
    #   config(displayModeBar = F) 
    # 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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