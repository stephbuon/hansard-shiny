# get the count for each keyword
# show a line chart for frequenty over each decade, waithin the actual debate text

# search_ngrams <- function(input_kw) {
#   
#   input_dir <- "/home/stephbuon/projects/hansard-shiny/app-data/debate-text/tokenized_hansard_count"
#   
#   files <- list.files(path = input_dir, pattern = "*.csv", full.names = TRUE)
#   
#   decades <- c("1800", "1810", "1820", "1830", "1840", "1850", "1860", "1870", "1880",
#                "1890", "1900", "1910")
#   
#   
#   all <- data.frame()
#   
#   for(file in files) {
#     
#     tokenized_decade <- fread(file)
#     
#     tokenized_decade <- tokenized_decade %>%
#       filter(ngrams == input_kw)
#     
#     all <- bind_rows(all, tokenized_decade) }
#   
#   
#   return(all) }


















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
                    list(Overview = list("overview" = "overview"),
                         `Speeches Histogram` = list("Short Speeches (1-49 words)" = "Short (1-49 Word)",
                                                    "Mid-Length Speeches (50-999 words) " = "Mid-Length (50-999 Word)",
                                                    "Long Speeches (1000+ words)" = "long"))),
        
        conditionalPanel(
          condition = "input.drop_down_hist == 'overview'", ns = ns,
          
            HTML("
<div id='speech_lengths-test' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
     <label class='control-label' for='speech_lengths-test'>Select Keyword:</label>
<div class='shiny-options-group'>
     <hr class ='radio'>
     
     Property
     <div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='tenant' checked='checked'>
               <span>Tenant</span>
          </label>
     </div>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='property'>
               <span>Property</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='landlord'>
               <span>Landlord</span>
          </label>
     </div>
     
     
     
     Concerns
     <div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='poor'>
               <span>Poor</span>
          </label>
     </div>
     </div>
     <div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='coal'>
               <span>Coal</span>
          </label>
     </div>
     </div> 
     
     
     
     Nations
     <div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='ireland'>
               <span>Ireland</span>
          </label>
     </div>
     </div>
     
     
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='scotland'>
               <span>Scotland</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='india'>
               <span>India</span>
          </label>
     </div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='france'>
               <span>France</span>
          </label>
     </div>
     
     Cities
     <div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='dublin'>
               <span>Dublin</span>
          </label>
     </div>
     </div>
     <div>
     <div class='radio'>
          <label>
               <input type='radio' name='speech_lengths-test' value='london'>
               <span>London</span>
          </label>
     </div>
     </div>
     
     
     
     
     
     
     
     
</div>
</div>"
                 
                 
                 
                 
                 
                 ),
          
          
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
        br(), br(),
        br(), br(),
        br(), br(),
        br(), br(),
        br(), br(),
        br(), br(),
        plotlyOutput(NS(id, "plot_2"))))
        #textOutput(NS(id, "selection"))))
    
    
  ) }


speech_lengths_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")
    hansard_speech_lengths <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths.csv") #%>%
    #rename(speech_length = n)
    
    output$speech_lengths <- renderPlotly({
      
      
      if (input$drop_down_hist == "overview") {
        viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")
        
        
        #render_text(input$test) 
        
        # aaa <- search_ngrams(input$test)
        # 
        # print(aaa)
        
        #plot_2_()
        
        # mid number / total number 
        # x 100
        
        
        
        
        splitted_list <- split(viz, viz$decade)
        plot_list <- lapply(splitted_list, 
                            plot_ly, 
                            x = ~speech_length_type, 
                            y = ~n, 
                            type = 'bar', 
                            marker = list(color = c('rgb(135, 206, 235)', 'rgb(70, 130, 180)', 'rgb(15, 82, 186)'))) 
        fig <- subplot(plot_list, nrows = 3, margin = c(.02, .02, .03, .03)) %>%
          layout(title = "Speech Lengths from 1800 to 1910",
                 plot_bgcolor='#e5ecf6') %>%
          config(displayModeBar = F)
        
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
                 yaxis12 = list(range = c(0, 100000))) 
        
        
         
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
    
    
    
    
    
    
    render_text <- function(d){
       output$selection <- renderText({
         
         if (input$drop_down_hist == "overview") {
           d
         } })  }
    
    
    

      
    
      output$plot_2 <- renderPlotly({
        
        if (input$drop_down_hist == "overview") {
          
          word <- input$test
          
          a <- fread("/home/stephbuon/projects/hansard-shiny/app-data/debate-text/tokenized_hansard_count/tokenized_hansard_count_all_decades_speech_length_type_0.csv") %>%
            filter(ngrams == word)
          
          
          b <- fread("/home/stephbuon/projects/hansard-shiny/app-data/debate-text/tokenized_hansard_count/tokenized_hansard_count_all_decades_speech_length_type_1.csv") %>%
            filter(ngrams == word)
          
          
          c <- fread("/home/stephbuon/projects/hansard-shiny/app-data/debate-text/tokenized_hansard_count/tokenized_hansard_count_all_decades_speech_length_type_2.csv") %>%
            filter(ngrams == word)
          
          
          all <- bind_rows(a, b, c)
          
        f <-  all %>%
            group_by(decade, speech_length_type) %>%
            summarise(n = sum(token_count)) %>%
            mutate(prop = n / sum(n)) %>%
            ungroup()
          
        
        q <- f %>%
          filter(speech_length_type == "0")
        
        w <- f %>%
          filter(speech_length_type == "1")
        
        e <- f %>%
          filter(speech_length_type == "2")
        
        
        fig <- plot_ly(q, 
                       x = ~decade, 
                       y = ~prop, 
                       type = 'scatter', 
                       mode = 'lines') %>%
          add_trace(data=w, name="line1", x = ~decade, y = ~prop) %>%
          add_trace(data=e, name="line2", x = ~decade, y = ~prop) %>%
          config(displayModeBar = F)
        
          
          
          # fig <- plot_ly(a, 
          #                x = ~decade, 
          #                y = ~token_count, 
          #                type = 'scatter', 
          #                mode = 'lines') %>%
          #   add_trace(data=b, name="line1", x = ~decade, y = ~token_count) %>%
          #   add_trace(data=c, name="line2", x = ~decade, y = ~token_count) %>%
          #   config(displayModeBar = F)
          # 
          fig
          
          
          
          
          
        
        
        }
        
        
        
        
      })
      
    #}
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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