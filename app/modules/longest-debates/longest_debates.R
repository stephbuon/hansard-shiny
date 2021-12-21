longest_debates_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        width =2),
      
      mainPanel(plotlyOutput(NS(id, "longest_debates")),
                
                br(),
                br(),
                plotOutput(NS(id, 'tbl99'))))
    )}



longest_debates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    longest_debates <- fread(paste0(data_dir, "debates/longest_debates.csv"))
    zz <- fread(paste0(data_dir, "debates/clean_longest_debates_wordcloud.csv"))
    
    output$longest_debates <- renderPlotly({ 
      
      render_value_3(longest_debates)
      render_value_4(longest_debates, zz)
      
      plot_ly(data = longest_debates, 
              x = ~speechdate, 
              y = ~words_per_debate,
              type = "scatter",
              source = "subset_3",
              marker = list(
                color = 'LightSkyBlue',
                size = 7,
                opacity = 0.8,
                line = list(
                  color = 'black',
                  width = 1 )),
              text = ~paste0('Name: ',  "<b>", debate, "</b>", '\n',
                             'Speech Date: ', "<b>", speechdate, "</b>", '\n',
                             'Word Count ', "<b>",words_per_debate, "</b>"),
              hoverinfo = 'text') %>%
        layout(xaxis = list(title = "Year"),
               yaxis = list(title = "Word Count")) %>%
        config(displayModeBar = F) }) 

render_value_3 = function(NN){
  output$tbl6 <- renderUI({
    s <- event_data("plotly_click", source = "subset_3")
    
    NN <- NN[, .(words_per_debate == s$y & speechdate == s$x)] # this is filter and select
    
    str1 <- paste(NN$debate)
    str2 <- paste(NN$debate_id)
    str3 <- paste(NN$speechdate)
    str4 <- paste(NN$words_per_debate)
    HTML(paste("<h3>", 
               "Debate Title:", str1, "<br>",
               "Debate ID:", str2, "<br>",
               "Speech Date:", str3, "<br>",
               "Words Per Debate:", str4))
  }) } 


render_value_4 <- function(NN, zz) {
  
  output$tbl99 <- renderPlot({
    
    s <- event_data("plotly_click", source = "subset_3")
    
    validate(need(!is.null(s), "Click on a point to view the debate's top words."))
    
    NN <- NN[words_per_debate == s$y & speechdate == s$x]
    
    zz <- zz[debate == NN$debate & speechdate == NN$speechdate]

    set.seed(42)
    ggplot(zz, 
           aes(label = lemma, 
               size = token_count)) +
      geom_text_wordcloud_area(rm_outside = TRUE,
                               shape = "square") +
      scale_size_area(max_size = 25) +
      theme_minimal() +
      ggtitle(paste0("Top words in ", zz$debate)) + 
      theme(plot.title = element_text(hjust = 0.5,
                                      size=22,
                                      vjust= -10)) })
  
  }




} ) }