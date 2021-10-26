

similarity_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_we_similarity"), 
                     "About This Page",
                     style="color: #fff; background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
        p(),
        textInput(NS(id, "search_similarity"), "Keyword:", value = "harvest"),
        actionButton(NS(id, 'download_similarity'), 
                     "Download Plot",
                     style = "width: 179px;"),
        width = 2),
      
      mainPanel(plotlyOutput(NS(id, "similarity")),
                tags$script('document.getElementById("similarity-download_similarity").onclick = function() {
                                     var gd = document.getElementById("similarity-similarity");
                                     Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                                     var a = window.document.createElement("a");
                                     a.href = url;
                                     a.type = "image/png";
                                     a.download = "plot.png";
                                     document.body.appendChild(a);
                                     a.click();
                                     document.body.removeChild(a);
                                     });
                } ')) 
      ))}






similarity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$similarity <- renderPlotly({
      
      out <- data.frame()
      decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860)
      
      for(d in 1:length(decades)) {
        
        fdecade <- decades[d] 
        
        table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
        word_vectors <- as.matrix(read.table(table, as.is = TRUE))
        

        if(input$search_similarity %in% rownames(word_vectors)) { 
          
          kw = word_vectors[input$search_similarity, , drop = F]
          
          cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
          
          forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:16])
          
          colnames(forplot)[1] <- "similarity"
          
          forplot$word <- rownames(forplot)
          
          forplot <- forplot %>%
            mutate(decade = fdecade)
          
          out <- bind_rows(out, forplot) } }
      
      plot_ly(data = out, 
              x = ~decade, 
              y = ~similarity,
              mode = "markers+text",
              text = ~word,
              type = "scatter",
              marker = list(color = 'rgb(158,202,225)',
                            size = 15,
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5)),
              textposition = "center right",
              height=650) %>%
        config(displayModeBar = F) })
    
    
    observeEvent(input$about_we_similarity, {
      showModal(modalDialog(
        title = "",
        "DEFINE",
        br(),
        p(),
        strong("Controls:"),
        "Type a single word into the search box to view its most closely associated words throughout the period.",
        br(),
        p(),
        strong("Measurement:"),
        "" ))
    })
    
    

  }) }

# 
#    ui <- fluidPage(
#      similarity_ui("similarity")
#    )
#    server <- function(input, output, session) {
#      similarity_server("similarity")
#    }
#    shinyApp(ui, server)  
#    


