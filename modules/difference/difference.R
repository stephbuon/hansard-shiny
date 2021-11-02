# not working

vals = reactiveValues(btn = FALSE, text = FALSE)


difference_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_difference"), 
                     "About This Page",
                     style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
        p(),
        textInput(NS(id, "wv_textbox"), 
                  "Keyword:", 
                  "tenant"),
        uiOutput(NS(id, "wv_action_button"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_2"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_3"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_4"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_5"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_6"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_7"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "wv_action_button_8"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        tags$style("#wv_text_box_1 {background-color:#E8E8E8;}"),
        textInput(NS(id, "wv_text_box_1"), 
                  "Custom Search:", ""),
        br(),
        actionButton(NS(id, 'download_try_2'), 
                     "Download Plot",
                     style = "width: 179px;"
        ),
        width = 2),
     
      
      
      
      mainPanel(plotlyOutput(NS(id, "wv_test"), 
                             height = "650"),
                
                tags$script('document.getElementById("download_try_2").onclick = function() {
                                     var gd = document.getElementById("wv_test");
                                     Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                                     var a = window.document.createElement("a");
                                     a.href = url; 
                                     a.type = "image/png";
                                     a.download = "plot.png";
                                     document.body.appendChild(a);
                                     a.click();
                                     document.body.removeChild(a);
                                     });
                                     } ')
                
                
      
    ) ) 
    

    
    
  ) }


difference_server <- function(id) {
  moduleServer(id, function(input, output, session) {

output$wv_action_button <- renderUI({
  forplot <- get_button() 
  if (nrow(forplot) > 0) {
    actionButton("wv_action_button", label = forplot[1,1], style = "width: 179px;") } else { # 1,2
      return() } 
})

output$wv_action_button_2 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_2", label = forplot_2[2,1], style = "width: 179px;") } else { # 2,2
      return() } 
})

output$wv_action_button_3 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_3", label = forplot_2[3,1], style = "width: 179px;") } else { # 3,2
      return() } 
})

output$wv_action_button_4 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_4", label = forplot_2[4,1], style = "width: 179px;") } else { # 4,2
      return() }
})

output$wv_action_button_5 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_5", label = forplot_2[5,1], style = "width: 179px;") } else { # 5,2 
      return() } })

output$wv_action_button_6 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_6", label = forplot_2[6,1], style = "width: 179px;") } else { # 6,2 
      return() }
})

output$wv_action_button_7 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_7", label = forplot_2[7,1], style = "width: 179px;") } else { 
      return() }
})


output$wv_action_button_8 <- renderUI({
  forplot_2 <- get_button()
  if (nrow(forplot_2) > 0) {
    actionButton("wv_action_button_8", label = forplot_2[8,1], style = "width: 179px;") } else { 
      return() }
})


input_loop <- function(input, decades, first_range, second_range, make_m, make_decade, ...) {
  
  out <- data.frame()
  il_decades <- decades
  
  for(fdecade in il_decades) {
    #fdecade <- il_decades[d] 
    
    table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    if(input %in% rownames(word_vectors)) {
      kw = word_vectors[input, , drop = F]
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[first_range:second_range])
      colnames(forplot)[1] <- paste0("similarity")
      
      forplot$word <- rownames(forplot)
      
      if (make_decade == TRUE) {
        forplot <- forplot %>%
          mutate(decade = fdecade) %>%
          filter(word == ...) } 
      
      if (make_m == TRUE) {
        rownames(forplot) <- NULL
        forplot <- forplot %>%
          select(word) }
      
      out <- bind_rows(out, forplot) } }
  
  if (make_m == TRUE) {
    out <- dplyr::distinct(out)
  }
  
  
  return(out) }



get_button <- function() {
  decades <- c(1800, 1850)
  
  out <- input_loop(input$wv_textbox, decades, 2, 401, make_m = TRUE, make_decade = FALSE) # from 201 -- 301 may have been better 
  
  cycle = 0
  
  for(fdecade in decades) {
    cycle = cycle + 1
    #fdecade <- decades[d] 
    
    table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
    # table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_1800.txt")
    
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    rn <- rownames(word_vectors)
    if(input$wv_textbox %in% rn) {
      kw = word_vectors[input$wv_textbox, , drop = F]
      
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:401]) # from 101
      
      colnames(forplot)[1] <- paste0("similarity", cycle)
      
      forplot$word <- rownames(forplot)
      
      # forplot <- forplot %>%
      #    mutate(decade = fdecade)
      
      rownames(forplot) <- NULL
      
      out <- left_join(out, forplot, by = "word") }}
  
  b <- out %>%
    drop_na()
  
  if ( nrow(b) >= 8 && ncol(out) == 3 ) { 
    
    out$all_sim <- abs(out$similarity1 - out$similarity2)
    
    out <- out %>%
      drop_na()
    
    out <- out %>%
      arrange(desc(all_sim))
    
    return(out) } else {
      
      out <- out %>%
        slice(10:18)
      
      return(out) } }



observeEvent(input$btnLabel,{
  vals$btn=TRUE
  vals$text=FALSE })

observeEvent(input$wv_text_box_1,{
  vals$btn=FALSE
  vals$text=TRUE })



output$wv_test <- renderPlotly({
  
  decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860)
  
  if(vals$btn){
    out <- input_loop(input$wv_textbox, decades, 2, 401, make_decade = TRUE, input_button_label = input$btnLabel, make_m = FALSE) # same -- from 201 
  } else {
    out <- input_loop(input$wv_textbox, decades, 2, 401, make_m = FALSE, make_decade = TRUE, input_button_label= input$wv_text_box_1) # same -- from 201 
  }
  
  
  ### to add zeros
  # for(d in 1:length(decades)) {
  # dec <- decades[d] 
  # if (!dec %in% out$decade) {
  #   similarity <- NA
  #   word <- input$wv_textbox
  #   decade <- dec
  #   out_1 <- data.table(similarity, word, decade)
  #   out <- bind_rows(out, out_1)
  #   out <- out %>%
  #     arrange(decade) }}
  
  if(vals$btn){
    ff <- input$btnLabel
  } else {
    ff <- input$wv_text_box_1
  }
  
  if (isTruthy(input$wv_textbox)) {
    plot_ly(data = out, 
            x = ~decade, 
            y = ~similarity,
            mode = "lines+markers",
            type = "scatter",
            line = list(color = "black", width = 1),
            marker = list(color = 'rgb(158,202,225)',
                          size = 15,
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(xaxis = list(autotick = F, # why does this not work??
                          tickmode = "array", 
                          tickvals = c(1800, 1810, 1820, 1830, 1840, 1850, 1860),
                          dtick = 10,
                          range = c(1800, 1910)),
             #yaxis = list(rangemode = "tozero"),
             title = paste0("Relationship of ", "\"", ff, "\"", " to ", "\"", input$wv_textbox, "\"", " over time")) %>%
      config(displayModeBar = F) } else {
        
        df <- data.frame(decade=integer(),
                         similarity=integer())
        
        plot_ly(data = df,
                x = ~decade,
                y = ~similarity) %>%
          config(displayModeBar = F) } })






observeEvent(input$about_difference, {
  showModal(modalDialog(
    title = "Debate Titles: Proportion of Debate Titles that Include Keywords",
    "Define",
    br(),
    p(),
    strong("Controls:"),
    "Use the \"Keywords List\" drop down box to select a scholar curated vocabulary list, or choose \"Blank Plot\" to start with an empty graph.",
    "Type search terms in each . The ",
    br(),
    p(),
    strong("Measurement:"),
    "Here we are using proportion instead of ENTER"))
})

  } ) }


# 
    ui <- fluidPage(
      difference_ui("wv_test")
    )
    server <- function(input, output, session) {
      difference_server("wv_test")
    }
    shinyApp(ui, server)  
#    
