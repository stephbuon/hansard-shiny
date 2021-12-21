
vals = reactiveValues(btn = FALSE, text = FALSE)
range_start = 2 # skip the first word of the matrix, which is the same as the search term

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
        actionButton(NS(id, 'download_plot'), 
                     "Download Plot",
                     style = "width: 179px;"),
        
        width = 2),
      
      mainPanel(plotlyOutput(NS(id, "difference"), 
                             height = "650"),
                             
                             tags$script('document.getElementById("difference-download_plot").onclick = function() {
                             var gd = document.getElementById("difference-difference");
                             Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                             var a = window.document.createElement("a");
                             a.href = url; 
                             a.type = "image/png";
                             a.download = "plot.png";
                             document.body.appendChild(a);
                             a.click();
                             document.body.removeChild(a);
                             });
                             } '))) 
    )}

difference_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$wv_action_button <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button", label = forplot[1,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_2 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_2", label = forplot[2,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_3 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_3", label = forplot[3,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_4 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_4", label = forplot[4,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_5 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_5", label = forplot[5,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_6 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_6", label = forplot[6,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_7 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_7", label = forplot[7,1], style = "width: 179px;") } 
      else {
        return() } })
    
    output$wv_action_button_8 <- renderUI({
      forplot <- get_button(input$wv_textbox) 
      if (nrow(forplot) > 0) {
        actionButton("wv_action_button_8", label = forplot[8,1], style = "width: 179px;") } 
      else {
        return() } })
    
    
observeEvent(input$btnLabel,{
  vals$btn=TRUE
  vals$text=FALSE })

observeEvent(input$wv_text_box_1,{
  vals$btn=FALSE
  vals$text=TRUE })



output$difference <- renderPlotly({
  
  decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890)
  
  if(vals$btn) { # If button is clicked take the value of the button
    out <- input_loop(input$wv_textbox, decades, range_start, add_decade_col = TRUE, input_button_label = input$btnLabel, make_m = FALSE) 
    label <- input$btnLabel  } 
  else { # otherwise take the value of the text boz
    out <- input_loop(input$wv_textbox, decades, range_start, make_m = FALSE, add_decade_col = TRUE, input_button_label = input$wv_text_box_1) 
    label <- input$wv_text_box_1 }

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
      layout(xaxis = list(autotick = F,
                          tickmode = "array", 
                          tickvals = c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890),
                          dtick = 10,
                          range = c(1800, 1900)),
             title = paste0("Relationship of ", "\"", label, "\"", " to ", "\"", input$wv_textbox, "\"", " over time")) %>%
      config(displayModeBar = F) } else {
        
        df <- data.frame(decade=integer(),
                         similarity=integer())
        
        plot_ly(data = df,
                x = ~decade,
                y = ~similarity) %>%
          config(displayModeBar = F) } })


observeEvent(input$about_difference, {
  showModal(modalDialog(
    title = "Word Embeddings: Difference",
    "Define",
    br(),
    p(),
    strong("Controls:"),
    "Use the \"Keywords List\" drop down box to select a scholar curated vocabulary list, or choose \"Blank Plot\" to start with an empty graph.",
    "Type search terms in each . The ",
    br(),
    p(),
    strong("Measurement:"),
    "Here we are using proportion instead of ENTER")) })

  } ) }
