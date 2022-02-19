#library(shinyjs)
vals = reactiveValues(btn = FALSE, text = FALSE)


word_context_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(

        #useShinyjs(),
        #extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click', 'null'); }", functions = c("resetClick")),

        actionButton(NS(id, "about_word_context"),
                     "About This Page",
                     style="color: #fff; background-color: #337ab7;
                                       border-color: #2e6da4;
                                       width: 179px;
                                       padding:4px;
                                       font-size:90%"),
        p(),
        textInput(NS(id, "search_similarity"),
                  "Keyword:",
                  value = "harvest"),

        sliderTextInput(
          inputId = NS(id,"window_size"),
          label = "Window Size: ",
          grid = TRUE,
          force_edges = TRUE,
          choices = c("1",
                      "2",
                      "3",
                      "4",
                      "5",
                      "6",
                      "7",
                      "8",
                      "9",
                      "10",
                      "Full"),
          selected = "5"),

        actionButton(NS(id, 'download_similarity'),
                     "Download Plot",
                     style = "width: 179px;"),
        width = 2),

      mainPanel(plotlyOutput(NS(id, "word_context")),
                br(), br(),
                br(), br(),
                br(), br(),
                br(), br(),
                br(), br(),
                br(), br(),
                dataTableOutput(NS(id, 'aab')),

                tags$script('document.getElementById("word_context-download_similarity").onclick = function() {
                                     var gd = document.getElementById("word_context-word_context");
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






word_context_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$word_context <- renderPlotly({

      # observeEvent(input$search_similarity, {
      #   js$resetClick()
      # })

      search_word <- tolower(input$search_similarity)

      out <- data.frame()
      decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890)

      for(d in 1:length(decades)) {

        fdecade <- decades[d]

        table <- paste0(data_dir, "word_embeddings/hansard_decades_wordvectors_11222021/hansard_word_vectors_", fdecade, ".txt")
        word_vectors <- as.matrix(read.table(table, as.is = TRUE))

        if(search_word %in% rownames(word_vectors)) {

          kw = word_vectors[search_word, , drop = F]

          cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")

          forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:16])

          colnames(forplot)[1] <- "similarity"

          forplot$word <- rownames(forplot)

          forplot <- forplot %>%
            mutate(decade = fdecade)

          out <- bind_rows(out, forplot) } }


      out <- out %>%
        mutate(row_id = seq(along.with = out$word, from = 0)) # could be any column

      render_value_8(out) # added

      plot_ly(data = out,
              x = ~decade,
              y = ~similarity,
              mode = "markers+text",
              text = ~word,
              type = "scatter",
              source = "aa", # added
              marker = list(color = 'rgb(158,202,225)',
                            size = 15,
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5)),
              textposition = "center right",
              height=650) %>%
        layout(xaxis = list(autotick = F,
                            tickmode = "array",
                            tickvals = c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890),
                            dtick = 10,
                            range = c(1790, 1900)),
               title = paste0("Words Context for ", "\"", input$search_similarity, "\"", " via Embeddings")) %>%
        config(displayModeBar = F) })


    render_value_8 = function(NN){
      output$aab <- renderDataTable({
        s <- event_data("plotly_click", source = "aa")  # change on click, not on shiny input change

        validate(need(!is.null(s), "Click on a point to see PLACEHOLDER"))

        jkl <- NN %>%
          filter(row_id == s$pointNumber)

        word <- jkl$word

        # what if I filter -- then make corpus??

        #j <- as.data.table(memo_quanteda_kwic(cached_hansard_1800, word))
        #j <- set_window_size(j, input$window_size)
        #j <- select(j, -docname, -to, -from, -pattern)

        j <- memo_quanteda_kwic(cached_hansard_1800, word)


        j <- set_window_size(j, input$window_size)

        return(datatable(j,
                         options = list(dom = 'ip'),
                         filter = list(position = "top")))
        }) }



    observeEvent(input$about_word_context, {
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


   # ui <- fluidPage(
   #   similarity_ui("similarity")
   # )
   # server <- function(input, output, session) {
   #   similarity_server("similarity")
   # }
   # shinyApp(ui, server)