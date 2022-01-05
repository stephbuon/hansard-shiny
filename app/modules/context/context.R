# data_dir <- "~/projects/hansard-shiny/app/app-data/"
# modules_dir <- "~/projects/hansard-shiny/app/modules/"

vals = reactiveValues(btn = FALSE, text = FALSE)


context_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_collocates"),
                     "About This Page",
                     style = "color: #fff;
                     background-color: #337ab7;
                     border-color: #2e6da4;
                     width: 179px;
                     padding:4px;
                     font-size:90%"),
        p(),
        selectInput(NS(id, "vocabulary"),
                    "Vocabulary:",
                    c("All" = "all",
                      "Property" = "property",
                      "Concerns" = "concerns",
                      "Nations" = "nations",
                      "Cities" = "cities")),

        selectInput(NS(id, "measure"),
                    "Measure:",
                    list(`Collocates` = list("Raw Count" = "count",
                                             "tf-idf" = "tf-idf"),
                         `Word Embeddings` = list("Similarity" = "similarity"))),


        uiOutput(NS(id, "suggestion_1"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "suggestion_2"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "suggestion_3"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "suggestion_4"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),

        textInput(NS(id, "custom_search"),
                  "Custom Search:", ""),

        radioButtons(NS(id, "match_type"),
                     "",
                     c("Includes Keyword" = "include",
                       "Matches Keyword" = "match"),
                     selected = "include"),

        conditionalPanel(
          condition = "input.measure == 'count' || input.measure == 'tf-idf'", ns = ns,
          sliderTextInput(
            inputId = NS(id, "decade_collocates_top"),
            label = "Decade (Top): ",
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

          sliderTextInput(
            inputId = NS(id,"decade_collocates_bottom"),
            label = "Decade (Bottom): ",
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
                        "1890"),
            selected = "1840"),

          sliderTextInput(
            inputId = NS(id,"sentiment"),
            label = "Sentiment: ",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("All",
                        "Positive",
                        "Negative"))),

        conditionalPanel(
          condition = "input.measure == 'similarity'", ns = ns,

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
            selected = "5")),

        width = 2),

      mainPanel(plotlyOutput(NS(id, "collocates_top")),
                plotlyOutput(NS(id, "collocates_bottom")),
                DTOutput(NS(id, 'aab')))

      ))}


context_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$suggestion_1 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_1", label = "law", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_1", label = "tenant", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_1", label = "poor", style = "width: 179px;") }  })

    output$suggestion_2 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_2", label = "woman", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_2", label = "property", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_2", label = "coal", style = "width: 179px;") }  })

    output$suggestion_3 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_3", label = "men", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_3", label = "rent", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_3", label = "future", style = "width: 179px;") } })

    output$suggestion_4 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_4", label = "government", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_4", label = "landlord", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_4", label = "industry", style = "width: 179px;") }})


    observeEvent(input$btnLabel,{
      vals$btn=TRUE
      vals$text=FALSE })

    observeEvent(input$custom_search,{
      vals$btn=FALSE
      vals$text=TRUE })


    get_tf_idf <- reactive({
      if (input$measure == "tf-idf") {
        fname <- paste0(data_dir, "collocates/tf-idf-data/tf_idf_", input$decade_collocates_top, "_", input$decade_collocates_bottom, "_", input$vocabulary, "_adj_noun_pairs.csv")
        fname_reverse <- paste0(data_dir, "collocates/tf-idf-data/tf_idf_", input$decade_collocates_bottom, "_", input$decade_collocates_top, "_", input$vocabulary, "_adj_noun_pairs.csv")
        if (file.exists(fname)|file.exists(fname_reverse)) {
          if (file.exists(fname)) {
            df <- fread(fname) }
          else {
            df <- fread(fname_reverse) } }
        else {
          tf_idf_ct(collocates_top(), collocates_bottom(), input$vocabulary, input$custom_search, fname) } } })


    collocates_top <- reactive ({
      collocates_top <- import_collocates_data(data_dir, input$decade_collocates_top, input$vocabulary)})

    collocates_bottom <- reactive ({
      collocates_bottom <- import_collocates_data(data_dir, input$decade_collocates_bottom, input$vocabulary) })




    return_data <- function(collocates, measure, decade_slider, vals, match_type, custom_search, btnLabel, sentiment) {

      #if (measure == "count") {
      #collocates <- collocates_top() }
      if (measure == "tf-idf") {
        collocates <- get_tf_idf() }

      setkey(collocates, decade)
      collocates <- collocates[.(as.numeric(decade_slider))]

      collocates <- search_ct(collocates, vals, match_type, custom_search, btnLabel)

      collocates <- filter_sentiment_ct(collocates, sentiment)

      collocates <- collocates[order(collocates, -n)]
      top <- collocates[1:20] }


    output$collocates_top <- renderPlotly({

      if (input$measure == "count" | input$measure == "tf-idf") {

       top_plot <- return_data(collocates_top(), input$measure, input$decade_collocates_top, vals$text, input$match_type, input$custom_search, input$btnLabel, input$sentiment)

      if (input$measure == "count") {
        xlab <- list(title ="Raw Count") }
      else if (input$measure == "tf-idf") {
        xlab <- list(title ="tf-idf") }

      plot_ly(data = top_plot,
              x = ~n,
              y = ~reorder(grammatical_collocates, n),
              type = 'bar',
              text = n,
              orientation = 'h',
              marker= list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
        layout(title = input$decade_collocates_top,
               xaxis = xlab,
               yaxis = list(title = "")) %>%
        config(displayModeBar = F) }

      else {

        if(vals$btn == TRUE) {
          search_word <- tolower(input$btnLabel)
          vals$text == FALSE }

        if(vals$text == TRUE) {
          search_word <- tolower(input$custom_search)
          vals$btn == FALSE }

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

        render_value_8(out)

        validate(need(out$decade, "Search for a word from the Hansard Corpus"))

        plot_ly(data = out,
                x = ~decade,
                y = ~similarity,
                mode = "markers+text",
                text = ~word,
                type = "scatter",
                source = "aa",
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
                 title = paste0("Words Context for ", "\"", search_word, "\"", " via Embeddings")) %>%
          config(displayModeBar = F)


      }

      })
    
    render_value_8 = function(NN){
      output$aab <- renderDataTable({
        s <- event_data("plotly_click", source = "aa")  # change on click, not on shiny input change
        
        print(s)

        validate(need(!is.null(s), "Click on a point to see PLACEHOLDER"))

        jkl <- NN %>%
          filter(row_id == s$pointNumber)

        word <- jkl$word

        
        
        cached_hansard_1800 <- cache(s$x)
        
        j <- cached_hansard_1800[cached_hansard_1800 %like% word]
        
        #j <- quanteda_kwic(cached_hansard_1800, word)
        
        # I've decided to use phrase() irregardless of whether the word is 1 or more
        # I suspect this will make my code more veristile 
        
        #validate(need(!is.null(j), "placeholder"))
        
        j <- as.data.table(kwic(j, pattern = phrase(word), window = 300, valuetype = "fixed", separator = " ", case_insensitive = TRUE))
        j <- select(j, -docname, -to, -from, -pattern)

        
        #j <- memo_quanteda_kwic(cached_hansard_1800, word)


        j <- set_window_size(j, input$window_size)


        return(datatable(j,
                         options = list(dom = 'ip'),
                         filter = list(position = "top")))
      }) }






    output$collocates_bottom <- renderPlotly({

      if (input$measure == "count" | input$measure == "tf-idf") {


      bottom_plot <- return_data(collocates_bottom(), input$measure, input$decade_collocates_bottom, vals$text, input$match_type, input$custom_search, input$btnLabel, input$sentiment)

      if (input$measure == "count") {
        xlab <- list(title ="Raw Count") }
      else if (input$measure == "tf-idf") {
        xlab <- list(title ="tf-idf") }

      plot_ly(data = bottom_plot,
              x = ~n,
              y = ~reorder(grammatical_collocates, n),
              type = 'bar',
              text = n,
              orientation = 'h',
              marker= list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
        layout(title = input$decade_collocates_bottom,
               xaxis = xlab,
               yaxis = list(title = "")) %>%
        config(displayModeBar = F)

      }


      })


    observeEvent(input$about_collocates, {
      showModal(modalDialog(
        title = "Special Vocabulary: Sentiment Laden Collocates",
        "A collocate is a series of words that co-occur in text.
        A grammatical collocate represents the co-occuring words that share a sentence-level grammatical relationship.",
        br(),
        p(),
        strong("Controls:"),
        "Click a radio button under \"Special Vocabulary\" to select a scholar curated vocabulary list to guide your search.",
        br(),
        p(),
        "Slide the dials under \"Decade\" to change time periods",
        br(),
        p(),
        "Choose a keyword from the drop down box to narrow your analysis to just collocates containing that word.
        The keywords will update based on the selected vocabulary list.",
        br(),
        p(),
        "Slide the dial under \"Sentiment\" to narrow your analysis from all sentiment laden collocates to just those with positive or negative scores.",
        br(),
        p(),
        "Click a radio button under \"Measure\" to return results based on:
        a) a frequency count; b) term frequency - inverse document frequency (tf-idf); or c) Jenson-Shannon divergence (jsd).",
        br(),
        p(),
        strong("Measurements:"),
        "\"Count\" refers to the number of times a pair of collocates appeared in a sentence in the debate text.",
        br(),
        p(),
        "\"tf-idf\" is a numerical statistic that reflects how \"distinctive\" a word is to a corpus.
        The tf–idf value increases proportionally to the number of times a word appears in a decade and is offset by the other decade that contains the word,
        which helps to adjust the results for the fact that some words appear more frequently in general.",
        br(),
        p(),
        "\"jsd\" is a " )) })


  } ) }




 ui <- fluidPage(
   context_ui("context")
 )
 server <- function(input, output, session) {
   context_server("context")
 }
 shinyApp(ui, server)




