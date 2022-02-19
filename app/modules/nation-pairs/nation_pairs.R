nation_pairs_ui <- function(id) {
  tagList(

    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_nation_pairs"),
                     "About This Page",
                     style="color: #fff;
                                       background-color: #337ab7;
                                       border-color: #2e6da4;
                                       width: 179px;
                                       padding:4px;
                                       font-size:90%"),
        p(),

        sliderTextInput(
          inputId = NS(id, "decade_2"),
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

        width = 2),

      mainPanel(
        plotlyOutput(NS(id, "nation_pairs")),



        plotlyOutput(NS(id, "ut")



      )))



  ) }










nation_pairs_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    #nations_count <- fread(paste0(data_dir, "nations/hansard_c19_debate_title_nation_count.csv"))
    nations_count <- fread(paste0(data_dir, "nations/hansard_nation_counts.csv"))


    nations_count$decade <- as.character(nations_count$decade)

    np <- fread(paste0(data_dir, "nations/nation_pairs_in_debate_titles.csv"))

    output$nation_pairs <- renderPlotly({

      np <- np %>%
        filter(decade == input$decade_2) %>%
        arrange(desc(n)) %>%
        slice(1:20)

      render_value_np(np)

      plot_ly(data = np,
              x = ~n,
              y = ~reorder(nation_pair, n),
              type = 'bar',
              hovertext = ~paste0("Nation Pair: ", "<b>", nation_pair, "</b>", "\n",
                             "Frequency: ", "<b>", n, "</b>", "\n"),
              hoverinfo = "text",
              orientation = 'h',
              source = "np",
              marker= list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
        layout(xaxis = list(title ="Frequency"),
               yaxis = list(title = "")) %>%
        config(displayModeBar = F) })



    render_value_np = function(NN){
      output$ut <- renderPlotly({
        s <- event_data("plotly_click", source = "np")

        validate(need(!is.null(s), "Click on a bar to visualize each nation's count over time"))

        hh <- separate(s, y, into = c("left_nation", "right_nation"), sep = "-")

        ln <- nations_count %>%
          filter(nation == hh$left_nation) %>%
          rename(lcount = nation_count)

        ln$nation <- "left_nation_1"

        ln <- dcast(ln, decade ~ nation)

        rn <- nations_count %>%
          filter(nation == hh$right_nation) %>%
          rename(rcount = nation_count)

        rn$nation <- "right_nation_1"

        rn <- dcast(rn, decade ~ nation)

        all <- left_join(ln, rn, on = "decade")

        plot_ly(all,
                x = ~decade,
                y = ~left_nation_1,
                type = 'bar',
                marker= list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)),
                name = hh$left_nation) %>%
          add_trace(y = ~right_nation_1,
                    marker= list(color = 'rgb(58,200,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)),
                    name = hh$right_nation) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group') %>%
          config(displayModeBar = F)
      }) }



    observeEvent(input$about_nation_pairs, {
      showModal(modalDialog(
        title = "Count of Nations and Nation Pairs",
        "Define",
        br(),
        p(),
        strong("Controls:"),
        "Slide the dial under \"Decade\" to change time periods",
        "Click on a bar to visualize the raw count for each nation in the pair over the course of the 19th-century.",
        br(),
        p(),
        strong("Measurement:"),
        "Raw count" ))
    })



  } ) }
