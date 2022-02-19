
counts <- "<h3>Count Totals:</h3>"
str1 <- "<h4>Debates: 173,275</h4>"
str2 <- "<h4>Speakers: X</h4>"
str3 <- "<h4>Sentences: 10,979,009</h4>"


introduction_ui <- function(id) {
  tagList(
    
    splitLayout(cellWidths = c("75%", "25%"),
                cellArgs = list(style = "padding: 6px"),
                plotlyOutput(NS(id,"ndbs")),
                HTML(paste(counts, str1, str2, str3, sep = '<br/>'))),
    
    fluidRow(column(width = 7, 
                    offset = 1,
                    br(),
                    p(),
                    br(),
                    p("Tools for mining text can open a window onto politics making what happens in government more transparent to citizens."),
                    p("This protoype app belongs to a preliminary series of public-facing web apps designed to show the language features of debates. 
                      Users can apply an array of data-mining and statistical tools to gain insight into the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts."),
                    strong("Controls:"),
                    "Use the navigation bar at the top of the page to change between different views of the Hansard corpus.",
                    br(),
                    p("Click on the blue button found in the top left corner of the following pages to learn more about the data and the methods of measurement used to produce a visualization."),
                    p(),
                    p()
))) }

introduction_server <- function(id) {
  moduleServer(id, function(input, output, session) {

number_of_debates_from_1803_1910 <- fread(paste0(data_dir, "introduction/number_of_debates_from_1803_1910.csv"))
number_of_debates_from_1803_1910$decade <- as.factor(number_of_debates_from_1803_1910$decade)

output$ndbs <- renderPlotly({
  
  number_of_debates_from_1803_1910 %>%
    highlight_key(~decade) %>%
    plot_ly(
          x = ~decade, 
          y = ~no_of_debates, 
          type = 'bar', 
          hovertext = ~paste0("Decade: ", "<b>", decade, "</b>", "\n",
                         "Number of Debates: ", "<b>", no_of_debates, "</b>", "\n"),
          hoverinfo = "text",
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5))) %>% 
    highlight(~decade, on = "plotly_click", off = "plotly_doubleclick") %>%
    layout(barmode = "overlay",
           title = paste0("The Hansard Parliamentary Debates", "\n", "Debate Count by Decade: 1803—1910"),
           xaxis = list(title = ""),
           yaxis = list(title = "")) %>%
    config(displayModeBar = F) 
  
  
  
  
})




  }) }


# 
# 
#    ui <- fluidPage(
#      similarity_ui("ndbs")
#    )
#    server <- function(input, output, session) {
#      similarity_server("ndbs")
#    }
#    shinyApp(ui, server)
