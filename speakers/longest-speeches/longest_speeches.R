library(tidyverse)
library(lubridate)

b = 5
d = 5

tokenized_hansard <- read_csv("/scratch/group/pract-txt-mine/tokenized_hansard.csv")

metadata <- tokenized_hansard %>%
  select(speaker, speechdate, speech_id, debate) %>%
  distinct()

# what was the longest single speech?
words_per_speech <- tokenized_hansard %>%
  group_by(speech_id) %>%
  summarize(count_per_speech = n()) %>%
  ungroup()

words_per_speech <- words_per_speech %>% 
  inner_join(metadata, on = "speech_id") %>%
  mutate(year = year(speechdate)) %>%
  mutate(sp_ranking = rank(desc(count_per_speech))) %>%
  group_by(year(speechdate)) %>%
  mutate(sp_ranking_yr = rank(desc(count_per_speech))) %>%
  ungroup()

longest_speech_decade <- words_per_speech %>%
  group_by(10*floor(year/10)) %>%
  mutate(sp_ranking_decade = rank(desc(count_per_speech))) %>%
  filter(sp_ranking_decade < d) %>%
  ungroup() %>%
  inner_join(metadata) 

write_csv(longest_speech_decade, "longest_speeches.csv")


xlab <- list(
  title = "Date of Speech"
)
ylab <- list(
  title = "Number of Words"
)

ui <- fluidPage(
  
  titlePanel("What Were the Longest Debates in Parliament?"),
  
  sidebarPanel(
    
    helpText("I have removed the sidebar functionality for now.")
    
  ),
  
  
  mainPanel(
    
    plotlyOutput("plot_2")
    
  )
)

server <- function(input, output) {
  
  output$plot_2 <- renderPlotly({
    
    plot_ly(data = longest_speech_decade, x = ~speechdate, y = ~count_per_speech, 
            text = ~paste('Debate: ', debate)) %>%
      
      layout(xaxis = xlab, yaxis = ylab) %>%
      
      config(displayModeBar = F)
  })
}


shinyApp(ui, server)