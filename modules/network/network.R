search_svo <- function(df, s, v, o) {
  if(s == "" & v == "" & o == "") {
    return(df) } else if (s != "" & (v == "" & o == "")) {
      
      df <- df %>%
        filter(str_detect(to_name, paste0("^", s)))
      
      return(df) } else if (s == "" & v != "" & o == "") { 
        print("take 2")
        
      } else if(s == "" & v == "" & o != ""){
        print("take 3")
      } else if(s != "" & v != "" & o == ""){
        print("take 4")
      } else if(s == "" & v != "" & o != ""){
        print("take 5")
      } else if(s != "" & v == "" & o != ""){
        print("take 6")
      } else if(s != "" & v != "" & o != ""){
        print("take 7")
      } 
}




e1 <- fread("~/projects/hansard-shiny/app-data/network/edges_test_data.csv")
n1 <- fread("~/projects/hansard-shiny/app-data/network/nodes_test_data.csv")


e1 <- e1 %>%
  filter(decade == 1850)

n1 <- n1 %>%
  filter(decade == 1850)


network_ui <- function(id) {
  tagList(
    
    
    sidebarLayout(
      sidebarPanel(
        helpText("Check the boxes to explore the langauge of different speakers."),
        
        checkboxGroupInput(NS(id, "subreddit"), 
                           "Speaker:",
                           c("William E. Gladstone" = "Mr. Gladstone",
                             "Benjamin Disraeli" = "bdisraeli",
                             "Arthur Balfour" = "abalfour",
                             "Mr. Placeholder" = "Mr. Placeholder"),
                           selected = c("Mr. Gladstone", "Mr. Placeholder")),
        
        tags$hr(style="border-color: black;"),
        helpText("Slide the dial to change decades."),
        
        sliderTextInput(
          inputId = NS(id, "decade"), 
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
        
        tags$hr(style="border-color: black;"),
        helpText("Use the search boxes to filter for triples that contain parts-of-speech."),
        
        textInput(NS(id, "search_subject"), 
                     "Subject:", ""),
        textInput(NS(id, "search_verb"), 
                  "Verb:", ""),
        textInput(NS(id, "search_object"), 
                     "Object:", ""),
        actionButton(NS(id, 'download_network'), 
                     "Download Plot",
                     style = "width: 179px;"),
        
        width = 2),
      
      mainPanel(visNetworkOutput(NS(id, "network")),
                DTOutput(NS(id, 'tbl')),
                
                
      ))
    
    
    
  ) }




network_server <- function(id) {
  moduleServer(id, function(input, output, session) {


#observeEvent(input$download_network, {

#  webshot("network",delay=0.5,zoom=2,file=paste0("network",".png"),
#          vwidth=900,vheight=900)

#visExport(output$network(),
#  type = "png",
#  name = "network_download",
#  label = paste0("Export as ", type),
#  background = "#fff")
#})



reactive_nodes <- reactive({
  
  e2 <- e1 %>%
    filter(decade == input$decade)
  
  e2 <- e2 %>%
    filter(from_name %in% input$subreddit)
  
  e2 <- search_svo(e2, input$search_subject, input$search_verb, input$search_object)
  
  # cast these edges to a set
  edges_from_list <- e2$from_name
  edges_to_list <- e2$to_name
  total <- c(edges_from_list, edges_to_list)
  total <- unique(total)
  
  # filter for nodes related to an edge
  out <- data.table() 
  for(i in 1:length(total)) {
    
    keyword <- total[i]
    
    filtered_nodes <- n1 %>%
      filter(decade == input$decade) %>%
      filter(str_detect(label, keyword)) #regex(paste0("^", keyword, "$", ignore_case = TRUE))))
    
    out <- rbind(out, filtered_nodes) }
  
  # count the number of times the word was mentioned 
  c <- e2 %>%
    count(to_name) %>%
    rename(value = n,
           label = to_name)
  
  
  out <- left_join(out, c, by = 'label')
  
  out <- unique(out)})


reactive_edges <- reactive({
  e3 <- e1 %>%
    filter(from_name %in% input$subreddit) %>%
    add_count(to_name, from_name) %>%
    rename(weight = n) })


output$network <- renderVisNetwork({
  
  visNetwork(reactive_nodes(), # 
             reactive_edges(),  # e1
             height = "500px", 
             width = "100%") %>%
    visOptions(#selectedBy = "group", # great for debugging
      #nodesIdSelection = TRUE, # great for debugging
      highlightNearest = list(hover = TRUE)) %>%
    visLayout(randomSeed = 2) %>%
    visPhysics(stabilization = TRUE) %>%
    visEdges(smooth = FALSE,
             length = 210) %>%
    #visNodes(color = list(hover = "green")) %>%
    visInteraction(#hover = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      navigationButtons = FALSE) %>%
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -100)) %>%
    
    visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_selection', nodes.nodes);
                ;}") 
  
})

observe({ # is this function speeding things up? 
  visNetworkProxy("network") })


output$tbl <- renderDT({
  
  validate(need(!is.null(input$current_node_selection), "Click on a node to see its context"))
  
  # e1 <- e1 %>%
  #    filter(from_name %in% input$subreddit | to_name %in% input$subreddit)
  
  #print("currentNodeSection")
  #print(input$current_node_selection)
  
  # This dt might not reflect the correct count -- I need that count function 
  
  datatable(e1 <- e1 %>% 
              filter(from %in% input$current_node_selection | to %in% input$current_node_selection) %>%
              select(-from, -to, -decade),
            options = list(dom = 'ip'),
            filter = list(position = "top"))
  
  
  # I could return KWIC-like text
  
}) 


} ) }