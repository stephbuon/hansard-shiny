purpose_ui <- function(id) {
  tagList( 
    
    
    fluidPage(
      fluidRow(
        column(10,
               offset = 1, 
               h3("The Hansard Viewer"),
               br(),
               p(),
               "Tools for mining text can open a window onto politics making what happens in government more transparent to citizens.",
               p(),
               "For seven years Democracy Lab has been operating at the juncture of political, historical, and textual analysis of democratic debates, publishing numerous articles and collaborating with the builders of infrastructure to make text mining accessible to the public.
                                            This app belongs to a series of preliminary public-facing web applications 
                                            Users of our prototype application can use an array of data-mining and statistical tools to explore the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts. 
                                            Citizens using our toolset can navigate from an overview showing change over time, to a depiction of how different candidates talk about the same issue, to the in-text mentions of word use that a computer has used to produce the visualizations in question.",
               p(),
               "This data-informed angle into the Hansard debates enables users to perceive change over time at scale. A quantitative approach emphasizes trends, enter. 
                                            
                                            Despite -- it can still show unique occurances in the form of statistical outliers. 
                                            
                                            The usefulness of any application for citizens to understand democracy depends upon how much they can trust the data and its analysis. 
                                            For users to trust the analysis, an app must cut a line between transparency, precision, and innovation. 
                                            Our tool does this by employing methods such as word counts, tf-idf measures, topic models, word embeddings, and triples analysis. 
                                            
                                            
                                            
                                            In general, our app’s design will try to compensate for user mistrust of algorithm- and algebra- dependent measures by linking visualizations to the citations that explain each algorithm or measure and links in lay language. We will also privilege certain measures -- for instance triples analysis, a tool that is simultaneously transparent, precise, and sensitive.")
      ) )  )}