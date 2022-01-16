# Hansard Viewer
A new app for understanding historical change in the annals of parliamentary democracies

This app belongs to a series of preliminary public-facing web applications Users of our prototype application can use an array of data-mining and statistical tools to explore the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts. 

Citizens and scholars using our toolset can navigate from an overview showing change over time, to a depiction of how different candidates talk about the same issue, to the in-text mentions of word use that a computer has used to produce the visualizations in question. 

This repository stores our open source code used by our app. See a link in the description for our public-facing prototype web app. 

What can you view with Hansard Viewer?

Time
    • Use to compare two decades
        ◦ The default view are the raw word counts. Not very useful
        ◦ Tf-idf more interesting
    • Use a controlled vocabulary
        ◦ Try ‘concerns’
    • Search for a word
        ◦ Try ‘coal’
        ◦ Adjust time
    • Custom search
        ◦ Try ‘terror’

Speakers
    • Longest speakers
        ◦ We wanted to understand who the most important speakers are. So here they are – by who spoke the most. 
        ◦ Try mousing over: Mr. Brougham, 1820 
    • Top speakers
        ◦ All their words per year
        ◦ Bar charts below showing their favorite words
    • Speaker comparison
        ◦ Default: raw count
        ◦ Try: tf-idf measure
        ◦ Pull-down: adjective-noun collocates (will hang for a second)
    • Debate titles
        ◦ Default: property list
        ◦ Add your own words
        ◦ Nation concerns
            ▪ Try zooming in on Denmark
        ◦ Nation pairs
    • Debate text
        ◦ Length of debate: hypothesis is that the longest speeches are the most important
            ▪ Click on dot to see wordcloud
        ◦ Speech length: testing how long the debate is on different subjets
            ▪ Small, medium, large. 
            ▪ Graph over time
            ▪ Select controlled vocab

Language
        ◦ PCA vector space
        ◦ Similarity: word embeddings over time
            ▪ Try changing the keyword
            ▪ Try changing window size
            ▪ Zoom in
