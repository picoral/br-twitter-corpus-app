#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# load data
tweet_data <- read_tsv("data/combined_data.tsv") %>%
    filter(!problematic)
tweet_data$text <- gsub("@[A-Za-z0-9_]+\\b",
                        "@pessoa",
                        tweet_data$text)

# function that formats tweets for display
display_tweets <- function(my_data, selected_location, selected_pronoun, 
                           my_search, output){
    display_data <- my_data
    if (selected_location != "todas") {
        display_data <- display_data %>%
            filter(our_location == selected_location)
    }
     
    if (selected_pronoun != "todos") {
        display_data <- display_data %>%
            filter(pronoun_category == selected_pronoun)
    }
    
    if (my_search != "") {
        display_data <- display_data %>%
            filter(grepl(my_search, text, ignore.case = TRUE))
        
        highlight_replacement <- '<font color=orange>\\1</font>'
        search_term <- paste0("(", my_search, ")")
        display_data$text <- gsub(search_term, 
                                  highlight_replacement, 
                                  display_data$text, 
                                  ignore.case = TRUE)
            
    }

    
    
    display_data$display_text <- paste('<div class="list-group"><a href="#" class="list-group-item list-group-item-action flex-column align-items-start"><font color=orange>',
                                       display_data$our_location, "</font> ",
                                       "<font color=red><small>",
                                       display_data$pronoun, '</small></font><p class="text-muted">',
                               display_data$text, "</p></a></div><br>")
    
    randomized_rows <- sample(nrow(display_data))
    display_data <- display_data[randomized_rows, ]
    
    output$exibidos <- renderText({ 
        
        selected <- paste("Número de tweets exibidos:",
                          nrow(display_data))
        
        HTML(selected)
    })
    
    return(display_data$display_text)
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
                
                
                tags$head(tags$style(
                    HTML('
         .well {
            background-color: black;
        }')
                )),

    # Application title
    titlePanel("Corpus de Tweets do Português Brasileiro"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Filtrar Tweets"),
            selectInput("location", "por região:",
                        c("todas",
                          "Florianópolis",
                          "João Pessoa",
                          "Recife",
                          "Rio de Janeiro",
                          "Salvador",
                          "São Paulo",
                          "Porto Alegre")),
            selectInput("pronoun", "por pronome:",
                        c("todos",
                          "nós",
                          "a gente",
                          "você",
                          "tu")),
            textInput("search", "Procurar:", ""),
            textOutput("total"),
            textOutput("exibidos")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("tweets")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$total <- renderText({ 
        
        total <- paste("Número de tweets totais:",
                       nrow(tweet_data))
        
        HTML(total)
        
        })

    
    output$tweets <- renderUI({
        HTML(display_tweets(tweet_data, 
                            input$location, 
                            input$pronoun,
                            input$search,
                            output))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
