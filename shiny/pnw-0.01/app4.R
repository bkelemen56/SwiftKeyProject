library(shiny)

library(stringr)
library(quanteda)

# source our models and global variables
source('~/R Workspace/data science class/10- capstone/SwiftKeyProject/src/model_4_4_util.R', echo = FALSE)

ui <- fluidPage(
  textAreaInput("text", "Text to process", "Enter more text here", 
                width = "1000px", height = "200px"),
  
  textOutput("predictions")
)

server <- function(input, output) {
  output$predictions <- renderText({ 
    
    s <- input$text

    space <- if_else((s != ""), (str_sub(s, str_length(s)) == " "), FALSE)
    
    s <- s %>%
      clean_text(to_lower = TRUE) %>%
      str_split(" ")
    s <- s[[1]]
    
    cat(input$text, "\n")
    
    m <- length(s)
    print(s)
    print(m)
    print(space)
    
    if (m < 3) {
      "Enter 3 or more words to start prediction algorithm"
      
    } else if (space) {
      text_to_predict <- paste(s[m-2], s[m-1], s[m])
      
      paste0(
        "Predicting on: ", text_to_predict, "<br><br>",
        "Predictions are:<br>",
        "word#1 50%<br>",
        "word#2 25%<br>",
        "word#3 10%<br>")
    } else {
      ""
    }
  })
}

shinyApp(ui, server)
