library(shiny)

library(stringr)
library(quanteda)

# source our models and global variables
source('~/R Workspace/data science class/10- capstone/SwiftKeyProject/src/model_4_4_util.R', echo = FALSE)

# load pre-calculated model as this is an expensive operation
model <- load_model_from_cache("model-4-4.001-a.cache")

ui <- fluidPage(
  # Application title
  titlePanel("Word prediction app [model 4.4]"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Text to process", "Enter more text here", width = "300px", height = "300px"),
      sliderInput("discount_factor", "Backoff discount factor:", min = 0, max = 1, value = .5),
      checkboxInput("use_unigram", "Use unigram in backoff:", FALSE)
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  
  do_prediction <- eventReactive(input$text, {
    s <- input$text
    
    # only predict when a space is pressent at the end.
    # could also predice if the last word is known (in model[[1]])
    space <- if_else((s != ""), (str_sub(s, str_length(s)) == " "), FALSE)
    if (!space) return(NULL)
      
    clean_sentences <- clean_data(list(s))
    clean_text_split <- str_split(clean_sentences[length(clean_sentences)], " ")[[1]]
    
    m <- length(clean_text_split)
    
    # print(s)
    # print(clean_text_split)
    # print(m)
    
    if (m < 3) return("Enter 3 or more words to start prediction algorithm")
    
    text_to_predict <- paste(clean_text_split[m - 2], clean_text_split[m - 1], clean_text_split[m])
    
    # print(text_to_predict)
    # print(typeof(text_to_predict))
    # print(class(text_to_predict))
    
    predicted_words <- predict_words(model, 
                                     text_to_predict, 
                                     discount_factor = input$discount_factor, 
                                     use_unigram = input$use_unigram,
                                     max_model_level = 4)
    if (!is.null(predicted_words)) return(predicted_words)
    
    return("can't predict - no match")
  })
  
  output$prediction <- renderText({
    dt_predic <- do_prediction()
    
    # cat(paste('dt_predic typeof = ', typeof(dt_predic)))
    if (!is.null(dt_predic)) print(dt_predic)
    
    if ("data.frame" %in% class(dt_predic)) {
      pretty_fmt_prediction(dt_predic)
    }
  })

}

shinyApp(ui, server)
