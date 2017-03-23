# ---------------------------------------------------------------------
# model #5.0 - Final model
# shiny application
#
# this shiny app will display a text box where the user can type text. 
# when a space is entered at the end and there are enought words, a
# prediction is made and the top words are displayed.
# ---------------------------------------------------------------------

# we have a symlink from shiny_app/R folder to source R folder
source('R/globals.R')
source('R/bad_words.R')
source('R/model_util.R')

library(shiny)

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

# model to use
model_fname <- paste0(MODEL_ID, ".001-c.cache")

# ---------------------------------------------------------------------
# initialization
# ---------------------------------------------------------------------

# initialize the logger and start logging...
init_logger(threshold = DEBUG, filename = "shiny-app", timestamp = TRUE, tee = FALSE)
flog.info("start: shiny_app")

# load pre-calculated model as this is an expensive operation
model <- load_model_from_cache(model_fname)
flog.info(paste("using model ", model_fname))

# ---------------------------------------------------------------------
# shiny functions
# ---------------------------------------------------------------------

ui <- fluidPage(
  # Application title
  titlePanel(paste0("Word prediction app [", MODEL_ID, "]")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Text to process", "Enter more text here", width = "300px", height = "300px"),
      sliderInput("discount_factor", "Backoff discount factor:", min = 0, max = 1, value = .5),
      checkboxInput("use_unigram", "Use unigram in backoff:", TRUE)
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("prediction"),
      uiOutput("word1"),
      uiOutput("word2"),
      uiOutput("word3"),
      uiOutput("word4"),
      uiOutput("word5")
    )
  )
)

server <- function(input, output, session) {
  
  do_prediction <- eventReactive(input$text, {
    s <- input$text
    
    # only predict when a space is pressent at the end.
    # could also predice if the last word is known (in model[[1]])
    space <- if_else((s != ""), (str_sub(s, str_length(s)) == " "), FALSE)
    if (!space) return(NULL)
      
    clean_sentences <- clean_documents(list(s))
    clean_text_split <- str_split(clean_sentences[length(clean_sentences)], " ")[[1]]
    
    m <- length(clean_text_split)
    
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
  
  # add buttons for preducted words
  
  add_button <- function(i) {
    dt_predic <- do_prediction()
    
    if ("data.frame" %in% class(dt_predic) && i <= nrow(dt_predic)) {
      # TODO: add later fluidRow....and top 5 words
      actionButton(paste0("word", i), dt_predic[i]$word)
    }
  }
  
  output$word1 <- renderUI({ add_button(1) })
  output$word2 <- renderUI({ add_button(2) })
  output$word3 <- renderUI({ add_button(3) })
  output$word4 <- renderUI({ add_button(4) })
  output$word5 <- renderUI({ add_button(5) })
  
  # process add buttons for preducted words
  
  update_text <- function(i) {
    dt_predic <- do_prediction()
    updateTextAreaInput(session, inputId = "text", value = paste0(input$text, dt_predic[i]$word, " "))
  }
  
  observeEvent(input$word1, { update_text(1) })
  observeEvent(input$word2, { update_text(2) })
  observeEvent(input$word3, { update_text(3) })
  observeEvent(input$word4, { update_text(4) })
  observeEvent(input$word5, { update_text(5) })
}

shinyApp(ui, server)
