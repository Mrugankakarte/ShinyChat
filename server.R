library(shiny)
library(stringr)
library(keras)
library(ggplot2)
library(tm)
library(qdap)

load("tx_model.RData")

model_best <- load_model_hdf5("model_tx_ml_class.hdf5")
tokenizer <- load_text_tokenizer("tx_tokenizer")

# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to Shiny Chat!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

shinyServer(function(input, output, session) {
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                     tags$span(class="user-exit",
                       sessionVars$username,
                       "left the room.")))
    })
  })
  
  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                        tags$span(class="user-enter",
                          sessionVars$username,
                          "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Note the change in the chat log
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                        tags$span(class="user-change",
                          paste0("\"", sessionVars$username, "\""),
                          " -> ",
                          paste0("\"", input$user, "\""))))
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
        # Add the current entry to the chat log.
        vars$chat <<- c(vars$chat, 
                        paste0(linePrefix(),
                               tags$span(class="username",
                                         tags$abbr(title=Sys.time(), sessionVars$username)
                               ),
                               ": ",
                               tagList(input$entry)))
      
      })
      # Clear out the text entry field.
      updateTextInput(session, "entry", value="")
    
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  chatInput <- reactive({
    chat_text <- clean_text(input$entry)
    #print(chat_text)
    chat_matrix <- texts_to_matrix(tokenizer, list(chat_text), mode = "binary")
    chat_pred <- predict(model_best,chat_matrix)
    
    level <- chat_pred
    type <- labels
    level <- as.vector(level)
    df <- data.frame(type = type, level = level)
    df
  })
  
  # Fill in the spot we created for a plot
  output$toxicPlot <- renderPlot({
    
    df <- chatInput()
    df$type <- factor(df$type, levels = df$type)
    toxic_level <- findInterval(df$level, c(0, 0.25, 0.5, 0.75, 1.5))
    
    g <- ggplot(df, aes(x = type, y = toxic_level, fill = toxic_level))
    g <- g + geom_bar(stat = "identity")
    g <- g + scale_fill_gradient2(low="green", high = "red",mid = "orange", midpoint = 2,
                                  na.value = "transparent",
                                  breaks=c(0,1,2,3,4),labels=c("None","Low","Med","High","Very High"),
                                  limits=c(0,4))
    g <- g +  ylim(0,4)
    g <- g + labs(x = "Toxic Types", y = "Toxic Level", colour = "Toxic Levels\n", 
                  title = "Toxic Comments Analysis")
    g <- g + theme(axis.text.x = element_text(face="bold", size = 11, 
                                              color = "brown", angle = 45, hjust = 1),
                   plot.title=element_text(size=15, face="bold", color="darkgreen"),
                   axis.title.x=element_text(size=12),
                   axis.title.y =element_text(size=12),
                   legend.title=element_blank())
    g
    #Render a barplot
    
  })
  observe({
    df <- chatInput()
    #print(sum(df$level))
    if(sum(df$level)>=1.5){
      shinyjs::disable("send")
    } else {
      shinyjs::enable("send")
    }
  })
})
