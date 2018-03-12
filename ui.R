library(shiny)
library(shinyjs)

shinyUI(
  bootstrapPage(
    # We'll add some custom CSS styling -- totally optional
    includeCSS("shinychat.css"),
    
    # And custom JavaScript -- just to send a message when a user hits "enter"
    # and automatically scroll the chat window for us. Totally optional.
    #includeScript("sendOnEnter.js"),
    div(
      # Setup custom Bootstrap elements here to define a new layout
      class = "container-fluid", 
      div(class = "row-fluid",
          # Set the page title
          tags$head(tags$title("ShinyChat")),
          
          # Create the header
          div(class="span6", style="padding: 5px 0px;",
              h1("Friendly Chat"), 
              h5("Based on ShinyChat - featured with Toxic Comments Analysis and Filter functions."),
              h5("By: Huy Tran, email: huytquoc@gmail.com")
          ), div(class="span6", id="play-nice",
            "No toxic comments are allowed in this Chat room... be friendly! be respectful!."
          )
          
      ),
      shinyjs::useShinyjs(),
      # The main panel
      div(
        class = "row-fluid", 
        mainPanel(
          # Create a spot for a dynamic UI containing the chat contents.
          uiOutput("chat"),
          
          # Create the bottom bar to allow users to chat.
          fluidRow(
            # div(class="span10",
            #   textInput("entry", "")
            # ),
            # div(class="span2 center",
            #     actionButton("send", "Send")
            # )
            textInput("entry", ""),
            actionButton("send", "Send"),
            h5("NOTE: Your chat contents are analyzed, detected for toxic comments."),
            h5("The analysis chart is displayed on the right-hand side panel."),
            h5("If your chat contains 'toxic' contents, the 'Send' button will be blocked until you clean up the text."),
            h5("Enjoy friendly chatting. Thank you!")
          )
        ),
        # The right sidebar
        sidebarPanel(
          # Let the user define his/her own ID
          textInput("user", "Your User ID:", value=""),
          tags$hr(),
          h5("Connected Users"),
          # Create a spot for a dynamic UI containing the list of users.
          uiOutput("userList"),
          tags$hr(),
          #helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.<p>Source code available <a href =\"https://github.com/trestletech/ShinyChat\">on GitHub</a>.")),
          plotOutput("toxicPlot")
        )
      )
    )
  )
)
