library(shiny)
library(brapi)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- dashboardPage(

  # Application title
  dashboardHeader(title = "Connect to a database via BrAPI" ),
  dashboardSidebar(disable = TRUE),
  # Sidebar with a slider input for number of bins
  dashboardBody(

   fluidRow(
   shinydashboard::tabBox(width = 4,
     tabPanel(title = "About",
       HTML("This is a small demo of using the BrAPI protocol to access phenotypic trial data.")
     ),
     tabPanel(title = "Settings",
      brapiUI::baui_connect()
     )
   ),
   box(title = "Fieldbook overview", width = 8,
      shiny::verbatimTextOutput("baui_prms")
   )
   )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  out <- brapiUI::basv_prms(input, output, session)
}

# Run the application
shinyApp(ui = ui, server = server)

