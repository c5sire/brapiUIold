library(shiny)
library(brapi)
library(DT)

bdb <- brapi::ba_db()

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Connect to BrAPI database"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        shiny::selectInput("bdb", "BrAPI database", names(bdb)),
        shiny::uiOutput("ui_prgs"),
        shiny::uiOutput("ui_stds")

      ),

      # Show a plot of the generated distribution
      mainPanel(
         verbatimTextOutput("con_det"),
         DT::dataTableOutput("bdb_det"),
         DT::dataTableOutput("std_det")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  con <- reactive({
    brapi::ba_db()[[input$bdb]]
  })

  data_prg <- reactive({
    withProgress({
      brapi::ba_programs(con())
    })
  })

  data_std <- reactive({
    withProgress({
      prg <- brapi::ba_studies_search(con())
      prg[prg$programDbId == input$progrs, ]
    })
  })

  output$con_det <- renderPrint({
    req(input$bdb)
    brapi::ba_db()[[input$bdb]]
  })

  output$bdb_det <- DT::renderDataTable({
    req(input$progrs)
    datatable(data_prg()[data_prg()$programDbId == input$progrs, ], options = list(dom = "t"))
  })

  output$std_det <- DT::renderDataTable({
    req(input$studs)
    datatable(data_std()[data_std()$studyDbId == input$studs, ], options = list(dom = "t"))
  })

  output$ui_prgs <- renderUI({
    prg <- as.list(data_prg()$programDbId)
    names(prg) <- data_prg()$name
    selectInput("progrs", "Breeding programs", choices = prg,
                selected = prg[1])
  })

  output$ui_stds <- renderUI({
    std <- as.list(data_std()$studyDbId)
    names(std) <- data_std()$studyName
    selectInput("studs", "Breeding studies (fieldbooks)", choices = std,
                selected = std[1])
  })


}

# Run the application
shinyApp(ui = ui, server = server)

