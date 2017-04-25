library(shiny)
library(brapi)
library(DT)
library(shinydashboard)

bdb <- brapi::ba_db()

ndb <- names(bdb)
ndb <- ndb[!ndb %in% "mockbase"]

baui_connect <- function() {
  tagList(
    shiny::selectInput("baui_bdb", "BrAPI database", ndb),
    shiny::checkboxInput("baui_chk_prg", "Use Breeding Programs as filter", value = FALSE),
    shiny::uiOutput("baui_prgs"),
    shiny::uiOutput("baui_stds"),

    shiny::verbatimTextOutput("baui_prms")
  )
}

# Define UI for application that draws a histogram
ui <- dashboardPage(

  # Application title
  dashboardHeader(title = "Connect to a database via BrAPI" ),
  dashboardSidebar(disable = TRUE),
  # Sidebar with a slider input for number of bins
  dashboardBody(

   fluidRow(
   box(title = "Settings", width = 3,
      baui_connect()
   ),

   # Show a plot of the generated distribution
   tabBox(width = 9,

      tabPanel(title = "Overview",
      verbatimTextOutput("con_det"),
      fluidRow(
       box(title = "Breeding program details",
         tableOutput("bdb_det")
       ),
       box(title = "Fieldbook/Study details",
         tableOutput("std_det")
       )
      )
      ),
      tabPanel(title = "Fieldbook",
               div(style = 'overflow-x: scroll',
               DT::dataTableOutput("fdb_det") )
               )
      )



   )
   )


)

baui_fb <- function(input, output, session) {
  con <- reactive({
    brapi::ba_db()[[input$baui_bdb]]
  })

  data_prg <- reactive({
    withProgress(message = "Connecting", detail = "Loading programs",{
      brapi::ba_programs(con())
    })
  })

  data_std <- reactive({
    withProgress(message = "Connecting", detail = "Loading studies",  {
      std <- brapi::ba_studies_search(con())
      if (input$baui_chk_prg) {
        std <- std[std$programDbId == input$progrs, ]
      }
      return(std)
    })
  })

  data_fdb <- reactive({
    withProgress(message = "Connecting", detail = "Loading fieldbook",  {
      std <- brapi::ba_studies_table(con(), input$studs)
      return(std)
    })
  })

  return(
    list(con = con(),
         data_prg = data_prg(),
         data_std = data_std(),
         data_fdb = data_fdb()
         )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # values <-
  #   baui_fb(input, output, session)




  #baui_fb(input, output, session)
  con <- reactive({
    brapi::ba_db()[[input$baui_bdb]]
  })

  data_prg <- reactive({
    withProgress(message = "Connecting", detail = "Loading programs",{
      brapi::ba_programs(con())
    })
  })

  data_std <- reactive({
    withProgress(message = "Connecting", detail = "Loading studies",  {
      std <- brapi::ba_studies_search(con())
      if (input$baui_chk_prg) {
        std <- std[std$programDbId == input$progrs, ]
      }
      return(std)
    })
  })

  data_fdb <- reactive({
    withProgress(message = "Connecting", detail = "Loading fieldbook",  {
      std <- brapi::ba_studies_table(con(), input$studs, rclass = "data.frame")
      return(std)
    })
  })

  data_prm <- reactive({
    nms <- names(data_fdb())
    fil <- stringr::str_detect(nms, "\\|")
    fcs <- nms[!fil]
    vrs <- nms[fil]
    dat <- data_fdb()
    list(
      dsgn = unique(dat[, "studyDesign"]),
      year = unique(dat[, "studyYear"]),
      env  = unique(dat[, "locationName"]),
      geno = unique(dat[, "germplasmName"]),
      blk  = unique(dat[, "blockNumber"]),
      rep  = unique(dat[, "replicate"]),
      fac  = fcs,
      vrs  = vrs
    )
  })

  #########

  output$con_det <- renderPrint({
    req(input$baui_bdb)
    brapi::ba_db()[[input$baui_bdb]]
  })

  output$bdb_det <- renderTable({
    req(input$progrs)
    #datatable(data_prg()[data_prg()$programDbId == input$progrs, ], options = list(dom = "t"))
    if (input$baui_chk_prg) {
      dat <- t(data_prg()[data_prg()$programDbId == input$progrs, ])
      dat <- as.data.frame(cbind(row.names(dat), dat))[, 1:2]
      names(dat) <- c("Variable", "Value")
    } else {
      dat <- NULL
    }
    if (!is.data.frame(dat)) return(NULL)

    dat
  })

  output$std_det <- renderTable({
    req(input$baui_bdb)
    req(input$studs)

      dats <- t(data_std()[data_std()$studyDbId == input$studs, ])
      dats <- as.data.frame(cbind(row.names(dats), dats))[, 1:2]
      if (!is.data.frame(dats)) return(NULL)
      names(dats) <- c("Variable", "Value")
    dats
  })

  output$fdb_det <- renderDataTable({
    req(input$baui_bdb)
    req(input$studs)

    dat <- data_fdb()

    if (!is.data.frame(dat)) return(NULL)

    datatable(dat)
  })

  output$baui_prgs <- renderUI({
    req(input$baui_chk_prg)
    if (input$baui_chk_prg) {
    prg <- as.list(data_prg()$programDbId)
    names(prg) <- data_prg()$name
    selectInput("progrs", "Breeding programs", choices = prg,
                selected = prg[1])
    }
  })

  output$baui_stds <- renderUI({
    std <- as.list(data_std()$studyDbId)
    names(std) <- data_std()$studyName
    #print(std)
    std <- std[!is.na(std)]

    selectInput("studs", "Breeding studies (fieldbooks)", choices = std,
                selected = std[1])
  })

  output$baui_geno <- renderUI({
    req(input$studs)
    if (!is.data.frame(data_fdb())) return(NULL)

    facs <- names(data_fdb())
    filt <- !stringr::str_detect(facs, "\\|")
    facs <- facs[!filt]
    selectInput("geno", "Genotypes", choices = facs, selected = "germplasmName")
  })


  output$baui_prms <- renderPrint({
    req(input$studs)
    data_prm()
  })


}

# Run the application
shinyApp(ui = ui, server = server)

