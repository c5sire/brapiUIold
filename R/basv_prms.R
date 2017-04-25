
#' basv_prms
#'
#' server backend to get a fieldbook
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#'
#' @return list of data and paramter
#' @export
basv_prms <- function(input, output, session){

  con <- shiny::reactive({
    brapi::ba_db()[[input$baui_bdb]]
  })

  data_prg <- shiny::reactive({
    shiny::withProgress(message = "Connecting", detail = "Loading programs",{
      brapi::ba_programs(con())
    })
  })

  data_std <- shiny::reactive({
    shiny::withProgress(message = "Connecting", detail = "Loading studies",  {
      std <- brapi::ba_studies_search(con())
      if (input$baui_chk_prg) {
        std <- std[std$programDbId == input$progrs, ]
      }
      return(std)
    })
  })

  data_fdb <- shiny::reactive({
    shiny::withProgress(message = "Connecting", detail = "Loading fieldbook",  {
      std <- brapi::ba_studies_table(con(), input$studs, rclass = "data.frame")
      return(std)
    })
  })

  data_prm <- shiny::reactive({
    nms <- names(data_fdb())
    fil <- stringr::str_detect(nms, "\\|")
    fcs <- nms[!fil]
    vrs <- nms[fil]
    dat <- data_fdb()

    com <- 100 - mean(is.na(dat[, vrs])) * 100
    dpt <- nrow(dat[, vrs]) * length(vrs)

    list(
      comp = com,
      dtpt = dpt,
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


  output$baui_prgs <- shiny::renderUI({
    shiny::req(input$baui_chk_prg)
    if (input$baui_chk_prg) {
      prg <- as.list(data_prg()$programDbId)
      names(prg) <- data_prg()$name
      shiny::selectInput("progrs", "Breeding programs", choices = prg,
                  selected = prg[1])
    }
  })

  output$baui_stds <- shiny::renderUI({
    std <- as.list(data_std()$studyDbId)
    names(std) <- data_std()$studyName
    #print(std)
    std <- std[!is.na(std)]

    shiny::selectInput("studs", "Breeding studies (fieldbooks)", choices = std,
                selected = std[1])
  })


  output$baui_prms <- shiny::renderPrint({
    shiny::req(input$studs)
    data_prm()
  })

  return(list(
    prgs = data_prg,
    stds = data_std,
    data = data_fdb,
    prms = data_prm
  ))
}
