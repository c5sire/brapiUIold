
#' baui_connect
#'
#' Provides interface to connect to BrAPI database
#' @export
baui_connect <- function() {
  bdb <- brapi::ba_db()

  ndb <- names(bdb)
  ndb <- ndb[!ndb %in% c("mockbase", "ricebase")]
  ndb <- ndb[stringr::str_detect(ndb, "base")]

  shiny::tagList(
    shiny::selectInput("baui_bdb", "BrAPI database", ndb),
    shiny::checkboxInput("baui_chk_prg", "Use Breeding Programs as filter", value = FALSE),
    shiny::uiOutput("baui_prgs"),
    shiny::uiOutput("baui_stds")
  )
}
