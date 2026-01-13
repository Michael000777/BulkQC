#' pca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' pca Server Functions
#'
#' @noRd 
mod_pca_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_pca_ui("pca_1")
    
## To be copied in the server
# mod_pca_server("pca_1")
