library(flacco)
library(shiny)
`%then%` <- shiny:::`%OR%`


#' Shiny component for visualizing flacco FeatureSets
#'
#' \code{FeatureSetVisualizationComponent} is a shiny component which can be added to your shiny app
#' so that you can display different Feature Set Graphs.
#'
#' It will get a flacco feature.object for the next steps
#'
#'@param id ID for the shiny component
#'@export
FeatureSetVisualizationComponent <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Sidebar with a slider input for the number of bins
  div(
    uiOutput(ns("visualization_select_output")),
    plotOutput(ns("visualization_plotOutput")))
}


#' Shiny server function for FeatureSet Component
#'
#' \code{FeatureSetVisualization} is a Shiny server function which will control all aspects
#' of the FeatureSetVisualizationComponent UI Module. Will be called with \code{callModule()}
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringAsFactors
#'
#' @export
#'
FeatureSetVisualization <- function(input, output, session, stringsAsFactors, feat.object) {
  ns <- session$ns #in modules use module's namespace for UI components
  output$visualization_select_output <- renderUI({
    userSelection <- input$visualization_method #retrieve selected value so that user will see same plot again when function has changed
    if (feat.object()["dim"] == 2)
    {
      selectInput(ns("visualization_method"), label = "Visualization method", choices = c("Cell-Mapping" = 1, "Barrier-Tree 2D" = 2, "Barrier-Tree 3D" = 3, "Information Content" = 4), selected = userSelection)
    } else {
      selectInput(ns("visualization_method"), label = "Visualization method", choices = c("Information Content" = 4))
    }}
  )
  output$visualization_plotOutput <- renderPlot({
    if (input$visualization_method == 1)
    {
      plotCellMapping(feat.object(), control = list(gcm.approach = "near"))
    } else if (input$visualization_method == 2) {
      plotBarrierTree2D(feat.object(), control = list(gcm.approach = "near", bt.cm_surface = FALSE))
    } else if (input$visualization_method == 3) {
      plotBarrierTree3D(feat.object(), control = list(gcm.approach = "near"))
    } else if (input$visualization_method == 4) {
      plotInformationContent(feat.object())
    }
  })
}
