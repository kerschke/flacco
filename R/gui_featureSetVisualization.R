

#' Shiny component for visualizing flacco FeatureSets
#'
#' \code{FeatureSetVisualizationComponent} is a shiny component which can be added to your shiny app
#' so that you can display different Feature Set Graphs.
#'
#' It integrates a select input where the user can select the plot which should be created. 
#'
#'@param id ID for the shiny component
#'@export
FeatureSetVisualizationComponent <- function(id) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::div(
    shiny::uiOutput(ns("visualization_select_output")),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("visualization_method"), "'] == 1 || 
                         input['", ns("visualization_method"), "'] == 2 || 
                         input['", ns("visualization_method"), "'] == 3 "),
      shiny::selectInput(ns("gcm_approach"), label = "GCM-Approach", choices = c("Minimum" = "min", "Mean" = "mean", "Nearest" = "near"))
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("visualization_method"), "'] == 4"),
        shiny::splitLayout(
        shiny::numericInput(ns("ic_xlim_low"),label="Lower X-Limit", value = -5),
        shiny::numericInput(ns("ic_xlim_up"),label="Upper X-Limit ", value = 15))
        #shiny::numericInput(ns("ic_ylim"),label="Y-Limit ", value = 0.5)
    ),
    shiny::plotOutput(ns("visualization_plotOutput")),
    shiny::downloadButton(ns("visualization_downloadBt"), "Download Graphic"))
}


#' Shiny server function for FeatureSet Component
#'
#' \code{FeatureSetVisualization} is a Shiny server function which will control all aspects
#' of the FeatureSetVisualizationComponent UI Module. Will be called with \code{callModule()}
#'
#'It will take the user input and plot the selected visualization. To create a flacco plot, function needs a flacco featureObject.
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringsAsFactors How to treat strings in application (for shiny internally)
#' @param feat.object The featureObject which will be used to generate the flacco plots.
#'
#' @export
#'
FeatureSetVisualization <- function(input, output, session, stringsAsFactors, feat.object) {
  `%then%` <- shiny:::`%OR%`
  ns <- session$ns #in modules use module's namespace for UI components
  output$visualization_select_output <- shiny::renderUI({
    userSelection <- input$visualization_method #retrieve selected value so that user will see same plot again when function has changed
    if (feat.object()["dim"] == 2)
    {
      shiny::selectInput(ns("visualization_method"), label = "Visualization method", choices = c("Function-Plot" = 5, "Cell-Mapping" = 1, "Barrier-Tree 2D" = 2, "Barrier-Tree 3D" = 3, "Information Content" = 4), selected = userSelection)
    } else if (feat.object()["dim"] != 1){
      shiny::selectInput(ns("visualization_method"), label = "Visualization method", choices = c("Information Content" = 4))
    } else if (feat.object()["dim"] == 1){
      shiny::selectInput(ns("visualization_method"), label = "Visualization method", choices = c("Function-Plot" = 5))
    }}
  )
  
  plotflaccoVisualization <- shiny::reactive( {
    if (input$visualization_method == 1)
    {
      return(plotCellMapping(feat.object(), control = list(gcm.approach = input$gcm_approach)))
    } else if (input$visualization_method == 2) {
      return(plotBarrierTree2D(feat.object(), control = list(gcm.approach = input$gcm_approach, bt.cm_surface = FALSE)))
    } else if (input$visualization_method == 3) {
      return(plotBarrierTree3D(feat.object(), control = list(gcm.approach = input$gcm_approach)))
    } else if (input$visualization_method == 4) {
      return(plotInformationContent(feat.object(), control = list(ic.plot.xlim = c(input$ic_xlim_low, input$ic_xlim_up), ic.epsilon = c(0, 10^seq(input$ic_xlim_low, input$ic_xlim_up, length.out = 1000)))))
    } else if (input$visualization_method == 5) {
      if (feat.object()$dim == 1) {
        return(smoof::plot1DNumeric(x = feat.object()$fun, render.levels = TRUE))
      } else if (feat.object()$dim == 2) {
        return(smoof::plot2DNumeric(x = feat.object()$fun, render.levels = TRUE))
      } else if (feat.object()$dim == 3) {
        return(smoof::plot3D(x = feat.object()$fun, render.levels = TRUE))
      }
    }
  })
  
  output$visualization_plotOutput <- shiny::renderPlot({
    plotflaccoVisualization()
  })
  
  output$visualization_downloadBt <- shiny::downloadHandler(
    filename = function() { paste("flacco_plot",as.numeric(Sys.time()), '.png', sep='') },
    content = function(file) {
      grDevices::png(file)
      plotflaccoVisualization()
      grDevices::dev.off()
    },
    contentType = "image/png")
}
