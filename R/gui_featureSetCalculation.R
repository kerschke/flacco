

#' Shiny ui-module for calculating and displaying flacco FeatureSets
#'
#' \code{FeatureSetCalculationComponent} is a shiny ui-component which can be added to your shiny app
#' so that you can calculate and display different Feature Sets.
#'
#' The component integrates a select-Input for choosing the FeatureSet, which should be calculated and displayed in a table.
#' With the download button the calculated features can be exported as CSV-file
#'
#'@param id ID for the shiny component
#'@export
FeatureSetCalculationComponent <- function(id) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::div(
    shiny::selectInput(ns("FeatureSet_function"),label="Feature Set",choices=c("all Features",listAvailableFeatureSets()), selected = "cm_angle"),
    shiny::tableOutput(ns("FeatureTable_function")),
    shiny::downloadButton(ns('downloadData_function'), 'Download'))
}


#' Shiny server function for FeatureSet Component
#'
#' \code{FeatureSetCalculation} is a Shiny server function which will control all aspects
#' of the FeatureSetCalculationComponent UI Module. Will be called with \code{callModule()}
#' 
#' It will take the user input and calculate the selected featureSet. To calculate a featureSet the function needs a flacco featureObject.
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringsAsFactors How to treat strings in application (for shiny internally)
#' @param feat.object The featureObject which will be used to calculate the featureSets. 
#'
#' @export
#'
FeatureSetCalculation <- function(input, output, session, stringsAsFactors, feat.object) {
  `%then%` <- shiny:::`%OR%`
  
  # wrap the feature calculation in a reactive so it will only recalculated, when user input has changed
  features <- shiny::reactive({
    if (input$FeatureSet_function == "all Features")
    {
      features <- flacco::calculateFeatures(feat.object()) #calculate all the features
      features <- data.frame(t(data.frame(features)),stringsAsFactors=FALSE) #flip data.frame around
    } else {
      print(feat.object)
      features <- flacco::calculateFeatureSet(feat.object(), set = input$FeatureSet_function) #calculate the features
      features <- data.frame(t(data.frame(features)),stringsAsFactors=FALSE) #flip data.frame around
    }
    return(features)
  })

  #render Table with features
  output$FeatureTable_function <- shiny::renderTable({
    features()
  },rownames = TRUE,colnames=FALSE)

  #click Function for the download button
  output$downloadData_function <- shiny::downloadHandler(
    filename = function() { paste(input$FeatureSet_function, '.csv', sep='') },
    content = function(file) {
      utils::write.csv(features(), file)
    }
  )
}
