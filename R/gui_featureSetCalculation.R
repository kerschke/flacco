#' Shiny UI-Module for Calculating and Displaying Feature Sets
#'
#' \code{FeatureSetCalculationComponent} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}}
#' UI-component which can be added to your \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} app so
#' that you can calculate and display different feature sets.
#'
#' The component integrates a select-Input for choosing the feature set,
#' which should be calculated and displayed in a table.
#' With the download button the calculated features can be exported as CSV-file.
#'
#' @template arg_ui_id
#'
#' @export
FeatureSetCalculationComponent = function(id) {
  # Create a namespace function using the provided id
  ns = shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::div(
    shiny::selectInput(ns("FeatureSet_function"), label = "Feature Set",
      choices = c("all Features", listAvailableFeatureSets()), selected = "cm_angle"),
    shiny::tableOutput(ns("FeatureTable_function")),
    shiny::downloadButton(ns('downloadData_function'), 'Download'))
}


#' Shiny Server Function for Feature Set Component
#'
#' \code{FeatureSetCalculation} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} server function
#' which will control all aspects of the \code{FeatureSetCalculationComponent}
#' UI Module. Will be called with \code{\link[shiny]{callModule}}.
#'
#' It will take the user input and calculate the selected feature set.
#' In order to calculate a feature set, the function needs a \code{\link{FeatureObject}}.
#'
#' @template arg_ui_input
#' @template arg_ui_output
#' @template arg_ui_session
#' @template arg_ui_stringsAsFactors
#' @template arg_feat_object
#'
#' @export
# FIXME: Do we need the session parameter? It's not used within the function.
FeatureSetCalculation = function(input, output, session, stringsAsFactors, feat.object) {
  # wrap the feature calculation in a reactive so it will only recalculated, when user input has changed
  features = shiny::reactive({
    if (input$FeatureSet_function == "all Features") {
      # calculate all the features
      features = calculateFeatures(feat.object(), control = list(ela_curv.sample_size = min(200L, feat.object()$n.obs)))
      # flip data.frame
      features = data.frame(t(data.frame(features)), stringsAsFactors = stringsAsFactors)
    } else {
      print(feat.object)
      # calculate the features
      features = calculateFeatureSet(feat.object(), set = input$FeatureSet_function,
        control = list(ela_curv.sample_size = min(200L, feat.object()$n.obs)))
      # flip data.frame
      features = data.frame(t(data.frame(features)), stringsAsFactors = stringsAsFactors)
    }
    return(features)
  })

  # render table with features
  output$FeatureTable_function = shiny::renderTable({
    features()
  }, rownames = TRUE, colnames = FALSE)

  # click function for the download button
  output$downloadData_function = shiny::downloadHandler(
    filename = function() {
      paste0(input$FeatureSet_function, '.csv')
    },
    content = function(file) {
      utils::write.csv(features(), file)
    }
  )
}
