#' Shiny Component for Visualizing the Feature Sets
#'
#' \code{FeatureSetVisualizationComponent} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} component
#' which can be added to your \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} app so that you can display
#' different feature set plots.
#'
#' It integrates a select input where the user can select the plot which should be created. 
#'
#' @template arg_ui_id
#'
#' @export
FeatureSetVisualizationComponent = function(id) {
  # Create a namespace function using the provided id
  ns = shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::div(
    shiny::uiOutput(ns("visualization_select_output")),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("visualization_method"), "'] == 1 || 
        input['", ns("visualization_method"), "'] == 2 || 
        input['", ns("visualization_method"), "'] == 3 "),
      shiny::selectInput(ns("gcm_approach"), label = "GCM-Approach",
        choices = c("Minimum" = "min", "Mean" = "mean", "Nearest" = "near"))
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("visualization_method"), "'] == 4"),
        shiny::splitLayout(
          shiny::numericInput(ns("ic_xlim_low"), label = "Lower X-Limit", value = -5),
          shiny::numericInput(ns("ic_xlim_up"), label = "Upper X-Limit ", value = 15))
          #shiny::numericInput(ns("ic_ylim"),label="Y-Limit ", value = 0.5)
    ),
    shiny::plotOutput(ns("visualization_plotOutput"))
  )
}


#' Shiny Server Function for Feature Set Component
#'
#' \code{FeatureSetVisualization} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} server function
#' which will control all aspects of the \code{FeatureSetVisualizationComponent}
#' UI-Module. It will be called with \code{\link[shiny]{callModule}}.
#'
#' It will take the user input and plot the selected visualization. To create a
#' flacco plot, the function needs a \code{\link{FeatureObject}}.
#'
#' @template arg_ui_input
#' @template arg_ui_output
#' @template arg_ui_session
#' @template arg_ui_stringsAsFactors
#' @template arg_feat_object
#'
#' @export
#'
FeatureSetVisualization = function(input, output, session, stringsAsFactors, feat.object) {
  # in modules use module's namespace for UI components
  ns = session$ns
  output$visualization_select_output = shiny::renderUI({
    # retrieve selected value so that user will see same plot again once the function has changed
    userSelection = input$visualization_method
    if (feat.object()["dim"] == 2) {
      shiny::selectInput(ns("visualization_method"), label = "Visualization method",
        choices = c("Contour Plot" = 5, "Surface Plot" = 6, 
          "Cell-Mapping" = 1, "Barrier-Tree 2D" = 2,
          "Barrier-Tree 3D" = 3, "Information Content" = 4), selected = userSelection)
    } else if (feat.object()["dim"] == 1) {
      shiny::selectInput(ns("visualization_method"), label = "Visualization method",
        choices = c("Function Plot" = 5))
    } else if (feat.object()["dim"] > 2){
      shiny::selectInput(ns("visualization_method"), label = "Visualization method",
        choices = c("Information Content" = 4))
    }
  })

  plotflaccoVisualization = shiny::reactive({
    if (input$visualization_method == 1) {
      return(plotCellMapping(feat.object(), control = list(gcm.approach = input$gcm_approach)))
    } else if (input$visualization_method == 2) {
      return(plotBarrierTree2D(feat.object(),
        control = list(gcm.approach = input$gcm_approach, bt.cm_surface = FALSE)))
    } else if (input$visualization_method == 3) {
      return(plotBarrierTree3D(feat.object(),
        control = list(gcm.approach = input$gcm_approach)))
    } else if (input$visualization_method == 4) {
      return(plotInformationContent(feat.object(),
        control = list(ic.plot.xlim = c(input$ic_xlim_low, input$ic_xlim_up),
          ic.epsilon = c(0, 10^seq(input$ic_xlim_low, input$ic_xlim_up, length.out = 1000)))))
    } else if (input$visualization_method == 5) {
      feat.obj = feat.object()
      foo = feat.obj$fun
      ps = ParamHelpers::getParamSet(foo)
      ps$pars[[1]]$lower = feat.obj$lower
      ps$pars[[1]]$upper = feat.obj$upper
      attr(foo, "par.set") = ps
      if (feat.object()$dim == 1) {
        return(smoof::plot1DNumeric(x = foo, render.levels = TRUE))
      } else if (feat.object()$dim == 2) {
        return(smoof::plot2DNumeric(x = foo, render.levels = TRUE))
      }
    } else if (input$visualization_method == 6) {
      feat.obj = feat.object()
      foo = feat.obj$fun
      ps = ParamHelpers::getParamSet(foo)
      ps$pars[[1]]$lower = feat.obj$lower
      ps$pars[[1]]$upper = feat.obj$upper
      attr(foo, "par.set") = ps
      return(smoof::plot3D(x = foo, render.levels = TRUE))
      # return(smoof::plot3D(x = foo, package = "plotly"))
    }
  })

  output$visualization_plotOutput = shiny::renderPlot({
    plotflaccoVisualization()
  })
}
