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
    ),
    # shiny::plotOutput(ns("visualization_plotOutput"))
    shiny::conditionalPanel(
      condition = paste0("input['", ns("visualization_method"), "'] == 1 ||
        input['", ns("visualization_method"), "'] == 2 ||
        input['", ns("visualization_method"), "'] == 3 ||
        input['", ns("visualization_method"), "'] == 4"),
      shiny::plotOutput(ns("visualization_plotOutput"))
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("visualization_method"), "'] == 5 ||
        input['", ns("visualization_method"), "'] == 6 ||
        input['", ns("visualization_method"), "'] == 7"),
      plotly::plotlyOutput(ns("visualization_plotlyOutput"))
    )
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
        choices = c("Function Plot" = 7))
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
    }
  })

  plotlyflaccoVisualization = shiny::reactive({
    feat.obj = feat.object()
    length.out = 150L
    foo = feat.obj$fun
    ps = ParamHelpers::getParamSet(foo)
    ps$pars[[1]]$lower = feat.obj$lower
    ps$pars[[1]]$upper = feat.obj$upper
    attr(foo, "par.set") = ps
    if (feat.obj$dim == 2) {
      lower = ParamHelpers::getLower(ps)
      upper = ParamHelpers::getUpper(ps)
      x = seq(lower[1], upper[1], length.out = length.out)
      y = seq(lower[2], upper[2], length.out = length.out)
      grid = expand.grid(x, y)
      z = apply(grid, 1, foo)
      if (input$visualization_method == 6) {
        df = data.frame(x = x, y = y, z = z)
        return(plotly::plot_ly(x = x, y = y, z = matrix(z, length.out, length.out), type = "surface"))
      } else if (input$visualization_method == 5) {
        grid2 = cbind(grid, z)
        colnames(grid2) = c("x", "y", "z")
        g = ggplot2::ggplot(data = grid2)
        g = g + ggplot2::aes(x = x, y = y, z = z, fill = z)
        g = g + ggplot2::geom_tile()
        g = g + ggplot2::stat_contour(colour = "black")
        g = g + ggplot2::ggtitle(smoof::getName(foo))
        g = g + ggplot2::scale_fill_distiller(palette = "Spectral")
        g = g + ggplot2::theme(legend.position = "none",
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
        return(plotly::plotly_build(g))
      }
    } else if (feat.obj$dim == 1) {
      lower = ParamHelpers::getLower(ps)
      upper = ParamHelpers::getUpper(ps)
      ## the following are basically a copy of smoof:::generateDataframeForGGPlot
      df = do.call(expand.grid, list(seq(lower, upper, length.out = length.out)))
      df$y = apply(df, 1, foo)
      df.list = ParamHelpers::dfRowsToList(par.set = ps, df = df)
      df$y = unlist(lapply(df.list, function(df.row) {
        return(foo(df.row))
      }))
      g = ggplot2::ggplot(data = df, ggplot2::aes(x = df[,1], y = df[,2]))
      g = g + ggplot2::geom_line()
      g = g + ggplot2::xlab(ps$pars[[1]]$id)
      g = g + ggplot2::ylab("y")
      g = g + ggplot2::ggtitle(smoof::getName(foo))
      g = g + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
      return(plotly::plotly_build(g))
    }
  })

  output$visualization_plotOutput = shiny::renderPlot({
    plotflaccoVisualization()
  })

  output$visualization_plotlyOutput = plotly::renderPlotly({
    plotlyflaccoVisualization()
  })
}
