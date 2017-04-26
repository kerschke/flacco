library(shiny)
library(smoof)
#dependencies for shinyapps.io follow
library(flacco)
library(expm)
library(RANN)
library(plyr)
library(numDeriv)
library(e1071)
library(mda)
library(expm)
library(MASS)
library(Matrix)
library(mlbench)
library(parallel)
library(parallelMap)
library(rpart)
library(rPython)
library(shape)
library(testthat)
library(lhs)

ui = shiny::navbarPage("flaccoGUI",theme = "flacco.css",
  shiny::tabPanel("Single Function Analysis",
    shiny::sidebarLayout(
      featureObject_sidebar("feature_function"),
      shiny::mainPanel(
        # Tabset panel for the different possibilites to put input in the app
        shiny::tabsetPanel(
          shiny::tabPanel("Feature Calculation",
            FeatureSetCalculationComponent("featureSet_Calculation")
          ),
          shiny::tabPanel("Visualization",
            FeatureSetVisualizationComponent("featureSet_Visualization")
          )
        )
      )
    )
  ),
  #CSV-Import tab for BBOB Functions
  shiny::tabPanel("BBOB-Import",
    BBOBImportPage("BBOB_import_page")
  ),

  #CSV-Import for other smoof Functions
  shiny::tabPanel("smoof-Import",
    SmoofImportPage("smoof_import_page")
  )
)

server = function(input, output) {
  featureObject = shiny::callModule(functionInput, "feature_function",
    stringsAsFactors = FALSE)
  shiny::callModule(FeatureSetCalculation, "featureSet_Calculation",
    stringsAsFactors = FALSE, shiny::reactive(featureObject()))
  shiny::callModule(FeatureSetVisualization, "featureSet_Visualization",
    stringsAsFactors = FALSE, shiny::reactive(featureObject()))
  shiny::callModule(BBOBImport, "BBOB_import_page",
    stringsAsFactors = FALSE)
  shiny::callModule(SmoofImport, "smoof_import_page",
    stringsAsFactors = FALSE)
}

shiny::shinyApp(ui,server)
