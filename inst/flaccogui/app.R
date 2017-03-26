library(shiny)
library(smoof)
library(flaccogui)
#dependencies for shinyapps.io follow
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
library(shape)
library(testthat)
library(lhs)

ui <- navbarPage("flaccoGUI",theme = "flacco.css",
  tabPanel("Single Function Analysis",
   sidebarLayout(
     featureObject_sidebar("feature_function"),
     mainPanel(
       tabsetPanel( # Tabset panel for the different possibilites to put input in the app
        tabPanel("Feature Calculation",
          FeatureSetCalculationComponent("featureSet_Calculation")
        ),
        tabPanel("Visualization",
          FeatureSetVisualizationComponent("featureSet_Visualization")
        )
       )
     )
    )
  ),

    #CSV-Import tab for BBob Functions
    tabPanel("BBOB-Import",
      BBOBImportPage("BBOB_import_page")
    ),

    #CSV-Import für andere smoof funktionen
    tabPanel("smoof-Import",
             SmoofImportPage("smoof_import_page")
    )
)

server <- function(input, output) {

  featureObject <- callModule(functionInput, "feature_function",
             stringsAsFactors = FALSE)
  callModule(FeatureSetCalculation, "featureSet_Calculation",
             stringsAsFactors = FALSE, reactive(featureObject()))
  callModule(FeatureSetVisualization, "featureSet_Visualization",
              stringsAsFactors = FALSE, reactive(featureObject()))
  callModule(BBOBImport, "BBOB_import_page",
             stringsAsFactors = FALSE)
  callModule(SmoofImport, "smoof_import_page",
             stringsAsFactors = FALSE)


}

shinyApp(ui,server)
