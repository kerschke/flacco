
ui <- shiny::navbarPage("flaccoGUI",theme = "flacco.css",
   shiny::tabPanel("Single Function Analysis",
   shiny::sidebarLayout(
     featureObject_sidebar("feature_function"),
     shiny::mainPanel(
       shiny::tabsetPanel( # Tabset panel for the different possibilites to put input in the app
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

    #CSV-Import tab for BBob Functions
    shiny::tabPanel("BBOB-Import",
      BBOBImportPage("BBOB_import_page")
    ),

    #CSV-Import für andere smoof funktionen
    shiny::tabPanel("smoof-Import",
             SmoofImportPage("smoof_import_page")
    )
)

server <- function(input, output) {

  featureObject <- shiny::callModule(functionInput, "feature_function",
             stringsAsFactors = FALSE)
  shiny::callModule(FeatureSetCalculation, "featureSet_Calculation",
             stringsAsFactors = FALSE, reactive(featureObject()))
  shiny::callModule(FeatureSetVisualization, "featureSet_Visualization",
              stringsAsFactors = FALSE, reactive(featureObject()))
  shiny::callModule(BBOBImport, "BBOB_import_page",
             stringsAsFactors = FALSE)
  shiny::callModule(SmoofImport, "smoof_import_page",
             stringsAsFactors = FALSE)


}

shiny::shinyApp(ui,server)
