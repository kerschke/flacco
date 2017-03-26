library(flacco)
library(shiny)
`%then%` <- shiny:::`%OR%`


#' Shiny component for Function Input
#'
#' \code{featObject_sidebar} is a shiny component which can be added to your shiny app
#' so that you can easily generate a feature object by putting in all relevant information
#'
#'
#'@param id ID for the shiny component
#'@export
featureObject_sidebar <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  #sidebar for the configuration of different function parameters
    sidebarPanel(
      radioButtons(ns("function_option"), label = "Function input",
                   choices = list("User defined function" = 1, "smoof" = 2, "BBOB"=4, "File-Import" = 3),
                   selected = 1),

      conditionalPanel(
        condition = paste0("input['", ns("function_option"), "'] == 1"),
        textInput(ns("function_input"), label="Function")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("function_option"), "'] == 2"),
        selectInput(ns("smoof_function_select"), label = "Function name", choices = filterFunctionsByTags("single-objective"))
      ),

      conditionalPanel(
        condition = paste0("input['", ns("function_option"), "'] == 3"),
        fileInput(ns("import_file"), label = "File to import")
      ),

      conditionalPanel(
        condition = paste0("input['", ns("function_option"), "'] == 4"),
        splitLayout(
          numericInput(ns("BBOB_fid"),label="BBOB-FID", value = 1),
          numericInput(ns("BBOB_iid"),label="BBOB-IID ", value = 1))
      ),

      conditionalPanel(
        condition = paste0("input['", ns("function_option"), "'] != 3"),
        splitLayout(
          numericInput(ns("dimension_size"), label="Dimensions", value = 1),
          selectInput(ns("sampletype"), label = "Sample type", choices = c("random","lhs"))),

        splitLayout( #put lower and upper bound in one line
          textInput(ns("samplelow"), label="Lower bound", value = "0"),
          textInput(ns("sampleup"), label="Upper bound", value = "1")),
        sliderInput(ns("ssize"),
                    "Sample size",
                    min = 100,
                    max = 10000,
                    value = 30)
      ),
      textInput(ns("block_input"), label="Blocks (comma sperated per dimension)")
    )
}

#' Shiny server function for feature calculation of function input
#'
#' \code{functionInput} is a Shiny server function which will control all aspects
#' of the FlaccoFunctionInput UI Module. Will be called with \code{callModule()}
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringAsFactors
#'
#' @export
#'
functionInput <- function(input, output, session, stringsAsFactors) {

    #function to control the output data if smoof is selected for function input
    smoof_input_createFeatures <- reactive({
      # smoofFunctionPage  is using the smoof package for implementing them
      if (!requireNamespace("smoof", quietly = TRUE)) {
        stop("smoof needed for this function to work. Please install it.",
             call. = FALSE)
      }
      # transform the text input for lower and upper bound to an vector with the respective values
      validate(
        need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X <- flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control = ctrl)
      f <- smoof::makeFunctionsByName(input$smoof_function_select, dimensions = input$dimension_size)
      y <- apply(X, 1, f[[1]])
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        validate(
            need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
            need(try(feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f[[1]], blocks=blocks)), "Please insert a valid function")
        )
      } else {
        feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f[[1]])
      }
      feat.object
    })

    #function to control the output data if smoof BBOB is selected for function input
    smoof_BBOB_input_createFeatures <- reactive({
      # smoofFunctionPage  is using the smoof package for implementing them
      if (!requireNamespace("smoof", quietly = TRUE)) {
        stop("smoof needed for this function to work. Please install it.",
             call. = FALSE)
      }
      # transform the text input for lower and upper bound to an vector with the respective values
      validate(
        need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X <- flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control = ctrl)
      f <- smoof::makeBBOBFunction(dimension = input$dimension_size, fid = input$BBOB_fid, iid = input$BBOB_iid)
      y <- apply(X, 1, f)
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        validate(
          need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          need(try(feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f, blocks=blocks)), "Please insert a valid function")
        )
      } else {
        feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f)
      }
      feat.object
    })


    #reactive element which is called when function input is selected and which controlls the output of features
    createFeatures_function <- reactive({
      # transform the text input for lower and upper bound to an vector with the respective values
      validate(
        need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X = flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control=ctrl)

      #check if there is a block input
      if (input$block_input==""){
        # validate if the function the user has put in can be evaluated by R
        validate(
          need(try( f <- eval(parse(text=paste("function(x) ",input$function_input)))), "Please insert a valid function") %then%
          need(try(feat.object <- flacco::createFeatureObject(X = X, fun = f)), "Please insert a valid function")
        )
      } else {
        validate(
          need(try( f <- eval(parse(text=paste("function(x) ",input$function_input)))), "Please insert a valid function") %then%
          need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          need(try(feat.object <- flacco::createFeatureObject(X = X, fun = f, blocks=blocks)), "Please insert a valid function")

        )
      }
      feat.object
    })


    #function for controlling the file input app
    createFeatures_import <- reactive({
      importdata=read.csv(input$import_file$datapath) #load values from uploaded file
      #y <- apply(X, 1, eval(parse(text=paste("function(x) ",input$function_input))))
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        validate(
          need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          need(try(feat.object <- flacco::createFeatureObject(X = data.frame(importdata[,-ncol(importdata)]), y = importdata[,2], blocks=blocks)), "Please insert valid funciton values")
        )
      } else {
        feat.object <- flacco::createFeatureObject(X = data.frame(importdata[,-ncol(importdata)]), y = importdata[,2])
      }
      feat.object
    })

    featureObject <- reactive({
      if (input$function_option==1)
      {
        createFeatures_function()
      } else if (input$function_option==2) {
        smoof_input_createFeatures()
      } else if (input$function_option==3) {
        createFeatures_import()
      } else if (input$function_option==4) {
        smoof_BBOB_input_createFeatures()
      }
    })

    return(featureObject)
}

