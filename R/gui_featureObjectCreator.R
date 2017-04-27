

#' Shiny ui-module for Function Input
#'
#' \code{featObject_sidebar} is a shiny ui-component which can be added to your shiny app
#' so that you can easily generate a feature object by putting in all relevant information
#'
#'
#'@param id ID for the shiny component
#'@export
featureObject_sidebar <- function(id) {
  if (!requireNamespace("smoof", quietly = TRUE)) {
    stop(" smoof needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)
  

  #sidebar for the configuration of different function parameters
  shiny::sidebarPanel(
    shiny::radioButtons(ns("function_option"), label = "Function input",
                   choices = list("User defined function" = 1, "smoof" = 2, "BBOB"=4, 
                                  "MPM2"=5, 
                                  "File-Import" = 3),
                   selected = 1),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("function_option"), "'] == 1"),
      shiny::textInput(ns("function_input"), label="Function")
      ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("function_option"), "'] == 2"),
      shiny::selectInput(ns("smoof_function_select"), label = "Function name", choices = smoof::filterFunctionsByTags("single-objective"))
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("function_option"), "'] == 3"),
      shiny::fileInput(ns("import_file"), label = "File to import")
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("function_option"), "'] == 4"),
      shiny::splitLayout(
        shiny::numericInput(ns("BBOB_fid"),label="BBOB-FID", value = 1),
        shiny::numericInput(ns("BBOB_iid"),label="BBOB-IID ", value = 1))
    ),
    # shiny::conditionalPanel(
    #   condition = paste0("input['", ns("function_option"), "'] == 5"),
    #   shiny::splitLayout(
    #     shiny::numericInput(ns("MPM2_npeaks"),label="Peaks", value = 1),
    #     shiny::numericInput(ns("MPM2_seed"),label="Seed", value = 1)),
    #   shiny::splitLayout(
    #     shiny::selectInput(ns("MPM2_topology"), label = "Topology", choices = c("random","funnel")),
    #     shiny::selectInput(ns("MPM2_shape"),label = "Shape", choices = c("sphere","ellipse"))),
    #   shiny::checkboxInput(ns("MPM2_rotated"), label = "Rotated")
    # ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("function_option"), "'] != 3"),
      shiny::splitLayout(
        shiny::numericInput(ns("dimension_size"), label="Dimensions", value = 1),
        shiny::selectInput(ns("sampletype"), label = "Sample type", choices = c("random","lhs"))),

      shiny::splitLayout( #put lower and upper bound in one line
        shiny::textInput(ns("samplelow"), label="Lower bound", value = "0"),
        shiny::textInput(ns("sampleup"), label="Upper bound", value = "1")),
        shiny::sliderInput(ns("ssize"),
                  "Sample size",
                  min = 40,
                  max = 2000,
                  value = 50)
    ),
    shiny::textInput(ns("block_input"), label="Blocks (comma sperated per dimension)", value = "2")
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
#' @param stringsAsFactors How to treat strings in application (for shiny internally)
#'
#' @export
#'
functionInput <- function(input, output, session, stringsAsFactors) {
  `%then%` <- shiny:::`%OR%`
    #function to control the output data if smoof is selected for function input
    smoof_input_createFeatures <- shiny::reactive({
      # smoofFunctionPage  is using the smoof package for implementing them
      if (!requireNamespace("smoof", quietly = TRUE)) {
        stop("smoof needed for this function to work. Please install it.",
             call. = FALSE)
      }
      # transform the text input for lower and upper bound to an vector with the respective values
      shiny::validate(
        shiny::need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        shiny::need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X <- flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control = ctrl)
      f <- smoof::makeFunctionsByName(input$smoof_function_select, dimensions = input$dimension_size)
      y <- apply(X, 1, f[[1]])
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        shiny::validate(
            shiny::need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
            shiny::need(max(input$dimension_size^blocks) <= 10000, "Block value in combination with the dimensions is too high!") %then%
            shiny::need(try(feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f[[1]], blocks=blocks)), "Please insert a valid function")
        )
      } else {
        feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f[[1]])
      }
      feat.object
    })

    #function to control the output data if smoof BBOB is selected for function input
    smoof_BBOB_input_createFeatures <- shiny::reactive({
      # smoofFunctionPage  is using the smoof package for implementing them
      if (!requireNamespace("smoof", quietly = TRUE)) {
        stop("smoof needed for this function to work. Please install it.",
             call. = FALSE)
      }
      # transform the text input for lower and upper bound to an vector with the respective values
      shiny::validate(
        shiny::need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        shiny::need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X <- flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control = ctrl)
      f <- smoof::makeBBOBFunction(dimension = input$dimension_size, fid = input$BBOB_fid, iid = input$BBOB_iid)
      y <- apply(X, 1, f)
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        shiny::validate(
          shiny::need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          shiny::need(max(input$dimension_size^blocks) <= 10000, "Block value in combination with the dimensions is too high!") %then%
          shiny::need(try(feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f, blocks=blocks)), "Please insert a valid function")
      )
      } else {
        feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f)
      }
      feat.object
    })

    #function to control the output data if smoof MPM2 is selected for function input
    smoof_MPM2_input_createFeatures <- shiny::reactive({
      # smoofFunctionPage  is using the smoof package for implementing them
      if (!requireNamespace("smoof", quietly = TRUE)) {
        stop("smoof needed for this function to work. Please install it.",
             call. = FALSE)
      }
      # transform the text input for lower and upper bound to an vector with the respective values
      shiny::validate(
        shiny::need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        shiny::need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X <- flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control = ctrl)
      f <- smoof::makeMPM2Function(dimensions = input$dimension_size, n.peaks = input$MPM2_npeaks, 
                                   seed = input$MPM2_seed, topology = input$MPM2_topology)
                                   #peak.shape = input$MPM2_shape, rotated = input$MPM2_rotated)
      y <- apply(X, 1, f)
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        shiny::validate(
          shiny::need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          shiny::need(max(input$dimension_size^blocks) <= 10000, "Block value in combination with the dimensions is too high!") %then%
          shiny::need(try(feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f, blocks=blocks)), "Please insert a valid function")
        )
      } else {
        feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f)
      }
      feat.object
    })
    
    
    #reactive element which is called when function input is selected and which controlls the output of features
    createFeatures_function <- shiny::reactive({
      # transform the text input for lower and upper bound to an vector with the respective values
      shiny::validate(
        shiny::need(try( lowerbound <- eval(parse(text = paste("c(",input$samplelow,")")))), "Please insert valid lowerbound defintion") %then%
        shiny::need(try( upperbound <- eval(parse(text = paste("c(",input$sampleup,")")))), "Please insert valid upperbound defintion")
      )
      ctrl=list(init_sample.type = input$sampletype,
                init_sample.lower = lowerbound,
                init_sample.upper = upperbound) #get ctrl values for creation of initial Sample
      X = flacco::createInitialSample(n.obs = input$ssize, dim = input$dimension_size, control=ctrl)
      params <- list()
      for (i in 1:input$dimension_size)
      {
        params[[i]] <- makeNumericParam(paste("x",i,sep=''), lower=lowerbound[1], upper = upperbound[1])
      }
      print(params)
      #check if there is a block input
      if (input$block_input==""){
        # validate if the function the user has put in can be evaluated by R
        shiny::validate(
          shiny::need(try( f <- eval(parse(text=paste("function(x) ",input$function_input)))), "Please insert a valid function") %then%
          shiny::need(try(fun <- smoof::makeSingleObjectiveFunction(name = input$function_input, fn = f, par.set = makeParamSet(params = params))), "Please insert a valid function") %then%
          shiny::need(try(feat.object <- flacco::createFeatureObject(X = X, fun = fun)), "Please insert a valid function")
        )
        
      } else {
        shiny::validate(
          shiny::need(try( f <- eval(parse(text=paste("function(x) ",input$function_input)))), "Please insert a valid function") %then%
          shiny::need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          shiny::need(max(input$dimension_size^blocks) <= 10000, "Block value in combination with the dimensions is too high!") %then%
          shiny::need(try(fun <- smoof::makeSingleObjectiveFunction(name = input$function_input, fn = f, par.set = makeParamSet(params = params))), "Please insert a valid function") %then%
          shiny::need(try(feat.object <- flacco::createFeatureObject(X = X, fun = fun, blocks=blocks)), "Please insert a valid function")
        )
      }
      feat.object
    })


    #function for controlling the file input app
    createFeatures_import <- shiny::reactive({
      importdata <- utils::read.csv(input$import_file$datapath) #load values from uploaded file
      #y <- apply(X, 1, eval(parse(text=paste("function(x) ",input$function_input))))
      if (input$block_input!=""){ #check if input for blocks is available
        #validate the input for block
        shiny::validate(
          shiny::need(try( blocks <- eval(parse(text=paste("c(",input$block_input,")")))), "Please insert valid block defintion") %then%
          shiny::need(max(ncol(importdata)^blocks) <= 10000, "Block value in combination with the dimensions is too high!") %then%
          shiny::need(try(feat.object <- flacco::createFeatureObject(X = data.frame(importdata[,-ncol(importdata)]), y = importdata[,ncol(importdata)], blocks=blocks)), "Please insert valid funciton values")
        )
      } else {
        feat.object <- flacco::createFeatureObject(X = data.frame(importdata[,-ncol(importdata)]), y = importdata[,2])
      }
      feat.object
    })

    featureObject <- shiny::reactive({
      if (input$function_option == 1)
      {
        createFeatures_function()
      } else if (input$function_option == 2) {
        smoof_input_createFeatures()
      } else if (input$function_option == 3) {
        createFeatures_import()
      } else if (input$function_option == 4) {
        smoof_BBOB_input_createFeatures()
      } else if (input$function_option == 5) {
        smoof_MPM2_input_createFeatures()
      }
    })

    return(featureObject)
}

