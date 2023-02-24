if(FALSE) {
  options(shiny.reactlog=TRUE)
  options(useFancyQuotes=FALSE)
  x <- rjson::fromJSON(file="D:/OtterDrive/testFile.json")
  rec <- tacrolimuskws::computeRecommendationCore(x)
  tacrolimuskws:::shinyApp(x, rec, mc.maxpts=5)
}

#' @importFrom dplyr rename mutate transmute
#' 
#' @export
toShinyTdmore <- function(recommendation) {
  args <- recommendation$tdmoreArgs
  
  result <- list()
  result$model <- recommendation$model
  regimen <- args$regimen
  result$regimen <- args$regimen %>%
    transmute(
      time= args$t0 + lubridate::dhours(TIME),
      dose=AMT,
      formulation=factor(FORM, levels=shinytdmore:::getFormulationList(recommendation$model)),
      fix=FIX
    )
  result$observed <- args$observed %>%
    transmute(
      time=args$t0 + lubridate::dhours(TIME),
      dv = Cwb,
      use=TRUE
    )
  result$covariates <- args$covariates %>%
    rename(time=TIME) %>% mutate(time = args$t0 + lubridate::dhours(time))
  result$now <- args$t0 + lubridate::dhours(args$now)
  result
}

#' Creating a shiny app displaying the given patient
#' 
#' @param request a JSON string or an already parsed JSON list
#' @param recommendation a recommendation as computed by [computeRecommendationCore()]
#' @import shiny
#' @export
shinyDebugApp <- function(request, recommendation, mc.maxpts=100) {
  if(is.character(request)) request <- rjson::fromJSON(paste(request, collapse="\n"))
  
  modelName <- request$model %||% "D0_14_Troughs_1cpt_BASE_TSFD_MPC"
  
  if(missing(recommendation)) {
    message("Calculating recommendation...", appendLF=F)
    recommendation <- computeRecommendationCore(patient=request, modelName=modelName)
    message("DONE")
  }
  
  shinytdmoreArgs <- toShinyTdmore(recommendation)
  
  ui <- fluidPage(
    tabsetPanel(
      tabPanel(
        title="KWS Request",
        timevis::timevisOutput("timevis")
        ),
      shinytdmore::predictionTabUI("prediction")
    )
  )
  server <- function(input, output, session) {
    state <- reactiveValues()
    state$fit <- recommendation$fit
    state$recommendation <- recommendation$recommendation$regimen
    for(i in names(shinytdmoreArgs)) state[[i]] <- shinytdmoreArgs[[i]]
    #debug( .subset2(state, "impl")$set )   #special way to set breakpoint for reactiveValues() set
    
    cr <- shinytdmore::calculationReactives(state, mc.maxpts=mc.maxpts)
    callModule(shinytdmore::predictionTab, "prediction", state=state, cr=cr)
    output$timevis <- timevis::renderTimevis({
      plot_kwsRequest(request, NULL)
    })
  }
  shiny::shinyApp(ui=ui, server=server)
}

#' @export
shinyApp <- function(db, modelDir=system.file("models", package="tacrolimuskws")) {
  ui <- fluidPage(
    tags$style(".swal-modal {width: 80%;}"),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
              height: 100px;
              width: 40%;
              position:fixed;
              top: calc(50% - 50px);;
              right: calc(5%);
            }
           ")
        )
      ),
    shinyWidgets::useSweetAlert(),
    navbarPage(
      "Tacrolimus TDM app",
      tabPanel(title="Request",
               htmlOutput("timevisHtml"),
               timevis::timevisOutput("timevis")
      ),
      shinytdmore::predictionTabUI("prediction", height="80vh"), #80% of viewport height
      #shinytdmore::modelTabUI("model", dir=modelDir),
      shinytdmore::aboutTabUI("about", htmlFile=system.file("about.html", package="tacrolimuskws"))
    ))
  
  server <- function(input, output, session) {
    state <- reactiveValues()
    #debug( .subset2(state, 'impl')$set )
    requestVal <- reactiveVal()
    recommendationVal <- reactiveVal()
    
    ## Function to display error message
    errorMessage <- function(title="An error occurred...", message, endSession=TRUE) {
      shinyWidgets::closeSweetAlert(session)
      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId=if(endSession) "endSession" else "dummyInput",
        title = title,
        btn_labels="OK",
        text = tags$pre(message),
        type="error", closeOnClickOutside=FALSE, showCloseButton=FALSE,
        html=TRUE
      )
    }
    observeEvent(input$endSession, session$close(), ignoreInit=TRUE)
    
    cr <- shinytdmore::calculationReactives(state, mc.maxpts=0)
    readRequestFromQuery <- function(query) {
      demoPatient <- query[["demo"]]
      if(!is.null(demoPatient)) {
        requestJson <- readLines(system.file(basename(demoPatient), package="tacrolimuskws", mustWork=TRUE))
        request <- list(request=requestJson)
        return(request)
      }
      value <- query[["id"]]
      nonce <- query[["nonce"]]
      if(is.null(value) || is.null(nonce)) {
        errorMessage(message="`id` or `nonce` not specified in URL")
        return(NULL)
      }
      request <- tryCatch(getRequest(id=value, db, oid=nonce), error=function(e) e)
      if(inherits(request, "error") || is.null(request)) {
        errorMessage(message="Could not find KWS request...")
        return(NULL)
      }
      return(request)
    }
    initialize <- function() {
      shinyWidgets::progressSweetAlert(session, "init",
        title = "Loading patient...",
        display_pct = TRUE, value = 0
      )
      query <- parseQueryString(session$clientData$url_search)
      request <- readRequestFromQuery(query)
      if(is.null(request)) return(FALSE)
      requestVal(request$request) #set request for plot_kwsRequest
      
      output <- tryCatch( {
        shinyWidgets::updateProgressBar(session, "init", 25)
        # load JSON
        requestJson <- paste(request$request, collapse="\n")
        patient <- rjson::fromJSON(requestJson)
        # load model
        modelName <- patient$model
        if(is.null(modelName)) modelName <- "D0_14_Troughs_1cpt_BASE_TSFD_MPC"
        # compute recommendation
        recommendation <- safeRun({ 
          computeRecommendationCore(patient, modelName=modelName, par=unlist(request$coef), control=list(maxit=0))
        })
        recommendationVal(recommendation) #set recommendation for plot_kwsRequest
        shinyWidgets::updateProgressBar(session, "init", 50)
        
        if(is.null(recommendation$value)) {
          ## at least try to set up
          recommendation$value <- tryCatch({
            computeRecommendationCore(
              patient, 
              modelName=modelName, 
              par=unlist(request$coef), 
              control=list(maxit=0),
              robust=TRUE
              )}, error = function(e) {
                # unrecoverable! Just pass the original error back
                stop(recommendation$error$message)
              }
            )
          recommendation$warnings <- c(recommendation$warnings, list(list(message="Data reloaded without error checking!")))
          recommendationVal(recommendation)
        }
        
        # set up state and fit
        result <- tacrolimuskws::toShinyTdmore(recommendation$value)
        for(i in names(result)) state[[i]] <- result[[i]]
        cr$updateFit( recommendation$value$fit )
        
        if(!is.null(recommendation$error)) stop(recommendation$error$message) #pass up the error if required
        
        shinyWidgets::updateProgressBar(session, "init", 75)
        NULL
      }, error=function(e) e)
      if(inherits(output, "error")) {
        errorMessage(title="Error calculating recommendation...", message=output$message, endSession=FALSE)
        return(FALSE) #close the alert
      }
      return(TRUE)
    }
    observeEvent(session$clientData$url_search, {
      x <- initialize()
      
      if(isTRUE(x)) shinyWidgets::closeSweetAlert(session)
    })
    
    callModule(shinytdmore::predictionTab, "prediction", state, cr=cr)
    callModule(shinytdmore::modelTab, "model", state)
    callModule(shinytdmore::aboutTab, "about")
    
    debugRequest <- reactive({
      shiny::req(requestVal())
      plot_kwsRequest(requestVal(), recommendationVal(), as.list=TRUE)
    })
    
    output$timevis <- timevis::renderTimevis({
      debugRequest()$timevis
    })
    output$timevisHtml <- renderUI({
      result <- debugRequest()
      do.call(htmltools::tagList, args=result[ setdiff(names(result), "timevis") ] )
    })
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  app
}
