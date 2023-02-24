#' Plot a kws request
#' @param x either a character string (JSON) or a named list
#' @param recommendation the computed recommendation from `safeRun(computeRecommendationCore(x))`. Specify NULL to skip this step.
#' @param as.list instead of a htmlwidget, return a list with items timevis, css, script, extraWarnings, error, title
#' @return a timevis htmlwidget that displays all events
#' @export
plot_kwsRequest <- function(x, recommendation, title=NULL, as.list=FALSE) {
  if(is.character(x)) {
    if(length(x) > 1) x <- paste(x, collapse="\n")
    x <- rjson::fromJSON(x)
  }
  
  events <- as.eventlist(x)
  
  ## Add extra events: recommendation
  if(missing(recommendation)) recommendation <- safeRun({computeRecommendationCore(x)})
  if(!is.null(recommendation$value)) {
    recommendations <- purrr::map_dfr(
      recommendation$value$updatedPrescriptions,
      function(x) {
        event <- events[ match(x$voorschriftNr, events$id), ]
        event$event <- "recommendation"
        event$dosis <- list(x$dosis)
        event$nota <- x$nota
        event
      }
    )
    events <- bind_rows(events, recommendations)
  }
  
  # Add extra events: validatie
  validatie <- events %>% filter(event == "voorschrift") %>% transmute(
      event = "validatie",
      voorschriftNr=id,
      id=paste0(id,"V"),
      time=validatieDatum,
      datumTot=validatieDatum+0.001
    )
  events <- bind_rows(events, validatie)
  
  # Mark the trigger for the request
  events$trigger <- FALSE
  trigger <- x$request$trigger
  if(setequal(names(trigger), c("uitvoeringNr", "cnk", "omschrijving", "datumVan", "datumTot", "dosis", "voorschriftNr"))) {
    events$trigger[events$id == trigger$uitvoeringNr] <- TRUE
  } else if ("zorgNr" %in% names(trigger)) {
    events$trigger[events$id == trigger$zorgNr] <- TRUE
  } else if(all(c("voorschriftNr", "cnk", "omschrijving", "datumVan", "datumTot", "dosis", "validatieDatum") %in% names(trigger))) {
    events$trigger[events$id == trigger$voorschriftNr] <- TRUE
  } else if(setequal(names(trigger), c("datum", "idType", "id", "numeriek", "soort", "ucumUnit"))) {
    # prett rough, but labs do not have an ID :-(
    events$trigger[events$id == trigger$id & events$time == anytime::anytime(trigger$datum)] <- TRUE
  }
  
  ## functions
  ingreep <- max( events %>% filter(event=="ingreep") %>% pull(time) )
  style <- function(event, omschrijving, trigger) {
    res <- dplyr::case_when(
      omschrijving=="PROGRAFT" ~ "background-color: fuchsia;",
      omschrijving=="ADVAGRAF" ~ "background-color: khaki;",
      omschrijving=="Tacrolimus (FK 506) (bloed)" ~ "background-color: darkcyan;",
      omschrijving=="Hematocriet (bloed)" ~ "background-color: hotpink;",
      stringr::str_detect(omschrijving, "INSULINE") ~ "background-color: slategray; color: white;",
      TRUE ~ ""
    )
    if(trigger) {
      res <- paste0(res, "border-width: thick; border-style: double; border-color: red;")
    }
    res
  }
  content <- function(event, id, omschrijving, dosis, voorschriftNr, attributen, nota) {
    content <- if(event == "request") {
      id
      } else if (event %in% c("uitvoering", "voorschrift", "voorbereiding", "recommendation")) {
        paste0("", dosis$value, dosis$ucumUnit)
      } else if (event =="labo") {
        if(length(attributen) == 0) "" else paste0(attributen$numeriek, attributen$ucumUnit) 
      } else if (event == "zorg") { 
        paste(attributen, collapse="\n") 
      } else {
        omschrijving
      }
    if(!is.na(nota)) content <- paste0("<u>", content, "</u>") #there is a nota, make users see it
    #if(!is.na(nota)) content <- paste0("<p class='nota'><span class='nota'>", nota, "</span>", content, "</p>")
    if(!is.na(id)) content <- paste0("<div id='custom-",id,"'>", content, "</div>")
    if(event %in% c("uitvoering", "validatie") && !is.na(voorschriftNr)) {
      content <- paste0(content,
        "<connection id='connections-", id, "' from='#custom-", id, "' to='#custom-", voorschriftNr, "' color='red' width='1px' tail/>"
      )
    }
    content
  }
  events$title <- events$omschrijving
  events$title[!is.na(events$nota)] <- paste0(events$omschrijving, events$nota)[!is.na(events$nota)]
  data <- events %>%
    filter(event != "patient") %>%
    transmute(
      id=ifelse(event=="labo", NA, id), #ignore ID from labo; not unique...
      group=event,
      ## TEP-705 
      ## >> De thuistherapie heeft een datumTot, maar geen datumVan.
      ## Dit klopt: thuistherapie is een van de datasources in KWS die heel “sparse” ingevuld worden. Als er geen datumVan beschikbaar is, dan betekent dit dat de persoon die de registratie gedaan heeft, niet weet vanaf wanneer de patient de medicatie neemt. Geen datumVan is synoniem met “al lang”.
      ## Een datumTot is trouwens ook niet verplicht. Geen datumTot betekent: de patient neemt dit nu, en zal dit waarschijnlijk nog een hele tijd blijven nemen… De einddatum ligt niet vast.
      start=time,
      end=datumTot,
      title=title,
      style=purrr::pmap_chr(list(event, omschrijving, trigger), style),
      content=purrr::pmap_chr(list(event, id, omschrijving, dosis, voorschriftNr, attributen, nota), content),
    )
  data$id[ data$group == "recommendation"] <- NA
  thuistherapie <- data$group == "thuistherapie"
  data$start[thuistherapie & is.na(data$start)] <- ingreep
  
  i <- is.na(data$end) | data$start == data$end
  data$end[i] <- data$start[i] + lubridate::hours(10)
  
  extraWarnings <- ""
  
  duplicateId <- na.omit(  data$id[ duplicated(data$id) ]  )
  i <- data$id %in% duplicateId
  if(any(i)) {
    extraWarnings <- paste0(extraWarnings, "<p><emph>Duplicate IDs detected: ", paste(data$group[i], "#", data$id[i], collapse=", ", sep=""), "</emph></p>")
  }
  data <- data[is.na(data$id) | !duplicated(data$id), ] #remove the duplicates
  # remove duplicate labo values
  i <- duplicated(paste(data$start, data$content, sep=";"))
  if(any(i)) {
    extraWarnings <- paste0(extraWarnings, "<p><emph>Duplicate lab values detected:", paste(data$start[i], data$content[i], sep=";", collapse=" "), "</emph></p>")
  }
  data <- data[ data$group != "labo" | 
                  (!i)
                ,]
  
  #data <- dplyr::filter(data, start >= ingreep) #include all data
  
  groups <- tibble::tibble(
    id=c("thuistherapie", "request", "ingreep", "zorg", "labo", "voorbereiding", "validatie", "voorschrift", "uitvoering", "recommendation"),
    content=c(
      "<b>thuistherapie</b><br/>what is given at home",
      "<b>request</b><br/>time of request",
      "<b>ingreep</b><br/>time of transplant",
      "<b>zorg</b><br/>measured patient",
      "<b>labo</b><br/>measured lab values",
      "<b>voorbereiding</b><br/>planned, not validated",
      "<b>validatie</b><br/>validations",
      "<b>voorschrift</b><br/>planned, validated",
      "<b>uitvoering</b><br/>actually administered",
      "<b>recommendation</b><br/>by software"
    )
  )
  config <- list(
    editable = FALSE,
    align = "left",
    orientation = "both",
    snap = NULL,
    showCurrentTime=FALSE#,
    #min = ingreep
  )
  
  z <- timevis::timevis(data,
                   options=config,
                   #height=600,
                   width=1200,
                   groups=groups,
                   showZoom = FALSE)
  z <- timevis::setWindow(z,
                          start=ingreep,
                          end=max(c(data$start, data$end)),
                          options=list(animation=FALSE))
  z <- timevis::addCustomTime(z, data$start[ data$group=="request"] + 3*60*60, "requestTime")
  
  patientDescription <- events %>% filter(event=="patient") %>% select(-event) %>% unlist() %>%
    na.omit() %>% paste(names(.), "=", ., collapse=", " )
  
  
  #a list with items timevis, css, script, extraWarnings, error, warnings, title
  result <- list()
  result$timevis <- z
  result$extraWarnings <- htmltools::HTML(extraWarnings)
  result$patient <- htmltools::HTML(patientDescription)
  if(!is.null(title)) result$title <- htmltools::HTML(paste0("<h1>", title, "<h1>"))
  result$css <- htmltools::includeCSS(system.file(package = "tacrolimuskws", "timevis/domarrow.js-master/domarrow.css"))
  result$css2 <- htmltools::includeCSS(system.file(package = "tacrolimuskws", "timevis/nota-hover/nota.css"))
  result$script <- htmltools::includeScript(system.file(package = "tacrolimuskws", "timevis/domarrow.js-master/domarrow.js"))
  if(!is.null(recommendation$error)) {
    result$error <- htmltools::tagList(
      htmltools::tags$h4(paste("Error", recommendation$error$codeName)),
      htmltools::tags$pre(recommendation$error$message)
    )
  }
  if(length(recommendation$warnings) > 0) {
    warningHtml <- lapply(recommendation$warnings, function(w) {
      list(htmltools::h5(w$codeName), htmltools::pre(w$message))
    })
    result$warnings <- htmltools::tagList(
      htmltools::h4("Warnings"),
      warningHtml
    )
  }
  
  if(as.list) return(result)
  
  z <- result$timevis
  for(i in setdiff(names(result), "timevis")) z <- htmlwidgets::prependContent(z, result[[i]])
  return(z)
}


## The code below can be used to generate HTML and PNG files of a complete database dump
if(FALSE) {
library(parallel)
library(filelock)

files <- list.files(path="D:/OtterDrive/KwsDump20191220/", pattern="^.*.json$", full.names=TRUE)
toHTML <- function(i, overwrite=TRUE) {
  patient <- rjson::fromJSON(file=i)
  TxDate <- tacrolimuskws::as.eventlist(patient) %>% filter(event=="ingreep") %>% pull(time) %>% .[1]
  
  file <- file.path(dirname(i), TxDate, paste0(basename(i),".html") )
  dir.create(dirname(file), showWarnings=F)
  if(!overwrite && file.exists(file)) return()
  
  cat("Reading ", i,"... ")
  json <- readLines(i)
  z1 <- tacrolimuskws::plot_kwsRequest(json)
  lock <- filelock::lock("lock")
  htmlwidgets::saveWidget(z1, file, selfcontained = FALSE, libdir=file.path(dirname(file), "lib"), title = basename(i))
  filelock::unlock(lock)
  cat("-> ", file, "\n" )
}

lapply(files, toHTML, overwrite=TRUE)
cl <- makeCluster(detectCores() - 1)
parallel::clusterApply(cl, files, toHTML, overwrite=TRUE)

library(webdriver)


toPNG <- function(i, overwrite=FALSE) {
  cat("Parsing ", i, "...")
  pngFile <- paste0(i, ".png")
  if(!overwrite && file.exists(pngFile)) {
    if(file.info(pngFile)$mtime > file.info(i)$mtime) { #only skip if pngFile is newer
      cat("SKIP\n")
      return() #skip
    } else {
      cat("Existing file is too old...")
    }
  }
  ses <- tryCatch({get("ses", envir=globalenv())},
                  error=function(e){
                    library(webdriver)
                    lock <- filelock::lock("lock")
                    pjs <- run_phantomjs()
                    filelock::unlock(lock)
                    Session$new(port = pjs$port)
                  })
  assign("ses", ses, envir=globalenv())
  ses$go(paste0("file:///", i))
  ses$getTitle()
  ses$getUrl()
  ses$takeScreenshot(file=pngFile)
  cat("DONE\n")
}

htmlFiles <- list.files(path="D:/OtterDrive/KwsDump20191125/", pattern="^.*.html$", full.names=TRUE, recursive = TRUE)
parallel::clusterApply(cl, htmlFiles, toPNG, overwrite=TRUE)
lapply(htmlFiles, toPNG, overwrite=FALSE)


}
