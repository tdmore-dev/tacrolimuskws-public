##
## Script name: 
##
## Purpose of script:
## Launch the Tacrolimus-KWS server
##
## Author: Ruben Faelens
##
## Date Created: Fri Mar 08 11:42:38 2019
##
## Copyright (c) Ruben Faelens, 2019
## Email: ruben.faelens@gmail.com
##
## ---------------------------
##
## Notes:
## This uses R to launch the services required for Tacrolimus KWS.
## Please note that this instance needs to connect to an existing mongodb
## We configure this using environment variables
## ---------------------------

library(callr)
library(tacrolimuskws) #try to load tacrolimuskws package
Sys.setenv(TZ="Europe/Brussels")
options(warn=1) #print warnings as they occur

#myDir <- system.file(package="tacrolimuskws", mustWork=TRUE)

myEnv <- callr::rcmd_safe_env()
myConfig <- c(
    'KWS_CONFIG_MONGODB_ADMINUSERNAME', 
    'KWS_CONFIG_MONGODB_ADMINPASSWORD',
    'KWS_CONFIG_MONGODB_SERVER',
    'KWS_CONFIG_SHINY_URL',
    'KWS_CONFIG_API_KEY',
    'KWS_CONFIG_HTTP_AUTH'
    )
for(i in myConfig) myEnv[i] <- Sys.getenv(i, unset=NA)

message("Starting plumber...")
plumber <- callr::r_bg(
    function() {
        tacrolimuskws::runPlumber()
    },
    supervise=TRUE,
    env = myEnv,
    stderr="2>&1",
    stdout="|"
)

message("Starting shiny...")
shiny <- callr::r_bg(
    function() {
        fpath <- system.file("shiny", "app.R", package="tacrolimuskws", mustWork=TRUE)
        shiny::runApp(fpath, port=8081, host = "0.0.0.0")
    },
    supervise=TRUE,
    env = myEnv,
    stderr="2>&1",
    stdout="|"
)

phantom <- callr::r_bg(
    function() {
        TIMEOUT <- 3*60
        message("Launching shiny smoke test...")
        message("starting phantomjs")
        pjs <- webdriver::run_phantomjs() ## beware, the processx supervisor ALSO manages phantomjs
        web <- webdriver::Session$new(port=pjs$port)
        message("Timeout: ", TIMEOUT)
        for(i in seq(1, 5)) {
            testResult <- try(tacrolimuskws::testApplication(web, "http://127.0.0.1:8081/?demo=demoRequest1.json", TIMEOUT))
            if(isTRUE(testResult)) {
                break
            } else {
                message("WARNING WARNING WARNING: Smoke test failed!")
                cat(web$getSource())
                message("Retrying... (try #", i, ")")
                #shiny$kill()
                #plumber$kill()
            }
        }
        pjs$process$kill() ## definitely kill phantomjs to ensure the below code works!
    },
    supervise=TRUE,
    stderr="2>&1",
    stdout="|"
)

printStdout <- function(processes) {
    pollResult <- processx::poll(processes, 120*1000)
    isReady <- purrr::map_lgl(pollResult, function(x){x['output']=="ready"})
    for(i in which(isReady)) {
        x <- processes[[i]]$read_output()
        if( sum(nchar(x, type="width")) >0) {
            x <- unlist( strsplit(x, "\n") )
            cat( paste(names(processes)[i],":\t", x, collapse="\n", sep=""))
            cat("\n")
        }
    }
}


## !! stdout needs to be polled constantly
tryCatch({
    while(plumber$is_alive() && shiny$is_alive()) {
        printStdout(list(PhantomJS=phantom, Plumber=plumber, Shiny=shiny))
        Sys.sleep(1) #only output every second
    }
    message("Plumber:")
    message( plumber$get_result() )
    message("is_alive?")
    message( plumber$is_alive() )
    message("Shiny:")
    message( shiny$get_result() )
    message("is_alive?")
    message( shiny$is_alive() )
}, finally={
    message("Ensuring all processes killed...")
    plumber$kill_tree()
    shiny$kill_tree()
    message("All processes exited; exiting supervisor. Goodbye.")
})
