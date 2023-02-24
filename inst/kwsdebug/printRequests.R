library(tacrolimuskws)
library(tidyverse)
length(demoDatabase3Json)

showRequest <- function(i) {
    cat("\014") # clear console
    cat("=== Request", i, "\n")
    json <- demoDatabase3Json[[i]]
    json %>% jsonlite::prettify(.) %>% cat
    json %>% rjson::fromJSON(.) %>% getPatientPrescriptions %>% arrange(DATE_FROM) %>% print
    json %>% rjson::fromJSON(.) %>% getPatientLabResults %>% print
    readline()
}
lapply(seq_along(demoDatabase3Json), showRequest)

showRequest(38)
colnames(bcfiAtcdpp)
bcfiAtcdpp %>% filter(mppcv %in% c(1402312, 2065415)) %>% View
