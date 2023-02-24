mail_address <- function(email, name) {
  list(Email=email, Name=name)
}
email_message <- function(from, to, subject, textPart, htmlPart, customId) {
  if(!is.null( names(to) )) to <- list(to)
  msg <- list(
    From=from, To=to, Subject=subject, TextPart=textPart,
    HTMLPart=htmlPart,
    CustomID=customId
  )
}
send_email <- function(message) {
  body <- list(
    Messages=list(message)
  )
  jsonBody <- rjson::toJSON(body)
  x <- httr::POST(url="https://api.mailjet.com/v3.1/send", body = jsonBody,
                  httr::authenticate(
                    user=Sys.getenv("KWS_MAILJET_USER"),
                    password=Sys.getenv("KWS_MAILJET_PASSWORD")
                  )
  )
  status <- httr::status_code(x)
  if(status == 200) {
    cat("Alert email sent\n")
  } else {
    cat("Alert mail failed! Email message: \n", str(message), "\n Response: \n", str(httr::content(x, as="text")),"\n")
  }
}

