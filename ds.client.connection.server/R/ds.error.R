#' @name ds.error
#' @title Displays DataSHIELD R session errors  
#' @description Shows server-side and/or client-side error message. 
#' @param error The error thrown by R
#' @param client If TRUE the client-side code error is indicated. If FALSE server-side
#' @return \code{ds.error} returns client-side or server-side errors. 
#'@export
ds.error <- function(error, client = TRUE)
{
  if(client)
  {
    .show.client.error(error)
  }
  else
  {
    .show.server.error(error)
  }
}


.show.client.error <- function(error)
{
  function.name <- .get.function.name(error)
  if(!identical(function.name, "NF"))
  {
    .message.client.side.error(function.name, error)
  }
}

.show.server.error <- function(error)
{
  client.function.name <- error[[1]][1]
  server.function.name <- .get.function.name(error[[2]][1])
  .message.server.side.error(client.function.name, server.function.name, error[[3]])
}

.get.function.name <- function(error)
{
  outcome    <- "NF"
  error.char <- as.character(error)
 
  if (grepl(pattern = "::", x = error.char))
  { 
    error.list <- strsplit(error.char, "::")
    outcome    <- error.list[[1]][2]
  }
  else if (grepl(pattern = "\\(", x = error.char))
  {
    error.list <- strsplit(error.char, "\\(")
    outcome    <- error.list[[1]][1]
  }
  else
  {
    outcome    <- error
  }
  
  return(outcome)
}



.message.server.side.error <- function(client.function.name, server.function.name, server.error)
{
  
  #finding the error
  error       <- ""
  #split error message provided by DSI and the error thrown by the server
  error.split <- lapply(X = server.error, function(x) strsplit(x = x, "->"))
  
  #transform each server entry with two vectors. It is easier to manipulate.
  error.split <- lapply(X = error.split, function(x) unlist(x))
  
  #keep only the errors from the server.
  errors      <- lapply(X = error.split, function(x) return(x[2]))
  
  #get the error thrown by the server
  errors      <- lapply(X = errors, function(x) unlist(strsplit(x,"() : ")))
  
  #obtain error code or message sent by server. The last element.
  errors     <- lapply(X = errors, function(x) return(x[length(x)]))
  
  messages   <- lapply(X = errors, function(x) find.error.message(x))
  
 
  #matches the names of each server with an error message.
  messages   <- lapply(seq_along(messages), function(y, n, i){paste("server",n[[i]], " : " , y[[i]])}, 
                                                         y = messages, n = names(errors))
  # displaying the errors
  client.function <- paste0("The client-side function named ", client.function.name, 
                          " is not working has expected.\n")
  server.function <- paste0("An error has occurred on the server. The function ", server.function.name, 
                            " has not been able to either assign or return an aggregation. \n")
  
  message(client.function, server.function)
  lapply(messages, function(x) message(x))
}

.message.client.side.error <- function(function.name, client.error)
{
  error.message <- paste0("The function ", function.name, " is not working has expected.", "\n")
  # find the error thrown 
  is.error <- "error" %in% class(client.error)
  if(is.error)
  {
    error <- client.error$message
  }
  else
  {
     error <- unlist(strsplit(client.error, "::"))
     # remove additional characters
     error <- strsplit(error, ">")
  }
  message       <- find.error.message(error)
  message(error.message, message )
}