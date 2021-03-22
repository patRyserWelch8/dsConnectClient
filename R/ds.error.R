#' @name ds.error
#' @title Displays DataSHIELD R session errors  
#' @description Shows server-side and/or client-side error message. 
#' @param error The error thrown by R
#' @param client If TRUE the client-side code error is indicated. If FALSE server-side
#' @return \code{ds.error} returns client-side or server-side errors. 
#'@export
ds.error <- function(error, client = TRUE)
{
  print(error)
  if(client)
  {
    dser.show.client.error(error)
  }
  else
  {
    dser.show.server.error(error)
  }
}


dser.show.client.error <- function(error)
{
  
  function.name <- dser.get.function.name(error)
  if(!identical(function.name, "NF"))
  {
    dser.message.client.side.error(function.name, error)
  }
}

dser.show.server.error <- function(error)
{
  client.function.name <- error[[1]][1]
  server.function.name <- dser.get.function.name(error[[2]][1])
  dser.message.server.side.error(client.function.name, server.function.name, error[[3]])
}

dser.get.function.name <- function(error)
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

dser.message.show.error.message <- function(error.code = NULL)
{
  search.list        <- lapply(1:length(search()), function(x)return(ls(pos=x, pattern="error_message")))
  indeces.errors.env <- which(search.list == "error_message", arr.ind = TRUE)
  index.env          <- 1
  continue           <- index.env <= length(indeces.errors.env)
  errors             <- error.code
 
 
  while(continue)
  {
      err    <- get("error_message", pos = index.env)
      filter <- err[1] == "ERR:003"
      errors <- err[filter,] 
      
      if (length(errors) == 2)
      {
        errors   <- errors[2]
        continue <- FALSE
      }
      else
      {
        continue <- index.env <= length(indeces.errors.env)
      }
      
      index.env <- index.env + 1
      print(errors)
  }
  
  return(errors)
}

dser.message.server.side.error <- function(client.function.name, server.function.name, server.error)
{
  message(unlist(server.error))
  #lapply(server.error, function(x) message(x))
  if (FALSE)
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
  messages    <- lapply(X = errors, function(x) return(x[length(x)]))
  
  #messages   <- lapply(X = errors, function(x) find.error.message(x))
  
 
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
}

dser.message.client.side.error <- function(function.name, client.error)
{
  error.message <- paste0("The function ", function.name, " is not working has expected.", "\n")
  # find the error thrown 
  is.error      <- "error" %in% class(client.error)
  error         <- ""
  if(is.error)
  {
    
    error <- client.error$message
    error <- unlist(strsplit(error, "::"))
    error <- strsplit(error, ">")
  }
  else
  {
    error <- unlist(strsplit(client.error, "::"))
    error <- unlist(strsplit(client.error, "::"))
    # remove additional characters
    error <- strsplit(error, ">")
  }
  
  message(error)
  #message       <- dser.message.show.error.message(error)

  #message(error.message, message )
}