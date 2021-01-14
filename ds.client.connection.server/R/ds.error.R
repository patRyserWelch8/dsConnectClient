#' @name ds.error
#' @title Displays DataSHIELD R session errors  
#' @description Shows server-side and/or client-side error message. 
#' @param error The error thrown by R
#' @param client If TRUE the client-side code error is indicated. If FALSE server-side
#' @return \code{ds.error} returns client-side or server-side errors. 
#'@export
ds.error <- function(error, client = TRUE)
{
  print("========")
  print(error)

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

#need more work
.get.error.messages <- function(file.name = NULL)
{
  print(2)
  errors <- data.frame(error = c(), message = c())
  path <- paste0(system.file(package="ds.connect.client"),"/", file.name)
  print(path)
  if(file.exists(path))
  {
    print(3)
    errors <- read_csv(path)
    print(errors)
  }
  print(4)
  return(errors)
  
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
  error.message <- paste0("The function ", client.function.name, 
                          " is not working has expected. An error has occurred on the server.", 
                          "The function ", server.function.name, " has not been able to either assign or return an aggregation. ")
  
  #error.message <- paste0(error.message, messages)
  
  #if (length(unique(errors)) >= 1)
  #{
  #  error <- as.character(errors[1])
  #}
  #print(error)
  

  message(error.message)
  lapply(messages, function(x) message(x))
}

.message.client.side.error <- function(function.name, client.error)
{
  error.message <- paste0("The function ", function.name, " is not working has expected.", "\n")
  
  if (grepl("ERR:001",client.error))
  {
    error.message <- paste0(error.message, "Some elements are missing. Check you have passed as arguments the expected information ")
  }
  else if (grepl("ERR:002",client.error))
  {
    error.message <- paste0(error.message,"All URLs should starts with `https'. It is more secure.")
  }
  else if (grepl("ERR:003",client.error))
  {
    error.message <- paste0(error.message,"You have yet to specify some data computers name.")
  }
  else if (grepl("ERR:004",client.error))
  {
    error.message <- paste0(error.message,"You have yet to specify the URLs of each data computer.")
  }
  else if (grepl("ERR:005",client.error))
  {
    error.message <- paste0(error.message,"You have yet to specify the data tables.")
  }
  else if(grepl("ERR:006",client.error))
  {
    error.message <- paste0(error.message,"Have you connected to the Opal server? \n")
    error.message <- paste0(error.message, "Have you successfully started your Opal Servers? \n")
    error.message <- paste0(error.message,"You need to pass a valid connection. Please use ds.login \n")
  }
  else if (grepl("ERR:007",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide a valid function. It should either be a character or a call type.") 
  }
  else if (grepl("ERR:008",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide an appropriate variable.name. It should a character of length greater than 1") 
  }
  else if (grepl("ERR:009",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide a valid value. It can be a valid 'assign server-side function' or a value.") 
  }
  else if (grepl("ERR:010",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide some login details. Have you build your login data frame?") 
  }
  else if (grepl("ERR:011",client.error))
  {
    error.message <- paste0(error.message,"The login data frame is too short. No server is specified. It needs to be greater of equal 1. Have you build your login data frame? ") 
  }
  else if (grepl("ERR:012",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide a valid class type. It should a valid R type.") 
  }
  else if (grepl("ERR:013",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide some users id; one for each server.") 
  }
  else if (grepl("ERR:014",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide some user passwords; one for each server.") 
  }
  else if (grepl("ERR:015",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide some ssl options; one for each server.") 
  }
  else if (grepl("ERR:016",client.error))
  {
    error.message <- paste0(error.message,"You have yet to provide some drivers for connecting to the servers; one for each server.") 
  }
  else if (grepl("ERR:017",client.error))
  {
    error.message <- paste0(error.message,"The connection data frame returned was set to null. Something must have gone wrong with the connection to the server(s). Check they are running or restart them.") 
  }
  else if (grepl("ERR:018",client.error))
  {
    error.message <- paste0(error.message,"The parameters could not be shared. Some settings could not be created on each server involved in the exchanged.") 
  }
  else if (grepl("ERR:019",client.error))
  {
    error.message <- paste0(error.message,"The parameters could not be shared. Some errors occurred during the exchange.") 
  }
  else if (grepl("ERR:020",client.error))
  {
    error.message <- paste0(error.message,"More than one DataSHIELD server is required for sharing parameters. ") 
  }
  
  message(error.message)
}