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
  server.function.name  <- .get.function.name(error[[2]][1])
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
  error.split <- lapply(X = server.error, function(x) strsplit(x = x, "->"))
  error.split <- lapply(X = error.split, function(x) unlist(x))
  errors      <- lapply(X = error.split, function(x) return(x[2]))
  
  
  # displaying the errors
  error.message <- paste0("The function ", client.function.name, 
                          " is not working has expected. An error has occurred on the server.", 
                          "The function ", server.function.name, " has not been able to either assign or return an aggregation. ")
  
  if (length(unique(errors)) >= 1)
  {
    error <- as.character(errors[1])
  }
  
  

  if(any(grepl("SERVER::ERR::PARAM::003",error)))
  {
    error.message <- paste0(error.message, "Some data were not encoded at all.Have you encoded any data before calling this function?") 
  } 
  else if(any(grepl("SERVER::ERR::PARAM::004",error)))
  {
    error.message <- paste0(error.message, "Some data could be obtained securely. Start again the whole exchange.") 
  } 
  else if(any(grepl("SERVER::ERR::PARAM::009",error)))
  {
    error.message <- paste0(error.message, "Some parameters may not have been encoded on one server. Start again the whole exchange.") 
  }
  else if(any(grepl("SERVER::ERR::PARAM::007",error)))
  {
    error.message <- paste0(error.message, "Some data may not have been received on a server. Start again the whole exchange.") 
  }
  else if(any(grepl("SERVER::ERR::PARAM::008",error)))
  {
    error.message <- paste0(error.message, "The parameters may have not been created on the server yet. Please use ds.ls() function to check.") 
  }
  else if(any(grepl("SERVER::ERR::PARAM::006",error)))
  {
    error.message <- paste0(error.message, "The arguments passed to the servers are incorrect. Their length may indicate they have no character. Numerical values may be set to 0 or be negative. Check the server-side function call.") 
  }
  else if(any(grepl("SERVER::ERR::PARAM::005",error)))
  {
    error.message <- paste0(error.message, "The arguments passed to the servers are incorrect. The data type is either not numerical or character. Check the server-side function call.") 
  }
  else if (any(grepl("SERVER::ERR::PARAM::001",error)))
  {
    error.message <- paste0(error.message, "A server is not allowed to taking part in sharing parameters.") 
  }
  else if (any(grepl("SERVER::ERR::PARAM::002",error)))
  {
    error.message <- paste0(error.message, "A function on the server has not received the appropriate arguments.") 
  }
  else if(any(grepl("SERVER-ERR-000",error)))
  {
    error.message <- paste0(error.message, "Some error thrown by stop function on the server in an aggregate function.")
  }
  else if(is.na(error))
  {
    error.message <- paste0(error.message, "A function or R object may not exists on the server(s).",
                            "ds.ls() and the tools made available on the Opal server can help you resolving this issue.")
  }
  else if (length(error) > 1)
  {
    error.message <- paste0(error.message, "R has thrown the following error: \n", error)
  }
 
  message(error.message)
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
    error.message <- paste0(error.message,"The parameters could not be shared. Some errors occurred during the exchanged.") 
  }
  else if (grepl("ERR:020",client.error))
  {
    error.message <- paste0(error.message,"More than one DataSHIELD is required for sharing parameters. ") 
  }
  
  message(error.message)
}