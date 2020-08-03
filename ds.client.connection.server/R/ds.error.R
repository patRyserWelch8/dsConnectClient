ds.error <- function(error)
{
  client.error <- error
  function.name <- .get.function.name(client.error)
  print(error)
  print(paste0("The function ", function.name, " is not working has expected.", "\n"))
  
  if (grepl("ERR:001",client.error))
  {
    print("Some elements are missing. Check you have passed as arguments the expected information ")
  }
  else if (grepl("ERR:003",client.error))
  {
    print("You have yet to specify some data computers name.")
  }
  else if (grepl("ERR:004",client.error))
  {
    print("You have yet to specify the URLs of each data computer.")
  }
  else if (grepl("ERR:005",client.error))
  {
    print("You have yet to specify the data tables.")
  }
  else if(grepl("ERR:006",client.error))
  {
    print("Have you connected to the Opal server?")
    print("Have you successfully started your Opal Servers?")
    print("You need to pass a valid connection. Please use ds.login")
  }
  else if (grepl("ERR:007",client.error))
  {
    print("You have yet to provide a valid function. It should either be a character or a call type.") 
  }
  else if (grepl("ERR:008",client.error))
  {
    print("You have yet to provide an appropriate variable.name. It should a character of length greater than 1") 
  }
  else if (grepl("ERR:009",client.error))
  {
    print("You have yet to provide a valid value. It can be a valid 'assign server-side function' or a value.") 
  }
  else if (grepl("ERR:012",client.error))
  {
    print("You have yet to provide a valid class type. It should a valid R type.") 
  }
  else if (grepl("ERR:013",client.error))
  {
    print("You have yet to provide some users id; one for each server.") 
  }
  else if (grepl("ERR:014",client.error))
  {
    print("You have yet to provide some user passwords; one for each server.") 
  }
  else if (grepl("ERR:015",client.error))
  {
    print("You have yet to provide some ssl options; one for each server.") 
  }
  else if (grepl("ERR:016",client.error))
  {
    print("You have yet to provide some drivers for connecting to the servers; one for each server.") 
  }
  else
  {
    server.errors <- DSI::datashield.errors()
    print(server.errors)
  }
}

.get.function.name <- function(error)
{
  outcome    <- ""
  error.char <- as.character(error)
  error.list <- strsplit(error.char, "::")
 
  if (length(error.list[[1]]) >= 2)
  {
     outcome    <- error.list[[1]][2]
  }
  
  return(outcome)
}