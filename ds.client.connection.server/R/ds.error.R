ds.error <- function(error)
{
  client.error <- error
  function.name <- .get.function.name(client.error)
   print(error)
  print(paste0("The function ", function.name, " is not working has expected.", "\n"))
  
  if(grepl("ERR:006",client.error))
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
  else
  {
    server.errors <- datashield.errors()
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