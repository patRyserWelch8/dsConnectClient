

find.error.message <- function(error)
{
 
  error.message <- NULL
  if(any(grepl("ERR::SERVER::TESTING::001",error)))
  {
     error.message <- "TESTING OPTION IS SET TO 1"
  }
  else if(any(grepl("ERR::SERVER::TESTING::002",error)))
  {
    error.message <- "TESTING OPTION IS SET TO 2"
  }
  else if(any(grepl("SERVER::ERR::SHARING::003",error)))
  {
    error.message <-  "Some data were not encoded at all.Have you encoded any data before calling this function?" 
  } 
  else if(any(grepl("SERVER::ERR::SHARING::004",error)))
  {
    error.message <-  "Some data could be obtained securely. Start again the whole exchange."
  } 
  else if(any(grepl("SERVER::ERR::SHARING::009",error)))
  {
    error.message <-  "Some SHARINGeters may not have been encoded on one server. Start again the whole exchange." 
  }
  else if(any(grepl("SERVER::ERR::SHARING::007",error)))
  {
    error.message <-  "Some data may not have been received on a server. Start again the whole exchange." 
  }
  else if(any(grepl("SERVER::ERR::SHARING::008",error)))
  {
    error.message <-  "The SHARINGeters may have not been created on the server yet. Please use ds.ls() function to check." 
  }
  else if(any(grepl("SERVER::ERR::SHARING::006",error)))
  {
    error.message <-  "The arguments passed to the servers are incorrect. Their length may indicate they have no character. Numerical values may be set to 0 or be negative. Check the server-side function call." 
  }
  else if(any(grepl("SERVER::ERR::SHARING::005",error)))
  {
    error.message <-  "The arguments passed to the servers are incorrect. The data type is either not numerical or character. Check the server-side function call." 
  }
  else if (any(grepl("SERVER::ERR::SHARING::001",error)))
  {
    error.message <-  "A server is not allowed to taking part in sharing SHARINGeters." 
  }
  else if (any(grepl("SERVER::ERR::SHARING::002",error)))
  {
    error.message <-  "A function on the server has not received the appropriate arguments." 
  }
  else if(any(grepl("SERVER-ERR-000",error)))
  {
    error.message <-  "Some error thrown by stop function on the server in an aggregate function."
  }
  else if(is.na(error))
  {
    error.message <-  "A function or R object may not exists on the server(s) ds.ls() and the tools made available on the Opal server can help you resolving this issue."
  }
  else if (grepl("ERR:001",error))
  {
    error.message <-  "Some elements are missing. Check you have passed as arguments the expected information "
  }
  else if (grepl("ERR:002",error))
  {
    error.message <- "All URLs should starts with `https'. It is more secure."
  }
  else if (grepl("ERR:003",error))
  {
    error.message <- "You have yet to specify some data computers name."
  }
  else if (grepl("ERR:004",error))
  {
    error.message <- "You have yet to specify the URLs of each data computer."
  }
  else if (grepl("ERR:005",error))
  {
    error.message <- "You have yet to specify the data tables."
  }
  else if(grepl("ERR:006",error))
  {
    error.message <- "Have you connected to the Opal server? Have you successfully started your Opal Servers? Have You need to pass a valid connection. Please use ds.login \n"
  }
  else if (grepl("ERR:007",error))
  {
    error.message <- "You have yet to provide a valid function. It should either be a character or a call type." 
  }
  else if (grepl("ERR:008",error))
  {
    error.message <- "You have yet to provide an appropriate variable.name. It should a character of length greater than 1" 
  }
  else if (grepl("ERR:009",error))
  {
    error.message <- "You have yet to provide a valid value. It can be a valid 'assign server-side function' or a value." 
  }
  else if (grepl("ERR:010",error))
  {
    error.message <- "You have yet to provide some login details. Have you build your login data frame?" 
  }
  else if (grepl("ERR:011",error))
  {
    error.message <- "The login data frame is too short. No server is specified. It needs to be greater of equal 1. Have you build your login data frame? " 
  }
  else if (grepl("ERR:012",error))
  {
    error.message <- "You have yet to provide a valid class type. It should a valid R type." 
  }
  else if (grepl("ERR:013",error))
  {
    error.message <- "You have yet to provide some users id; one for each server." 
  }
  else if (grepl("ERR:014",error))
  {
    error.message <- "You have yet to provide some user passwords; one for each server." 
  }
  else if (grepl("ERR:015",error))
  {
    error.message <- "You have yet to provide some ssl options; one for each server." 
  }
  else if (grepl("ERR:016",error))
  {
    error.message <- "You have yet to provide some drivers for connecting to the servers; one for each server." 
  }
  else if (grepl("ERR:017",error))
  {
    error.message <- "The connection data frame returned was set to null. Something must have gone wrong with the connection to the server(s). Check they are running or restart them." 
  }
  else if (grepl("ERR:018",error))
  {
    error.message <- "The parameters could not be shared. Some settings could not be created on each server involved in the exchanged." 
  }
  else if (grepl("ERR:019",error))
  {
    error.message <- "The parameters could not be shared. Some errors occurred during the exchange." 
  }
  else if (grepl("ERR:020",error))
  {
    error.message <- "More than one DataSHIELD server is required for sharing parameters. " 
  }
  else
  {
    error.message <- error
  }
  return(error.message)
   
}