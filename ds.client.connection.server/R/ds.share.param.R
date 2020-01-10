#'@name ds.share.param  
#'@title client function TO DO 
#'@description  TODO
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@author Patricia Ryser-Welch
#'@export ds.logout
#'

library(DSI)
library(DSOpal)
library(httr)


ds.share.param <- function(connection)
{
  
  no.connections <- length(connection)
  for (i in 1:no.connections)
  {
    print("****")
    server.call <- paste("rUnifDS(",100,",",14,",",50,",",10,")",sep="")
    print(server.call)
    a <- ds.assign.value(connection[i], new.variable.name = paste("var", i), value = server.call)
    print(a)
  }
  print(ds.aggregate(connection, "ls()"))
  
  if (FALSE)
  {
  for(j in 1:Nstudies){
    selected.opal <- datasources[j]
    mean.study.specific <- ss.mat[j,1]
    Nvalid.study.specific <- ss.mat[j,3]
    # SAVE VALIDITY MESSAGE
    opal::datashield.assign(selected.opal, "mean.study.specific", as.symbol(mean.study.specific))
    opal::datashield.assign(selected.opal, "Nvalid.study.specific", as.symbol(Nvalid.study.specific))
  }
  }
}




.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.logout'
  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}