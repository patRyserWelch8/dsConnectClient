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


ds.share.param <- function(connections=NULL,param.name = NULL)
{
  success <- FALSE
  tryCatch(
    {success <- .share.parameter(connections, param.name)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(success)})
}

.share.parameter <- function(connections=NULL,expression=NULL)
{
  if(length(connections) > 1 & is.character(expression))
  {
    outcome <- FALSE
    if (nchar(expression) > 0)
    {
        success <- .assignSettings(connections)
        if (success)
        {
          outcome <- .complete.exchange(connections,expression)
        }
    }
    else
    {
      stop("ERR:003")
    }
  }
  else
  {
    warning("WAR:001")
  }
}

.assignSettings <- function(connections)
{
  successful <- FALSE
  outcome    <- ds.aggregate(connections, "assignSharingSettingsDS()")
  if(is.list(outcome))
  {
    outcome.vector <- unlist(outcome)
    successful     <- all(outcome.vector == TRUE)
  }
  else
  {
    successful     <- outcome
  }

  if (!successful)
  {
    stop("ERR:002")
  }
  
  return(successful)
}

.complete.exchange <- function(connections, expression)
{
  outcome <- FALSE
  last <- length(connections)-1
  for(current in 1:last)
  {
    master     <- connections[[current]]
    receiver   <- connections[[current+1]]
    param.name <- .save.param(master,expression)
    outcome    <- .exchange(master, receiver, param.name)
  }
  return(outcome)
}


.save.param        <- function(connection, expression)
{
  param.name <- ""
  param.name <- ds.aggregate(connection, expression)
  return(param.name)
  
}

.exchange <- function(master, receiver, param.name)
{
  outcome    <- FALSE
  step       <-  1
  max.steps  <-  14
  while(step <= max.steps)
  {
    success <- switch(          
      step,
      .encrypt_data(master,master_mode = TRUE, preserve_mode = FALSE), #1
      .transfer.encrypted.matrix(master,receiver,master_mode = TRUE), #2
      .encrypt_data(receiver,master_mode = FALSE, preserve_mode = FALSE), #3
      .transfer.encrypted.matrix(receiver,master,master_mode = FALSE), #4
      .decrypt_data(master), #5
      .encrypt_param(master,param.name), #6
      .removeEncryptionData(master), #7
      .removeEncryptionData(receiver), #8
      .encrypt_data(receiver,master_mode = TRUE, preserve_mode = TRUE), #9
      .transfer.encrypted.matrix(receiver,master), #10
      .encrypt_data(master,master_mode = FALSE, preserve_mode = TRUE), #11
      .transfer.encrypted.matrix(master,receiver), #12
      .decrypt_data(receiver), #13
      .decrypt_param(receiver, param.name) #14
    )
   
    if (success)
    {
      step <- step + 1
    }
    else
    {
      step <- step * 1000
    }
  }
  
  if(step == max.steps + 1)
  {
    outcome <- TRUE
  }
  return(outcome)
}


.encrypt_data <- function(connection, master_mode=TRUE, preserve_mode = FALSE)
{
   expression <- paste0("encryptDataDS(master_mode=",master_mode,", preserve_mode=", preserve_mode,")")
   outcome    <- ds.aggregate(connection, expression)
   return(outcome)
}

.encrypt_param <- function(connection,param.name)
{
  expression <- paste0("encryptParamDS(param_name='",param.name, "')")
  outcome    <- .aggregate(connection, expression)
  return(outcome)
}

.decrypt_data <- function(connection)
{
  expression <- paste0("decryptDataDS()")
  outcome    <- .aggregate(connection, expression)
  return(outcome)
}

.decrypt_param <- function(connection, param.name)
{
  expression <- paste0("decryptParamDS('",param.name,"')")
  outcome    <- .aggregate(connection, expression)
  return(outcome)
}

.transfer.encrypted.matrix <- function(sender = NULL, receiver = NULL, master_mode = TRUE)
{
  outcome <- FALSE
  # retrieve from master server the encoded data
  received.data    <- ds.aggregate(sender, "getDataDS()")
  if(grepl(received.data$header,"FM1"))
  {
      # assign on the server the encoded data
      master.param     <- paste0("master_mode=", master_mode)
      header.param     <- paste0("header='", received.data$header,"'") 
      payload.param    <- paste0("payload='", received.data$payload, "'")
      property.a.param <- paste0("property.a=",received.data$property.a)
      property.b.param <- paste0("property.b=",received.data$property.b)
      property.c.param <- paste0("property.c=",received.data$property.c)
      property.d.param <- paste0("property.d=",received.data$property.d)
    
      expression <- paste0("assignDataDS(", master.param, "," ,header.param, ",", 
                           payload.param, ",", property.a.param , ",", 
                           property.b.param , ",", property.c.param , ",", property.d.param , ")")
      outcome <- ds.aggregate(receiver, expression)
  }
  return(outcome)
}

.encodeParam <- function(connection,param.name)
{
  expression <- paste0("encodeParamDS('",param.name, "')")
  outcome   <- ds.aggregate(connection,expression)
}

.removeEncryptionData <- function(connection)
{
  expression <- paste0("removeEncryptingDataDS()")
 
  outcome    <- ds.aggregate(connection,expression)
 
}

.warning <- function(message)
{
  
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
  if (grepl("WAR:001",error))
  {
    message(paste(header, "::",  "WAR:001\n", "More than one connection is required for sharing parameters.")) 
  }
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.share.param'
  if(grepl("ERR:002",error))
  {
    message(paste(header, "::",  "ERR:002\n", " The exchange of parameter could not be initiated. The process on the server could not be completed.")) 
  }
  else if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:003",error))
  {
    message(paste(header, "::",  "ERR:003\n", " You have yet to provide a valid aggregate function.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}