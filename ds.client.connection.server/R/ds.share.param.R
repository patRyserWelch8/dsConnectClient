#'@name ds.share.param  
#'@title sharing parameter between 
#'@description  TODO
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@author Patricia Ryser-Welch
#'@export ds.logout
#'

library(DSI)
library(DSOpal)
library(httr)


ds.share.param <- function(connections=NULL,param.names = NULL, tolerance = 15)
{
  success <- FALSE
  tryCatch(
    {success <- .share.parameter(connections, param.names)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {ds.error(error)},
    finally = {return(success)})
}

.share.parameter <- function(connections=NULL,param.names = NULL, tolerance = 15)
{
  outcome <- FALSE
  if(length(connections) > 1 & is.character(param.names))
  {
    if (length(param.names) > 0)
    {
        success <- .assign.settings(connections)
        if (success)
        {
          outcome <- .complete.exchange(connections,param.names, tolerance)
        }
        .remove.exchange.data(connections)
    }
    else
    {
      stop("::ds.share.param::ERR:019")
    }
  }
  else
  {
    warning("WAR:001")
  }
  return(outcome)
}

.assign.settings <- function(connections)
{
  successful <- FALSE
  if (!is.null(connections))
  {
    outcome    <- ds.aggregate(connections, call("assignSharingSettingsDS"))
  
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
      stop("::ds.share.param::ERR:018")
    }
  }
  return(successful)
}

.remove.exchange.data <- function(connections)
{
  if (!is.null(connections))
  {
    outcome <- ds.aggregate(connections, "removeExchangeDataDS()")
  }
}

.remove.existing.parameters <- function(connections, param.names = NULL)
{
  for (param.name in param.names)
  {
    ds.remove.variable(connectios,param.name,"numeric")
  }
}

.complete.exchange <- function(connections, param.names = NULL, tolerance = 15)
{
  outcome        <- FALSE
  no.connections <- length(connections)
  if(no.connections > 1)
  {
    last        <- no.connections-1
    master      <- connections[[1]]
    continue    <- TRUE
    current     <- 1
    
    while(continue)
    {
      master     <- connections[[current]]
      receiver   <- connections[[current+1]]
      success    <- .exchange(master, receiver, param.names, tolerance)
      continue   <- success 
      
      if(current < last)
      {
         current  <- current + 1 
      }
      else
      {
         continue <- FALSE
      }
    }
    outcome <- success
  }
  return(outcome)
}


.exchange <- function(master, receiver, param.names = NULL, tolerance = 15)
{
  outcome    <- FALSE
  step       <-  1
  max.steps  <-  16
 
  while(step <= max.steps)
  {
    print(step)
    success <- switch(          
       step,
      .encrypt.data(master,master_mode = TRUE, preserve_mode = FALSE), #1
      .transfer.encrypted.matrix(master,receiver,master_mode = TRUE), #2
      .encrypt.data(receiver,master_mode = FALSE, preserve_mode = FALSE), #3
      .transfer.encrypted.matrix(receiver,master,master_mode = FALSE), #4
      .decrypt.data(master), #5
      .assign.param.settings(master, param.names), #6
      .transfer.coordinates(master, receiver), #7 
      .encrypt.param(master), #8
      .remove.encryption.data(master, master.mode = TRUE), #9
      .remove.encryption.data(receiver, master.mode = FALSE),  #10 
      .encrypt.data(receiver,master_mode = TRUE, preserve_mode = TRUE),  #11
      .transfer.encrypted.matrix(receiver,master), #12
      .encrypt.data(master,master_mode = FALSE, preserve_mode = TRUE), #13
      .transfer.encrypted.matrix(master,receiver), #14
      .decrypt.data(receiver), #15
      .decrypt.param(receiver, param.names, tolerance) #16
    )
    print(success)
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

.transform.outcome.to.logical <- function(value)
{
  outcome <- FALSE
  if(is.logical(value))
  {
    outcome <- value
  }
  else 
  {
    if(is.list(value))
    {
      outcome.vector <- unlist(value)
      outcome        <- all(TRUE %in% outcome.vector)
    }
  }
  return(outcome)
}

.assign.param.settings <- function(connection, param.names = NULL)
{
  outcome <- FALSE
  if(is.character(param.names) & is.vector(param.names))
  {
    names.var.on.server <-  paste(param.names, collapse=";")
    #names.var.on.server <-  paste0("'",names.var.on.server,"'")
    #expression <- paste0("assignParamSettingsDS(param_names = ", names.var.on.server,")")
    expression <- call("assignParamSettingsDS",names.var.on.server)
    #outcome    <- ds.aggregate(connection, expression)
    outcome    <- DSI::datashield.aggregate(connection, expression)
  }
  return(.transform.outcome.to.logical(outcome))
}

.encrypt.data <- function(connection, master_mode=TRUE, preserve_mode = FALSE)
{
   #server function signature: encryptDataDS <- function(master_mode=TRUE, preserve_mode = FALSE)
   expression <- call("encryptDataDS", master_mode, preserve_mode)
   outcome    <- ds.aggregate(connection, expression)
   return(.transform.outcome.to.logical(outcome))
}

.encrypt.param <- function(connection)
{
  #expression <- paste0("encryptParamDS()")
  expression <- call("encryptParamDS")
  outcome    <- ds.aggregate(connection, expression)
  return(.transform.outcome.to.logical(outcome))
}

.decrypt.data <- function(connection)
{
  #expression <- paste0("decryptDataDS()")
  expression <- call("decryptDataDS")
  outcome    <- ds.aggregate(connection, expression)
  return(.transform.outcome.to.logical(outcome))
}

.decrypt.param <- function(connection, param.names, tolerance = 15)
{
  names.var.on.server <-  paste(param.names, collapse=";")
  #names.var.on.server <-  paste0("'",names.var.on.server,"'")
  #expression <- paste0("decryptParamDS(",names.var.on.server,",", tolerance, ")")
  expression <- call("decryptParamDS",names.var.on.server, tolerance)
  
  outcome    <- ds.aggregate(connection, expression)
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value_B")')
  return(.transform.outcome.to.logical(outcome))
}

.transfer.coordinates <- function(sender = NULL, receiver = NULL)
{
  outcome <- FALSE
 
  if(!is.null(sender) & !is.null(receiver))
  {
    
     received.coordinates <- ds.aggregate(sender, call("getCoordinatesDS"))
     field.names          <- names(received.coordinates)
     expected.field.names <- c("header","payload","property.a","property.b","property.c","property.d")
     has.correct.field    <- all(expected.field.names %in% field.names)
    
     if (has.correct.field)
     {
       
       if(grepl(received.coordinates$header,"FM1"))
       {
        
           #header.param     <- paste0("header='", received.coordinates$header,"'") 
           #payload.param    <- paste0("payload='", received.coordinates$payload, "'")
           #property.a.param <- paste0("property.a=",received.coordinates$property.a)
           #property.b.param <- paste0("property.b=",received.coordinates$property.b)
           #property.c.param <- paste0("property.c=",received.coordinates$property.c)
           #property.d.param <- paste0("property.d=",received.coordinates$property.d)
           #expression       <- paste0("assignCoordinatesDS(", header.param, ",", 
           #                                                    payload.param, ",", 
           #                                                    property.a.param , ",", 
           #                                                    property.b.param , ",", 
           #                                                    property.c.param , ",", 
           #                                                    property.d.param , ")")
           expression <- call("assignCoordinatesDS",received.coordinates$header, received.coordinates$payload,
                               received.coordinates$property.a, received.coordinates$property.b, received.coordinates$property.c,
                               received.coordinates$property.d)
          
           outcome <- ds.aggregate(receiver, expression)
           
           outcome <- .transform.outcome.to.logical(outcome)
       }
     }
  }
  
  return(outcome)
}

.transfer.encrypted.matrix <- function(sender = NULL, receiver = NULL, master_mode = TRUE)
{
  outcome <- FALSE
  
  if(!is.null(sender) & !is.null(receiver))
  {
      # retrieve from master server the encoded data
      received.data        <- ds.aggregate(sender, call("getDataDS"))
      
      field.names          <- names(received.data)
      expected.field.names <- c("header","payload","property.a","property.b","property.c","property.d")
      has.correct.field    <- all(expected.field.names %in% field.names)
      if (has.correct.field)
      {
        if(grepl(received.data$header,"FM1"))
        {
            # assign on the server the encoded data
            
            #master.param     <- paste0("master_mode=", master_mode)
            #header.param     <- paste0("header='", received.data$header,"'") 
            #payload.param    <- paste0("payload='", received.data$payload, "'")
            #property.a.param <- paste0("property.a=",received.data$property.a)
            #property.b.param <- paste0("property.b=",received.data$property.b)
            #property.c.param <- paste0("property.c=",received.data$property.c)
            #property.d.param <- paste0("property.d=",received.data$property.d)
          
            #expression <- paste0("assignDataDS(", master.param, "," ,header.param, ",", 
            #                     payload.param, ",", property.a.param , ",", 
            #                     property.b.param , ",", property.c.param , ",", property.d.param , ")")
            expression <- call("assignDataDS", master_mode, received.data$header, received.data$payload,
                               received.data$property.a,
                               received.data$property.b, received.data$property.c, received.data$property.d)
            
            outcome <-  ds.aggregate(receiver, expression)
            
        }
      }
   }
   return(.transform.outcome.to.logical(outcome))
}


.remove.encryption.data <- function(connection = NULL, master.mode = TRUE)
{
  #expression <- paste0("removeEncryptingDataDS(master_mode = ", master.mode, ")")
  expression <- call("removeEncryptingDataDS", master.mode)
  print(expression)
  outcome    <- .aggregate(connection,expression)
  print(outcome)
  return(.transform.outcome.to.logical(outcome))
}

.warning <- function(message)
{
  
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
  if (grepl("WAR:001",error))
  {
    message(paste(header, "::",  "WAR:001\n", "More than one connection is required for sharing parameters.")) 
  }
}

