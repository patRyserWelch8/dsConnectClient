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
        
       
        #master   <- connections[[1]]
        
        #print("step 0")
        #param.name <- ds.aggregate(master, expression)
        #
        #print(outcome)
        
        #if(FALSE)
        #{
        #print(outcome)
        #for(current in 1:last)
        #{
        #  print("step 1")
        #  master   <- connections[[current]]
        #  receiver <- connections[[current+1]]
        #  outcome <- ds.aggregate(master, "environmentInfoDS()")
        #  print(outcome)
        #  outcome <- ds.aggregate(receiver, "environmentInfoDS()")
        #  print(outcome)
          
        #  print("step 2")
        #  .initiateExchange(master, master=TRUE)
        #  outcome <- ds.aggregate(master, "environmentInfoDS()")
        #  print(outcome)
        #  print("step 3")
        #  .transfer.encoded.matrix(master, receiver)
        #   outcome <- ds.aggregate(receiver, "environmentInfoDS()")
           
        #   print("step 4")
        #  .initiateExchange(receiver,master=FALSE)
        #  print("RECEIVER AFTER SHARING AND init ")
        #  print(outcome)
        #  outcome <- ds.aggregate(receiver, "ls(sharing)")
        #  print(outcome)
        #  .transfer.encoded.matrix(receiver, master)
        #  print("end of phase III")
        #  outcome <- ds.aggregate(master, "ls(sharing)")
        #  print(outcome)
          
        #  print("step 5")
        #  .encodeParam(master,param.name)
        # outcome <- ds.aggregate(receiver, "ls(sharing)")
        #  print(outcome)
         
          #outcome <- ds.remove.variable(master,"sharing")
        #}
        
    
        outcome <- TRUE
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
  .assignSettings(connections)
  last <- length(connections)-1
  for(current in 1:last)
  {
    master     <- connections[[current]]
    receiver   <- connections[[current+1]]
    param.name <- .save.param(master,expression)
    outcome    <- .exchange(master, receiver, param.name)
   
  }
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
  max.steps  <-  4
  print(max.steps)
  while(step <= max.steps)
  {
    print(step)
    success <- switch(          
      step,
      .encrypt_data(master,master_mode = TRUE, preserve_mode = FALSE), #1
      .transfer.encrypted.matrix(master,receiver,master_mode = TRUE), #2
       print(ds.aggregate(master,"existsDS('sharing','.GlobalEnv','list')")),
      .encrypt_data(receiver,master_mode = FALSE, preserve_mode = FALSE), #3
      .transfer.encrypted.matrix(receiver,master,master_mode = FALSE), #6
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
    
    print(step)
    
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
   d <- ds.aggregate(connection,"DANGERgetSharing()")
   print(d$no_columns)
   print(d$no_rows)
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
  print("HHHHHHH")
  expression <- paste0("decryptParamDS('",param.name,"')")
  print(expression)
  outcome    <- .aggregate(connection, expression)
  return(outcome)
}

.transfer.encrypted.matrix <- function(sender = NULL, receiver = NULL, master_mode = TRUE)
{
  outcome <- FALSE
  # retrieve from master server the encoded data
  received.data    <- ds.aggregate(sender, "getDataDS()")
  print(received.data$header)
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
      print(expression)
      outcome <- ds.aggregate(receiver, expression)
      print(outcome)
      d <- ds.aggregate(receiver,"DANGERgetSharing()")
      print(d$no_columns)
      print(d$no_rows)
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
  print(expression)
  outcome    <- .aggregate(connection,expression)
  print(outcome)
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