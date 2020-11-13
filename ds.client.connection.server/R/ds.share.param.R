#' @name ds.share.param  
#' @title sharing parameter between 
#' @description  TODO
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' @param param.names a character vector specifying the name of the variables. 
#' @param tolerance threshold for ignoring small floating point difference when 
#' comparing numeric vectors
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6.2, for older versions see the Wiki
#'   # Connecting to the Opal servers
#'   
#'   # Only for windows user 
#'   ## (switches implementation of SSL used by  the curl R package to "openssl")
#'   Sys.setenv(CURL_SSL_BACKEND = "openssl")
#' 
#'   # Load necessary client packages
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   require('ds.client.connection.server')
#' 
#'   # Append login information for a specific server
#'   
#'     #Data computers name
#'     server.names   <- c("Paris", "Newcastle", "New York")
#'     
#'     # Data computers url
#'     url_Paris     <- 'https://192.168.56.100:8443'
#'     url_Newcastle <- 'https://192.168.56.100:8443'
#'     url_NewYork   <-  'https://192.168.56.100:8443'
#'     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
#'     
#'     # Assign datasets
#'     table_Paris     <- "TESTING.DATASET1"
#'     table_Newcastle <- "TESTING.DATASET2"
#'     table_NewYork   <- "TESTING.DATASET3"
#'     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
#'
#'     # Set user and password to access the DataSHIELD servers
#'     user_Paris      <-  "administrator"
#'     user_Newcastle  <-  "administrator"
#'     user_NewYork    <-  "administrator"
#'     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
#'
#'     password_Paris      <-  "datashield_test&"
#'     password_Newcastle  <-  "datashield_test&"
#'     password_NewYork    <-  "datashield_test&"
#'     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
#'     
#'     # Set drivers
#'     driver_Paris     <- "OpalDriver"
#'     driver_Newcastle <- "OpalDriver"
#'     driver_NewYork   <- "OpalDriver"
#'     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
#'
#'     # Set SSL drivers
#'     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
#'     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
#'     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
#'     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
#'       
#'     # Create login data frame
#'     login.data <- ds.build.login.data.frame(server.names,
#'                                             server.urls,
#'                                             server.tables,
#'                                             server.users.id,
#'                                             server.users.pwd,
#'                                             server.ssl.options,
#'                                             server.drivers)
#'   # Log in to DataSHIELD server                                         
#'   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
#'   
#'   # Clear the Datashield/R sessions and logout
#'   ds.logout(connections) 
#' }
#' @author Patricia Ryser-Welch for DataSHIELD development
#' @export ds.share.param 
#'



ds.share.param <- function(param.names = NULL, tolerance = 15, datasources = NULL)
{
  success <- FALSE
  tryCatch(
    {success <- .share.parameter(param.names, tolerance, datasources)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {ds.error(error)},
    finally = {return(success)})
}

.share.parameter <- function(param.names = NULL, tolerance = 15, datasources = NULL)
{
 
  outcome <- FALSE
  
  if(length(datasources) > 1 & is.character(param.names))
  {
    if (length(param.names) > 0)
    {

        success <- .assign.settings(datasources)
        if (success)
        {
          outcome <- .complete.exchange(param.names, tolerance, datasources)
        }
        .remove.exchange.data(datasources)
    }
    else
    {
      stop("::ds.share.param::ERR:019")
    }
  }
  else
  {
    stop("::ds.share.param::ERR:020")
  }
  return(outcome)
}

.assign.settings <- function(connections)
{
  successful <- FALSE
  if (!is.null(connections))
  {
    outcome    <- ds.aggregate(expression = call("assignSharingSettingsDS"), datasources = connections )
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
    outcome <- ds.aggregate(expression = call("removeExchangeDataDS"), datasources = connections)
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
    
    
    if (success)
    {
      step <- step + 1
    }
    else
    {
      step <- step * 1000
    }
  }
  
  if(step == (max.steps + 1))
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
    expression <- call("assignParamSettingsDS",names.var.on.server)
    outcome    <- ds.aggregate(expression = expression, datasources = connection)
  }
  return(.transform.outcome.to.logical(outcome))
}

.encrypt.data <- function(connection, master_mode=TRUE, preserve_mode = FALSE)
{
   expression <- call("encryptDataDS", master_mode, preserve_mode)
   outcome    <- ds.aggregate(expression = expression, datasources = connection)
   return(.transform.outcome.to.logical(outcome))
}

.encrypt.param <- function(connection)
{
  expression <- call("encryptParamDS")
  outcome    <- ds.aggregate(expression = expression, datasources = connection)
  return(.transform.outcome.to.logical(outcome))
}

.decrypt.data <- function(connection)
{
  expression <- call("decryptDataDS")
  outcome    <- ds.aggregate(expression = expression, datasources = connection)
  return(.transform.outcome.to.logical(outcome))
}

.decrypt.param <- function(connection, param.names, tolerance = 15)
{
  names.var.on.server <-  paste(param.names, collapse=";")
  expression          <- call("decryptParamDS",names.var.on.server, tolerance)
  
  outcome    <- ds.aggregate(expression = expression, datasources = connection)
  return(.transform.outcome.to.logical(outcome))
}

.transfer.coordinates <- function(sender = NULL, receiver = NULL)
{
  outcome <- FALSE
 
  if(!is.null(sender) & !is.null(receiver))
  {
    
     received.coordinates <- ds.aggregate(expression = call("getCoordinatesDS"), datasources = sender)
     field.names          <- names(received.coordinates)
     expected.field.names <- c("header","payload","property.a","property.b","property.c","property.d")
     has.correct.field    <- all(expected.field.names %in% field.names)
    
     if (has.correct.field)
     {
       
       if(grepl(received.coordinates$header,"FM1"))
       {
           expression <- call("assignCoordinatesDS",received.coordinates$header, received.coordinates$payload,
                               received.coordinates$property.a, received.coordinates$property.b, received.coordinates$property.c,
                               received.coordinates$property.d)
          
           outcome <- ds.aggregate(expression = expression, datasources = receiver)
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
      received.data        <- ds.aggregate(expression = call("getDataDS"), datasources = sender )
      
      field.names          <- names(received.data)
      expected.field.names <- c("header","payload","property.a","property.b","property.c","property.d")
      has.correct.field    <- all(expected.field.names %in% field.names)
      if (has.correct.field)
      {
        if(grepl(received.data$header,"FM1"))
        {
            expression <- call("assignDataDS", master_mode, received.data$header, received.data$payload,
                               received.data$property.a,
                               received.data$property.b, received.data$property.c, received.data$property.d)
            
            outcome <-  ds.aggregate(expression = expression , datasources = receiver)
            
        }
      }
    }
  return(.transform.outcome.to.logical(outcome))
}


.remove.encryption.data <- function(connection = NULL, master.mode = TRUE)
{
  expression <- call("removeEncryptingDataDS", master.mode)
  outcome    <- ds.aggregate(expression = expression, datasources = connection)
  return(.transform.outcome.to.logical(outcome))
}

.warning <- function(message)
{
  
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
  if (grepl("WAR:001",message))
  {
    message(paste(header, "::",  "WAR:001\n", "More than one connection is required for sharing parameters.")) 
  }
}
                             