#'@title logs to some dataSHIELD server using some login information
#'@description This function uses the login details to log on to the DataSHIELD server(s) specified. The login details can be validated
#'using the ds.build.login.data.frame function. 
#'@param login.data.frame  A dataframe table that holds login details. This table holds five elements required
#'   to login to the servers where the data to analyse is stored. The expected column names are
#'   'driver' (the \code{\link{DSDriver-class}} name, default is "OpalDriver"),
#'   'server' (the server name), url' (the server url), 'user' (the user name or the certificate file path),
#'   'password' (the user password or the private key file path), table' (the fully qualified name of
#'   the table in the data repository), 'options' (the SSL options). An additional column 'identifiers'
#'   can be specified for identifiers mapping (if supported by data repository). See also the documentation
#'   of the examplar input table \code{logindata} for details of the login elements.
#'@param assign A boolean which tells whether or not data should be assigned from the data repository
#'   table to R after login into the server(s). It is set to FALSE by default.
#'@param variables Specific variables to assign. If \code{assign} is set to FALSE this argument is ignored
#'   otherwise the specified variables are assigned to R. If no variables are specified (default) the whole
#'   data repository's table is assigned.
#'@param symbol A character, the name of the data frame to which the data repository's table will be
#'   assigned after login into the server(s). It is set to "D' by default.
#'@return object(s) of class DSConnection or NULL if some parameters are incorrect
#'@example
#'server <- c("study1", "study2")
#'url <- c("https://some.opal.host:8443","https://another.opal.host")
#'user <- c("user1", "datashield-certificate.pem")
#'password <- c("user1pwd", "datashield-private.pem")
#'table <- c("TESTING.DATASET1", "TESTING.DATASET2")
#'variables <-  list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER',
'FACTOR_INTEGER')
#'options <- c("","c(ssl.verifyhost=2,ssl.verifypeer=1)")
#'driver <- c("","OpalDriver")
#'login.data.frame <- ds.build.login.data.frame.o(server,url,table,user,password)
#'connections <- ds.login(login.data.frame, FALSE, variables,'V') 
#'@author Patricia Ryser-Welch
#'@export
#'
#'
library(DSI)
library(DSOpal)
library(httr)

ds.login <- function(login.data.frame = NULL, assign = FALSE, variables = NULL, symbol = 'D')
{
  connection <- NULL
  tryCatch(
    {connection <- .make.connection(login.data.frame, assign, variables, symbol)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(connection)})
}

.make.connection <- function(login.data.frame, assign, variables, symbol)
{
  if (is.null(login.data.frame))
  {
    stop("ERR:003", call. = FALSE)
  }
  else
  {
    if(length(login.data.frame[,1]) > 0)
    {
      
      connection <- return(DSI::datashield.login(login.data.frame, assign, variables, symbol))
     
      if (is.null(connection))
      {
        stop("ERR:005", call. = FALSE)
      }
      else
      {
        return(connection)
      }
    
    }
    else
    {
      stop("ERR:004", call. = FALSE)
    }
  }
}

.warning <- function(message)
{

  messaget(paste("Warning : ",   message ))
 
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.login'
  print(header)
  
  if (grepl("ERR:003",error))
  {
      message(paste(header, "::",  "ERR:003\n", " You have yet to provide some login details.")) 
  }
  else if (grepl("ERR:004",error))
  {
     message(paste(header, "::",   "ERR:004\n", " The length of the vectors passed as arguments must be greater than 1."))
  }
  else if (grepl("ERR:005",error))
  {
    message(paste(header, "::",   "ERR:004\n", " The connection data frame is null. Something must have gone wrong with the connection to the server. Check it is running or restart it."))
  }
  else
  {
    message(paste(header,"\n", error))
  }
}


