#' @name ds.build.login.data.frame
#' @title Builds a data frame on the server-side to login to DataSHIELD 
#' @description This function generates a valid data frame, that can be used to login
#' to some data computers (i.e. Opal servers). The data frame models a double-entry table. 
#' The columns are defined as server, url, user, password and table name.
#' Each row holds the information concerning one data computer.
#' The values for each column are passed  to the function as arguments.
#'
#' @param data.computers.name characters vector listing all the names of the data computers.
#' @param data.computers.url  characters vector listing each data computer HTTP address. 
#' The format is \code{https://[TCPIP address or host name][:port]}
#' @param data.computers.table.name characters vector listing the name of 
#' the table stored in a data computer.
#' @param users.id characters vector listing a valid user name to log to each server.
#' @param users.password characters vector listing the password for each 
#' user to log in to a data computer.
#' @param options.ssl the vector used to set the options of the connection. 
#' Set by defaults the SSL values.
#' @param driver.connection a vector used to set the name of the driver. 
#' It is set by default to \code{OpalDriver}.
#' @return \code{ds.build.login.data.frame} returns a data frame formatter in this manner:
#'  \code{("server","url","user","password","table")}.
#' If the arguments are not correct. Then a empty data frame will be created. 
#'
#' The expectations are as follow:\cr
#' Expectation no 0: the return value is a data frame.\cr
#' Expectation no 1: the number of columns is equal to 7.\cr
#' Expectation no 2: the number of rows is equal to the number of servers.\cr
#' Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table 
#' is smaller than the length of the server\cr
#' Expectation no 4: the number of rows is 0, if any of the URLs do not start with \code{http}\cr
#' 
#' @author Patricia Ryser-Welch for DataSHIELD development team
#'
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6.2, for older versions see the Wiki
#'   # Connecting to the Opal servers
#' 
#'   # Load necessary client packages
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   require('dsConnectClient')
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
#' }
#' @export

ds.build.login.data.frame <- function (  data.computers.name = NULL, 
                                         data.computers.url = NULL, 
                                         data.computers.table.name = NULL,
                                         users.id = NULL, 
                                         users.password = NULL, 
                                         options.ssl = NULL, 
                                         driver.connection = NULL)
{
  return.data.frame <- NULL
  tryCatch(
    {return.data.frame <- dsdf.build.data.frame(data.computers.name,
                                           data.computers.url,
                                           data.computers.table.name,
                                           users.id,
                                           users.password,
                                           options.ssl,
                                           driver.connection) },
    warning = function(warning) {ds.warning("ds.build.login.data.frame",warning)},
    error = function(error) {ds.error(error)},
    finally = {return(return.data.frame)})
}


dsdf.build.data.frame <- function(data.computers.name, 
                              data.computers.url, 
                              data.computers.table.name, 
                              users.id, 
                              users.password, 
                              options.ssl, 
                              driver.connection) 
{
  
  if(!is.vector.argument.correct(data.computers.name))
  {
    stop("::ds.build.login.data.frame::ERR:003")
  }
  
  if(!is.vector.argument.correct(data.computers.url))
  {
    stop("::ds.build.login.data.frame::ERR:004")
  }
  
  if(!all(startsWith(data.computers.url,"https")))
  {
    stop("::ds.build.login.data.frame::ERR:002")
  }

  if(!is.vector.argument.correct(data.computers.table.name))
  {
    stop("::ds.build.login.data.frame::ERR:005")
  }
  
  if(!is.vector.argument.correct(users.id))
  {
    stop("::ds.build.login.data.frame::ERR:013")
  }
  
  if(!is.vector.argument.correct(users.password))
  {
    stop("::ds.build.login.data.frame::ERR:014")
  }
  
  if(!is.vector.argument.correct(options.ssl))
  {
    stop("::ds.build.login.data.frame::ERR:015")
  }
  
  if(!is.vector.argument.correct(driver.connection))
  {
    stop("::ds.build.login.data.frame::ERR:016")
  }
  
  return(dsdf.use.builder(data.computers.name, data.computers.url, 
                      data.computers.table.name,users.id, users.password, 
                      options.ssl, 
                      driver.connection))
}



dsdf.use.builder <- function(data.computers.name, data.computers.url, 
                    data.computers.table.name,users.id, users.password, 
                    options.ssl, 
                    driver.connection)
{
  #assign the arguments to the data frame format.
  server <- as.character(data.computers.name)
  url <- as.character(data.computers.url)
  user <- as.character(users.id)
  password <- as.character(users.password)
  table <- as.character(data.computers.table.name)
  options.ssl <- as.character(options.ssl)
  driver <- as.character(driver.connection)
  
  builder <- DSI::newDSLoginBuilder()
  for (i in 1:length(server))
  {
    builder$append(server=server[i],
                   url=url[i],
                   table = table[i],
                   driver = driver[i],
                   user = user[i],
                   password = password[i],
                   options = options.ssl[i])
  }
  
  return(builder$build())
}


