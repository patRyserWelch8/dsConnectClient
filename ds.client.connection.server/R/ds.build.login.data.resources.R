#' @name ds.build.login.data.resources
#' @title Assign a resource in the server-side
#' @description Assign a resource object of class "\code{ResourceClient}" to an R symbol in the
#' DataSHIELD R session. 
#'
#' @param servers a vector of characters listing all the names of the data computers
#' @param urls a vector of characters listing each data computer HTTP address. The format is 
#' \code{https://[TCPIP address or host name][:port]}
#' @param users   a vector of characters listing a valid user name to log on on each server.
#' @param passwords a vector of characters listing the password 
#' for each user to log in to a data computer.
#' @param resources a vector listing the resources name. 
#' @param options.ssl a vector is used to set the options of the connection. 
#' Set by defaults the SSL values.
#' @param drivers  a vector is used to set the name of the driver. 
#' It is set by default to OpalDriver.
#' @return \code{ds.build.login.data.frame} returns a data frame formatter in this manner:
#'  \code{("server","url","user","password","table")}.
#' If the arguments are not correct. Then a empty data frame will be created. 
#'
#' The expectations are as follow:\cr
#' Expectation no 0: the return value is a data frame\cr
#' Expectation no 1: the number of columns is equal to 7\cr
#' Expectation no 2: the number of rows is equal to the number of servers \cr
#' Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table is 
#' smaller than the length of the server \cr
#' Expectation no 4: the number of rows is 0, if any of the URLs do not start with HTTP
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
#'   # Build the login data frame
#'   login.data.resources<-ds.build.login.data.resources( servers = "liver",
#'                                                        urls = "https://192.168.56.100:8443",
#'                                                        users = "administrator",
#'                                                        passwords = "datashield_test&",
#'                                                        resources = "OMICS.tcga_liver",
#'                                                        options.ssl ="list(ssl_verifyhost=0, 
#'                                                                      ssl_verifypeer=0)",
#'                                                        drivers = "OpalDriver")
#'   # Log in to DataSHIELD server 
#'   connections <- ds.login(login.data.frame = login.data.resources, assign = TRUE, symbol = "D") 
#'   
#'   # Assign a resource to a "RangedSummarizedExperiment" which is the type of the object
#'   ds.assign.value("rse",quote(as.resource.object(D)), "RangedSummarizedExperiment", FALSE, connections)
#'   
#'   # Clear the Datashield/R sessions and logout
#'   ds.logout(connections) 
#'   }
#' @author Patricia Ryser-Welch and Leire Abarrategui for DataSHIELD development team
#' @export
ds.build.login.data.resources <- function (servers, urls, users, passwords, resources, options.ssl, drivers)
{
  return.data.object <- data.frame()
  tryCatch(
    {return.data.object <- .build.data.object(servers, urls, users, passwords,  resources, options.ssl,  drivers) },
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(return.data.object)})
}


.build.data.object <- function(some.servers, some.urls, some.users, some.passwords, some.resources, some.options.ssl, some.drivers) 
{

  #assign the arguments to the data frame format.
  servers <- as.character(some.servers)
  urls <- as.character(some.urls)
  users <- as.character(some.users)
  passwords <- as.character(some.passwords)
  resources <- as.character(some.resources)
  options.ssl <- as.character(some.options.ssl)
  drivers <- as.character(some.drivers)
 
  #Verify the length of each vector is the same
  NO_COLUMNS <- 7
  expected.elements <-length(servers) * NO_COLUMNS
  total.elements <- length(servers) + length(urls) + 
                    length(users) + length(passwords) + length(resources)  + 
                    length(options.ssl) + length(drivers)
  
  if (expected.elements != total.elements)
  {
      stop("ERR:001")
  } else if (length(servers) == 0 || length(urls) == 0 ||  length(users) == 0 || length(passwords) == 0 || length(resources) == 0 || length(options) == 0 || length(drivers) == 0)
  {
      stop("ERR:004")
  } else if (all(startsWith(urls,"https")))
  {
      return(.build.object(servers, urls, users, passwords,resources, options.ssl, drivers))
  }  else
  {
      stop("ERR:002")
  }
}

.build.object <- function(servers, urls, users, passwords,resources, options.ssl, drivers)
{
  builder <- DSI::newDSLoginBuilder()
  for(i in 1:length(servers))
  {
    builder$append(server = servers[i],url = urls[i], user = users[i], password = passwords[i], resource = resources[i], options = options.ssl[i], driver = drivers[i])
  }
  login.data <- builder$build()
  return(login.data)
}

.warning <- function(message)
{
  if(!is.null(message))
  {
    messaget(paste("ds.client.connection.server::ds.build.login.data.frame :",   message ))
  }
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.build.login.data.frame'

  if (grepl("ERR:004",error))
  {
    message(paste(header, "::",  "ERR:004\n", " The length of the vectors passed as arguments must be at least 1.")) 
  }
  else if (grepl("ERR:001",error))
  {
    message(paste(header, "::",   "ERR:001\n", " The length of the vectors passed as arguments are not the same length."))
  }
  else if (grepl("ERR:002",error))
  {
    message(paste(header, "::",   "ERR:002\n", " The URL should starts with https"))
  }
  else
  {
    message(paste(header,"\n", error))
  }
}


