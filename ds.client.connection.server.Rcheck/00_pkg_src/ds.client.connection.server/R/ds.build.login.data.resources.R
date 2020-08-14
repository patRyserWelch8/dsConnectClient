#'@name ds.build.login.data.resources [ TO DO DOCUMENTATION]
#'@title Builds a dataframe to login to datashield
#'@description This function generates a valid data frame, that can be used to login
#'to some data computers (i.e. opal servers). The data frame models a double-entry table. The
#'columns are defined as server, url, user, password and table name. Each row holds the information
#'in relation to one data computer. The values for each column are passed  to the function as arguments.
#'
#'@param servers A vector of characters listing all the names of the data computers
#'@param urls A vector of characters listing each data computer HTTP address. The format is https://[TCPIP address or host name][:port]
#'@param users   A vector of characters listing a valid user name to log on on each server.
#'@param passwords A vector of characters listing the password for each user to log in to a data computer.
#'@param resources A vectors listing the resources name. 
#'@param options.ssl A vector used to set the options of the connection. Set by defaults the SSL values.
#'@param drivers  A vector used to set the name of the driver. It is set by default to OpalDriver.
#'@return a data frame. If the arguments are not correct. Then a data.frame with no rows is created.
#'
#'The expactactions are as follow:
#'Expectation no 0: the return value is a data.frame
#'Expectation no 1: the number of columns is equal 7.
#'Expectation no 2: the number of rows is equal to the number of servers
#'Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table is smaller than the length of server
#'Expectation no 4: the number of row is 0, if any of the urls does not start with http
#'@author Patricia Ryser-Welch
#'@export
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
  }
  else if (length(servers) == 0 || length(urls) == 0 ||  length(users) == 0 || length(passwords) == 0 || length(resources) == 0 
           || length(options) == 0 || length(drivers) == 0)
  {
      stop("ERR:004")
  }
  else if (all(startsWith(urls,"https")))
  {
      return(.build.object(servers, urls, users, passwords,resources, drivers))
  }
  else
  {
      stop("ERR:002")
  }
}

.build.object <- function(servers, urls, users, passwords,resources, drivers)
{
  builder <- DSI::newDSLoginBuilder()
  for(i in 1:length(servers))
  {
    builder$append(server = servers[i],url = urls[i], user = users[i], password = passwords[i], resource = resources[i], driver = drivers[i])
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


