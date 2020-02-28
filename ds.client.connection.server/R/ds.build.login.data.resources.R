#'@name ds.build.login.data.resources [ TO DO DOCUMENTATION]
#'@title Builds a dataframe to login to datashield
#'@description This function generates a valid data frame, that can be used to login
#'to some data computers (i.e. opal servers). The data frame models a double-entry table. The
#'columns are defined as server, url, user, password and table name. Each row holds the information
#'in relation to one data computer. The values for each column are passed  to the function as arguments.
#'
#'@param data.computers.name A vector of characters listing all the names of the data computers
#'@param data.computers.url A vector of characters listing each data computer HTTP address. The format is https://[TCPIP address or host name][:port]
#'@param data.computers.table.name A vector of characters listing the name of the table stored in a data computer
#'@param users.id A vector of characters listing a valid user name to log on on each server.
#'@param users.password A vector of characters listing the password for each user to log in to a data computer.
#'@param options.ssl A vector used to set the options of the connection. Set by defaults the SSL values.
#'@param driver.connection A vector used to set the name of the driver. It is set by default to OpalDriver.
#'@return a data frame formatter in this manner: (server,url,user,password,table). If the arguments are not correct. Then a data.frame with no rows is created.
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
  return.data.object <- NULL
  print("GGGGG")
  tryCatch(
    {return.data.object <- .build.data.object(servers, urls, users, passwords,  resources, options.ssl,  drivers) },
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(return.data.object)})
}


.build.data.object <- function(servers, urls, users, passwords, resources, options.ssl, drivers) 
{
 
  print("UUUUUUU")
  #assign the arguments to the data frame format.
  servers <- as.character(servers)
  urls <- as.character(urls)
  users <- as.character(users)
  passwords <- as.character(passwords)
  resources <- as.character(resources)
  options.ssl <- as.character(options.ssl)
  drivers <- as.character(drivers)
 
 print("1")
  #Verify the length of each vector is the same
  NO_COLUMNS <- 7
  expected.elements <-length(servers) * NO_COLUMNS
  total.elements <- length(servers) + length(urls) + 
                    length(users) + length(passwords) + length(tables)  + 
                    length(options.ssl) + length(drivers)
 print("2")
  
  if (expected.elements != total.elements)
  {
      stop("ERR:001")
  }
  else if (length(server) == 0 || length(url) == 0 ||  length(user) == 0 || length(password) == 0 || length(table) == 0 
           || length(options) == 0 || length(driver) == 0)
  {
      stop("ERR:004")
  }
  else if (all(startsWith(url,"https")))
  {
      return(.build.object(servers, urls, users, passwords,  resources, drivers))
  }
  else
  {
      stop("ERR:002")
  }
}

.build.object <- function(servers, urls, users, passwords,  resources, drivers)
{
  builder <- DSI::newDSLoginBuilder()
  for(i in 1:length(servers))
  {
    print(servers[i])
    builder$append(server = servers[i],url = urls[i], user = users[i], password = passwords[i], resource = resources[i], driver = drivers[i] )
  }
  login.data <- builder$build()
  print("a")
  print(login.data)
  return( login.data)
  
 # builder <- DSI::newDSLoginBuilder()
#  builder$append(server = "study1", url = "https://opal-test.obiba.org", user = "dsuser", password = "password", resource = "test.CNSIM1", driver = "OpalDriver")
#  builder$append(server = "study2", url = "https://opal-test.obiba.org", user = "dsuser", password = "password", resource = "test.CNSIM2", driver = "OpalDriver")
#  builder$append(server = "study3", url = "https://opal-test.obiba.org", user = "dsuser", password = "password", resource = "test.CNSIM3", driver = "OpalDriver")
#  logindata <- builder$build()
  
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


