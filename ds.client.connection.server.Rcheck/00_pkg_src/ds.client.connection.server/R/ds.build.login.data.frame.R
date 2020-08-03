#'@name ds.build.login.data.frame
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
#'The expectactions are as follow:
#'Expectation no 0: the return value is a data.frame
#'Expectation no 1: the number of columns is equal 7.
#'Expectation no 2: the number of rows is equal to the number of servers
#'Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table is smaller than the length of server
#'Expectation no 4: the number of row is 0, if any of the urls does not start with http
#'@author Patricia Ryser-Welch
#'@export
ds.build.login.data.frame <- function (  data.computers.name, 
                                         data.computers.url, 
                                         data.computers.table.name,
                                         users.id, 
                                         users.password, 
                                         options.ssl, 
                                         driver.connection)
{
  return.data.frame <- NULL
  
  
  tryCatch(
    {return.data.frame <- .build.data.frame(data.computers.name,
                                           data.computers.url,
                                           data.computers.table.name,
                                           users.id,
                                           users.password,
                                           options.ssl,
                                           driver.connection) },
    warning = function(warning) {.warning(warning)},
    error = function(error) {ds.error(error)},
    finally = {return(return.data.frame)})
}


.build.data.frame <- function(data.computers.name, 
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
  print("********")
  print(data.computers.table.name)
  if(!is.vector.argument.correct(data.computers.table.name))
  {
    stop("::ds.build.login.data.frame::ERR:005")
  }
  print(users.id)
  if(!is.vector.argument.correct(users.id))
  {
    stop("::ds.build.login.data.frame::ERR:013")
  }
  
  if(!is.vector.argument.correct(users.password))
  {
    stop("::ds.build.login.data.frame::ERR:014")
  }
  
  return(.use.builder(data.computers.name, data.computers.url, 
                      data.computers.table.name,users.id, users.password, 
                      options.ssl, 
                      driver.connection))
}



.use.builder <- function(data.computers.name, data.computers.url, 
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


.warning <- function(message)
{
  if(!is.null(message))
  {
    print(paste("ds.client.connection.server::ds.build.login.data.frame :",   message ))
  }
}
