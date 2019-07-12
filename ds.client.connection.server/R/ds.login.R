#'@titlet to do
#'@description to do
#'@param data.computers.name A vector of characters listing all the names of the data computers
#'@return   to do
#'
#'The expactactions are as follow:
#'Expectation no 0: the return value is a data.frame
#'Expectation no 1: the number of columns is equal 5.
#'Expectation no 2: the number of rows is equal to the number of servers
#'Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table is smaller than the length of server
#'Expectation no 4: the number of row is 0, if any of the urls does not start with http
#'@author Patricia Ryser-Welch
#'@export
#'
#'
library(DSI)
library(DSOpal)

ds.login <- function(login.data.frame = NULL, assign = FALSE, variables = NULL, symbol = 'D')
{
  
  if (is.null(login.data.frame))
  {
     message("You have yet to provide some login details.")
     stop("ERR:003")
  }
  else
  {
    if(length(login.data.frame[,1]) > 0)
    {
       return(datashield.login(login.data.frame, assign, variables, symbol))
    }
    else
    {
      message("The length of the vectors passed as arguments must be greater than 1.")
      stop("ERR:004")
    }
  }
}

