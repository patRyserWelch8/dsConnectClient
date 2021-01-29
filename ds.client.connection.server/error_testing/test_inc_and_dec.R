#Libraries 
library(DSI)
library(DSOpal)
library(httr)
library(dsConnectClient)
library(dsBaseClient)

# one server 
server.names      <- c("Paris", "NewYork", "Cambridge")
url_Paris         <- 'https://192.168.56.100:8443'
table_Paris       <- "TESTING.DATASET1"
user_Paris        <-  "administrator"
password_Paris    <-  "datashield_test&"
ssl_options_Paris <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
driver_Paris      <- "OpalDriver"


url_NewYork           <- 'https://192.168.56.100:8443'
table_NewYork         <- "TESTING.DATASET3"
user_NewYork          <-  "administrator"
password_NewYork      <-  "datashield_test&"
ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
driver_NewYork        <- "OpalDriver"

url_Cambridge           <- 'https://192.168.56.100:8443'
table_Cambridge         <- "TESTING.DATASET3"
user_Cambridge        <-  "administrator"
password_Cambridge      <-  "datashield_test&"
ssl_options_Cambridge  <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
driver_Cambridge      <- "OpalDriver"


server.urls       <- c(url_Paris, url_NewYork,url_Cambridge)
server.tables     <- c(table_Paris, table_NewYork,table_Cambridge)
server.users.id   <- c(user_Paris, user_NewYork, user_Cambridge)
server.users.pwd  <-  c(password_Paris, password_NewYork, password_Cambridge)
server.ssl.options <- c(ssl_options_Paris, ssl_options_NewYork, ssl_options_Cambridge)
server.drivers   <- c(driver_Paris,driver_NewYork,driver_Cambridge)

# build datasets 
login.data <- ds.build.login.data.frame(server.names,
                                        server.urls,
                                        server.tables,
                                        server.users.id,
                                        server.users.pwd,
                                        server.ssl.options,
                                        server.drivers)
print(login.data)

# connect to server
server.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                   'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                   'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC',
                   'FACTOR_CHARACTER','FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER',
                   'CATEGORY')
connections <- ds.login(login.data, variables = server.var, symbol = 'D')
print(connections)


# sanity check errors option set to true by default
server.function.call <- call('dimDS','D')
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, datasources = connections)
print(outcome)

if (FALSE)
{
print("incDS")
server.function.call <- call('incDS')
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, datasources = connections)
print(outcome)

print("decDS")
server.function.call <- call('decDS')
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, datasources = connections)
print(outcome)
}
pi_a < pi
outcome    <- ds.aggregate(expression = call("assignSharingSettingsDS"), error.stop = TRUE,
                            datasources = connections)
print(outcome)
outcome    <- ds.aggregate(expression = call("removeExchangeDataDS"), error.stop = TRUE,
                            datasources = connections )
print(outcome)
print("_______")
print(ds.share.param(param.names = "pi_a", datasources = connections))

outcome <- ds.logout(connections)
print(outcome)