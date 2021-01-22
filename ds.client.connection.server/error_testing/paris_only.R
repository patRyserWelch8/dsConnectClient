#Libraries 
library(DSI)
library(DSOpal)
library(httr)
library(dsConnectClient)
library(dsBaseClient)

# one server 
server.names      <- c("Paris")
url_Paris         <- 'https://192.168.56.100:8443'
table_Paris       <- "TESTING.DATASET1"
user_Paris        <-  "administrator"
password_Paris    <-  "datashield_test&"
ssl_options_Paris <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
driver_Paris      <- "OpalDriver"

server.urls       <- c(url_Paris)
server.tables     <- c(table_Paris)
server.users.id   <- c(user_Paris)
server.users.pwd  <-  c(password_Paris)
server.ssl.options <- c(ssl_options_Paris)
server.drivers   <- c(driver_Paris)

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


server.function.call <- call('lsDS', NULL,".GlobalEnv")
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, error.stop = TRUE, datasources = connections)
print(outcome)

# more sanity check
ds.dataFrame(c("D$INTEGER"), newobj = "df", datasources = connections)
print(datashield.errors())

server.function.call <- call('lsDS', NULL,".GlobalEnv")
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, error.stop = TRUE, datasources = connections)
print(outcome)



#rNormDS<-function (n, mean = 0, sd = 1, force.output.to.k.decimal.places=9)
server.function.call <- call('rNormDS',1000)
DSI::datashield.assign(conns = connections, symbol = "aNormDist1", value = server.function.call, async = FALSE)


server.function.call <- call('lsDS', NULL,".GlobalEnv")
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, error.stop = TRUE, datasources = connections)
print(outcome)


#rNormDS<-function (n, mean = 0, sd = 1, force.output.to.k.decimal.places=9)
server.function.call <- call('test_assign_mixed_errorDS')
DSI::datashield.assign(conns = connections, symbol = "aPieInTheSky", value = server.function.call, async = FALSE)


server.function.call <- call('lsDS', NULL,".GlobalEnv")
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, error.stop = TRUE, datasources = connections)
print(outcome)


outcome <- ds.logout(connections)
print(outcome)