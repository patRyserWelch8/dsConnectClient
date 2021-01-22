#Libraries 
library(DSI)
library(DSOpal)
library(httr)
library(dsConnectClient)
library(dsBaseClient)

# one server 
server.names      <- c("Paris")
url_Paris         <- 'https://192.168.57.101:8443'
table_Paris       <- "CNSIM.CNSIM1"
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
server.var <- list('LAB_TSC','LAB_TRIG','LAB_HDL','PM_BMI_CONTINUOUS','DIS_CVA','MEDI_LPD','DIS_DIAB','DIS_AMI', 'GENDER',
                   'PM_BMI_CATEGORICAL')
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
ds.dataFrame(c("D$GENDER"), newobj = "df", datasources = connections)
print(datashield.errors())

server.function.call <- call('lsDS', NULL,".GlobalEnv")
print(server.function.call)
outcome <- ds.aggregate(expression = server.function.call, asynchronous = FALSE, error.stop = TRUE, datasources = connections)
print(outcome)


outcome <- ds.logout(connections)
print(outcome)