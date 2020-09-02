## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#Libraries 
library(DSI)
library(DSOpal)
library(httr)
library(ds.client.connection.server)
library(dsBaseClient)

## -----------------------------------------------------------------------------
server.names <- c("Paris", "Newcastle", "New York")

## -----------------------------------------------------------------------------
url_Paris     <- 'https://192.168.56.100:8443'
url_Newcastle <- 'https://192.168.56.100:8443'
url_NewYork   <- 'https://192.168.56.100:8443'
server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)

## -----------------------------------------------------------------------------
table_Paris     <- "TESTING.DATASET1"
table_Newcastle <- "TESTING.DATASET2"
table_NewYork   <- "TESTING.DATASET3"
server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)

## -----------------------------------------------------------------------------
user_Paris      <-  "administrator"
user_Newcastle  <-  "administrator"
user_NewYork    <-  "administrator"
server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)

## -----------------------------------------------------------------------------
password_Paris      <-  "datashield_test&"
password_Newcastle  <-  "datashield_test&"
password_NewYork    <-  "datashield_test&"
server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)

## -----------------------------------------------------------------------------
ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)

## -----------------------------------------------------------------------------
driver_Paris     <- "OpalDriver"
driver_Newcastle <- "OpalDriver"
driver_NewYork   <- "OpalDriver"
server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)

## -----------------------------------------------------------------------------
login.data <- ds.build.login.data.frame(server.names,
                                        server.urls,
                                        server.tables,
                                        server.users.id,
                                        server.users.pwd,
                                        server.ssl.options,
                                        server.drivers)
print(login.data)

## ---- fig.show='hold'---------------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

