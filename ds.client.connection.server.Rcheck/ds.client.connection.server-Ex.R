pkgname <- "ds.client.connection.server"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ds.client.connection.server-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ds.client.connection.server')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ds.build.login.data.frame")
### * ds.build.login.data.frame

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.build.login.data.frame
### Title: Builds a dataframe to login to datashield
### Aliases: ds.build.login.data.frame

### ** Examples

#Libraries 
library(DSI)
library(DSOpal)
library(httr)
library(ds.client.connection.server)
library(dsBaseClient)

#data computers name
server.names   <- c("Paris", "Newcastle", "New York")
#data computers url
url_Paris     <- 'https://192.168.56.100:8443'
url_Newcastle <- 'https://192.168.56.100:8443'
url_NewYork   <-  'https://192.168.56.100:8443'
server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
table_Paris     <- "TESTING.DATASET1"
table_Newcastle <- "TESTING.DATASET2"
table_NewYork   <- "TESTING.DATASET3"
server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)

user_Paris      <-  "administrator"
user_Newcastle  <-  "administrator"
user_NewYork    <-  "administrator"
server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)

driver_Paris     <- "OpalDriver"
driver_Newcastle <- "OpalDriver"
driver_NewYork   <- "OpalDriver"
server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)

password_Paris      <-  "datashield_test&"
password_Newcastle  <-  "datashield_test&"
password_NewYork    <-  "datashield_test&"
server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)

ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
login.data <- ds.build.login.data.frame(server.names,
                                       server.urls,
                                       server.tables,
                                       server.users.id,
                                       server.users.pwd,
                                       server.ssl.options,
                                       server.drivers)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.build.login.data.frame", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
