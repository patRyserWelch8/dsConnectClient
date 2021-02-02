pkgname <- "dsConnectClient"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "dsConnectClient-Ex.timings", pos = 'CheckExEnv')
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
library('dsConnectClient')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ds.aggregate")
### * ds.aggregate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.aggregate
### Title: Executes a function on some R server sessions and returns the
###   outcome to the client
### Aliases: ds.aggregate

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
##D 
##D   # Execute aggretate server functions
##D   ds.aggregate(expression = quote(dimDS("D")),
##D                datasources = connections)
##D            
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections) 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.aggregate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.assign.value")
### * ds.assign.value

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.assign.value
### Title: Assign some values on some DataSHIELD servers
### Aliases: ds.assign.value

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
##D   
##D   # Assign some values in the server-side
##D   
##D   ds.assign.value(new.variable.name = "new_var",
##D                   value = "D$INTEGER", 
##D                   class.type = "integer", 
##D                   datasources = connections)
##D            
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections) 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.assign.value", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.build.login.data.frame")
### * ds.build.login.data.frame

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.build.login.data.frame
### Title: Builds a data frame on the server-side to login to DataSHIELD
### Aliases: ds.build.login.data.frame

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.build.login.data.frame", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.build.login.data.resources")
### * ds.build.login.data.resources

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.build.login.data.resources
### Title: Assign a resource in the server-side
### Aliases: ds.build.login.data.resources

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D   
##D   # Build the login data frame
##D   login.data.resources<-ds.build.login.data.resources( servers = "liver",
##D                                                        urls = "https://192.168.56.100:8443",
##D                                                        users = "administrator",
##D                                                        passwords = "datashield_test&",
##D                                                        resources = "OMICS.tcga_liver",
##D                                                        options.ssl ="list(ssl_verifyhost=0, 
##D                                                                      ssl_verifypeer=0)",
##D                                                        drivers = "OpalDriver")
##D   # Log in to DataSHIELD server 
##D   connections <- ds.login(login.data.frame = login.data.resources, assign = TRUE, symbol = "D") 
##D   
##D   # Assign a resource to a "RangedSummarizedExperiment" which is the type of the object
##D   ds.assign.value("rse",quote(as.resource.object(D)), "RangedSummarizedExperiment", FALSE, connections)
##D   
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections) 
##D   
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.build.login.data.resources", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.exists.on.server")
### * ds.exists.on.server

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.exists.on.server
### Title: Is an object defined with a specific class on the server(s)?
### Aliases: ds.exists.on.server

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
##D   
##D   # Assign new variable in the server
##D   ds.assign.value(new.variable.name = "new_var",
##D                   value = "D$INTEGER", 
##D                   class.type = "integer", 
##D                   datasources = connections)
##D   
##D   # Look if the variable exists on the server-side
##D   ds.exists.on.server(variable.name = "new_var",
##D                       class.type = "integer",
##D                       datasources = connections)
##D            
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections) 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.exists.on.server", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.login")
### * ds.login

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.login
### Title: DataSHIELD servers login
### Aliases: ds.login

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D") 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.login", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.logout")
### * ds.logout

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.logout
### Title: log out from some DataSHIELD servers
### Aliases: ds.logout

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
##D   
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections) 
## End(Not run)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.logout", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.remove.variable")
### * ds.remove.variable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.remove.variable
### Title: Delete a variable of a specific given type on the server-side
### Aliases: ds.remove.variable

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D   
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('dsConnectClient')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
##D 
##D   # Assign some values in the server-side
##D   ds.assign.value(new.variable.name = "new_var",
##D                   value = "D$INTEGER", 
##D                   class.type = "integer", 
##D                   datasources = connections)
##D                   
##D   #Remove from the server-side the assigned variable
##D   ds.remove.variable(variable.name = "new_var",
##D                      class.type = "integer",
##D                      datasources = connections)
##D            
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections)  
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.remove.variable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ds.share.param")
### * ds.share.param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ds.share.param
### Title: sharing parameter between
### Aliases: ds.share.param

### ** Examples

## Not run: 
##D 
##D   ## Version 6.2, for older versions see the Wiki
##D   # Connecting to the Opal servers
##D   
##D   # Only for windows user 
##D   ## (switches implementation of SSL used by  the curl R package to "openssl")
##D   Sys.setenv(CURL_SSL_BACKEND = "openssl")
##D 
##D   # Load necessary client packages
##D   require('DSI')
##D   require('DSOpal')
##D   require('dsBaseClient')
##D   require('ds.client.connection.server')
##D 
##D   # Append login information for a specific server
##D   
##D     #Data computers name
##D     server.names   <- c("Paris", "Newcastle", "New York")
##D     
##D     # Data computers url
##D     url_Paris     <- 'https://192.168.56.100:8443'
##D     url_Newcastle <- 'https://192.168.56.100:8443'
##D     url_NewYork   <-  'https://192.168.56.100:8443'
##D     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
##D     
##D     # Assign datasets
##D     table_Paris     <- "TESTING.DATASET1"
##D     table_Newcastle <- "TESTING.DATASET2"
##D     table_NewYork   <- "TESTING.DATASET3"
##D     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
##D 
##D     # Set user and password to access the DataSHIELD servers
##D     user_Paris      <-  "administrator"
##D     user_Newcastle  <-  "administrator"
##D     user_NewYork    <-  "administrator"
##D     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
##D 
##D     password_Paris      <-  "datashield_test&"
##D     password_Newcastle  <-  "datashield_test&"
##D     password_NewYork    <-  "datashield_test&"
##D     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
##D     
##D     # Set drivers
##D     driver_Paris     <- "OpalDriver"
##D     driver_Newcastle <- "OpalDriver"
##D     driver_NewYork   <- "OpalDriver"
##D     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
##D 
##D     # Set SSL drivers
##D     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
##D     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
##D       
##D     # Create login data frame
##D     login.data <- ds.build.login.data.frame(server.names,
##D                                             server.urls,
##D                                             server.tables,
##D                                             server.users.id,
##D                                             server.users.pwd,
##D                                             server.ssl.options,
##D                                             server.drivers)
##D   # Log in to DataSHIELD server                                         
##D   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
##D   
##D   # Clear the Datashield/R sessions and logout
##D   ds.logout(connections) 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ds.share.param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
