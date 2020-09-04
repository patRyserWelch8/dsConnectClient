# TESTING.DATASET3.
# Author: Patricia Ryser-Welch, DataSHIELD team

# Connect to three servers and the three datasets. Four local variables named ds.test_env$local.values.1,
# ds.test_env$local.values.2, ds.test_env$local.values.3 and ds.test_env$local.values are created.
# version 2.1a - 17/02/2020 - Some issues with accessing the ds.test_env environment were experienced. Only inside certain 
# functions the connections were able to be reached. For that reason, the connect functions have been altered to return a connection.


init.all.datasets <- function(ds.test_env = NULL)
{
 
  if (exists("login.details"))
  {
    #reading data from local files 
    ds.test_env$local.values.1 <- read.csv("data_files/DATASET1.csv", header = TRUE)
    ds.test_env$local.values.2 <- read.csv("data_files/DATASET2.csv", header = TRUE)
    ds.test_env$local.values.3 <- read.csv("data_files/DATASET3.csv", header = TRUE)
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
    
    if (login.details$driver == "OpalDriver") 
    {
     
      #connecting to the servers
      ds.test_env$server   <- c("study1", "study2", "study3")
      ds.test_env$table    <- c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
      
      ds.test_env$login.data <- ds.build.login.data.frame(ds.test_env$server,
                                                         login.details$get_ip_addresses(3),
                                                         ds.test_env$table,
                                                         login.details$get_users(3),
                                                         login.details$get_passwords(3),
                                                         login.details$get_ssl_options(3),
                                                         login.details$get_drivers(3))
      
       
      
    }
    else 
    {
      ds.test_env$login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
    }
    
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY')
  }
}

# Connect to one server and the three datasets. One local variables named ds.test_env$local.values.3 is created.
init.dataset.3 <- function(ds.test_env = NULL)
{
  if (exists("login.details"))
  {
    ds.test_env$local.values.3 <- read.csv("data_files/DATASET3.csv", header = TRUE)
    if (login.details$driver == "OpalDriver")
    {
      #connecting to the servers
      ds.test_env$server   <- c("study3")
      ds.test_env$table    <- c("TESTING.DATASET3")
      ds.test_env$login.data <- ds.build.login.data.frame(ds.test_env$server,
                                                          login.details$get_ip_addresses(1),
                                                          ds.test_env$table,
                                                          login.details$get_users(1),
                                                          login.details$get_passwords(1),
                                                          login.details$get_ssl_options(1),
                                                          login.details$get_drivers(1))
      
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study3")
    }
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY')
  }
}

# Connect to one server and the three datasets. One local variables named ds.test_env$local.values.2 is created.
init.dataset.2 <- function(ds.test_env = NULL)
{
  
  if (exists("login.details"))
  {
    ds.test_env$local.values.2 <- read.csv("data_files/DATASET2.csv", header = TRUE)
    if (login.details$driver == "OpalDriver")
    {
      #connecting to the servers
      ds.test_env$server   <- c("study2")
      ds.test_env$table    <- c("TESTING.DATASET2")
      ds.test_env$login.data <- ds.build.login.data.frame(ds.test_env$server,
                                                          login.details$get_ip_addresses(1),
                                                          ds.test_env$table,
                                                          login.details$get_users(1),
                                                          login.details$get_passwords(1),
                                                          login.details$get_ssl_options(1),
                                                          login.details$get_drivers(1))
      
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study2")
    } 
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY')
  }
}

# Connect to one server and the three datasets. One local variables named ds.test_env$local.values.2is created.
init.dataset.1 <- function(ds.test_env = NULL)
{
  if (exists("login.details"))
  {
    ds.test_env$local.values.1 <- read.csv("data_files/DATASET1.csv", header = TRUE)
    if (login.details$driver == "OpalDriver")
    {
      #connecting to the servers
      ds.test_env$server   <- c("study1")
      ds.test_env$table    <- c("TESTING.DATASET1")
      ds.test_env$login.data <- ds.build.login.data.frame(ds.test_env$server,
                                                          login.details$get_ip_addresses(1),
                                                          ds.test_env$table,
                                                          login.details$get_users(1),
                                                          login.details$get_passwords(1),
                                                          login.details$get_ssl_options(1),
                                                          login.details$get_drivers(1))
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study1")
    }  
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY')
  }
}

log.in.data.server <- function(ds.test_env = NULL)
{
  ds.test_env$connections <- datashield.login(logins=ds.test_env$login.data, 
                                              assign=TRUE,
                                              variables=ds.test_env$stats.var)
  return(ds.test_env)
  
}


log.out.data.server <- function(connections= NULL)
{
  if (!is.null(connections))
  {
    datashield.logout(connections)
  }
  rm(list = ls())
  gc()
}

connect.all.datasets <- function(ds.test_env = NULL)
{
  
  init.all.datasets(ds.test_env)
  ds.test_env <- log.in.data.server(ds.test_env)
  return(ds.test_env$connections)
}

connect.dataset.1 <- function(ds.test_env = NULL)
{
  
  init.dataset.1(ds.test_env)
  ds.test_env <- log.in.data.server(ds.test_env)
  return(ds.test_env$connections)
}

connect.dataset.2 <- function(ds.test_env = NULL)
{
  
  init.dataset.2(ds.test_env)
  ds.test_env <- log.in.data.server(ds.test_env)
  return(ds.test_env$connections)
}

connect.dataset.3 <- function(ds.test_env = NULL)
{
  #source("connection_to_datasets/login_details.R")
  init.dataset.3(ds.test_env)
  ds.test_env <- log.in.data.server(ds.test_env)

  return(ds.test_env$connections)
}

disconnect.all.datasets <- function(connections)
{
  log.out.data.server(connections)
}

disconnect.dataset.1 <- function(connections)
{
  log.out.data.server(connections)
}

disconnect.dataset.2 <- function(connections)
{
  log.out.data.server(connections)
}

disconnect.dataset.3 <- function(connections)
{
  log.out.data.server(connections)
}
