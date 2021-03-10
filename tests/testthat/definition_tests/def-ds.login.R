source("connection_to_datasets/init_local_settings.R")
source("connection_to_datasets/init_all_datasets.R")


.test.no.login.info <- function()
{
  ds.login(NULL)
  expect_error(dsli.make.connection(NULL))
  expect_true(is.null(ds.login(NULL)))
}

.test.incorrect.format <- function(some.server,some.urls,some.users,some.passwords,some.tables)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  
  expect_error(dsli.make.connection(ds.build.login.data.frame(server,url,table,user,password),assign = FALSE,table))
}

.test.empty <- function()
{
  
  expect_error(dsli.make.connection(ds.build.login.data.frame(c(),c(),c(),c(),c()),assign = FALSE,table))
  expect_true(is.null(suppressMessages(ds.login(ds.build.login.data.frame(c(),c(),c(),c(),c()),assign = FALSE,table))))
}

.test.http.connection.multiple <- function()
{
  
  
  server <-   c('study1','study2', 'study3')
  url <-  c(login.details$ping_address,login.details$ping_address,login.details$ping_address)
  user <-  c(login.details$user_1,login.details$user_2,login.details$user_3)
  password <- c(login.details$password_1,login.details$password_2,login.details$password_3)
  table <-  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
  login <- data.frame(server,url,user,password,table)
  login <- ds.build.login.data.frame(server,url,user,password,table,options.ssl,drivers)
  connection <- ds.login(login,assign = FALSE,table)
  expect_true(length(connection) == length(server))
}

.test.http.connection.single <- function()
{
  server <-   c('study1')
  url <-  c(login.details$ping_address)
  user <-  c(login.details$user_1)
  password <- c(login.details$password_1)
  table <-  c("TESTING.DATASET1")
  login <- data.frame(server,url,user,password,table)
  connection <- ds.login(login,assign = FALSE,table)
  options.ssl <-  c("c(ssl.verifyhost=0,ssl.verifypeer=0)")
  drivers <- c("OpalDriver")
  login <- ds.build.login.data.frame(server,url,user,password,table,options.ssl,drivers)
  connection <- ds.login(login,assign = FALSE,table)
  expect_true(is.null(connection))
}


.test.https.connection.multiple <- function()
{
  
  #use this function set the relevant information
  stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                    'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                    'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                    'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY')
  ds.test_env <- init.all.datasets(list())
  server <-   c('study1','study2', 'study3')
  url <-  c(login.details$ping_address,login.details$ping_address,login.details$ping_address)
  user <-  c(login.details$user_1,login.details$user_2,login.details$user_3)
  password <- c(login.details$password_1,login.details$password_2,login.details$password_3)
  table <-  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
  login <- ds.build.login.data.frame(server,url,user,password,table,options.ssl,drivers)
  connection <- ds.login(login.data,assign = FALSE,stat.vars, "D")
  print(connection)
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  
}

.test.https.connection.single <- function()
{
  
  init.dataset.1()
  connection <- dsli.make.connection(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  
  init.dataset.2()
  connection <- dsli.make.connection(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  
  init.dataset.3()
  connection <- dsli.make.connection(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(!is.null(connection))
  ds.logout(connection,NULL)
  
}


.test.https.incorrect.URL <- function()
{
  server <-   c('study1')
  url <- c("https://my.website")
  user <-  c(ds.test_env$user_1)
  password <- c(ds.test_env$password_1)
  table <-  c("TESTING.DATASET1")
  options.ssl <-  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)")
  drivers <- c("OpalDriver","OpalDriver","OpalDriver")
  
  
  login <- data.frame(server,url,user,password,table)
  connection <- ds.login(login,assign = FALSE,table)
  expect_error(dsli.make.connection(ds.build.login.data.frame(server,url,table,user,password),assign = FALSE,table))
  expect_true(is.null(connection))
}