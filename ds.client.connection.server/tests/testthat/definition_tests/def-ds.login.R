
.test.no.login.info <- function()
{
  ds.login(NULL)
    expect_error(.make.connection(NULL))
    expect_true(is.null(ds.login(NULL)))
}

.test.incorrect.format <- function(some.server,some.urls,some.users,some.passwords,some.tables)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  
  expect_error(.make.connection(ds.build.login.data.frame(server,url,table,user,password),assign = FALSE,table))
}

.test.empty <- function()
{
  
  expect_error(.make.connection(ds.build.login.data.frame(c(),c(),c(),c(),c()),assign = FALSE,table))
  #expect_true(is.null(suppressMessages(ds.login(ds.build.login.data.frame(c(),c(),c(),c(),c()),assign = FALSE,table))))
}

.test.http.connection.multiple <- function()
{
  server <-   c('study1','study2', 'study3')
  url <-  c(ds.test_env$ping_address,ds.test_env$ping_address,ds.test_env$ping_address)
  user <-  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
  password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
  table <-  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
  login <- data.frame(server,url,user,password,table)
  connection <- ds.login(login,assign = FALSE,table)
  expect_true(length(connection) == length(server))
}

.test.http.connection.single <- function()
{
  server <-   c('study1')
  url <-  c(ds.test_env$ping_address)
  user <-  c(ds.test_env$user_1)
  password <- c(ds.test_env$password_1)
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
  server <-   c('study1','study2', 'study3')
  url <-  c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
  user <-  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
  password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
  table <-  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
  options.ssl <-  c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)")
  drivers <- c("OpalDriver","OpalDriver","OpalDriver")
  login <- ds.build.login.data.frame(server,url,user,password,table,options.ssl,drivers)
  connection <- ds.login(login,assign = FALSE,table)
  expect_true(!is.null(connection))
  
}

.test.https.connection.single <- function()
{
  server <-   c('study1')
  url <- c(ds.test_env$ip_address_1)
  user <-  c(ds.test_env$user_1)
  password <- c(ds.test_env$password_1)
  table <-  c("TESTING.DATASET1")
  options.ssl <-  c("c(ssl.verifyhost=0,ssl.verifypeer=0)")
  drivers <- c("OpalDriver")
  login <- ds.build.login.data.frame(server,url,user,password,table,options.ssl,drivers)
  connection <- ds.login(login,assign = FALSE,table)
  expect_true(!is.null(connection))
  
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
  expect_error(.make.connection(ds.build.login.data.frame(server,url,table,user,password),assign = FALSE,table))
  expect_true(is.null(connection))
}