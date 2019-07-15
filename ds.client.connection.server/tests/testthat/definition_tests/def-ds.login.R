
.test.no.login.info <- function()
{
    expect_error(ds.login(NULL),"ERR:003")
}

.test.incorrect.format <- function(some.server,some.urls,some.users,some.passwords,some.tables)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  expect_error(ds.login(ds.build.login.data.frame.o(server,url,table,user,password),assign = FALSE,table),"ERR:001")
  
  
  
}

.test.empty <- function()
{
  expect_error(ds.login(ds.build.login.data.frame.o(c(),c(),c(),c(),c()),assign = FALSE,table),"ERR:004")
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
 # c <- ds.login(ds.build.login.data.frame.o(server,url,table,user,password),assign = FALSE,table)
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
  expect_true(length(connection) == length(server))
}


.test.https.connection.multiple <- function()
{
  server <-   c('study1','study2', 'study3')
  url <-  c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
  user <-  c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
  password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
  table <-  c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
  connection <- ds.login(login,assign = FALSE,table)
  expect_true(length(connection) == length(server))
  
}

.test.https.connection.single <- function()
{
  server <-   c('study1')
  url <- c(ds.test_env$ip_address_1)
  user <-  c(ds.test_env$user_1)
  password <- c(ds.test_env$password_1)
  table <-  c("TESTING.DATASET1")
  connection <-ds.login(ds.build.login.data.frame.o(server,url,table,user,password),assign = FALSE,table)
  expect_true(length(connection) == length(server))
}
