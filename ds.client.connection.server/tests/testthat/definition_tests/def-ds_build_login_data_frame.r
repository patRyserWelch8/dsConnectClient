.test.correct.data <- function(some.server,some.urls,some.users,some.passwords,some.tables)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  login.info <- ds.build.login.data.frame.o(server,url,table,user,password)
  
  expect_true(length(login.info) == 5)
  expect_that(login.info,is_a('data.frame'))
  expect_that(colnames(login.info)[1], equals('server'))
  expect_that(colnames(login.info)[2], equals('url'))
  expect_that(colnames(login.info)[3], equals('user'))
  expect_that(colnames(login.info)[4], equals('password'))
  expect_that(colnames(login.info)[5], equals('table'))
}

.test.incorrrect.url <- function(some.server,some.urls,some.users,some.passwords,some.tables)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  expect_error(ds.build.login.data.frame.o(server,url,table,user,password),"ERR:002")
}

.test.incorrect.format<- function(some.server,some.urls,some.users,some.passwords,some.tables)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  expect_error(ds.build.login.data.frame.o(server,url,table,user,password),"ERR:001")
}


