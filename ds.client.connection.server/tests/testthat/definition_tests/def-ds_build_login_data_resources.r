.test.correct.data <- function(some.servers,some.urls,some.users,some.passwords,some.tables,some.options,some.drivers)
{
  login.data <- ds.build.login.data.resources(some.servers,some.urls,some.tables,some.users,some.passwords,some.options,some.drivers)
  .test.on.login.data(login.data)
}

.test.on.login.data <- function(login.data)
{
  expect_true(length(login.data) == 9)
  
  expect_that(login.data,is_a('data.frame'))
  expect_that(colnames(login.data)[1], equals('server'))
  expect_that(colnames(login.data)[2], equals("url"))
  expect_that(colnames(login.data)[3], equals('table'))
  expect_that(colnames(login.data)[4], equals('resource'))
  expect_that(colnames(login.data)[5], equals('driver'))
  expect_that(colnames(login.data)[6], equals('user'))
  expect_that(colnames(login.data)[7], equals('password'))
  expect_that(colnames(login.data)[8], equals('token'))
  expect_that(colnames(login.data)[9], equals('options'))
}

.test.incorrect.format <- function(some.server,some.urls,some.users,some.passwords,some.tables,some.options,some.drivers)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  options.ssl <- some.options
  drivers <- some.drivers
  expect_error(.build.data.object(server,url,table,user,password,option.ssl,drivers))
  #expect_true(is.null(ds.build.login.data.resources(server,url,table,user,password,option.ssl,drivers)))

}

.test.incorrrect.url <- function(some.server,some.urls,some.users,some.passwords,some.tables,some.options,some.drivers)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  options.ssl <- some.options
  drivers <- some.drivers
  
  expect_error(build.data.frame(server,url,table,user,password,option.ssl,drivers))
  expect_true(is.null(ds.build.login.data.frame(server,url,table,user,password,option.ssl,drivers)))
  
}

.test.empty<- function()
{
  
  expect_true(is.null(ds.build.login.data.frame(c(),c(),c(),c(),c(),c(),c())))
  expect_error(build.data.frame(server,url,table,user,password,option.ssl,drivers))
}



build.all.datasets <- function()
{
  return(c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3"))
}

build.all.ssl <- function()
{
  return(c("c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)","c(ssl.verifyhost=0,ssl.verifypeer=0)"))
}


