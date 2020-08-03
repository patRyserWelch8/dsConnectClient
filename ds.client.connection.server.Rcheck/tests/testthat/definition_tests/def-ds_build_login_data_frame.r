
.test.correct.data <- function(some.server,some.urls,some.users,some.passwords,some.tables,some.options,some.drivers)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  options.ssl <- some.options
  drivers <- some.drivers
  
  login.info <- ds.build.login.data.frame(server,url,table,user,password,options.ssl, drivers)


  expect_true(length(login.info) == 9)
  expect_that(login.info,is_a('data.frame'))
  expect_that(colnames(login.info)[1], equals('server'))
  expect_that(colnames(login.info)[2], equals('url'))
  expect_that(colnames(login.info)[3], equals('table'))
  expect_that(colnames(login.info)[4], equals('resource'))
  expect_that(colnames(login.info)[5], equals('driver'))
  expect_that(colnames(login.info)[6], equals('user'))
  expect_that(colnames(login.info)[7], equals('password'))
  expect_that(colnames(login.info)[8], equals('token'))
  expect_that(colnames(login.info)[9], equals('options'))
}

.test.incorrect.format <- function(some.server,
                                   some.urls,
                                   some.tables,
                                   some.users,
                                   some.passwords,
                                   some.options,
                                   some.drivers)
{
  print(some.users)
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  options.ssl <- some.options
  drivers <- some.drivers
  expect_error(.build.data.frame(server,url,table,user,password,option.ssl,drivers))
  #expect_true(is.null(ds.build.login.data.frame(server,url,table,user,password,option.ssl,drivers)))
  ds.build.login.data.frame(server,url,table,user,password,option.ssl,drivers)

}

.test.incorrrect.url <- function(some.server = NULL ,some.urls = NULL,some.users = NULL,
                                 some.passwords = NULL,some.tables = NULL,some.options = NULL,
                                 some.drivers = NULL)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  table <- some.tables
  options.ssl <- some.options
  drivers <- some.drivers
  
  expect_error(.build.data.frame(server,url,table,user,password,option.ssl,drivers))
  ds.build.login.data.frame(server,url,table,user,password,option.ssl,drivers)
  
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


