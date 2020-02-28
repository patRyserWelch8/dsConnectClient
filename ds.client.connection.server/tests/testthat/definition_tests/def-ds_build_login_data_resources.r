.test.correct.data <- function(some.server,some.urls,some.users,some.passwords,some.tables,some.options,some.drivers)
{
  server <-  some.server
  url <- some.urls
  user <- some.users
  password <- some.passwords
  resources <- some.tables
  options.ssl <- some.options
  drivers <- some.drivers
  
  login.data <- ds.build.login.data.resources(server,url,table,user,password,options.ssl, drivers)
  print(login.data)
  expect_true(is.data.frame(login.data))
  
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
  expect_error(build.data.frame(server,url,table,user,password,option.ssl,drivers))
  expect_true(is.null(ds.build.login.data.frame(server,url,table,user,password,option.ssl,drivers)))

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


