
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